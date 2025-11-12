open ReForm
open! Jest
open TestUtils

module Model = FormFixture.Model
module Form = ReForm.Make(Model)

let baseSchema = {
  module Validation = Form.Validation
  open Validation
  schema([nonEmpty(~error="Name required", Model.Name), email(~error="Email invalid", Model.Email)])
}

let schemaWithTagsValidator = {
  module Validation = Form.Validation
  open Validation
  let tagValidator = (values: Model.state) => {
    let queue = Belt.MutableQueue.make()
    let rec loop = index =>
      if index >= Belt.Array.length(values.tags) {
        ()
      } else {
        switch values.tags->Belt.Array.get(index) {
        | Some(tag) if tag == "" =>
          let errorRecord: ReSchema.childFieldError = {
            error: "Tag cannot be empty",
            index,
            name: "tags",
          }
          queue->Belt.MutableQueue.add(errorRecord)
        | _ => ()
        }
        loop(index + 1)
      }

    loop(0)
    let emptyTags = queue->Belt.MutableQueue.toArray

    if emptyTags->Belt.Array.length > 0 {
      ReSchema.NestedErrors(emptyTags)
    } else {
      ReSchema.Valid
    }
  }

  schema([
    nonEmpty(~error="Name required", Model.Name),
    email(~error="Email invalid", Model.Email),
    custom(tagValidator, Model.Tags),
  ])
}

module ReactTestRenderer = {
  type renderer
  @module("react-test-renderer") external create: React.element => renderer = "create"
  @module("react-test-renderer") external act: (unit => 'a) => 'a = "act"
}

module Harness = {
  type callbacks = {
    onReady: Form.api => unit,
    onSubmit: Form.onSubmitAPI => unit,
    onSubmitFail: Form.onSubmitAPI => unit,
  }

  @react.component
  let make = (~callbacks, ~schema, ~validationStrategy: option<Form.validationStrategy>=?, ()) => {
    let api = Form.use(
      ~initialState=Model.emptyState,
      ~schema,
      ~onSubmit=payload => {
        callbacks.onSubmit(payload)
        None
      },
      ~onSubmitFail=payload => callbacks.onSubmitFail(payload),
      ~validationStrategy?,
      (),
    )

    React.useLayoutEffect(() => {
      callbacks.onReady(api)
      None
    }, [api])

    React.null
  }
}

let expectApi = box =>
  switch MutableBox.get(box) {
  | None => failwith("Form API was not initialised")
  | Some(api) => api
  }

type runContext = {
  apiBox: MutableBox.t<option<Form.api>>,
  submitCount: MutableBox.t<int>,
  lastSubmit: MutableBox.t<option<Form.onSubmitAPI>>,
  failCount: MutableBox.t<int>,
  lastFail: MutableBox.t<option<Form.onSubmitAPI>>,
}

let renderForm = (~schema, ~validationStrategy: option<Form.validationStrategy>=?) => {
  let context: runContext = {
    apiBox: MutableBox.make(None),
    submitCount: MutableBox.make(0),
    lastSubmit: MutableBox.make(None),
    failCount: MutableBox.make(0),
    lastFail: MutableBox.make(None),
  }

  ReactTestRenderer.act(() => {
    ignore(
      ReactTestRenderer.create(
        <Harness
          callbacks={
            onReady: api => MutableBox.set(context.apiBox, Some(api)),
            onSubmit: payload => {
              MutableBox.update(context.submitCount, count => count + 1)
              MutableBox.set(context.lastSubmit, Some(payload))
            },
            onSubmitFail: payload => {
              MutableBox.update(context.failCount, count => count + 1)
              MutableBox.set(context.lastFail, Some(payload))
            },
          }
          schema
          ?validationStrategy
        />,
      ),
    )
  })
  context
}

describe("ReForm.Make.use", () => {
  it("validates on change and submits successfully after correcting errors", () => {
    let ctx = renderForm(~schema=baseSchema)

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).handleChange(Model.Name, "")
        expectApi(ctx.apiBox).handleChange(Model.Email, "invalid-email")
        ()
      },
    )

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).validateField(Form.ReSchema.Field(Model.Name))
        expectApi(ctx.apiBox).validateField(Form.ReSchema.Field(Model.Email))
        ()
      },
    )

    let apiAfterInvalid = expectApi(ctx.apiBox)
    expect(apiAfterInvalid.isDirty)->toBe(true)->ignore
    expect(apiAfterInvalid.formState == Dirty)->toBe(true)->ignore

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).submit()
        ()
      },
    )
    expect(MutableBox.get(ctx.submitCount))->toBe(0)->ignore
    expect(MutableBox.get(ctx.failCount))->toBe(1)->ignore
    switch MutableBox.get(ctx.lastFail) {
    | Some(payload) => {
        let states = payload.state.fieldsState
        switch states->Belt.Array.get(0) {
        | Some((_, Error(message))) => expect(message)->toBe("Name required")->ignore
        | _ => expect(true)->toBe(false)->ignore
        }
      }
    | None => expect(true)->toBe(false)->ignore
    }

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).handleChange(Model.Name, "Ada Lovelace")
        expectApi(ctx.apiBox).handleChange(Model.Email, "ada@rescript-lang.org")
        ()
      },
    )

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).submit()
        ()
      },
    )
    let _apiOnSubmit = expectApi(ctx.apiBox)
    expect(MutableBox.get(ctx.submitCount))->toBe(1)->ignore
    expect(MutableBox.get(ctx.failCount))->toBe(1)->ignore

    switch MutableBox.get(ctx.lastSubmit) {
    | Some(payload) =>
      switch payload.state.formState {
      | Submitting
      | Valid =>
        expect(true)->toBe(true)->ignore
      | _ => expect(true)->toBe(false)->ignore
      }
    | None => expect(true)->toBe(false)->ignore
    }
  })

  it("supports on-demand validation flows", () => {
    let ctx = renderForm(~schema=baseSchema, ~validationStrategy=Form.OnDemand)

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).handleChange(Model.Name, "")
        ()
      },
    )
    let apiAfterChange = expectApi(ctx.apiBox)
    expect(apiAfterChange.getFieldState(Form.ReSchema.Field(Model.Name)) == Pristine)
    ->toBe(true)
    ->ignore

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).validateField(Form.ReSchema.Field(Model.Name))
        ()
      },
    )
    switch expectApi(ctx.apiBox).getFieldState(Form.ReSchema.Field(Model.Name)) {
    | Error(message) => expect(message)->toBe("Name required")->ignore
    | _ => expect(true)->toBe(false)->ignore
    }

    ReactTestRenderer.act(
      () => {
        let fields = {
          let arr = Array.make(~length=1, Form.ReSchema.Field(Model.Email))
          arr
        }
        ignore(expectApi(ctx.apiBox).validateFields(fields))
        ()
      },
    )
    expect(MutableBox.get(ctx.failCount))->toBe(0)->ignore

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).resetForm()
        ()
      },
    )
    expect(expectApi(ctx.apiBox).formState == Pristine)->toBe(true)->ignore
  })

  it("manages array helpers and nested error reporting", () => {
    let ctx = renderForm(~schema=schemaWithTagsValidator, ~validationStrategy=Form.OnChange)

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).arrayPush(Model.Tags, "initial")
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).arrayUpdateByIndex(~field=Model.Tags, ~index=0, "updated")
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).arrayPush(Model.Tags, "")
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).arrayRemoveBy(Model.Tags, tag => tag == "updated")
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).arrayRemoveByIndex(Model.Tags, 0)
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        let tags = {
          let arr = Array.make(~length=2, "tag-a")
          arr[1] = ""
          arr
        }
        expectApi(ctx.apiBox).setValues(
          values => {
            ...values,
            tags,
          },
        )
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).setFieldValue(Model.Accepted, true, ~shouldValidate=false, ())
        ()
      },
    )
    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).handleChangeWithCallback(Model.Name, value => value ++ "!")
        ()
      },
    )

    ReactTestRenderer.act(
      () => {
        let fields = {
          let arr = Array.make(~length=2, Form.ReSchema.Field(Model.Tags))
          arr[1] = Form.ReSchema.Field(Model.Name)
          arr
        }
        ignore(expectApi(ctx.apiBox).validateFields(fields))
        ()
      },
    )

    let apiAfterValidation = expectApi(ctx.apiBox)
    switch apiAfterValidation.getFieldState(Form.ReSchema.Field(Model.Tags)) {
    | NestedErrors(errors) =>
      expect(errors->Belt.Array.length)->toBe(1)->ignore
      expect(apiAfterValidation.getNestedFieldError(Form.ReSchema.Field(Model.Tags), 1))
      ->toEqual(None)
      ->ignore
      expect(apiAfterValidation.getNestedFieldError(Form.ReSchema.Field(Model.Tags), 0))
      ->toEqual(Some("Tag cannot be empty"))
      ->ignore
    | _ => expect(true)->toBe(false)->ignore
    }

    ReactTestRenderer.act(
      () => {
        expectApi(ctx.apiBox).raiseSubmitFailed(Some("Network issue"))
        ()
      },
    )
    expect(expectApi(ctx.apiBox).formState)
    ->toStrictEqual(SubmitFailed(Some("Network issue")))
    ->ignore
  })
})

describe("ReForm.Field", () => {
  it("uses fallback when no provider is present", () => {
    let renderCalled = MutableBox.make(false)

    ReactTestRenderer.act(
      () => {
        ignore(
          ReactTestRenderer.create(
            <Form.Field
              field=Model.Name
              render={_interface => {
                MutableBox.set(renderCalled, true)
                React.null
              }}
              renderOnMissingContext={<React.fragment />}
            />,
          ),
        )
      },
    )

    expect(MutableBox.get(renderCalled))->toBe(false)->ignore
  })

  module FieldProviderHarness = {
    @react.component
    let make = (~render) => {
      let api = Form.use(
        ~initialState=Model.emptyState,
        ~schema=baseSchema,
        ~onSubmit=_payload => None,
        ~onSubmitFail=_payload => (),
        ~validationStrategy=Form.OnChange,
        (),
      )

      let (initialised, setInitialised) = React.useState(() => false)

      React.useLayoutEffect(() => {
        if initialised == false {
          api.setFieldValue(Model.Name, "Context value", ())
          setInitialised(_ => true)
        }
        None
      }, [initialised])

      <Form.Provider value={Some(api)}>
        <Form.Field field=Model.Name render />
      </Form.Provider>
    }
  }

  it("renders the field interface from context", () => {
    let valueBox = MutableBox.make(None)
    let stateBox = MutableBox.make(None)

    let renderInterface = (interface: Form.fieldInterface<string>) => {
      MutableBox.set(valueBox, Some(interface.value))
      MutableBox.set(stateBox, Some(interface.state))
      React.null
    }

    ReactTestRenderer.act(
      () => {
        ignore(ReactTestRenderer.create(<FieldProviderHarness render=renderInterface />))
      },
    )

    expect(MutableBox.get(valueBox))->toEqual(Some("Context value"))->ignore
    expect(MutableBox.get(stateBox))->toEqual(Some(Valid))->ignore
  })
})
