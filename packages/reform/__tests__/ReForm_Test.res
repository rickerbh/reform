open ReForm
open! Jest
open TestUtils

module Model = FormFixture.Model
module Form = ReForm.Make(Model)

let schema = {
  module Validation = Form.Validation
  open Validation
  schema([nonEmpty(~error="Name required", Model.Name), email(~error="Email invalid", Model.Email)])
}

describe("ReForm.Make.getInitialFieldsState", () => {
  it("initialises every schema field as pristine", () => {
    let initialStates = Form.getInitialFieldsState(schema)
    let expected = [
      (Form.ReSchema.Field(Model.Name), (Pristine: fieldState)),
      (Form.ReSchema.Field(Model.Email), Pristine),
    ]

    expect(initialStates)->toEqual(expected)->ignore
  })
})

describe("ReForm.Helpers.handleChange", () => {
  it("forwards the event target value to the provided callback", () => {
    let store: MutableBox.t<option<string>> = MutableBox.make(None)
    let event = FakeEvent.change("Grace Hopper")

    ReForm.Helpers.handleChange(value => MutableBox.set(store, Some(value)), event)

    expect(MutableBox.get(store))->toEqual(Some("Grace Hopper"))->ignore
  })
})

describe("ReForm.Helpers.handleSubmit", () => {
  it("prevents the default browser behaviour before invoking the submitter", () => {
    let prevented = MutableBox.make(false)
    let submitted = MutableBox.make(false)
    let event = FakeEvent.submit(() => MutableBox.set(prevented, true))

    ReForm.Helpers.handleSubmit(() => MutableBox.set(submitted, true), event)

    expect(MutableBox.get(prevented))->toBe(true)->ignore
    expect(MutableBox.get(submitted))->toBe(true)->ignore
  })
})
