module MutableBox = {
  type t<'a> = {mutable value: 'a}
  let make = value => {value: value}
  let set = (box, value) => box.value = value
  let update = (box, fn) => box.value = fn(box.value)
  let get = box => box.value
}

module FakeEvent = {
  type changeTarget
  @obj external makeChangeTarget: (~value: string, unit) => changeTarget = ""
  @obj external makeChangeEvent: (~target: changeTarget, unit) => ReactEvent.Form.t = ""
  @obj
  external makeSubmitEvent: (~preventDefault: unit => unit, unit) => ReactEvent.Synthetic.t = ""

  let change = value => makeChangeEvent(~target=makeChangeTarget(~value, ()), ())
  let submit = callback => makeSubmitEvent(~preventDefault=callback, ())
}
