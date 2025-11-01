module Model = {
  type state = {
    name: string,
    email: string,
    tags: array<string>,
    accepted: bool,
  }

  type rec field<_> =
    | Name: field<string>
    | Email: field<string>
    | Tags: field<array<string>>
    | Accepted: field<bool>

  let set:
    type value. (state, field<value>, value) => state =
    (state, field, value) =>
      switch field {
      | Name => {...state, name: value}
      | Email => {...state, email: value}
      | Tags => {...state, tags: value}
      | Accepted => {...state, accepted: value}
      }

  let get:
    type value. (state, field<value>) => value =
    (state, field) =>
      switch field {
      | Name => state.name
      | Email => state.email
      | Tags => state.tags
      | Accepted => state.accepted
      }

  let emptyTags: array<string> = Array.make(~length=0, "")

  let emptyState: state = {
    name: "",
    email: "",
    tags: emptyTags,
    accepted: false,
  }
}
