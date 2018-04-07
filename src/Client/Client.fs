module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma
open Fulma.Layouts
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Components
open Fulma.BulmaClasses

open Fulma.BulmaClasses.Bulma
open Fulma.BulmaClasses.Bulma.Properties
open Fulma.Extra.FontAwesome

type Model = Counter option

type Msg =
| Increment
| Decrement
| Init of Result<Counter, exn>



let init () : Model * Cmd<Msg> =
  let model = None
  let cmd =
    Cmd.ofPromise 
      (fetchAs<int> "/api/init") 
      [] 
      (Ok >> Init) 
      (Error >> Init)
  model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model' =
    match model,  msg with
    | Some x, Increment -> Some (x + 1)
    | Some x, Decrement -> Some (x - 1)
    | None, Init (Ok x) -> Some x
    | _ -> None
  model', Cmd.none

let safeComponents =
  let intersperse sep ls =
    List.foldBack (fun x -> function
      | [] -> [x]
      | xs -> x::sep::xs) ls []

  let components =
    [
      "Saturn", "https://saturnframework.github.io/docs/"
      "Fable", "http://fable.io"
      "Elmish", "https://fable-elmish.github.io/"
      "Fulma", "https://mangelmaxime.github.io/Fulma" 
      "Bulma\u00A0Templates", "https://dansup.github.io/bulma-templates/"
    ]
    |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
    |> intersperse (str ", ")
    |> span [ ]

  p [ ]
    [ strong [] [ str "SAFE Template" ]
      str " powered by: "
      components ]

let show = function
| Some x -> string x
| None -> "Loading..."

let navBrand =
  Navbar.navbar [ Navbar.Color IsWhite ]
    [ Container.container [ ]
        [ Navbar.Brand.div [ ]
            [ Navbar.Item.a [ Navbar.Item.CustomClass "brand-text" ]
                [ str "SAFE Admin" ]
              Navbar.burger [ ] 
                [ span [ ] [ ]
                  span [ ] [ ]
                  span [ ] [ ] ] ]
          Navbar.menu [ ]
            [ Navbar.Start.div [ ]
                [ Navbar.Item.a [ ]
                    [ str "Home" ]
                  Navbar.Item.a [ ]
                    [ str "Orders" ]
                  Navbar.Item.a [ ]
                    [ str "Payments" ]
                  Navbar.Item.a [ ]
                    [ str "Exceptions" ] ] ] ] ]

let menu =
  Menu.menu [ ]
    [ Menu.label [ ]
        [ str "General" ]
      Menu.list [ ]
        [ Menu.item [ ]
            [ str "Dashboard" ]
          Menu.item [ ]
            [ str "Customers" ] ]
      Menu.label [ ]
        [ str "Administration" ]
      Menu.list [ ]
        [ Menu.item [ ]
            [ str "Team Settings" ]
          li [ ]
            [ a [ ]
                [ str "Manage Your Team" ]
              Menu.list [ ]
                [ Menu.item [ ]
                    [ str "Members" ]
                  Menu.item [ ]
                    [ str "Plugins" ]
                  Menu.item [ ]
                    [ str "Add a member" ] ] ]
          Menu.item [ ]
            [ str "Invitations" ]
          Menu.item [ ]
            [ str "Cloud Storage Environment Settings" ]
          Menu.item [ ]
            [ str "Authentication" ] ]
      Menu.label [ ]
        [ str "Transactions" ]
      Menu.list [ ]
        [ Menu.item [ ]
            [ str "Payments" ]
          Menu.item [ ]
            [ str "Transfers" ]
          Menu.item [ ]
            [ str "Balance" ] ] ]

let breadcrump =
  Breadcrumb.breadcrumb [ ]
    [ Breadcrumb.item [ ]
        [ a [ ] [ str "Bulma" ] ]
      Breadcrumb.item [ ]
        [ a [ ] [ str "Templates" ] ]
      Breadcrumb.item [ ]
        [ a [ ] [ str "Examples" ] ]
      Breadcrumb.item [ Breadcrumb.Item.IsActive true ]
        [ a [ ] [ str "Admin" ] ] ]

let hero =
  Hero.hero [ Hero.CustomClass "is-info welcome is-small" ]
    [ Hero.body [ ]
        [ Container.container [ ]
            [ h1 [ Class "title" ]
                [ str "Hello, Admin." ]
              h2 [ Class "subtitle" ]
                [ safeComponents ] ] ] ] 

let info =
  section [ Class "info-tiles" ]
    [ Tile.ancestor [ Tile.CustomClass Alignment.HasTextCentered ]
        [ Tile.parent [ ]
            [ article [ Class "tile is-child box" ]
                [ p [ Class "title" ] [ str "439k" ]
                  p [ Class "subtitle" ] [ str "Users" ] ] ]
          Tile.parent [ ]
            [ article [ Class "tile is-child box" ]
                [ p [ Class "title" ] [ str "59k" ]
                  p [ Class "subtitle" ] [ str "Products" ] ] ]
          Tile.parent [ ]
            [ article [ Class "tile is-child box" ]
                [ p [ Class "title" ] [ str "3.4k" ]
                  p [ Class "subtitle" ] [ str "Open Orders" ] ] ]
          Tile.parent [ ]
            [ article [ Class "tile is-child box" ]
                [ p [ Class "title" ] [ str "19" ]
                  p [ Class "subtitle" ] [ str "Exceptions" ] ] ] ] ]

let counter (model : Model) (dispatch : Msg -> unit) =
  Form.Field.div [ Form.Field.IsGrouped ] 
    [ Form.Control.p [ Form.Control.CustomClass "is-expanded"] 
        [ Form.Input.text
            [ Form.Input.Disabled true
              Form.Input.Value (show model) ] ]
      Form.Control.p [ ]
        [ Button.a 
            [ Button.Color IsInfo
              Button.OnClick (fun _ -> dispatch Increment) ]
            [ str "+" ] ]
      Form.Control.p [ ]
        [ Button.a 
            [ Button.Color IsInfo
              Button.OnClick (fun _ -> dispatch Decrement) ]
            [ str "-" ] ] ]

let columns (model : Model) (dispatch : Msg -> unit) =
  Columns.columns [ ]
    [ Column.column [ Column.Width (Column.All, Column.Is6) ]
        [ Card.card [ CustomClass "events-card" ]
            [ Card.header [ ]
                [ Card.Header.title [ ]
                    [ str "Events" ]
                  Card.Header.icon [ ]
                    [ Icon.faIcon [ ]
                        [ Fa.icon Fa.I.AngleDown ] ] ]
              div [ Class "card-table" ]
                [ Content.content [ ]
                    [ Table.table 
                        [ Table.IsFullwidth
                          Table.IsStripped ]
                        [ tbody [ ]
                            [ for _ in 1..10 ->
                                tr [ ]
                                  [ td [ Style [ Width "5%" ] ]
                                      [ Icon.faIcon
                                          [ ]
                                          [ Fa.icon Fa.I.BellO ] ]
                                    td [ ]
                                      [ str "Lorem ipsum dolor aire" ]
                                    td [ ]
                                      [ Button.a 
                                          [ Button.Size IsSmall
                                            Button.Color IsPrimary ]
                                          [ str "Action" ] ] ] ] ] ] ]
              Card.footer [ ]
                [ Card.Footer.item [ ]
                    [ str "View All" ] ] ] ]
      Column.column [ Column.Width (Column.All, Column.Is6) ]
        [ Card.card [ ]
            [ Card.header [ ]
                [ Card.Header.title [ ]
                    [ str "Inventory Search" ]
                  Card.Header.icon [ ]
                    [ Icon.faIcon [ ]
                        [ Fa.icon Fa.I.AngleDown ] ] ]
              Card.content [ ]
                [ Content.content [ ]
                    [ Control.div 
                        [ Control.HasIconLeft
                          Control.HasIconRight ]
                        [ Input.text 
                            [ Input.Size IsLarge ]
                          Icon.faIcon 
                            [ Icon.Size IsMedium
                              Icon.IsLeft ]
                            [ Fa.icon Fa.I.Search ]
                          Icon.faIcon 
                            [ Icon.Size IsMedium
                              Icon.IsRight ]
                            [ Fa.icon Fa.I.Check ] ] ] ] ]
          Card.card [ ]
            [ Card.header [ ]
                [ Card.Header.title [ ]
                    [ str "Counter" ]
                  Card.Header.icon [ ]
                    [ Icon.faIcon [ ]
                        [ Fa.icon Fa.I.AngleDown ] ] ]
              Card.content [ ]
                [ Content.content [ ]
                    [ counter model dispatch ] ] ] ] ]

let view (model : Model) (dispatch : Msg -> unit) =
  div [ ]
    [ navBrand
      Container.container [ ]
        [ Columns.columns [ ]
            [ Column.column [ Column.Width (Column.All, Column.Is3) ]
                [ menu ]
              Column.column [ Column.Width (Column.All, Column.Is9) ]
                [ breadcrump
                  hero
                  info
                  columns model dispatch ] ] ] ]

  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
