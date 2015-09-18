module Bingo where

import Debug
import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
import String exposing (toUpper, repeat, trimRight)

--  Model
newEntry phrase points id =
  {
    phrase = phrase,
    points = points,
    id = id,
    wasSpoken = False
  }

initialModel =
  {
    entries = [
      newEntry "Doing Agile" 200 2,
      newEntry "Cloud Enabled" 300 3,
      newEntry "Future-Proof" 100 1,
      newEntry "Rock-Star Ninja" 400 4
    ]
  }

-- Update

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int

update action model =
  case action of
    NoOp ->
      model

    Sort ->
      { model | entries <- List.sortBy .points model.entries }

    Delete id ->
      let
        remainingEntries =
          List.filter (\e -> e.id /= id) model.entries
      in
        { model | entries <- remainingEntries }

    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | wasSpoken <- ( not e.wasSpoken ) } else e
      in
        { model | entries <- List.map updateEntry model.entries }


-- View
greet name color food  animal =
  name ++ "'s favorites are: " ++ color ++ " " ++ food ++ " " ++ animal

multiply x y =
  x * y

title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader =
  h1 [ id "logo", class "title" ] [title "bingo" 3]

pageFooter =
  footer [  ]
    [
      a [ href "http://www.volusion.com" ]
        [ text "Volusion"]
    ]

entryItem address entry =
  li
    [
      classList [ ("highlight", entry.wasSpoken) ],
      onClick address (Mark entry.id)
    ]
    [
      span [ class "phrase" ] [ text (.phrase entry) ],
      span [ class "points" ] [ text (toString (.points entry)) ],
      button
        [ class "delete", onClick address (Delete entry.id) ]
        [ ]
    ]

entryList address entries =
  let
    entryItems =
      List.map (entryItem address) entries
  in
    ul [ ] entryItems

view address model =
  div [ ]
    [
      pageHeader,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "Sort" ],
      pageFooter
    ]

-- Wire it all up
main =
  -- view (update Sort initialModel)
  -- initialModel
  --   |> update NoOp
  --   |> view
  StartApp.start
    {
      model = initialModel,
      view = view,
      update = update
    }
