module Bingo where

import Debug
import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (toUpper, repeat, trimRight, isEmpty)
import BingoUtils as Utils

--  Model

type alias Entry =
  {
    id: Int,
    phrase: String,
    points: Int,
    wasSpoken: Bool
  }

type alias Model =
  {
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextID: Int
  }


newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  {
    phrase = phrase,
    points = points,
    id = id,
    wasSpoken = False
  }

initialModel : Model
initialModel =
  {
    entries = [
      -- newEntry "Doing Agile" 200 2,
      -- newEntry "Cloud Enabled" 300 3,
      -- newEntry "Future-Proof" 100 1,
      -- newEntry "Rock-Star Ninja" 400 4
    ],
    phraseInput = "",
    pointsInput = "",
    nextID = 1
  }

-- Update

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | AddEntry

update : Action -> Model -> Model
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

    UpdatePhraseInput contents ->
      { model | phraseInput <- contents }

    UpdatePointsInput contents ->
      { model | pointsInput <- contents }

    AddEntry ->
      let
        entryToAdd =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextID
        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model
        then model
        else
          { model |
            phraseInput <- "",
            pointsInput <- "",
            entries <- entryToAdd :: model.entries,
            nextID <- model.nextID + 1
          }


-- View
greet name color food  animal =
  name ++ "'s favorites are: " ++ color ++ " " ++ food ++ " " ++ animal

multiply x y =
  x * y

title : String -> Int -> Html
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

entryItem : Address Action -> Entry -> Html
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

totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.foldl (\entry sum -> sum + entry.points) 0 spokenEntries

totalItem :  Int -> Html
totalItem total =
  li
    [ class "total" ]
    [
      span [ class "label" ] [ text "Total" ],
      span [ class "points" ] [ text (toString total) ]
    ]


entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries) ]
  in
    ul [ ] items


entryForm : Address Action -> Model -> Html
entryForm address model =
  div [ ]
    [
      input [
        type' "text",
        placeholder "Phrase",
        value model.phraseInput,
        autofocus True,
        Utils.onInput address UpdatePhraseInput
      ] [ ],
      input [
        type' "number",
        placeholder "Points",
        value model.pointsInput,
        Utils.onInput address UpdatePointsInput
      ] [ ],
      button [ class "add", onClick address AddEntry ] [ text "Add"  ]
    ]


view : Address Action -> Model -> Html
view address model =
  div [ ]
    [
      pageHeader,
      entryForm address model,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "Sort" ],
      pageFooter
    ]

-- Wire it all up
main : Signal Html
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
