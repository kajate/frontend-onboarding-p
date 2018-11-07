module Main exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


type alias Model =
    { a : Float
    , b : Float
    , op : Ops
    }


initialModel : Model
initialModel =
    { a = 0
    , b = 0
    , op = Add
    }


type Ops
    = Add
    | Sub
    | Mul
    | Diff


type Msg
    = Incrementa
    | Decrementa
    | IncrementTena
    | DecrementTena
    | Incrementb
    | Decrementb
    | IncrementTenb
    | DecrementTenb
    | Reseta
    | Resetb
    | Resetall
    | SetOp Ops


update : Msg -> Model -> Model
update msg model =
    case msg of
        Incrementa ->
            { model | a = model.a + 1 }

        IncrementTena ->
            { model | a = model.a + 10 }

        Decrementa ->
            { model | a = model.a - 1 }

        DecrementTena ->
            { model | a = model.a - 10 }

        Incrementb ->
            { model | b = model.b + 1 }

        IncrementTenb ->
            { model | b = model.b + 10 }

        Decrementb ->
            { model | b = model.b - 1 }

        DecrementTenb ->
            { model | b = model.b - 10 }

        Reseta ->
            { model | a = 0 }

        Resetb ->
            { model | b = 0 }

        Resetall ->
            initialModel

        SetOp ops ->
            { model | op = ops }


selectOpData : Ops -> { sign : String, fun : Float -> Float -> Maybe Float }
selectOpData op =
    case op of
        Add ->
            { sign = "+", fun = \a b -> Just (a + b) }

        Sub ->
            { sign = "-", fun = \a b -> Just (a - b) }

        Mul ->
            { sign = "*", fun = \a b -> Just (a * b) }

        Diff ->
            { sign = "/", fun = safeDivide }


calc : Model -> Maybe Float
calc model =
    (selectOpData model.op).fun model.a model.b


themeColor : { one : Color, two : Color, three : Color }
themeColor =
    { one = rgb 220 100 100
    , two = rgb 100 220 100
    , three = rgb 100 100 220
    }


viewCalculator : Model -> Html Msg
viewCalculator model =
    let
        innerGrid =
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "1fr 1fr 1fr 1fr 1fr 1fr"
            , Css.property "grid-template-rows" "1fr 1fr 1fr"
            , Css.property "grid-column-start" "1"
            , Css.property "grid-column-end" "5"
            , width (px 200)
            ]

        divStyle =
            [ height (px 30)
            , width (px 60)
            , display inline
            , outline none
            , backgroundColor (rgb 255 255 255)
            , margin (px 3)
            , paddingTop (px 22)
            , paddingBottom (px 8)
            , Css.property "text-align" "Center"
            , Css.property "font-family" "Helvetica"
            , fontSize (px 12)
            ]

        red =
            [ border3 (px 1) solid themeColor.one
            , color themeColor.one
            , boxShadow4 (px 2) (px 2) (px 0) themeColor.one
            , hover
                [ border3 (px 1) solid themeColor.one
                , boxShadow4 (px 3) (px 3) (px 0) themeColor.one
                , color themeColor.one
                ]
            , active
                [ border3 (px 1) solid themeColor.one
                , color themeColor.one
                , boxShadow4 (px 1) (px 1) (px 0) themeColor.one
                ]
            ]

        green =
            [ border3 (px 1) solid themeColor.two
            , color themeColor.two
            , boxShadow4 (px 2) (px 2) (px 0) themeColor.two
            , hover
                [ border3 (px 1) solid themeColor.two
                , boxShadow4 (px 3) (px 3) (px 0) themeColor.two
                , color themeColor.two
                ]
            , active
                [ border3 (px 1) solid themeColor.two
                , color themeColor.two
                , boxShadow4 (px 1) (px 1) (px 0) themeColor.two
                ]
            ]

        blue =
            [ border3 (px 1) solid themeColor.three
            , color themeColor.three
            , boxShadow4 (px 2) (px 2) (px 0) themeColor.three
            , hover
                [ border3 (px 1) solid themeColor.three
                , boxShadow4 (px 3) (px 3) (px 0) themeColor.three
                , color themeColor.three
                ]
            , active
                [ border3 (px 1) solid themeColor.three
                , color themeColor.three
                , boxShadow4 (px 1) (px 1) (px 0) themeColor.three
                ]
            ]

        btn =
            [ height (px 62)
            , width (px 62)
            , display inline
            , outline none
            , margin (px 3)
            , fontSize (px 12)
            , Css.property "font-family" "Helvetica"
            , hover
                [ transform (translate3d (px -1) (px -1) (px 0))
                ]
            , active
                [ transform (translate3d (px 1) (px 1) (px 0))
                ]
            ]
    in
    div [ css innerGrid ]
        [ div [ css divStyle, css red ] [ text <| String.fromFloat model.a ]
        , button [ css btn, css red, onClick Decrementa ] [ text "- 1" ]
        , button [ css btn, css red, onClick Incrementa ] [ text "+ 1" ]
        , button [ css btn, css red, onClick DecrementTena ] [ text "- 10" ]
        , button [ css btn, css red, onClick IncrementTena ] [ text "+ 10" ]
        , button [ css btn, css red, onClick Reseta ] [ text "Reset" ]
        , div [ css divStyle, css blue ] [ text <| String.fromFloat model.b ]
        , button [ css btn, css blue, onClick Decrementb ] [ text "- 1" ]
        , button [ css btn, css blue, onClick Incrementb ] [ text "+ 1" ]
        , button [ css btn, css blue, onClick DecrementTenb ] [ text "- 10" ]
        , button [ css btn, css blue, onClick IncrementTenb ] [ text "+ 10" ]
        , button [ css btn, css blue, onClick Resetb ] [ text "Reset" ]
        , case calc model of
            Nothing -> text "error!!!!"
            Just result -> div [ css divStyle, css green ] [ text <| String.fromFloat result ]
        , button [ css btn, css green, onClick (SetOp Add) ] [ text (selectOpData Add).sign ]
        , button [ css btn, css green, onClick (SetOp Sub) ] [ text (selectOpData Sub).sign ]
        , button [ css btn, css green, onClick (SetOp Mul) ] [ text (selectOpData Mul).sign ]
        , button [ css btn, css green, onClick (SetOp Diff) ] [ text (selectOpData Diff).sign ]
        , button [ css btn, css green, onClick Resetall ] [ text "Reset all" ]
        ]


viewHeader : Html Msg
viewHeader =
    div []
        [ h2 [] [ text "Annoying Calculator" ]
        , h3 [] [ text "But it works" ]
        ]


view : Model -> Html Msg
view model =
    let
        gridWrapper =
            [ Css.property "display" "flex"
            , Css.property "flex-direction" "column"
            , Css.property "min-height" "100vh"
            , Css.property "grid-template-columns" "200px 1fr 200px"
            , Css.property "grid-template-rows" "auto 1fr auto"
            , height (vh 95)
            ]

        header =
            [ backgroundColor (rgb 255 255 255)
            , padding (px 20)
            , Css.property "font-family" "Helvetica"
            , Css.property "grid-column" "span 3"
            ]

        sidebar =
            [ backgroundColor (rgb 255 255 255)
            , padding (px 20)
            ]

        content =
            [ Css.property "flex" "1"
            , backgroundColor (rgb 255 255 255)
            , padding (px 20)
            ]

        aside =
            [ backgroundColor (rgb 255 255 255)
            , padding (px 20)
            ]

        footer =
            [ backgroundColor (rgb 255 255 255)
            , Css.property "grid-column" "span 3"
            ]
    in
    div
        [ css gridWrapper ]
        [ div [ css header ] [ viewHeader ]
        , div [ css aside ] [ text (selectOpData model.op).sign ]
        , div [ css content ] [ viewCalculator model ]
        , div [ css footer ] [ text "" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        }



--HELPER FUNCTIONS


safeDivide : Float -> Float -> Maybe Float
safeDivide a b =
    if b == 0 then
        Nothing
    else
        Just (a / b)