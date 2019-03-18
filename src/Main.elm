module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, h5, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, for, id, name, src, style, type_, value)
import Html.Events exposing (onClick)
import List



---- MODEL ----


type Size
    = Personal
    | Medium
    | Large
    | ExtraLarge


type Crust
    = Plain
    | GarlicButter
    | CheeseStuffed
    | Spicy
    | HouseSpecial


type Sauce
    = Marinara
    | White
    | Barbecue
    | NoSauce


type Cheese
    = Regular
    | ExtraCheese
    | NoCheese


type Meats
    = Pepperoni
    | Sausage
    | CanadianBacon
    | GroundBeef
    | Anchovies
    | Chicken


type Veggies
    = Tomatoes
    | Onions
    | Olives
    | GreenPeppers
    | Mushrooms
    | Pineapple
    | Spinach
    | Jalapeno


type alias Model =
    { size : Size
    , crust : Crust
    , sauce : Sauce
    , cheese : Cheese
    , meats : List Meats
    , veggies : List Veggies
    }


init : ( Model, Cmd Msg )
init =
    ( { size = Large
      , crust = Plain
      , sauce = Marinara
      , cheese = Regular
      , meats = [ Pepperoni ]
      , veggies = [ Tomatoes, Mushrooms, Pineapple ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Size Size
    | Crust Crust
    | Sauce Sauce
    | Cheese Cheese
    | Meats Meats
    | Veggies Veggies


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Size newSize ->
            ( { model | size = newSize }, Cmd.none )

        Crust newCrust ->
            ( { model | crust = newCrust }, Cmd.none )

        Sauce newSauce ->
            ( { model | sauce = newSauce }, Cmd.none )

        Cheese newCheese ->
            ( { model | cheese = newCheese }, Cmd.none )

        Meats toggleMeat ->
            if List.member toggleMeat model.meats then
                ( { model
                    | meats =
                        List.filter
                            (\n ->
                                if n == toggleMeat then
                                    False

                                else
                                    True
                            )
                            model.meats
                  }
                , Cmd.none
                )

            else
                ( { model | meats = toggleMeat :: model.meats }, Cmd.none )

        Veggies toggleVeggie ->
            if List.member toggleVeggie model.veggies then
                ( { model
                    | veggies =
                        List.filter
                            (\n ->
                                if n == toggleVeggie then
                                    False

                                else
                                    True
                            )
                            model.veggies
                  }
                , Cmd.none
                )

            else
                ( { model | veggies = toggleVeggie :: model.veggies }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "text-center" ] [ text "Let's order some pizza!" ]
        , div []
            [ div [ class "card", class "mx-4" ]
                [ div [ class "card-header" ] [ h3 [] [ text "Make your selection below" ] ]
                , div [ class "card-body" ]
                    [ div [ class "row" ]
                        [ div [ class "col-8" ]
                            [ h4 [ class "mb-4" ] [ text "Size and Type" ]
                            , div [ class "row" ]
                                [ div [ class "col" ]
                                    [ h5 [] [ text "Size" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li [] [ input [ type_ "radio", id "personal", name "size", class "mr-2", checked (model.size == Personal), onClick (Size Personal) ] [], label [ for "personal" ] [ text "Personal" ] ]
                                        , li [] [ input [ type_ "radio", id "medium", name "size", class "mr-2", checked (model.size == Medium), onClick (Size Medium) ] [], label [ for "medium" ] [ text "Medium" ] ]
                                        , li [] [ input [ type_ "radio", id "large", name "size", class "mr-2", checked (model.size == Large) ] [], label [ for "large", onClick (Size Large) ] [ text "Large" ] ]
                                        , li [] [ input [ type_ "radio", id "extra-large", name "size", class "mr-2", checked (model.size == ExtraLarge), onClick (Size ExtraLarge) ] [], label [ for "extra-large" ] [ text "Extra Large" ] ]
                                        ]
                                    ]
                                , div [ class "col" ]
                                    [ h5 [] [ text "Crust" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li []
                                            [ input [ type_ "radio", id "plain", name "crust", class "mr-2", checked (model.crust == Plain), onClick (Crust Plain) ] []
                                            , label [ for "plain" ] [ text "Plain Crust" ]
                                            ]
                                        , li [] [ input [ type_ "radio", id "garlic-butter", name "crust", class "mr-2", checked (model.crust == GarlicButter), onClick (Crust GarlicButter) ] [], label [ for "garlic-butter" ] [ text "Garlic Butter Crust" ] ]
                                        , li [] [ input [ type_ "radio", id "cheese-stuffed", name "crust", class "mr-2", checked (model.crust == CheeseStuffed), onClick (Crust CheeseStuffed) ] [], label [ for "cheese-stuffed" ] [ text "Cheese Stuffed Crust" ] ]
                                        , li [] [ input [ type_ "radio", id "spicy", name "crust", class "mr-2", checked (model.crust == Spicy), onClick (Crust Spicy) ] [], label [ for "spicy" ] [ text "Spicy Crust" ] ]
                                        , li [] [ input [ type_ "radio", id "house-special", name "crust", class "mr-2", checked (model.crust == HouseSpecial), onClick (Crust HouseSpecial) ] [], label [ for "house-special" ] [ text "House Special Crust" ] ]
                                        ]
                                    ]
                                , div [ class "col" ]
                                    [ h5 [] [ text "Sauce" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li [] [ input [ type_ "radio", id "marinara", name "sauce", class "mr-2", checked (model.sauce == Marinara), onClick (Sauce Marinara) ] [], label [ for "marinara" ] [ text "Marinara Sauce" ] ]
                                        , li [] [ input [ type_ "radio", id "white", name "sauce", class "mr-2", checked (model.sauce == White), onClick (Sauce White) ] [], label [ for "white" ] [ text "White Sauce" ] ]
                                        , li [] [ input [ type_ "radio", id "barbecue", name "sauce", class "mr-2", checked (model.sauce == Barbecue), onClick (Sauce Barbecue) ] [], label [ for "barbecue" ] [ text "Barbecue Sauce" ] ]
                                        , li [] [ input [ type_ "radio", id "no-sauce", name "sauce", class "mr-2", checked (model.sauce == NoSauce), onClick (Sauce NoSauce) ] [], label [ for "no-sauce" ] [ text "No Sauce" ] ]
                                        ]
                                    ]
                                , div [ class "col" ]
                                    [ h5 [] [ text "Cheese" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li [] [ input [ type_ "radio", id "regular", name "cheese", class "mr-2", checked (model.cheese == Regular), onClick (Cheese Regular) ] [], label [ for "regular" ] [ text "Regular" ] ]
                                        , li [] [ input [ type_ "radio", id "extra-cheese", name "cheese", class "mr-2", checked (model.cheese == ExtraCheese), onClick (Cheese ExtraCheese) ] [], label [ for "extra-cheese" ] [ text "Extra Cheese" ] ]
                                        , li [] [ input [ type_ "radio", id "no-cheese", name "cheese", class "mr-2", checked (model.cheese == NoCheese), onClick (Cheese NoCheese) ] [], label [ for "no-cheese" ] [ text "No Cheese" ] ]
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "col-4" ]
                            [ h4 [ class "mb-4" ] [ text "Toppings" ]
                            , div [ class "row" ]
                                [ div [ class "col" ]
                                    [ h5 [] [ text "Meats" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li [] [ input [ type_ "checkbox", id "pepperoni", class "mr-2", checked (List.member Pepperoni model.meats), onClick (Meats Pepperoni) ] [], label [ for "pepperoni" ] [ text "Pepperoni" ] ]
                                        , li [] [ input [ type_ "checkbox", id "sausage", class "mr-2", checked (List.member Sausage model.meats), onClick (Meats Sausage) ] [], label [ for "sausage" ] [ text "Sausage" ] ]
                                        , li [] [ input [ type_ "checkbox", id "canadian-bacon", class "mr-2", checked (List.member CanadianBacon model.meats), onClick (Meats CanadianBacon) ] [], label [ for "canadian-bacon" ] [ text "Canadian Bacon" ] ]
                                        , li [] [ input [ type_ "checkbox", id "ground-beef", class "mr-2", checked (List.member GroundBeef model.meats), onClick (Meats GroundBeef) ] [], label [ for "ground-beef" ] [ text "Ground Beef" ] ]
                                        , li [] [ input [ type_ "checkbox", id "anchovies", class "mr-2", checked (List.member Anchovies model.meats), onClick (Meats Anchovies) ] [], label [ for "anchovies" ] [ text "Anchovies" ] ]
                                        , li [] [ input [ type_ "checkbox", id "chicken", class "mr-2", checked (List.member Chicken model.meats), onClick (Meats Chicken) ] [], label [ for "chicken" ] [ text "Chicken" ] ]
                                        ]
                                    ]
                                , div [ class "col" ]
                                    [ h5 [] [ text "Veggies" ]
                                    , ul [ style "list-style" "none" ]
                                        [ li [] [ input [ type_ "checkbox", id "tomatoes", class "mr-2", checked (List.member Tomatoes model.veggies), onClick (Veggies Tomatoes) ] [], label [ for "tomatoes" ] [ text "Tomatoes" ] ]
                                        , li [] [ input [ type_ "checkbox", id "onions", class "mr-2", checked (List.member Onions model.veggies), onClick (Veggies Onions) ] [], label [ for "onions" ] [ text "Onions" ] ]
                                        , li [] [ input [ type_ "checkbox", id "olives", class "mr-2", checked (List.member Olives model.veggies), onClick (Veggies Olives) ] [], label [ for "olives" ] [ text "Olives" ] ]
                                        , li [] [ input [ type_ "checkbox", id "green-peppers", class "mr-2", checked (List.member GreenPeppers model.veggies), onClick (Veggies GreenPeppers) ] [], label [ for "green-peppers" ] [ text "Green Peppers" ] ]
                                        , li [] [ input [ type_ "checkbox", id "mushrooms", class "mr-2", checked (List.member Mushrooms model.veggies), onClick (Veggies Mushrooms) ] [], label [ for "mushrooms" ] [ text "Mushrooms" ] ]
                                        , li [] [ input [ type_ "checkbox", id "pineapple", class "mr-2", checked (List.member Pineapple model.veggies), onClick (Veggies Pineapple) ] [], label [ for "pineapple" ] [ text "Pineapple" ] ]
                                        , li [] [ input [ type_ "checkbox", id "spinach", class "mr-2", checked (List.member Spinach model.veggies), onClick (Veggies Spinach) ] [], label [ for "spinach" ] [ text "Spinach" ] ]
                                        , li [] [ input [ type_ "checkbox", id "jalapeno", class "mr-2", checked (List.member Jalapeno model.veggies), onClick (Veggies Jalapeno) ] [], label [ for "jalapeno" ] [ text "Jalapeño" ] ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "text-center" ]
                        [ button [ class "btn", class "btn-primary" ] [ text "Order Pizza!" ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
