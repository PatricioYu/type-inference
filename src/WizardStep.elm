module WizardStep exposing
    ( WizardStep(..)
    , initialStep
    , next
    , previous
    )

import Expr exposing (Expr, fromExpr)
import ExprParser exposing (parse)
import Html exposing (Html, button, div, h2, h3, input, label, li, ol, span, text, textarea, ul)
import Html.Attributes exposing (for, id, style, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Rectify exposing (rectify)
import Restrictions
    exposing
        ( Restrictions
        , fromMguError
        , fromRestrictions
        , mgu
        )
import Substitution exposing (fromSubstitution, substitute)
import Type exposing (Type, fromType)
import TypedExpr
    exposing
        ( Context
        , TypedExpr
        , annotate
        , fromContext
        , fromTypedExpr
        , infer
        , substituteContext
        , substituteExpr
        )


type WizardStep
    = Parse String
    | Rectify
        { input : String
        , parsedExpr : Expr
        }
    | Annotate
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        }
    | Infer
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , annotateLastFreshN : Int
        }
    | Unify
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , annotateLastFreshN : Int
        , exprType : Type
        , restrictions : Restrictions
        , inferLastFreshN : Int
        }


initialStep : String -> WizardStep
initialStep s =
    Parse s


next : WizardStep -> WizardStep
next step =
    case step of
        Parse input ->
            case parse input of
                Err _ ->
                    Parse input

                Ok parsedExpr ->
                    Rectify { input = input, parsedExpr = parsedExpr }

        Rectify { input, parsedExpr } ->
            Annotate
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectify parsedExpr
                }

        Annotate { input, parsedExpr, rectExpr } ->
            let
                ( context, annotatedExpr, nextFreshN ) =
                    annotate rectExpr
            in
            Infer
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , annotateLastFreshN = nextFreshN
                }

        Infer { input, parsedExpr, rectExpr, context, annotatedExpr, annotateLastFreshN } ->
            let
                ( maybeRes, inferLastFreshN ) =
                    infer annotatedExpr context annotateLastFreshN
            in
            case maybeRes of
                Nothing ->
                    step

                Just ( exprType, restrictions ) ->
                    Unify
                        { input = input
                        , parsedExpr = parsedExpr
                        , rectExpr = rectExpr
                        , context = context
                        , annotatedExpr = annotatedExpr
                        , annotateLastFreshN = annotateLastFreshN
                        , exprType = exprType
                        , restrictions = restrictions
                        , inferLastFreshN = inferLastFreshN
                        }

        Unify _ ->
            step


previous : WizardStep -> WizardStep
previous step =
    case step of
        Parse _ ->
            step

        Rectify { input } ->
            Parse input

        Annotate { input, parsedExpr } ->
            Rectify { input = input, parsedExpr = parsedExpr }

        Infer { input, parsedExpr, rectExpr } ->
            Annotate
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                }

        Unify { input, parsedExpr, rectExpr, context, annotatedExpr, annotateLastFreshN } ->
            Infer
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , annotateLastFreshN = annotateLastFreshN
                }
