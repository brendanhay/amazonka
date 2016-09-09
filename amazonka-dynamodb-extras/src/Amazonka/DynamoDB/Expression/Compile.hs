{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Compile
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Compilation of an expression consists of substituting either the first argument of a 'Traversable' or
-- both arguments of a 'Bitraversable' (such as 'Name' and 'Path') with
-- safe placeholders.
--
-- The expression is then pretty printed and the resulting placeholder mapping
-- is also returned.
module Amazonka.DynamoDB.Expression.Compile
    (
    -- * Compilation
      compile
    , bicompile

    -- * Grammar
    , conditionExpression
    , condition

    , updateExpression
    , update

    , projectionExpression

    , operand
    , path
    , name
    ) where

import Amazonka.DynamoDB.Expression.Internal    hiding (name)
import Amazonka.DynamoDB.Expression.Placeholder

import Control.Monad.Trans.State.Strict (runState)

import Data.Bitraversable     (Bitraversable (..))
import Data.Foldable          (foldl', toList)
import Data.Hashable          (Hashable)
import Data.List              (intersperse)
import Data.Maybe             (catMaybes)
import Data.Semigroup         ((<>))
import Data.Sequence          (Seq, ViewL (..))
import Data.Text.Lazy.Builder (Builder)

import Network.AWS.Data.Text (ToText (..))

import qualified Data.Sequence          as Seq
import qualified Data.Text.Lazy.Builder as Build

-- | Given an expression evaluator, compile the expression and return
-- the result along with any substituted placeholders for the 'Traversable'
-- argument.
--
-- Typical usage specializes as follows:
--
-- @
-- compile projectionExpression
--     :: ProjectionExpression Name -> (Builder, HashMap Name Builder)
-- @
--
-- Or to compile a 'Bitraversable' and render the first argument (such as a 'Name') verbatim:
--
-- @
-- compile conditionExpression . first name
--     :: ConditionExpression Name Value -> (Builder, HashMap Value Builder)
-- @
--
compile :: (Traversable t, Eq b, Hashable b)
        => (t Builder -> a) -- ^ An expression grammar.
        -> t b              -- ^ An expression to perform substitution on.
        -> (a, Placeholders b)
compile f = flip runState empty . fmap f . substitute
{-# INLINE compile #-}

-- | Given an expression evaluator, compile the expression and return
-- the result along with the substituted placeholders for the 'Bitraversable's
-- first and second arguments.
--
-- Typical usage specializes as follows:
--
-- @
-- bicompile conditionExpression
--     :: UpdateExpression Name Value -> (Builder, (HashMap Name Builder, HashMap Value Builder))
-- @
--
bicompile :: (Bitraversable p, Eq b, Hashable b, Eq c, Hashable c)
          => (p Builder Builder -> a) -- ^ An expression grammar.
          -> p b c                    -- ^ An expression to perform substitution on.
          -> (a, (Placeholders b, Placeholders c))
bicompile f = flip runState (empty, empty) . fmap f . bisubstitute
{-# INLINE bicompile #-}

{-|
@
expression ::=
      condition
    | operand IN ( operand (',' operand (, ...) ))
    | expression AND expression
    | expression OR expression
    | NOT expression
    | ( expression )
    | ''
@
-}
conditionExpression :: ConditionExpression Builder Builder -> Builder
conditionExpression = \case
    CondE c   -> condition c
    NotE  a   -> parens ("NOT" >>> conditionExpression a)
    AndE  a b -> parens (conditionExpression a >>> "AND" >>> conditionExpression b)
    OrE   a b -> parens (conditionExpression a >>> "OR"  >>> conditionExpression b)

{-|
@
relation ::=
      operand =  operand
    | operand <> operand
    | operand <  operand
    | operand <= operand
    | operand >  operand
    | operand >= operand

function ::=
      attribute_exists     (path)
    | attribute_not_exists (path)
    | attribute_type       (path, type)
    | begins_with          (path, substr)
    | contains             (path, operand)
    | size                 (path)

condition ::=
      relation
    | function
    | operand BETWEEN operand AND operand
@
-}
condition :: Condition a Builder Builder -> Builder
condition = \case
    Equal          a b -> operand a >>> "="  >>> operand b
    NotEqual       a b -> operand a >>> "<>" >>> operand b
    Less           a b -> operand a >>> "<"  >>> operand b
    LessOrEqual    a b -> operand a >>> "<=" >>> operand b
    Greater        a b -> operand a >>> ">"  >>> operand b
    GreaterOrEqual a b -> operand a >>> ">=" >>> operand b

    Exists     p   -> "attribute_exists"     >>> parens (path p)
    NotExists  p   -> "attribute_not_exists" >>> parens (path p)
    Size       p   -> "size"                 >>> parens (path p)
    Contains   p o -> "contains"             >>> tupled [path p, operand o]
    IsType     p t -> "attribute_type"       >>> tupled [path p, t]
    BeginsWith p x -> "begins_with"          >>> tupled [path p, x]

    Between a (b, c) ->
        operand a >>> "BETWEEN" >>> operand b >>> "AND" >>> operand c
    In      x xs     ->
        operand x >>> "IN" >>> tupled (operand <$> xs)
{-|
@
expression ::=
      SET set-action , ...
    | REMOVE remove-action , ...
    | ADD add-action , ...
    | DELETE delete-action , ...
@

FIXME: Assumes the invariant that an UpdateExpression has no empty/identity is held.
-}
updateExpression :: UpdateExpression Builder Builder -> Builder
updateExpression UnsafeUpdateExpression{..} =
    mconcat (intersperse " " statements)
  where
    statements =
        catMaybes
            [ command "SET"    setf _unsafeSet
            , command "REMOVE" path _unsafeRemove
            , command "ADD"    addf _unsafeAdd
            , command "DELETE" delf _unsafeDelete
            ]

    command :: Builder -> (a -> Builder) -> Seq a -> Maybe Builder
    command cmd f s =
        case Seq.viewl s of
            EmptyL  -> Nothing
            x :< xs -> Just $ foldl' (\r z -> r <> "," >>> f z) (cmd >>> f x) xs

    setf (p, u) = path p >>> "=" >>> update u
    addf (p, v) = path p >>> v
    delf (p, v) = path p >>> v

{-|
@
projection ::= path , ...
@
-}
projectionExpression :: ProjectionExpression Builder -> Builder
projectionExpression (ProjectionExpression (x, xs)) =
    path x <> foldMap (mappend ", " . path) xs

{-|
@
value ::=
      operand
    | operand \'+\' operand
    | operand \'-\' operand

operand ::=
      path
    | function

function ::=
      if_not_exists (path, operand)
    | list_append   (operand, operand)
@
-}
update :: Update Builder Builder -> Builder
update = \case
    Operand     o   -> operand o
    Plus        p u -> path p >>> "+" >>> update u
    Minus       p u -> path p >>> "-" >>> update u
    IfNotExists p u -> "if_not_exists" >>> tupled [path p, update u]
    ListAppend  a b -> "list_append"   >>> tupled [update a, update b]

{-|
@
operand ::=
      path
    | value
@
-}
operand :: Operand Builder Builder -> Builder
operand = \case
    Path  p -> path p
    Value v -> v
{-# INLINE operand #-}

{-|
@
path ::=
      name
    | path \'.\' path
    | path [integer]
@
-}
path :: Path Builder -> Builder
path = \case
    Attr   x   -> x
    Index  x i -> x <> "[" <> build i <> "]"
    Nested a b -> path a <> "." <> path b
{-# INLINE path #-}

{-|
@
name ::= string
@
-}
name :: Name -> Builder
name = Build.fromText . fromName

(>>>) :: Builder -> Builder -> Builder
(>>>) a b = a <> " " <> b
{-# INLINE (>>>) #-}

parens :: Builder -> Builder
parens a = "(" <> a <> ")"
{-# INLINE parens #-}

tupled :: Foldable f => f Builder -> Builder
tupled = parens . mconcat . intersperse ", " . toList
{-# INLINE tupled #-}

build :: ToText a => a -> Builder
build = Build.fromText . toText
{-# INLINE build #-}
