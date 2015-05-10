{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.Types.Ann
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Ann where

import           Compiler.TH
import           Compiler.Types.Id
import           Control.Lens
import           Data.Aeson        (ToJSON (..))
import           Data.Hashable
import           Data.Jason        hiding (ToJSON (..))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           GHC.Generics

type Solved = TType ::: [Derive] ::: [Instance]

data a ::: b = !a ::: !b
    deriving (Show)

infixr 5 :::

instance HasId a => HasId (a ::: b) where
    identifier (x ::: _) = identifier x

rassoc :: (a ::: b) ::: c -> a ::: b ::: c
rassoc ((x ::: y) ::: z) = x ::: y ::: z

data Mode
   = Input
   | Output
     deriving (Eq, Show)

data Direction
    = Both
    | Mode Mode
      deriving (Eq, Show)

instance Monoid Direction where
    mempty            = Both
    mappend Both _    = Both
    mappend _    Both = Both
    mappend a    b
        | a == b      = a
        | otherwise   = Both

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
    | Bool
      deriving (Show)

data TType
    = TType      Text
    | TLit       Lit
    | TNatural
    | TMaybe     TType
    | TSensitive TType
    | TList      TType
    | TList1     TType
    | TMap       TType TType
      deriving (Show)

data Derive
    = DEq
    | DOrd
    | DRead
    | DShow
    | DEnum
    | DNum
    | DIntegral
    | DReal
    | DRealFrac
    | DRealFloat
    | DMonoid
    | DSemigroup
    | DIsString
      deriving (Eq, Ord, Show, Generic)

instance Hashable Derive

instance FromJSON Derive where
    parseJSON = gParseJSON' (spinal & ctor %~ (. Text.drop 1))

data Instance
    = FromJSON  -- headers, status, json object
    | ToJSON
    | FromXML   -- headers, status, xml cursor
    | ToXML     -- xml
    | ToQuery   -- query params
    | FromBody  -- headers, status, streaming response body
    | ToPath    -- uri and query components
    | ToBody    -- streaming request body
    | ToHeaders -- headers
      deriving (Eq, Ord, Show, Generic)

instance Hashable Instance

instToText :: Instance -> Text
instToText = Text.pack . show

instance ToJSON Instance where
    toJSON = toJSON . instToText
