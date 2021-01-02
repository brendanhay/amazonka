{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Data
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Data where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Function as Function
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text as Text.Lazy
import Gen.Prelude
import Gen.Types.Ann
import Gen.Types.Help
import Gen.Types.Id
import Gen.Types.Map
import Gen.Types.TypeOf

data Fun = Fun'
  { _funName :: Text,
    _funDoc :: Help,
    _funDecls :: [LazyText]
  }
  deriving stock (Eq, Show)

instance ToJSON Fun where
  toJSON Fun' {..} =
    Aeson.object
      [ "type" .= Text.pack "function",
        "name" .= _funName,
        "documentation" .= _funDoc,
        "declarations" .= _funDecls
      ]

data Accessor = Accessor'
  { _accessorName :: Text,
    _accessorDecl :: LazyText,
    _accessorDoc :: Maybe Help
  }
  deriving stock (Eq, Show)

instance ToJSON Accessor where
  toJSON (Accessor' name decl doc) =
    Aeson.object
      [ "name" .= name,
        "declaration" .= decl,
        "documentation" .= doc
      ]

data Prod = Prod'
  { _prodName :: Text,
    _prodDoc :: Maybe Help,
    _prodDecl :: LazyText,
    _prodDeriving :: [LazyText],
    _prodCtor :: Fun,
    _prodLenses :: [Fun],
    _prodAccessors :: [Accessor],
    _prodDeps :: Set.Set Text
  }
  deriving stock (Eq, Show)

prodToJSON :: Solved -> Prod -> [LazyText] -> [Pair]
prodToJSON s Prod' {..} is =
  [ "type" .= Text.pack "product",
    "name" .= _prodName,
    "declaration" .= _prodDecl,
    "documentation" .= _prodDoc,
    "constructor" .= _prodCtor,
    "lenses" .= _prodLenses,
    "deriving" .= _prodDeriving,
    "accessors" .= _prodAccessors,
    "instances" .= is,
    "shared" .= isShared s,
    "eq" .= isEq s
  ]

data Pattern = Pattern'
  { _patName :: Text,
    _patText :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON Pattern where
  toJSON (Pattern' name text) =
    Aeson.object
      [ "name" .= name,
        "text" .= text
      ]

data Sum = Sum'
  { _sumName :: Text,
    _sumDoc :: Maybe Help,
    _sumDecl :: LazyText,
    _sumCtor :: Text,
    _sumPatterns :: [Pattern]
  }
  deriving stock (Eq, Show)

sumToJSON :: Solved -> Sum -> [LazyText] -> [Pair]
sumToJSON s Sum' {..} is =
  [ "type" .= Text.pack "sum",
    "name" .= _sumName,
    "constructor" .= _sumCtor,
    "patterns" .= _sumPatterns,
    "documentation" .= _sumDoc,
    "declaration" .= _sumDecl,
    "patterns" .= _sumPatterns,
    "instances" .= is,
    "shared" .= isShared s,
    "eq" .= True
  ]

data Gen = Gen'
  { _genName :: Text,
    _genDoc :: Maybe Help,
    _genDecl :: LazyText
  }
  deriving stock (Eq, Show)

instance ToJSON Gen where
  toJSON Gen' {..} =
    Aeson.object
      [ "type" .= Text.pack "error",
        "name" .= _genName,
        "documentation" .= _genDoc,
        "declaration" .= _genDecl
      ]

data SData
  = -- | A product type (record).
    Prod !Solved Prod [LazyText]
  | -- | A nullary sum type.
    Sum !Solved Sum [LazyText]
  | -- | A function declaration.
    Fun Fun
  deriving stock (Eq, Show)

instance Ord SData where
  compare a b =
    case (a, b) of
      (Prod _ x _, Prod _ y _) -> Function.on compare _prodName x y
      (Sum _ x _, Sum _ y _) -> Function.on compare _sumName x y
      (Fun _, Fun _) -> EQ
      (Prod {}, _) -> GT
      (_, Prod {}) -> LT
      (Sum {}, _) -> GT
      (_, Sum {}) -> LT

instance ToJSON SData where
  toJSON = \case
    Prod s x is -> Aeson.object (prodToJSON s x is)
    Sum s st is -> Aeson.object (sumToJSON s st is)
    Fun f -> Aeson.toJSON f

instance HasId SData where
  identifier = \case
    Prod _ p _ -> mkId (_prodName p)
    Sum _ s _ -> mkId (_sumName s)
    Fun (Fun' n _ _) -> mkId n

data WData = WData
  { _waitName :: Text,
    _waitOpName :: Id,
    _waitCtor :: Fun
  }
  deriving stock (Show)

$(Lens.makeLenses ''WData)

instance ToJSON WData where
  toJSON (WData n _ c) =
    Aeson.object
      [ "name" .= n,
        "constructor" .= c
      ]
