{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Types.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Data where

import           Compiler.Text
import           Compiler.TH
import           Compiler.Types.Help
import           Compiler.Types.Map
import           Compiler.Types.URI
import           Control.Error
import           Control.Lens              hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.List                 (sortOn)
import           Data.Monoid               hiding (Product, Sum)
import           Data.Ord
import qualified Data.SemVer               as SemVer
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import           Text.EDE                  (Template)

type Rendered = LText.Text

data Fun = Fun Text Help Rendered Rendered
    deriving (Show)

instance ToJSON Fun where
    toJSON (Fun n c s d) = object
        [ "name"        .= n
        , "comment"     .= c
        , "signature"   .= s
        , "declaration" .= d
        ]

data Prod = Prod'
    { _prodName          :: Text
    , _prodDocumentation :: Maybe Help
    , _prodDecl          :: Rendered
    , _prodCtor          :: Fun
    , _prodLenses        :: [Fun]
    } deriving (Show)

prodToJSON :: ToJSON a => Prod -> Map Text a -> [Pair]
prodToJSON Prod'{..} is =
    [ "type"          .= Text.pack "product"
    , "name"          .= _prodName
    , "constructor"   .= _prodCtor
    , "documentation" .= _prodDocumentation
    , "declaration"   .= _prodDecl
    , "lenses"        .= _prodLenses
    , "instances"     .= is
    ]

data Sum = Sum'
    { _sumName          :: Text
    , _sumDocumentation :: Maybe Help
    , _sumDecl          :: Rendered
    , _sumCtors         :: Map Text Text
    } deriving (Show)

sumToJSON :: Sum -> [Text] -> [Pair]
sumToJSON Sum'{..} is =
    [ "type"          .= Text.pack "sum"
    , "name"          .= _sumName
    , "constructors"  .= _sumCtors
    , "documentation" .= _sumDocumentation
    , "declaration"   .= _sumDecl
    , "instances"     .= is
    ]

data Data
    = Prod Prod (Map Text Rendered)
    | Sum  Sum  [Text]
      deriving (Show)

instance ToJSON Data where
    toJSON = \case
        Prod p is -> object (prodToJSON p is)
        Sum  s is -> object (sumToJSON  s is)
