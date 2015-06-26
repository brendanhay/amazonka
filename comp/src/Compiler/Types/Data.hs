{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Compiler.Types.Ann
import           Compiler.Types.Help
import           Compiler.Types.Id
import           Compiler.Types.Map
import           Compiler.Types.NS
import           Compiler.Types.URI
import           Control.Error
import           Control.Lens              hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Function             (on)
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

data Fun = Fun' Text Help Rendered Rendered
    deriving (Eq, Show)

instance ToJSON Fun where
    toJSON (Fun' n c s d) = object
        [ "type"          .= Text.pack "function"
        , "name"          .= n
        , "documentation" .= c
        , "signature"     .= s
        , "declaration"   .= d
        ]

data Prod = Prod'
    { _prodName   :: Text
    , _prodDoc    :: Maybe Help
    , _prodDecl   :: Rendered
    , _prodCtor   :: Fun
    , _prodLenses :: [Fun]
    } deriving (Eq, Show)

prodToJSON :: ToJSON a => Bool -> Prod -> Map Text a -> [Pair]
prodToJSON s Prod'{..} is =
    [ "type"          .= Text.pack "product"
    , "name"          .= _prodName
    , "constructor"   .= _prodCtor
    , "documentation" .= _prodDoc
    , "declaration"   .= _prodDecl
    , "lenses"        .= _prodLenses
    , "instances"     .= is
    , "shared"        .= s
    ]

data Sum = Sum'
    { _sumName  :: Text
    , _sumDoc   :: Maybe Help
    , _sumDecl  :: Rendered
    , _sumCtors :: Map Text Text
    } deriving (Eq, Show)

sumToJSON :: Bool -> Sum -> [Text] -> [Pair]
sumToJSON s Sum'{..} is =
    [ "type"          .= Text.pack "sum"
    , "name"          .= _sumName
    , "constructors"  .= _sumCtors
    , "documentation" .= _sumDoc
    , "declaration"   .= _sumDecl
    , "instances"     .= is
    , "shared"        .= s
    ]

data Gen = Gen'
    { _genName :: Text
    , _genDoc  :: Maybe Help
    , _genDecl :: Rendered
    } deriving (Eq, Show)

instance ToJSON Gen where
    toJSON Gen'{..} = object
        [ "type"          .= Text.pack "error"
        , "name"          .= _genName
        , "documentation" .= _genDoc
        , "declaration"   .= _genDecl
        ]

data SData
    = Prod !Bool Prod (Map Text Rendered) -- ^ A product type (record).
    | Sum  !Bool Sum  [Text]              -- ^ A nullary sum type.
    | Fun        Fun                      -- ^ A function declaration.
      deriving (Eq, Show)

instance Ord SData where
    compare a b =
        case (a, b) of
            (Prod _ x _, Prod _ y _) -> on compare _prodName x y
            (Sum  _ x _, Sum  _ y _) -> on compare _sumName  x y
            (Fun  _,     Fun  _)     -> EQ
            (Prod {},    _)          -> GT
            (_,          Prod {})    -> LT
            (Sum  {},    _)          -> GT
            (_,          Sum  {})    -> LT

instance ToJSON SData where
    toJSON = \case
        Prod s p  is -> object (prodToJSON s p  is)
        Sum  s st is -> object (sumToJSON  s st is)
        Fun  f       -> toJSON f

data WData = WData
    { _waitOpName :: Id
    , _waitSig    :: Rendered
    , _waitDecl   :: Rendered
    } deriving (Show)

makeLenses ''WData

instance ToJSON WData where
    toJSON (WData _ s d) = object
        [ "signature"   .= s
        , "declaration" .= d
        ]
