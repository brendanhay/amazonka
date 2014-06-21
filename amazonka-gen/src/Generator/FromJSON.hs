{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Generator.FromJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.FromJSON where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import qualified Data.Aeson           as Aeson
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Foldable        (any)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Unsafe     as Text
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)
import           Generator.Helpers
import           Prelude              hiding (any)
import           Text.EDE.Filters

fromField :: (Generic a, GFromJSON (Rep a))
          => (String -> String)
          -> Value
          -> Parser a
fromField f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

fromCtor :: (Generic a, GFromJSON (Rep a))
         => (String -> String)
         -> Value
         -> Parser a
fromCtor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }
