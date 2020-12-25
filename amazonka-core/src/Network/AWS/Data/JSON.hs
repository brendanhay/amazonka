{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.JSON
  ( -- * Serialisation
    ToJSONKey,
    ToJSON (..),
    Value (Object),
    object,
    (.=),

    -- * Deserialisation
    FromJSONKey,
    FromJSON (..),
    eitherDecode,
    eitherDecode',
    withObject,
    (.:),
    (.:?),
    (.!=),
    
    -- * Parsing response objects
    eitherParseJSON,
  )
where

import Data.Aeson (eitherDecode, eitherDecode')
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Network.AWS.Data.Text as AWS.Text
import Prelude

eitherParseJSON :: FromJSON a => Object -> Either String a
eitherParseJSON = parseEither parseJSON . Object
