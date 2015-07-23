{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Orphans where

import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS8
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Exts                   (toList)
import           Network.AWS.Data.List1
import           Network.AWS.Data.Map
import           Network.AWS.Data.Numeric
import           Network.AWS.Data.Sensitive
import           Network.AWS.Prelude
import           Network.HTTP.Types
import           Numeric.Natural

instance FromJSON ByteString where
    parseJSON = withText "bytestring" (either fail pure . fromText)

instance FromJSON (HashMap ByteString ByteString) where
    parseJSON = withObject "hashmap" $
          fmap Map.fromList
        . traverse go
        . Map.toList
      where
        go (k, v) = (Text.encodeUtf8 k,) <$> parseJSON v

instance FromJSON (HashMap HeaderName ByteString) where
    parseJSON = withObject "headers" $
          fmap Map.fromList
        . traverse go
        . Map.toList
      where
        go (k, v) = (CI.mk (Text.encodeUtf8 k),) <$> parseJSON v
