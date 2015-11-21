{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Orphans where

import           Data.Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text.Encoding   as Text
import           Data.Traversable     (traverse)
import           Network.AWS.Prelude

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
