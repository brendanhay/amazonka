{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CacheType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CacheType where

import Network.AWS.Prelude

data CacheType
  = CTLocal
  | CTNoCache
  | CTS3
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CacheType where
  parser =
    takeLowerText >>= \case
      "local" -> pure CTLocal
      "no_cache" -> pure CTNoCache
      "s3" -> pure CTS3
      e ->
        fromTextError $
          "Failure parsing CacheType from value: '" <> e
            <> "'. Accepted values: local, no_cache, s3"

instance ToText CacheType where
  toText = \case
    CTLocal -> "LOCAL"
    CTNoCache -> "NO_CACHE"
    CTS3 -> "S3"

instance Hashable CacheType

instance NFData CacheType

instance ToByteString CacheType

instance ToQuery CacheType

instance ToHeader CacheType

instance ToJSON CacheType where
  toJSON = toJSONText

instance FromJSON CacheType where
  parseJSON = parseJSONText "CacheType"
