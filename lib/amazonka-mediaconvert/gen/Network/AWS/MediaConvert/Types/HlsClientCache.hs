{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsClientCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsClientCache where

import Network.AWS.Prelude

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
data HlsClientCache
  = HCCDisabled
  | HCCEnabled
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

instance FromText HlsClientCache where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HCCDisabled
      "enabled" -> pure HCCEnabled
      e ->
        fromTextError $
          "Failure parsing HlsClientCache from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText HlsClientCache where
  toText = \case
    HCCDisabled -> "DISABLED"
    HCCEnabled -> "ENABLED"

instance Hashable HlsClientCache

instance NFData HlsClientCache

instance ToByteString HlsClientCache

instance ToQuery HlsClientCache

instance ToHeader HlsClientCache

instance ToJSON HlsClientCache where
  toJSON = toJSONText

instance FromJSON HlsClientCache where
  parseJSON = parseJSONText "HlsClientCache"
