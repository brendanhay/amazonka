{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsOfflineEncrypted where

import Network.AWS.Prelude

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
data HlsOfflineEncrypted
  = HOEDisabled
  | HOEEnabled
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

instance FromText HlsOfflineEncrypted where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HOEDisabled
      "enabled" -> pure HOEEnabled
      e ->
        fromTextError $
          "Failure parsing HlsOfflineEncrypted from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText HlsOfflineEncrypted where
  toText = \case
    HOEDisabled -> "DISABLED"
    HOEEnabled -> "ENABLED"

instance Hashable HlsOfflineEncrypted

instance NFData HlsOfflineEncrypted

instance ToByteString HlsOfflineEncrypted

instance ToQuery HlsOfflineEncrypted

instance ToHeader HlsOfflineEncrypted

instance ToJSON HlsOfflineEncrypted where
  toJSON = toJSONText

instance FromJSON HlsOfflineEncrypted where
  parseJSON = parseJSONText "HlsOfflineEncrypted"
