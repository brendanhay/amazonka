{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsRedundantManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsRedundantManifest where

import Network.AWS.Prelude

-- | Hls Redundant Manifest
data HlsRedundantManifest
  = HRMDisabled
  | HRMEnabled
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

instance FromText HlsRedundantManifest where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HRMDisabled
      "enabled" -> pure HRMEnabled
      e ->
        fromTextError $
          "Failure parsing HlsRedundantManifest from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText HlsRedundantManifest where
  toText = \case
    HRMDisabled -> "DISABLED"
    HRMEnabled -> "ENABLED"

instance Hashable HlsRedundantManifest

instance NFData HlsRedundantManifest

instance ToByteString HlsRedundantManifest

instance ToQuery HlsRedundantManifest

instance ToHeader HlsRedundantManifest

instance ToJSON HlsRedundantManifest where
  toJSON = toJSONText

instance FromJSON HlsRedundantManifest where
  parseJSON = parseJSONText "HlsRedundantManifest"
