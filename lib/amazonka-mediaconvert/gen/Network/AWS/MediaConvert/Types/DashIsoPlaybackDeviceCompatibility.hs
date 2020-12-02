{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility where

import Network.AWS.Prelude

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
data DashIsoPlaybackDeviceCompatibility
  = CencV1
  | UnencryptedSei
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

instance FromText DashIsoPlaybackDeviceCompatibility where
  parser =
    takeLowerText >>= \case
      "cenc_v1" -> pure CencV1
      "unencrypted_sei" -> pure UnencryptedSei
      e ->
        fromTextError $
          "Failure parsing DashIsoPlaybackDeviceCompatibility from value: '" <> e
            <> "'. Accepted values: cenc_v1, unencrypted_sei"

instance ToText DashIsoPlaybackDeviceCompatibility where
  toText = \case
    CencV1 -> "CENC_V1"
    UnencryptedSei -> "UNENCRYPTED_SEI"

instance Hashable DashIsoPlaybackDeviceCompatibility

instance NFData DashIsoPlaybackDeviceCompatibility

instance ToByteString DashIsoPlaybackDeviceCompatibility

instance ToQuery DashIsoPlaybackDeviceCompatibility

instance ToHeader DashIsoPlaybackDeviceCompatibility

instance ToJSON DashIsoPlaybackDeviceCompatibility where
  toJSON = toJSONText

instance FromJSON DashIsoPlaybackDeviceCompatibility where
  parseJSON = parseJSONText "DashIsoPlaybackDeviceCompatibility"
