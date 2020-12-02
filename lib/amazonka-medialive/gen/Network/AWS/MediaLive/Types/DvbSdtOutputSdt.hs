{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSdtOutputSdt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSdtOutputSdt where

import Network.AWS.Prelude

-- | Dvb Sdt Output Sdt
data DvbSdtOutputSdt
  = SdtFollow
  | SdtFollowIfPresent
  | SdtManual
  | SdtNone
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

instance FromText DvbSdtOutputSdt where
  parser =
    takeLowerText >>= \case
      "sdt_follow" -> pure SdtFollow
      "sdt_follow_if_present" -> pure SdtFollowIfPresent
      "sdt_manual" -> pure SdtManual
      "sdt_none" -> pure SdtNone
      e ->
        fromTextError $
          "Failure parsing DvbSdtOutputSdt from value: '" <> e
            <> "'. Accepted values: sdt_follow, sdt_follow_if_present, sdt_manual, sdt_none"

instance ToText DvbSdtOutputSdt where
  toText = \case
    SdtFollow -> "SDT_FOLLOW"
    SdtFollowIfPresent -> "SDT_FOLLOW_IF_PRESENT"
    SdtManual -> "SDT_MANUAL"
    SdtNone -> "SDT_NONE"

instance Hashable DvbSdtOutputSdt

instance NFData DvbSdtOutputSdt

instance ToByteString DvbSdtOutputSdt

instance ToQuery DvbSdtOutputSdt

instance ToHeader DvbSdtOutputSdt

instance ToJSON DvbSdtOutputSdt where
  toJSON = toJSONText

instance FromJSON DvbSdtOutputSdt where
  parseJSON = parseJSONText "DvbSdtOutputSdt"
