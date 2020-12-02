{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265CodecProfile where

import Network.AWS.Prelude

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
data H265CodecProfile
  = MAIN10High
  | MAIN10Main
  | Main42210BITHigh
  | Main42210BITMain
  | Main4228BITHigh
  | Main4228BITMain
  | MainHigh
  | MainMain
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

instance FromText H265CodecProfile where
  parser =
    takeLowerText >>= \case
      "main10_high" -> pure MAIN10High
      "main10_main" -> pure MAIN10Main
      "main_422_10bit_high" -> pure Main42210BITHigh
      "main_422_10bit_main" -> pure Main42210BITMain
      "main_422_8bit_high" -> pure Main4228BITHigh
      "main_422_8bit_main" -> pure Main4228BITMain
      "main_high" -> pure MainHigh
      "main_main" -> pure MainMain
      e ->
        fromTextError $
          "Failure parsing H265CodecProfile from value: '" <> e
            <> "'. Accepted values: main10_high, main10_main, main_422_10bit_high, main_422_10bit_main, main_422_8bit_high, main_422_8bit_main, main_high, main_main"

instance ToText H265CodecProfile where
  toText = \case
    MAIN10High -> "MAIN10_HIGH"
    MAIN10Main -> "MAIN10_MAIN"
    Main42210BITHigh -> "MAIN_422_10BIT_HIGH"
    Main42210BITMain -> "MAIN_422_10BIT_MAIN"
    Main4228BITHigh -> "MAIN_422_8BIT_HIGH"
    Main4228BITMain -> "MAIN_422_8BIT_MAIN"
    MainHigh -> "MAIN_HIGH"
    MainMain -> "MAIN_MAIN"

instance Hashable H265CodecProfile

instance NFData H265CodecProfile

instance ToByteString H265CodecProfile

instance ToQuery H265CodecProfile

instance ToHeader H265CodecProfile

instance ToJSON H265CodecProfile where
  toJSON = toJSONText

instance FromJSON H265CodecProfile where
  parseJSON = parseJSONText "H265CodecProfile"
