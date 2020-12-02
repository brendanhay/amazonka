{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel where

import Network.AWS.Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
data Vp8QualityTuningLevel
  = VMultiPass
  | VMultiPassHq
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

instance FromText Vp8QualityTuningLevel where
  parser =
    takeLowerText >>= \case
      "multi_pass" -> pure VMultiPass
      "multi_pass_hq" -> pure VMultiPassHq
      e ->
        fromTextError $
          "Failure parsing Vp8QualityTuningLevel from value: '" <> e
            <> "'. Accepted values: multi_pass, multi_pass_hq"

instance ToText Vp8QualityTuningLevel where
  toText = \case
    VMultiPass -> "MULTI_PASS"
    VMultiPassHq -> "MULTI_PASS_HQ"

instance Hashable Vp8QualityTuningLevel

instance NFData Vp8QualityTuningLevel

instance ToByteString Vp8QualityTuningLevel

instance ToQuery Vp8QualityTuningLevel

instance ToHeader Vp8QualityTuningLevel

instance ToJSON Vp8QualityTuningLevel where
  toJSON = toJSONText

instance FromJSON Vp8QualityTuningLevel where
  parseJSON = parseJSONText "Vp8QualityTuningLevel"
