{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel where

import Network.AWS.Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
data Vp9QualityTuningLevel
  = VQTLMultiPass
  | VQTLMultiPassHq
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

instance FromText Vp9QualityTuningLevel where
  parser =
    takeLowerText >>= \case
      "multi_pass" -> pure VQTLMultiPass
      "multi_pass_hq" -> pure VQTLMultiPassHq
      e ->
        fromTextError $
          "Failure parsing Vp9QualityTuningLevel from value: '" <> e
            <> "'. Accepted values: multi_pass, multi_pass_hq"

instance ToText Vp9QualityTuningLevel where
  toText = \case
    VQTLMultiPass -> "MULTI_PASS"
    VQTLMultiPassHq -> "MULTI_PASS_HQ"

instance Hashable Vp9QualityTuningLevel

instance NFData Vp9QualityTuningLevel

instance ToByteString Vp9QualityTuningLevel

instance ToQuery Vp9QualityTuningLevel

instance ToHeader Vp9QualityTuningLevel

instance ToJSON Vp9QualityTuningLevel where
  toJSON = toJSONText

instance FromJSON Vp9QualityTuningLevel where
  parseJSON = parseJSONText "Vp9QualityTuningLevel"
