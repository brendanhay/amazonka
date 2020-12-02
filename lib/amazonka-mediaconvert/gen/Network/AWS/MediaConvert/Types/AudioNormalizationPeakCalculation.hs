{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation where

import Network.AWS.Prelude

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
data AudioNormalizationPeakCalculation
  = ANPCNone
  | ANPCTruePeak
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

instance FromText AudioNormalizationPeakCalculation where
  parser =
    takeLowerText >>= \case
      "none" -> pure ANPCNone
      "true_peak" -> pure ANPCTruePeak
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationPeakCalculation from value: '" <> e
            <> "'. Accepted values: none, true_peak"

instance ToText AudioNormalizationPeakCalculation where
  toText = \case
    ANPCNone -> "NONE"
    ANPCTruePeak -> "TRUE_PEAK"

instance Hashable AudioNormalizationPeakCalculation

instance NFData AudioNormalizationPeakCalculation

instance ToByteString AudioNormalizationPeakCalculation

instance ToQuery AudioNormalizationPeakCalculation

instance ToHeader AudioNormalizationPeakCalculation

instance ToJSON AudioNormalizationPeakCalculation where
  toJSON = toJSONText

instance FromJSON AudioNormalizationPeakCalculation where
  parseJSON = parseJSONText "AudioNormalizationPeakCalculation"
