{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl where

import Network.AWS.Prelude

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
data AudioNormalizationAlgorithmControl
  = CorrectAudio
  | MeasureOnly
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

instance FromText AudioNormalizationAlgorithmControl where
  parser =
    takeLowerText >>= \case
      "correct_audio" -> pure CorrectAudio
      "measure_only" -> pure MeasureOnly
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationAlgorithmControl from value: '" <> e
            <> "'. Accepted values: correct_audio, measure_only"

instance ToText AudioNormalizationAlgorithmControl where
  toText = \case
    CorrectAudio -> "CORRECT_AUDIO"
    MeasureOnly -> "MEASURE_ONLY"

instance Hashable AudioNormalizationAlgorithmControl

instance NFData AudioNormalizationAlgorithmControl

instance ToByteString AudioNormalizationAlgorithmControl

instance ToQuery AudioNormalizationAlgorithmControl

instance ToHeader AudioNormalizationAlgorithmControl

instance ToJSON AudioNormalizationAlgorithmControl where
  toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithmControl where
  parseJSON = parseJSONText "AudioNormalizationAlgorithmControl"
