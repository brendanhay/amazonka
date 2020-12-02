{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import Network.AWS.Prelude

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'noiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'
  { _nrtfsPostTemporalSharpening ::
      !( Maybe
           NoiseFilterPostTemporalSharpening
       ),
    _nrtfsAggressiveMode ::
      !(Maybe Nat),
    _nrtfsStrength ::
      !(Maybe Nat),
    _nrtfsSpeed ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoiseReducerTemporalFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrtfsPostTemporalSharpening' - Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
--
-- * 'nrtfsAggressiveMode' - Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
--
-- * 'nrtfsStrength' - Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
--
-- * 'nrtfsSpeed' - The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
noiseReducerTemporalFilterSettings ::
  NoiseReducerTemporalFilterSettings
noiseReducerTemporalFilterSettings =
  NoiseReducerTemporalFilterSettings'
    { _nrtfsPostTemporalSharpening =
        Nothing,
      _nrtfsAggressiveMode = Nothing,
      _nrtfsStrength = Nothing,
      _nrtfsSpeed = Nothing
    }

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
nrtfsPostTemporalSharpening :: Lens' NoiseReducerTemporalFilterSettings (Maybe NoiseFilterPostTemporalSharpening)
nrtfsPostTemporalSharpening = lens _nrtfsPostTemporalSharpening (\s a -> s {_nrtfsPostTemporalSharpening = a})

-- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
nrtfsAggressiveMode :: Lens' NoiseReducerTemporalFilterSettings (Maybe Natural)
nrtfsAggressiveMode = lens _nrtfsAggressiveMode (\s a -> s {_nrtfsAggressiveMode = a}) . mapping _Nat

-- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
nrtfsStrength :: Lens' NoiseReducerTemporalFilterSettings (Maybe Natural)
nrtfsStrength = lens _nrtfsStrength (\s a -> s {_nrtfsStrength = a}) . mapping _Nat

-- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
nrtfsSpeed :: Lens' NoiseReducerTemporalFilterSettings (Maybe Int)
nrtfsSpeed = lens _nrtfsSpeed (\s a -> s {_nrtfsSpeed = a})

instance FromJSON NoiseReducerTemporalFilterSettings where
  parseJSON =
    withObject
      "NoiseReducerTemporalFilterSettings"
      ( \x ->
          NoiseReducerTemporalFilterSettings'
            <$> (x .:? "postTemporalSharpening")
            <*> (x .:? "aggressiveMode")
            <*> (x .:? "strength")
            <*> (x .:? "speed")
      )

instance Hashable NoiseReducerTemporalFilterSettings

instance NFData NoiseReducerTemporalFilterSettings

instance ToJSON NoiseReducerTemporalFilterSettings where
  toJSON NoiseReducerTemporalFilterSettings' {..} =
    object
      ( catMaybes
          [ ("postTemporalSharpening" .=) <$> _nrtfsPostTemporalSharpening,
            ("aggressiveMode" .=) <$> _nrtfsAggressiveMode,
            ("strength" .=) <$> _nrtfsStrength,
            ("speed" .=) <$> _nrtfsSpeed
          ]
      )
