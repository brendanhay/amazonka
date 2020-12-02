{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducer where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.NoiseReducerFilter
import Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import Network.AWS.Prelude

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
--
-- /See:/ 'noiseReducer' smart constructor.
data NoiseReducer = NoiseReducer'
  { _nrTemporalFilterSettings ::
      !(Maybe NoiseReducerTemporalFilterSettings),
    _nrSpatialFilterSettings ::
      !(Maybe NoiseReducerSpatialFilterSettings),
    _nrFilterSettings :: !(Maybe NoiseReducerFilterSettings),
    _nrFilter :: !(Maybe NoiseReducerFilter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoiseReducer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrTemporalFilterSettings' - Noise reducer filter settings for temporal filter.
--
-- * 'nrSpatialFilterSettings' - Noise reducer filter settings for spatial filter.
--
-- * 'nrFilterSettings' - Settings for a noise reducer filter
--
-- * 'nrFilter' - Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
noiseReducer ::
  NoiseReducer
noiseReducer =
  NoiseReducer'
    { _nrTemporalFilterSettings = Nothing,
      _nrSpatialFilterSettings = Nothing,
      _nrFilterSettings = Nothing,
      _nrFilter = Nothing
    }

-- | Noise reducer filter settings for temporal filter.
nrTemporalFilterSettings :: Lens' NoiseReducer (Maybe NoiseReducerTemporalFilterSettings)
nrTemporalFilterSettings = lens _nrTemporalFilterSettings (\s a -> s {_nrTemporalFilterSettings = a})

-- | Noise reducer filter settings for spatial filter.
nrSpatialFilterSettings :: Lens' NoiseReducer (Maybe NoiseReducerSpatialFilterSettings)
nrSpatialFilterSettings = lens _nrSpatialFilterSettings (\s a -> s {_nrSpatialFilterSettings = a})

-- | Settings for a noise reducer filter
nrFilterSettings :: Lens' NoiseReducer (Maybe NoiseReducerFilterSettings)
nrFilterSettings = lens _nrFilterSettings (\s a -> s {_nrFilterSettings = a})

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
nrFilter :: Lens' NoiseReducer (Maybe NoiseReducerFilter)
nrFilter = lens _nrFilter (\s a -> s {_nrFilter = a})

instance FromJSON NoiseReducer where
  parseJSON =
    withObject
      "NoiseReducer"
      ( \x ->
          NoiseReducer'
            <$> (x .:? "temporalFilterSettings")
            <*> (x .:? "spatialFilterSettings")
            <*> (x .:? "filterSettings")
            <*> (x .:? "filter")
      )

instance Hashable NoiseReducer

instance NFData NoiseReducer

instance ToJSON NoiseReducer where
  toJSON NoiseReducer' {..} =
    object
      ( catMaybes
          [ ("temporalFilterSettings" .=) <$> _nrTemporalFilterSettings,
            ("spatialFilterSettings" .=) <$> _nrSpatialFilterSettings,
            ("filterSettings" .=) <$> _nrFilterSettings,
            ("filter" .=) <$> _nrFilter
          ]
      )
