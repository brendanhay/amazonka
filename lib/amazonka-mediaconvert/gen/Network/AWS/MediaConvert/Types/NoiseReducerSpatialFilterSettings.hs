{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'noiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { _nrsfsStrength ::
      !(Maybe Nat),
    _nrsfsPostFilterSharpenStrength ::
      !(Maybe Nat),
    _nrsfsSpeed ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoiseReducerSpatialFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrsfsStrength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- * 'nrsfsPostFilterSharpenStrength' - Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
--
-- * 'nrsfsSpeed' - The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
noiseReducerSpatialFilterSettings ::
  NoiseReducerSpatialFilterSettings
noiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { _nrsfsStrength = Nothing,
      _nrsfsPostFilterSharpenStrength = Nothing,
      _nrsfsSpeed = Nothing
    }

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
nrsfsStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Natural)
nrsfsStrength = lens _nrsfsStrength (\s a -> s {_nrsfsStrength = a}) . mapping _Nat

-- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
nrsfsPostFilterSharpenStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Natural)
nrsfsPostFilterSharpenStrength = lens _nrsfsPostFilterSharpenStrength (\s a -> s {_nrsfsPostFilterSharpenStrength = a}) . mapping _Nat

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
nrsfsSpeed :: Lens' NoiseReducerSpatialFilterSettings (Maybe Int)
nrsfsSpeed = lens _nrsfsSpeed (\s a -> s {_nrsfsSpeed = a})

instance FromJSON NoiseReducerSpatialFilterSettings where
  parseJSON =
    withObject
      "NoiseReducerSpatialFilterSettings"
      ( \x ->
          NoiseReducerSpatialFilterSettings'
            <$> (x .:? "strength")
            <*> (x .:? "postFilterSharpenStrength")
            <*> (x .:? "speed")
      )

instance Hashable NoiseReducerSpatialFilterSettings

instance NFData NoiseReducerSpatialFilterSettings

instance ToJSON NoiseReducerSpatialFilterSettings where
  toJSON NoiseReducerSpatialFilterSettings' {..} =
    object
      ( catMaybes
          [ ("strength" .=) <$> _nrsfsStrength,
            ("postFilterSharpenStrength" .=)
              <$> _nrsfsPostFilterSharpenStrength,
            ("speed" .=) <$> _nrsfsSpeed
          ]
      )
