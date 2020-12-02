{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.DetectedProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.DetectedProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The detected properties of the input file. Elastic Transcoder identifies these values from the input file.
--
--
--
-- /See:/ 'detectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { _dpHeight ::
      !(Maybe Int),
    _dpFrameRate :: !(Maybe Text),
    _dpFileSize :: !(Maybe Integer),
    _dpWidth :: !(Maybe Int),
    _dpDurationMillis :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectedProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpHeight' - The detected height of the input file, in pixels.
--
-- * 'dpFrameRate' - The detected frame rate of the input file, in frames per second.
--
-- * 'dpFileSize' - The detected file size of the input file, in bytes.
--
-- * 'dpWidth' - The detected width of the input file, in pixels.
--
-- * 'dpDurationMillis' - The detected duration of the input file, in milliseconds.
detectedProperties ::
  DetectedProperties
detectedProperties =
  DetectedProperties'
    { _dpHeight = Nothing,
      _dpFrameRate = Nothing,
      _dpFileSize = Nothing,
      _dpWidth = Nothing,
      _dpDurationMillis = Nothing
    }

-- | The detected height of the input file, in pixels.
dpHeight :: Lens' DetectedProperties (Maybe Int)
dpHeight = lens _dpHeight (\s a -> s {_dpHeight = a})

-- | The detected frame rate of the input file, in frames per second.
dpFrameRate :: Lens' DetectedProperties (Maybe Text)
dpFrameRate = lens _dpFrameRate (\s a -> s {_dpFrameRate = a})

-- | The detected file size of the input file, in bytes.
dpFileSize :: Lens' DetectedProperties (Maybe Integer)
dpFileSize = lens _dpFileSize (\s a -> s {_dpFileSize = a})

-- | The detected width of the input file, in pixels.
dpWidth :: Lens' DetectedProperties (Maybe Int)
dpWidth = lens _dpWidth (\s a -> s {_dpWidth = a})

-- | The detected duration of the input file, in milliseconds.
dpDurationMillis :: Lens' DetectedProperties (Maybe Integer)
dpDurationMillis = lens _dpDurationMillis (\s a -> s {_dpDurationMillis = a})

instance FromJSON DetectedProperties where
  parseJSON =
    withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            <$> (x .:? "Height")
            <*> (x .:? "FrameRate")
            <*> (x .:? "FileSize")
            <*> (x .:? "Width")
            <*> (x .:? "DurationMillis")
      )

instance Hashable DetectedProperties

instance NFData DetectedProperties

instance ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    object
      ( catMaybes
          [ ("Height" .=) <$> _dpHeight,
            ("FrameRate" .=) <$> _dpFrameRate,
            ("FileSize" .=) <$> _dpFileSize,
            ("Width" .=) <$> _dpWidth,
            ("DurationMillis" .=) <$> _dpDurationMillis
          ]
      )
