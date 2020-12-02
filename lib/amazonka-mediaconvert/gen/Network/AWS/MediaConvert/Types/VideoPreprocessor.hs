{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoPreprocessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoPreprocessor where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.Deinterlacer
import Network.AWS.MediaConvert.Types.DolbyVision
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.TimecodeBurnin
import Network.AWS.Prelude

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /See:/ 'videoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { _vpTimecodeBurnin ::
      !(Maybe TimecodeBurnin),
    _vpDolbyVision :: !(Maybe DolbyVision),
    _vpColorCorrector :: !(Maybe ColorCorrector),
    _vpDeinterlacer :: !(Maybe Deinterlacer),
    _vpNoiseReducer :: !(Maybe NoiseReducer),
    _vpImageInserter :: !(Maybe ImageInserter),
    _vpPartnerWatermarking :: !(Maybe PartnerWatermarking)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoPreprocessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpTimecodeBurnin' - Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- * 'vpDolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video output.
--
-- * 'vpColorCorrector' - Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- * 'vpDeinterlacer' - Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
--
-- * 'vpNoiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- * 'vpImageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- * 'vpPartnerWatermarking' - If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
videoPreprocessor ::
  VideoPreprocessor
videoPreprocessor =
  VideoPreprocessor'
    { _vpTimecodeBurnin = Nothing,
      _vpDolbyVision = Nothing,
      _vpColorCorrector = Nothing,
      _vpDeinterlacer = Nothing,
      _vpNoiseReducer = Nothing,
      _vpImageInserter = Nothing,
      _vpPartnerWatermarking = Nothing
    }

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
vpTimecodeBurnin :: Lens' VideoPreprocessor (Maybe TimecodeBurnin)
vpTimecodeBurnin = lens _vpTimecodeBurnin (\s a -> s {_vpTimecodeBurnin = a})

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video output.
vpDolbyVision :: Lens' VideoPreprocessor (Maybe DolbyVision)
vpDolbyVision = lens _vpDolbyVision (\s a -> s {_vpDolbyVision = a})

-- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
vpColorCorrector :: Lens' VideoPreprocessor (Maybe ColorCorrector)
vpColorCorrector = lens _vpColorCorrector (\s a -> s {_vpColorCorrector = a})

-- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
vpDeinterlacer :: Lens' VideoPreprocessor (Maybe Deinterlacer)
vpDeinterlacer = lens _vpDeinterlacer (\s a -> s {_vpDeinterlacer = a})

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
vpNoiseReducer :: Lens' VideoPreprocessor (Maybe NoiseReducer)
vpNoiseReducer = lens _vpNoiseReducer (\s a -> s {_vpNoiseReducer = a})

-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
vpImageInserter :: Lens' VideoPreprocessor (Maybe ImageInserter)
vpImageInserter = lens _vpImageInserter (\s a -> s {_vpImageInserter = a})

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
vpPartnerWatermarking :: Lens' VideoPreprocessor (Maybe PartnerWatermarking)
vpPartnerWatermarking = lens _vpPartnerWatermarking (\s a -> s {_vpPartnerWatermarking = a})

instance FromJSON VideoPreprocessor where
  parseJSON =
    withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            <$> (x .:? "timecodeBurnin")
            <*> (x .:? "dolbyVision")
            <*> (x .:? "colorCorrector")
            <*> (x .:? "deinterlacer")
            <*> (x .:? "noiseReducer")
            <*> (x .:? "imageInserter")
            <*> (x .:? "partnerWatermarking")
      )

instance Hashable VideoPreprocessor

instance NFData VideoPreprocessor

instance ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    object
      ( catMaybes
          [ ("timecodeBurnin" .=) <$> _vpTimecodeBurnin,
            ("dolbyVision" .=) <$> _vpDolbyVision,
            ("colorCorrector" .=) <$> _vpColorCorrector,
            ("deinterlacer" .=) <$> _vpDeinterlacer,
            ("noiseReducer" .=) <$> _vpNoiseReducer,
            ("imageInserter" .=) <$> _vpImageInserter,
            ("partnerWatermarking" .=) <$> _vpPartnerWatermarking
          ]
      )
