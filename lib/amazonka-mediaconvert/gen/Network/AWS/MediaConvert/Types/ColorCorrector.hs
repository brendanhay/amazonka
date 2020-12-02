{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorCorrector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorCorrector where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ColorSpaceConversion
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.Prelude

-- | Settings for color correction.
--
-- /See:/ 'colorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { _ccSaturation ::
      !(Maybe Nat),
    _ccHue :: !(Maybe Int),
    _ccColorSpaceConversion :: !(Maybe ColorSpaceConversion),
    _ccHdr10Metadata :: !(Maybe Hdr10Metadata),
    _ccContrast :: !(Maybe Nat),
    _ccBrightness :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColorCorrector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSaturation' - Saturation level.
--
-- * 'ccHue' - Hue in degrees.
--
-- * 'ccColorSpaceConversion' - Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
--
-- * 'ccHdr10Metadata' - Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
--
-- * 'ccContrast' - Contrast level.
--
-- * 'ccBrightness' - Brightness level.
colorCorrector ::
  ColorCorrector
colorCorrector =
  ColorCorrector'
    { _ccSaturation = Nothing,
      _ccHue = Nothing,
      _ccColorSpaceConversion = Nothing,
      _ccHdr10Metadata = Nothing,
      _ccContrast = Nothing,
      _ccBrightness = Nothing
    }

-- | Saturation level.
ccSaturation :: Lens' ColorCorrector (Maybe Natural)
ccSaturation = lens _ccSaturation (\s a -> s {_ccSaturation = a}) . mapping _Nat

-- | Hue in degrees.
ccHue :: Lens' ColorCorrector (Maybe Int)
ccHue = lens _ccHue (\s a -> s {_ccHue = a})

-- | Specify the color space you want for this output. The service supports conversion between HDR formats, between SDR formats, from SDR to HDR, and from HDR to SDR. SDR to HDR conversion doesn't upgrade the dynamic range. The converted video has an HDR format, but visually appears the same as an unconverted output. HDR to SDR conversion uses Elemental tone mapping technology to approximate the outcome of manually regrading from HDR to SDR.
ccColorSpaceConversion :: Lens' ColorCorrector (Maybe ColorSpaceConversion)
ccColorSpaceConversion = lens _ccColorSpaceConversion (\s a -> s {_ccColorSpaceConversion = a})

-- | Use these settings when you convert to the HDR 10 color space. Specify the SMPTE ST 2086 Mastering Display Color Volume static metadata that you want signaled in the output. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator. When you set Color space conversion (ColorSpaceConversion) to HDR 10 (FORCE_HDR10), these settings are required. You must set values for Max frame average light level (maxFrameAverageLightLevel) and Max content light level (maxContentLightLevel); these settings don't have a default value. The default values for the other HDR 10 metadata settings are defined by the P3D65 color space. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
ccHdr10Metadata :: Lens' ColorCorrector (Maybe Hdr10Metadata)
ccHdr10Metadata = lens _ccHdr10Metadata (\s a -> s {_ccHdr10Metadata = a})

-- | Contrast level.
ccContrast :: Lens' ColorCorrector (Maybe Natural)
ccContrast = lens _ccContrast (\s a -> s {_ccContrast = a}) . mapping _Nat

-- | Brightness level.
ccBrightness :: Lens' ColorCorrector (Maybe Natural)
ccBrightness = lens _ccBrightness (\s a -> s {_ccBrightness = a}) . mapping _Nat

instance FromJSON ColorCorrector where
  parseJSON =
    withObject
      "ColorCorrector"
      ( \x ->
          ColorCorrector'
            <$> (x .:? "saturation")
            <*> (x .:? "hue")
            <*> (x .:? "colorSpaceConversion")
            <*> (x .:? "hdr10Metadata")
            <*> (x .:? "contrast")
            <*> (x .:? "brightness")
      )

instance Hashable ColorCorrector

instance NFData ColorCorrector

instance ToJSON ColorCorrector where
  toJSON ColorCorrector' {..} =
    object
      ( catMaybes
          [ ("saturation" .=) <$> _ccSaturation,
            ("hue" .=) <$> _ccHue,
            ("colorSpaceConversion" .=) <$> _ccColorSpaceConversion,
            ("hdr10Metadata" .=) <$> _ccHdr10Metadata,
            ("contrast" .=) <$> _ccContrast,
            ("brightness" .=) <$> _ccBrightness
          ]
      )
