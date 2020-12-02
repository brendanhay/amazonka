{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Vp9FramerateControl
import Network.AWS.MediaConvert.Types.Vp9FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp9ParControl
import Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp9RateControlMode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
--
-- /See:/ 'vp9Settings' smart constructor.
data Vp9Settings = Vp9Settings'
  { _vsQualityTuningLevel ::
      !(Maybe Vp9QualityTuningLevel),
    _vsParNumerator :: !(Maybe Nat),
    _vsGopSize :: !(Maybe Double),
    _vsHrdBufferSize :: !(Maybe Nat),
    _vsRateControlMode :: !(Maybe Vp9RateControlMode),
    _vsParControl :: !(Maybe Vp9ParControl),
    _vsBitrate :: !(Maybe Nat),
    _vsFramerateDenominator :: !(Maybe Nat),
    _vsFramerateConversionAlgorithm ::
      !(Maybe Vp9FramerateConversionAlgorithm),
    _vsFramerateControl :: !(Maybe Vp9FramerateControl),
    _vsFramerateNumerator :: !(Maybe Nat),
    _vsMaxBitrate :: !(Maybe Nat),
    _vsParDenominator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Vp9Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsQualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
--
-- * 'vsParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'vsGopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- * 'vsHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'vsRateControlMode' - With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
--
-- * 'vsParControl' - Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
--
-- * 'vsBitrate' - Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- * 'vsFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'vsFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'vsFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'vsFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'vsMaxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
--
-- * 'vsParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
vp9Settings ::
  Vp9Settings
vp9Settings =
  Vp9Settings'
    { _vsQualityTuningLevel = Nothing,
      _vsParNumerator = Nothing,
      _vsGopSize = Nothing,
      _vsHrdBufferSize = Nothing,
      _vsRateControlMode = Nothing,
      _vsParControl = Nothing,
      _vsBitrate = Nothing,
      _vsFramerateDenominator = Nothing,
      _vsFramerateConversionAlgorithm = Nothing,
      _vsFramerateControl = Nothing,
      _vsFramerateNumerator = Nothing,
      _vsMaxBitrate = Nothing,
      _vsParDenominator = Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
vsQualityTuningLevel :: Lens' Vp9Settings (Maybe Vp9QualityTuningLevel)
vsQualityTuningLevel = lens _vsQualityTuningLevel (\s a -> s {_vsQualityTuningLevel = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
vsParNumerator :: Lens' Vp9Settings (Maybe Natural)
vsParNumerator = lens _vsParNumerator (\s a -> s {_vsParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
vsGopSize :: Lens' Vp9Settings (Maybe Double)
vsGopSize = lens _vsGopSize (\s a -> s {_vsGopSize = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
vsHrdBufferSize :: Lens' Vp9Settings (Maybe Natural)
vsHrdBufferSize = lens _vsHrdBufferSize (\s a -> s {_vsHrdBufferSize = a}) . mapping _Nat

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
vsRateControlMode :: Lens' Vp9Settings (Maybe Vp9RateControlMode)
vsRateControlMode = lens _vsRateControlMode (\s a -> s {_vsRateControlMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
vsParControl :: Lens' Vp9Settings (Maybe Vp9ParControl)
vsParControl = lens _vsParControl (\s a -> s {_vsParControl = a})

-- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
vsBitrate :: Lens' Vp9Settings (Maybe Natural)
vsBitrate = lens _vsBitrate (\s a -> s {_vsBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vsFramerateDenominator :: Lens' Vp9Settings (Maybe Natural)
vsFramerateDenominator = lens _vsFramerateDenominator (\s a -> s {_vsFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
vsFramerateConversionAlgorithm :: Lens' Vp9Settings (Maybe Vp9FramerateConversionAlgorithm)
vsFramerateConversionAlgorithm = lens _vsFramerateConversionAlgorithm (\s a -> s {_vsFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
vsFramerateControl :: Lens' Vp9Settings (Maybe Vp9FramerateControl)
vsFramerateControl = lens _vsFramerateControl (\s a -> s {_vsFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vsFramerateNumerator :: Lens' Vp9Settings (Maybe Natural)
vsFramerateNumerator = lens _vsFramerateNumerator (\s a -> s {_vsFramerateNumerator = a}) . mapping _Nat

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
vsMaxBitrate :: Lens' Vp9Settings (Maybe Natural)
vsMaxBitrate = lens _vsMaxBitrate (\s a -> s {_vsMaxBitrate = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
vsParDenominator :: Lens' Vp9Settings (Maybe Natural)
vsParDenominator = lens _vsParDenominator (\s a -> s {_vsParDenominator = a}) . mapping _Nat

instance FromJSON Vp9Settings where
  parseJSON =
    withObject
      "Vp9Settings"
      ( \x ->
          Vp9Settings'
            <$> (x .:? "qualityTuningLevel")
            <*> (x .:? "parNumerator")
            <*> (x .:? "gopSize")
            <*> (x .:? "hrdBufferSize")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "parControl")
            <*> (x .:? "bitrate")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "framerateControl")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "parDenominator")
      )

instance Hashable Vp9Settings

instance NFData Vp9Settings

instance ToJSON Vp9Settings where
  toJSON Vp9Settings' {..} =
    object
      ( catMaybes
          [ ("qualityTuningLevel" .=) <$> _vsQualityTuningLevel,
            ("parNumerator" .=) <$> _vsParNumerator,
            ("gopSize" .=) <$> _vsGopSize,
            ("hrdBufferSize" .=) <$> _vsHrdBufferSize,
            ("rateControlMode" .=) <$> _vsRateControlMode,
            ("parControl" .=) <$> _vsParControl,
            ("bitrate" .=) <$> _vsBitrate,
            ("framerateDenominator" .=) <$> _vsFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _vsFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _vsFramerateControl,
            ("framerateNumerator" .=) <$> _vsFramerateNumerator,
            ("maxBitrate" .=) <$> _vsMaxBitrate,
            ("parDenominator" .=) <$> _vsParDenominator
          ]
      )
