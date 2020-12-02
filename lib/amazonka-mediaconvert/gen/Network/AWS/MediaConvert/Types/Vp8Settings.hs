{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Vp8FramerateControl
import Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp8ParControl
import Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp8RateControlMode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
--
-- /See:/ 'vp8Settings' smart constructor.
data Vp8Settings = Vp8Settings'
  { _vQualityTuningLevel ::
      !(Maybe Vp8QualityTuningLevel),
    _vParNumerator :: !(Maybe Nat),
    _vGopSize :: !(Maybe Double),
    _vHrdBufferSize :: !(Maybe Nat),
    _vRateControlMode :: !(Maybe Vp8RateControlMode),
    _vParControl :: !(Maybe Vp8ParControl),
    _vBitrate :: !(Maybe Nat),
    _vFramerateDenominator :: !(Maybe Nat),
    _vFramerateConversionAlgorithm ::
      !(Maybe Vp8FramerateConversionAlgorithm),
    _vFramerateControl :: !(Maybe Vp8FramerateControl),
    _vFramerateNumerator :: !(Maybe Nat),
    _vMaxBitrate :: !(Maybe Nat),
    _vParDenominator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Vp8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vQualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
--
-- * 'vParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'vGopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- * 'vHrdBufferSize' - Optional. Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'vRateControlMode' - With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
--
-- * 'vParControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- * 'vBitrate' - Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- * 'vFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'vFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'vFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'vFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'vMaxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
--
-- * 'vParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
vp8Settings ::
  Vp8Settings
vp8Settings =
  Vp8Settings'
    { _vQualityTuningLevel = Nothing,
      _vParNumerator = Nothing,
      _vGopSize = Nothing,
      _vHrdBufferSize = Nothing,
      _vRateControlMode = Nothing,
      _vParControl = Nothing,
      _vBitrate = Nothing,
      _vFramerateDenominator = Nothing,
      _vFramerateConversionAlgorithm = Nothing,
      _vFramerateControl = Nothing,
      _vFramerateNumerator = Nothing,
      _vMaxBitrate = Nothing,
      _vParDenominator = Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
vQualityTuningLevel :: Lens' Vp8Settings (Maybe Vp8QualityTuningLevel)
vQualityTuningLevel = lens _vQualityTuningLevel (\s a -> s {_vQualityTuningLevel = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
vParNumerator :: Lens' Vp8Settings (Maybe Natural)
vParNumerator = lens _vParNumerator (\s a -> s {_vParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
vGopSize :: Lens' Vp8Settings (Maybe Double)
vGopSize = lens _vGopSize (\s a -> s {_vGopSize = a})

-- | Optional. Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
vHrdBufferSize :: Lens' Vp8Settings (Maybe Natural)
vHrdBufferSize = lens _vHrdBufferSize (\s a -> s {_vHrdBufferSize = a}) . mapping _Nat

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
vRateControlMode :: Lens' Vp8Settings (Maybe Vp8RateControlMode)
vRateControlMode = lens _vRateControlMode (\s a -> s {_vRateControlMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
vParControl :: Lens' Vp8Settings (Maybe Vp8ParControl)
vParControl = lens _vParControl (\s a -> s {_vParControl = a})

-- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
vBitrate :: Lens' Vp8Settings (Maybe Natural)
vBitrate = lens _vBitrate (\s a -> s {_vBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vFramerateDenominator :: Lens' Vp8Settings (Maybe Natural)
vFramerateDenominator = lens _vFramerateDenominator (\s a -> s {_vFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
vFramerateConversionAlgorithm :: Lens' Vp8Settings (Maybe Vp8FramerateConversionAlgorithm)
vFramerateConversionAlgorithm = lens _vFramerateConversionAlgorithm (\s a -> s {_vFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
vFramerateControl :: Lens' Vp8Settings (Maybe Vp8FramerateControl)
vFramerateControl = lens _vFramerateControl (\s a -> s {_vFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
vFramerateNumerator :: Lens' Vp8Settings (Maybe Natural)
vFramerateNumerator = lens _vFramerateNumerator (\s a -> s {_vFramerateNumerator = a}) . mapping _Nat

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
vMaxBitrate :: Lens' Vp8Settings (Maybe Natural)
vMaxBitrate = lens _vMaxBitrate (\s a -> s {_vMaxBitrate = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
vParDenominator :: Lens' Vp8Settings (Maybe Natural)
vParDenominator = lens _vParDenominator (\s a -> s {_vParDenominator = a}) . mapping _Nat

instance FromJSON Vp8Settings where
  parseJSON =
    withObject
      "Vp8Settings"
      ( \x ->
          Vp8Settings'
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

instance Hashable Vp8Settings

instance NFData Vp8Settings

instance ToJSON Vp8Settings where
  toJSON Vp8Settings' {..} =
    object
      ( catMaybes
          [ ("qualityTuningLevel" .=) <$> _vQualityTuningLevel,
            ("parNumerator" .=) <$> _vParNumerator,
            ("gopSize" .=) <$> _vGopSize,
            ("hrdBufferSize" .=) <$> _vHrdBufferSize,
            ("rateControlMode" .=) <$> _vRateControlMode,
            ("parControl" .=) <$> _vParControl,
            ("bitrate" .=) <$> _vBitrate,
            ("framerateDenominator" .=) <$> _vFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _vFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _vFramerateControl,
            ("framerateNumerator" .=) <$> _vFramerateNumerator,
            ("maxBitrate" .=) <$> _vMaxBitrate,
            ("parDenominator" .=) <$> _vParDenominator
          ]
      )
