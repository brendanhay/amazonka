{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Av1FramerateControl
import Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
import Network.AWS.MediaConvert.Types.Av1RateControlMode
import Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization
import Network.AWS.Prelude

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- /See:/ 'av1Settings' smart constructor.
data Av1Settings = Av1Settings'
  { _asGopSize :: !(Maybe Double),
    _asNumberBFramesBetweenReferenceFrames :: !(Maybe Nat),
    _asSlices :: !(Maybe Nat),
    _asRateControlMode :: !(Maybe Av1RateControlMode),
    _asQvbrSettings :: !(Maybe Av1QvbrSettings),
    _asFramerateDenominator :: !(Maybe Nat),
    _asFramerateConversionAlgorithm ::
      !(Maybe Av1FramerateConversionAlgorithm),
    _asFramerateControl :: !(Maybe Av1FramerateControl),
    _asAdaptiveQuantization :: !(Maybe Av1AdaptiveQuantization),
    _asFramerateNumerator :: !(Maybe Nat),
    _asMaxBitrate :: !(Maybe Nat),
    _asSpatialAdaptiveQuantization ::
      !(Maybe Av1SpatialAdaptiveQuantization)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Av1Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asGopSize' - Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
--
-- * 'asNumberBFramesBetweenReferenceFrames' - Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
--
-- * 'asSlices' - Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
--
-- * 'asRateControlMode' - 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
--
-- * 'asQvbrSettings' - Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'asFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'asFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'asFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'asAdaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
--
-- * 'asFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'asMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- * 'asSpatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
av1Settings ::
  Av1Settings
av1Settings =
  Av1Settings'
    { _asGopSize = Nothing,
      _asNumberBFramesBetweenReferenceFrames = Nothing,
      _asSlices = Nothing,
      _asRateControlMode = Nothing,
      _asQvbrSettings = Nothing,
      _asFramerateDenominator = Nothing,
      _asFramerateConversionAlgorithm = Nothing,
      _asFramerateControl = Nothing,
      _asAdaptiveQuantization = Nothing,
      _asFramerateNumerator = Nothing,
      _asMaxBitrate = Nothing,
      _asSpatialAdaptiveQuantization = Nothing
    }

-- | Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
asGopSize :: Lens' Av1Settings (Maybe Double)
asGopSize = lens _asGopSize (\s a -> s {_asGopSize = a})

-- | Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
asNumberBFramesBetweenReferenceFrames :: Lens' Av1Settings (Maybe Natural)
asNumberBFramesBetweenReferenceFrames = lens _asNumberBFramesBetweenReferenceFrames (\s a -> s {_asNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
asSlices :: Lens' Av1Settings (Maybe Natural)
asSlices = lens _asSlices (\s a -> s {_asSlices = a}) . mapping _Nat

-- | 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
asRateControlMode :: Lens' Av1Settings (Maybe Av1RateControlMode)
asRateControlMode = lens _asRateControlMode (\s a -> s {_asRateControlMode = a})

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
asQvbrSettings :: Lens' Av1Settings (Maybe Av1QvbrSettings)
asQvbrSettings = lens _asQvbrSettings (\s a -> s {_asQvbrSettings = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
asFramerateDenominator :: Lens' Av1Settings (Maybe Natural)
asFramerateDenominator = lens _asFramerateDenominator (\s a -> s {_asFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
asFramerateConversionAlgorithm :: Lens' Av1Settings (Maybe Av1FramerateConversionAlgorithm)
asFramerateConversionAlgorithm = lens _asFramerateConversionAlgorithm (\s a -> s {_asFramerateConversionAlgorithm = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
asFramerateControl :: Lens' Av1Settings (Maybe Av1FramerateControl)
asFramerateControl = lens _asFramerateControl (\s a -> s {_asFramerateControl = a})

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
asAdaptiveQuantization :: Lens' Av1Settings (Maybe Av1AdaptiveQuantization)
asAdaptiveQuantization = lens _asAdaptiveQuantization (\s a -> s {_asAdaptiveQuantization = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
asFramerateNumerator :: Lens' Av1Settings (Maybe Natural)
asFramerateNumerator = lens _asFramerateNumerator (\s a -> s {_asFramerateNumerator = a}) . mapping _Nat

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
asMaxBitrate :: Lens' Av1Settings (Maybe Natural)
asMaxBitrate = lens _asMaxBitrate (\s a -> s {_asMaxBitrate = a}) . mapping _Nat

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
asSpatialAdaptiveQuantization :: Lens' Av1Settings (Maybe Av1SpatialAdaptiveQuantization)
asSpatialAdaptiveQuantization = lens _asSpatialAdaptiveQuantization (\s a -> s {_asSpatialAdaptiveQuantization = a})

instance FromJSON Av1Settings where
  parseJSON =
    withObject
      "Av1Settings"
      ( \x ->
          Av1Settings'
            <$> (x .:? "gopSize")
            <*> (x .:? "numberBFramesBetweenReferenceFrames")
            <*> (x .:? "slices")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "qvbrSettings")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "framerateControl")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "spatialAdaptiveQuantization")
      )

instance Hashable Av1Settings

instance NFData Av1Settings

instance ToJSON Av1Settings where
  toJSON Av1Settings' {..} =
    object
      ( catMaybes
          [ ("gopSize" .=) <$> _asGopSize,
            ("numberBFramesBetweenReferenceFrames" .=)
              <$> _asNumberBFramesBetweenReferenceFrames,
            ("slices" .=) <$> _asSlices,
            ("rateControlMode" .=) <$> _asRateControlMode,
            ("qvbrSettings" .=) <$> _asQvbrSettings,
            ("framerateDenominator" .=) <$> _asFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _asFramerateConversionAlgorithm,
            ("framerateControl" .=) <$> _asFramerateControl,
            ("adaptiveQuantization" .=) <$> _asAdaptiveQuantization,
            ("framerateNumerator" .=) <$> _asFramerateNumerator,
            ("maxBitrate" .=) <$> _asMaxBitrate,
            ("spatialAdaptiveQuantization" .=)
              <$> _asSpatialAdaptiveQuantization
          ]
      )
