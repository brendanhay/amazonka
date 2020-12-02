{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
import Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
import Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
import Network.AWS.MediaConvert.Types.Mpeg2FramerateControl
import Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
import Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
import Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
import Network.AWS.MediaConvert.Types.Mpeg2ParControl
import Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
import Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
import Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
import Network.AWS.MediaConvert.Types.Mpeg2SlowPal
import Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2Syntax
import Network.AWS.MediaConvert.Types.Mpeg2Telecine
import Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /See:/ 'mpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { _msQualityTuningLevel ::
      !(Maybe Mpeg2QualityTuningLevel),
    _msTemporalAdaptiveQuantization ::
      !(Maybe Mpeg2TemporalAdaptiveQuantization),
    _msSceneChangeDetect :: !(Maybe Mpeg2SceneChangeDetect),
    _msHrdBufferInitialFillPercentage :: !(Maybe Nat),
    _msSlowPal :: !(Maybe Mpeg2SlowPal),
    _msParNumerator :: !(Maybe Nat),
    _msGopSize :: !(Maybe Double),
    _msNumberBFramesBetweenReferenceFrames :: !(Maybe Nat),
    _msGopSizeUnits :: !(Maybe Mpeg2GopSizeUnits),
    _msHrdBufferSize :: !(Maybe Nat),
    _msRateControlMode :: !(Maybe Mpeg2RateControlMode),
    _msTelecine :: !(Maybe Mpeg2Telecine),
    _msIntraDcPrecision :: !(Maybe Mpeg2IntraDcPrecision),
    _msDynamicSubGop :: !(Maybe Mpeg2DynamicSubGop),
    _msMinIInterval :: !(Maybe Nat),
    _msInterlaceMode :: !(Maybe Mpeg2InterlaceMode),
    _msParControl :: !(Maybe Mpeg2ParControl),
    _msSoftness :: !(Maybe Nat),
    _msCodecProfile :: !(Maybe Mpeg2CodecProfile),
    _msBitrate :: !(Maybe Nat),
    _msFramerateDenominator :: !(Maybe Nat),
    _msFramerateConversionAlgorithm ::
      !(Maybe Mpeg2FramerateConversionAlgorithm),
    _msCodecLevel :: !(Maybe Mpeg2CodecLevel),
    _msFramerateControl :: !(Maybe Mpeg2FramerateControl),
    _msAdaptiveQuantization :: !(Maybe Mpeg2AdaptiveQuantization),
    _msFramerateNumerator :: !(Maybe Nat),
    _msMaxBitrate :: !(Maybe Nat),
    _msSyntax :: !(Maybe Mpeg2Syntax),
    _msGopClosedCadence :: !(Maybe Nat),
    _msParDenominator :: !(Maybe Nat),
    _msSpatialAdaptiveQuantization ::
      !(Maybe Mpeg2SpatialAdaptiveQuantization)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mpeg2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msQualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- * 'msTemporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- * 'msSceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
--
-- * 'msHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'msSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'msParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'msGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'msNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'msGopSizeUnits' - Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- * 'msHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'msRateControlMode' - Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
--
-- * 'msTelecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- * 'msIntraDcPrecision' - Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
--
-- * 'msDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'msMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'msInterlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- * 'msParControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- * 'msSoftness' - Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- * 'msCodecProfile' - Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
--
-- * 'msBitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'msFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'msFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'msCodecLevel' - Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
--
-- * 'msFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'msAdaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- * 'msFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'msMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- * 'msSyntax' - Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
--
-- * 'msGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'msParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- * 'msSpatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
mpeg2Settings ::
  Mpeg2Settings
mpeg2Settings =
  Mpeg2Settings'
    { _msQualityTuningLevel = Nothing,
      _msTemporalAdaptiveQuantization = Nothing,
      _msSceneChangeDetect = Nothing,
      _msHrdBufferInitialFillPercentage = Nothing,
      _msSlowPal = Nothing,
      _msParNumerator = Nothing,
      _msGopSize = Nothing,
      _msNumberBFramesBetweenReferenceFrames = Nothing,
      _msGopSizeUnits = Nothing,
      _msHrdBufferSize = Nothing,
      _msRateControlMode = Nothing,
      _msTelecine = Nothing,
      _msIntraDcPrecision = Nothing,
      _msDynamicSubGop = Nothing,
      _msMinIInterval = Nothing,
      _msInterlaceMode = Nothing,
      _msParControl = Nothing,
      _msSoftness = Nothing,
      _msCodecProfile = Nothing,
      _msBitrate = Nothing,
      _msFramerateDenominator = Nothing,
      _msFramerateConversionAlgorithm = Nothing,
      _msCodecLevel = Nothing,
      _msFramerateControl = Nothing,
      _msAdaptiveQuantization = Nothing,
      _msFramerateNumerator = Nothing,
      _msMaxBitrate = Nothing,
      _msSyntax = Nothing,
      _msGopClosedCadence = Nothing,
      _msParDenominator = Nothing,
      _msSpatialAdaptiveQuantization = Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
msQualityTuningLevel :: Lens' Mpeg2Settings (Maybe Mpeg2QualityTuningLevel)
msQualityTuningLevel = lens _msQualityTuningLevel (\s a -> s {_msQualityTuningLevel = a})

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
msTemporalAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2TemporalAdaptiveQuantization)
msTemporalAdaptiveQuantization = lens _msTemporalAdaptiveQuantization (\s a -> s {_msTemporalAdaptiveQuantization = a})

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
msSceneChangeDetect :: Lens' Mpeg2Settings (Maybe Mpeg2SceneChangeDetect)
msSceneChangeDetect = lens _msSceneChangeDetect (\s a -> s {_msSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
msHrdBufferInitialFillPercentage :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferInitialFillPercentage = lens _msHrdBufferInitialFillPercentage (\s a -> s {_msHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
msSlowPal :: Lens' Mpeg2Settings (Maybe Mpeg2SlowPal)
msSlowPal = lens _msSlowPal (\s a -> s {_msSlowPal = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
msParNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msParNumerator = lens _msParNumerator (\s a -> s {_msParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
msGopSize :: Lens' Mpeg2Settings (Maybe Double)
msGopSize = lens _msGopSize (\s a -> s {_msGopSize = a})

-- | Number of B-frames between reference frames.
msNumberBFramesBetweenReferenceFrames :: Lens' Mpeg2Settings (Maybe Natural)
msNumberBFramesBetweenReferenceFrames = lens _msNumberBFramesBetweenReferenceFrames (\s a -> s {_msNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
msGopSizeUnits :: Lens' Mpeg2Settings (Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = lens _msGopSizeUnits (\s a -> s {_msGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
msHrdBufferSize :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferSize = lens _msHrdBufferSize (\s a -> s {_msHrdBufferSize = a}) . mapping _Nat

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
msRateControlMode :: Lens' Mpeg2Settings (Maybe Mpeg2RateControlMode)
msRateControlMode = lens _msRateControlMode (\s a -> s {_msRateControlMode = a})

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
msTelecine :: Lens' Mpeg2Settings (Maybe Mpeg2Telecine)
msTelecine = lens _msTelecine (\s a -> s {_msTelecine = a})

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
msIntraDcPrecision :: Lens' Mpeg2Settings (Maybe Mpeg2IntraDcPrecision)
msIntraDcPrecision = lens _msIntraDcPrecision (\s a -> s {_msIntraDcPrecision = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
msDynamicSubGop :: Lens' Mpeg2Settings (Maybe Mpeg2DynamicSubGop)
msDynamicSubGop = lens _msDynamicSubGop (\s a -> s {_msDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
msMinIInterval :: Lens' Mpeg2Settings (Maybe Natural)
msMinIInterval = lens _msMinIInterval (\s a -> s {_msMinIInterval = a}) . mapping _Nat

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
msInterlaceMode :: Lens' Mpeg2Settings (Maybe Mpeg2InterlaceMode)
msInterlaceMode = lens _msInterlaceMode (\s a -> s {_msInterlaceMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
msParControl :: Lens' Mpeg2Settings (Maybe Mpeg2ParControl)
msParControl = lens _msParControl (\s a -> s {_msParControl = a})

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
msSoftness :: Lens' Mpeg2Settings (Maybe Natural)
msSoftness = lens _msSoftness (\s a -> s {_msSoftness = a}) . mapping _Nat

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
msCodecProfile :: Lens' Mpeg2Settings (Maybe Mpeg2CodecProfile)
msCodecProfile = lens _msCodecProfile (\s a -> s {_msCodecProfile = a})

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
msBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msBitrate = lens _msBitrate (\s a -> s {_msBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
msFramerateDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateDenominator = lens _msFramerateDenominator (\s a -> s {_msFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
msFramerateConversionAlgorithm :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateConversionAlgorithm)
msFramerateConversionAlgorithm = lens _msFramerateConversionAlgorithm (\s a -> s {_msFramerateConversionAlgorithm = a})

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
msCodecLevel :: Lens' Mpeg2Settings (Maybe Mpeg2CodecLevel)
msCodecLevel = lens _msCodecLevel (\s a -> s {_msCodecLevel = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
msFramerateControl :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateControl)
msFramerateControl = lens _msFramerateControl (\s a -> s {_msFramerateControl = a})

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
msAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = lens _msAdaptiveQuantization (\s a -> s {_msAdaptiveQuantization = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
msFramerateNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateNumerator = lens _msFramerateNumerator (\s a -> s {_msFramerateNumerator = a}) . mapping _Nat

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
msMaxBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msMaxBitrate = lens _msMaxBitrate (\s a -> s {_msMaxBitrate = a}) . mapping _Nat

-- | Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
msSyntax :: Lens' Mpeg2Settings (Maybe Mpeg2Syntax)
msSyntax = lens _msSyntax (\s a -> s {_msSyntax = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
msGopClosedCadence :: Lens' Mpeg2Settings (Maybe Natural)
msGopClosedCadence = lens _msGopClosedCadence (\s a -> s {_msGopClosedCadence = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
msParDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msParDenominator = lens _msParDenominator (\s a -> s {_msParDenominator = a}) . mapping _Nat

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
msSpatialAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2SpatialAdaptiveQuantization)
msSpatialAdaptiveQuantization = lens _msSpatialAdaptiveQuantization (\s a -> s {_msSpatialAdaptiveQuantization = a})

instance FromJSON Mpeg2Settings where
  parseJSON =
    withObject
      "Mpeg2Settings"
      ( \x ->
          Mpeg2Settings'
            <$> (x .:? "qualityTuningLevel")
            <*> (x .:? "temporalAdaptiveQuantization")
            <*> (x .:? "sceneChangeDetect")
            <*> (x .:? "hrdBufferInitialFillPercentage")
            <*> (x .:? "slowPal")
            <*> (x .:? "parNumerator")
            <*> (x .:? "gopSize")
            <*> (x .:? "numberBFramesBetweenReferenceFrames")
            <*> (x .:? "gopSizeUnits")
            <*> (x .:? "hrdBufferSize")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "telecine")
            <*> (x .:? "intraDcPrecision")
            <*> (x .:? "dynamicSubGop")
            <*> (x .:? "minIInterval")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "parControl")
            <*> (x .:? "softness")
            <*> (x .:? "codecProfile")
            <*> (x .:? "bitrate")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "codecLevel")
            <*> (x .:? "framerateControl")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "syntax")
            <*> (x .:? "gopClosedCadence")
            <*> (x .:? "parDenominator")
            <*> (x .:? "spatialAdaptiveQuantization")
      )

instance Hashable Mpeg2Settings

instance NFData Mpeg2Settings

instance ToJSON Mpeg2Settings where
  toJSON Mpeg2Settings' {..} =
    object
      ( catMaybes
          [ ("qualityTuningLevel" .=) <$> _msQualityTuningLevel,
            ("temporalAdaptiveQuantization" .=)
              <$> _msTemporalAdaptiveQuantization,
            ("sceneChangeDetect" .=) <$> _msSceneChangeDetect,
            ("hrdBufferInitialFillPercentage" .=)
              <$> _msHrdBufferInitialFillPercentage,
            ("slowPal" .=) <$> _msSlowPal,
            ("parNumerator" .=) <$> _msParNumerator,
            ("gopSize" .=) <$> _msGopSize,
            ("numberBFramesBetweenReferenceFrames" .=)
              <$> _msNumberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" .=) <$> _msGopSizeUnits,
            ("hrdBufferSize" .=) <$> _msHrdBufferSize,
            ("rateControlMode" .=) <$> _msRateControlMode,
            ("telecine" .=) <$> _msTelecine,
            ("intraDcPrecision" .=) <$> _msIntraDcPrecision,
            ("dynamicSubGop" .=) <$> _msDynamicSubGop,
            ("minIInterval" .=) <$> _msMinIInterval,
            ("interlaceMode" .=) <$> _msInterlaceMode,
            ("parControl" .=) <$> _msParControl,
            ("softness" .=) <$> _msSoftness,
            ("codecProfile" .=) <$> _msCodecProfile,
            ("bitrate" .=) <$> _msBitrate,
            ("framerateDenominator" .=) <$> _msFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _msFramerateConversionAlgorithm,
            ("codecLevel" .=) <$> _msCodecLevel,
            ("framerateControl" .=) <$> _msFramerateControl,
            ("adaptiveQuantization" .=) <$> _msAdaptiveQuantization,
            ("framerateNumerator" .=) <$> _msFramerateNumerator,
            ("maxBitrate" .=) <$> _msMaxBitrate,
            ("syntax" .=) <$> _msSyntax,
            ("gopClosedCadence" .=) <$> _msGopClosedCadence,
            ("parDenominator" .=) <$> _msParDenominator,
            ("spatialAdaptiveQuantization" .=)
              <$> _msSpatialAdaptiveQuantization
          ]
      )
