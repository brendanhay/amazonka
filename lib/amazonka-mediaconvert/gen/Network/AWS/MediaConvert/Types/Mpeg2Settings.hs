{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Settings
  ( Mpeg2Settings (..),

    -- * Smart constructor
    mkMpeg2Settings,

    -- * Lenses
    msQualityTuningLevel,
    msTemporalAdaptiveQuantization,
    msSceneChangeDetect,
    msHrdBufferInitialFillPercentage,
    msSlowPal,
    msParNumerator,
    msGopSize,
    msNumberBFramesBetweenReferenceFrames,
    msGopSizeUnits,
    msHrdBufferSize,
    msRateControlMode,
    msTelecine,
    msIntraDcPrecision,
    msDynamicSubGop,
    msMinIInterval,
    msInterlaceMode,
    msParControl,
    msSoftness,
    msCodecProfile,
    msBitrate,
    msFramerateDenominator,
    msFramerateConversionAlgorithm,
    msCodecLevel,
    msFramerateControl,
    msAdaptiveQuantization,
    msFramerateNumerator,
    msMaxBitrate,
    msSyntax,
    msGopClosedCadence,
    msParDenominator,
    msSpatialAdaptiveQuantization,
  )
where

import qualified Network.AWS.Lens as Lens
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
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /See:/ 'mkMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { qualityTuningLevel ::
      Lude.Maybe Mpeg2QualityTuningLevel,
    temporalAdaptiveQuantization ::
      Lude.Maybe Mpeg2TemporalAdaptiveQuantization,
    sceneChangeDetect :: Lude.Maybe Mpeg2SceneChangeDetect,
    hrdBufferInitialFillPercentage :: Lude.Maybe Lude.Natural,
    slowPal :: Lude.Maybe Mpeg2SlowPal,
    parNumerator :: Lude.Maybe Lude.Natural,
    gopSize :: Lude.Maybe Lude.Double,
    numberBFramesBetweenReferenceFrames :: Lude.Maybe Lude.Natural,
    gopSizeUnits :: Lude.Maybe Mpeg2GopSizeUnits,
    hrdBufferSize :: Lude.Maybe Lude.Natural,
    rateControlMode :: Lude.Maybe Mpeg2RateControlMode,
    telecine :: Lude.Maybe Mpeg2Telecine,
    intraDcPrecision :: Lude.Maybe Mpeg2IntraDcPrecision,
    dynamicSubGop :: Lude.Maybe Mpeg2DynamicSubGop,
    minIInterval :: Lude.Maybe Lude.Natural,
    interlaceMode :: Lude.Maybe Mpeg2InterlaceMode,
    parControl :: Lude.Maybe Mpeg2ParControl,
    softness :: Lude.Maybe Lude.Natural,
    codecProfile :: Lude.Maybe Mpeg2CodecProfile,
    bitrate :: Lude.Maybe Lude.Natural,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    framerateConversionAlgorithm ::
      Lude.Maybe Mpeg2FramerateConversionAlgorithm,
    codecLevel :: Lude.Maybe Mpeg2CodecLevel,
    framerateControl :: Lude.Maybe Mpeg2FramerateControl,
    adaptiveQuantization :: Lude.Maybe Mpeg2AdaptiveQuantization,
    framerateNumerator :: Lude.Maybe Lude.Natural,
    maxBitrate :: Lude.Maybe Lude.Natural,
    syntax :: Lude.Maybe Mpeg2Syntax,
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    parDenominator :: Lude.Maybe Lude.Natural,
    spatialAdaptiveQuantization ::
      Lude.Maybe Mpeg2SpatialAdaptiveQuantization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mpeg2Settings' with the minimum fields required to make a request.
--
-- * 'adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
-- * 'bitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
-- * 'codecLevel' - Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
-- * 'codecProfile' - Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
-- * 'dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
-- * 'gopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
-- * 'gopSizeUnits' - Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
-- * 'hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
-- * 'hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
-- * 'interlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
-- * 'intraDcPrecision' - Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
-- * 'maxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
-- * 'minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
-- * 'numberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
-- * 'rateControlMode' - Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
-- * 'sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'softness' - Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
-- * 'spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
-- * 'syntax' - Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
-- * 'telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
-- * 'temporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
mkMpeg2Settings ::
  Mpeg2Settings
mkMpeg2Settings =
  Mpeg2Settings'
    { qualityTuningLevel = Lude.Nothing,
      temporalAdaptiveQuantization = Lude.Nothing,
      sceneChangeDetect = Lude.Nothing,
      hrdBufferInitialFillPercentage = Lude.Nothing,
      slowPal = Lude.Nothing,
      parNumerator = Lude.Nothing,
      gopSize = Lude.Nothing,
      numberBFramesBetweenReferenceFrames = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      hrdBufferSize = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      telecine = Lude.Nothing,
      intraDcPrecision = Lude.Nothing,
      dynamicSubGop = Lude.Nothing,
      minIInterval = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      parControl = Lude.Nothing,
      softness = Lude.Nothing,
      codecProfile = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      codecLevel = Lude.Nothing,
      framerateControl = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      syntax = Lude.Nothing,
      gopClosedCadence = Lude.Nothing,
      parDenominator = Lude.Nothing,
      spatialAdaptiveQuantization = Lude.Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msQualityTuningLevel :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2QualityTuningLevel)
msQualityTuningLevel = Lens.lens (qualityTuningLevel :: Mpeg2Settings -> Lude.Maybe Mpeg2QualityTuningLevel) (\s a -> s {qualityTuningLevel = a} :: Mpeg2Settings)
{-# DEPRECATED msQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTemporalAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2TemporalAdaptiveQuantization)
msTemporalAdaptiveQuantization = Lens.lens (temporalAdaptiveQuantization :: Mpeg2Settings -> Lude.Maybe Mpeg2TemporalAdaptiveQuantization) (\s a -> s {temporalAdaptiveQuantization = a} :: Mpeg2Settings)
{-# DEPRECATED msTemporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead." #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSceneChangeDetect :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2SceneChangeDetect)
msSceneChangeDetect = Lens.lens (sceneChangeDetect :: Mpeg2Settings -> Lude.Maybe Mpeg2SceneChangeDetect) (\s a -> s {sceneChangeDetect = a} :: Mpeg2Settings)
{-# DEPRECATED msSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msHrdBufferInitialFillPercentage :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msHrdBufferInitialFillPercentage = Lens.lens (hrdBufferInitialFillPercentage :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferInitialFillPercentage = a} :: Mpeg2Settings)
{-# DEPRECATED msHrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead." #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSlowPal :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2SlowPal)
msSlowPal = Lens.lens (slowPal :: Mpeg2Settings -> Lude.Maybe Mpeg2SlowPal) (\s a -> s {slowPal = a} :: Mpeg2Settings)
{-# DEPRECATED msSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msParNumerator :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msParNumerator = Lens.lens (parNumerator :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: Mpeg2Settings)
{-# DEPRECATED msParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSize :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Double)
msGopSize = Lens.lens (gopSize :: Mpeg2Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: Mpeg2Settings)
{-# DEPRECATED msGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msNumberBFramesBetweenReferenceFrames :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msNumberBFramesBetweenReferenceFrames = Lens.lens (numberBFramesBetweenReferenceFrames :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberBFramesBetweenReferenceFrames = a} :: Mpeg2Settings)
{-# DEPRECATED msNumberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead." #-}

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSizeUnits :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = Lens.lens (gopSizeUnits :: Mpeg2Settings -> Lude.Maybe Mpeg2GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: Mpeg2Settings)
{-# DEPRECATED msGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msHrdBufferSize :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msHrdBufferSize = Lens.lens (hrdBufferSize :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferSize = a} :: Mpeg2Settings)
{-# DEPRECATED msHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRateControlMode :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2RateControlMode)
msRateControlMode = Lens.lens (rateControlMode :: Mpeg2Settings -> Lude.Maybe Mpeg2RateControlMode) (\s a -> s {rateControlMode = a} :: Mpeg2Settings)
{-# DEPRECATED msRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTelecine :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2Telecine)
msTelecine = Lens.lens (telecine :: Mpeg2Settings -> Lude.Maybe Mpeg2Telecine) (\s a -> s {telecine = a} :: Mpeg2Settings)
{-# DEPRECATED msTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
--
-- /Note:/ Consider using 'intraDcPrecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msIntraDcPrecision :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2IntraDcPrecision)
msIntraDcPrecision = Lens.lens (intraDcPrecision :: Mpeg2Settings -> Lude.Maybe Mpeg2IntraDcPrecision) (\s a -> s {intraDcPrecision = a} :: Mpeg2Settings)
{-# DEPRECATED msIntraDcPrecision "Use generic-lens or generic-optics with 'intraDcPrecision' instead." #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msDynamicSubGop :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2DynamicSubGop)
msDynamicSubGop = Lens.lens (dynamicSubGop :: Mpeg2Settings -> Lude.Maybe Mpeg2DynamicSubGop) (\s a -> s {dynamicSubGop = a} :: Mpeg2Settings)
{-# DEPRECATED msDynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead." #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMinIInterval :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msMinIInterval = Lens.lens (minIInterval :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {minIInterval = a} :: Mpeg2Settings)
{-# DEPRECATED msMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msInterlaceMode :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2InterlaceMode)
msInterlaceMode = Lens.lens (interlaceMode :: Mpeg2Settings -> Lude.Maybe Mpeg2InterlaceMode) (\s a -> s {interlaceMode = a} :: Mpeg2Settings)
{-# DEPRECATED msInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msParControl :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2ParControl)
msParControl = Lens.lens (parControl :: Mpeg2Settings -> Lude.Maybe Mpeg2ParControl) (\s a -> s {parControl = a} :: Mpeg2Settings)
{-# DEPRECATED msParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSoftness :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msSoftness = Lens.lens (softness :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {softness = a} :: Mpeg2Settings)
{-# DEPRECATED msSoftness "Use generic-lens or generic-optics with 'softness' instead." #-}

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCodecProfile :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2CodecProfile)
msCodecProfile = Lens.lens (codecProfile :: Mpeg2Settings -> Lude.Maybe Mpeg2CodecProfile) (\s a -> s {codecProfile = a} :: Mpeg2Settings)
{-# DEPRECATED msCodecProfile "Use generic-lens or generic-optics with 'codecProfile' instead." #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msBitrate :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msBitrate = Lens.lens (bitrate :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Mpeg2Settings)
{-# DEPRECATED msBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateDenominator :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msFramerateDenominator = Lens.lens (framerateDenominator :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateConversionAlgorithm :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2FramerateConversionAlgorithm)
msFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: Mpeg2Settings -> Lude.Maybe Mpeg2FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCodecLevel :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2CodecLevel)
msCodecLevel = Lens.lens (codecLevel :: Mpeg2Settings -> Lude.Maybe Mpeg2CodecLevel) (\s a -> s {codecLevel = a} :: Mpeg2Settings)
{-# DEPRECATED msCodecLevel "Use generic-lens or generic-optics with 'codecLevel' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateControl :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2FramerateControl)
msFramerateControl = Lens.lens (framerateControl :: Mpeg2Settings -> Lude.Maybe Mpeg2FramerateControl) (\s a -> s {framerateControl = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = Lens.lens (adaptiveQuantization :: Mpeg2Settings -> Lude.Maybe Mpeg2AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: Mpeg2Settings)
{-# DEPRECATED msAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateNumerator :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msFramerateNumerator = Lens.lens (framerateNumerator :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMaxBitrate :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msMaxBitrate = Lens.lens (maxBitrate :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: Mpeg2Settings)
{-# DEPRECATED msMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
--
-- /Note:/ Consider using 'syntax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSyntax :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2Syntax)
msSyntax = Lens.lens (syntax :: Mpeg2Settings -> Lude.Maybe Mpeg2Syntax) (\s a -> s {syntax = a} :: Mpeg2Settings)
{-# DEPRECATED msSyntax "Use generic-lens or generic-optics with 'syntax' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopClosedCadence :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msGopClosedCadence = Lens.lens (gopClosedCadence :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: Mpeg2Settings)
{-# DEPRECATED msGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msParDenominator :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msParDenominator = Lens.lens (parDenominator :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: Mpeg2Settings)
{-# DEPRECATED msParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSpatialAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2SpatialAdaptiveQuantization)
msSpatialAdaptiveQuantization = Lens.lens (spatialAdaptiveQuantization :: Mpeg2Settings -> Lude.Maybe Mpeg2SpatialAdaptiveQuantization) (\s a -> s {spatialAdaptiveQuantization = a} :: Mpeg2Settings)
{-# DEPRECATED msSpatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead." #-}

instance Lude.FromJSON Mpeg2Settings where
  parseJSON =
    Lude.withObject
      "Mpeg2Settings"
      ( \x ->
          Mpeg2Settings'
            Lude.<$> (x Lude..:? "qualityTuningLevel")
            Lude.<*> (x Lude..:? "temporalAdaptiveQuantization")
            Lude.<*> (x Lude..:? "sceneChangeDetect")
            Lude.<*> (x Lude..:? "hrdBufferInitialFillPercentage")
            Lude.<*> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "numberBFramesBetweenReferenceFrames")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "hrdBufferSize")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "intraDcPrecision")
            Lude.<*> (x Lude..:? "dynamicSubGop")
            Lude.<*> (x Lude..:? "minIInterval")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "softness")
            Lude.<*> (x Lude..:? "codecProfile")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "codecLevel")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "syntax")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "parDenominator")
            Lude.<*> (x Lude..:? "spatialAdaptiveQuantization")
      )

instance Lude.ToJSON Mpeg2Settings where
  toJSON Mpeg2Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("qualityTuningLevel" Lude..=) Lude.<$> qualityTuningLevel,
            ("temporalAdaptiveQuantization" Lude..=)
              Lude.<$> temporalAdaptiveQuantization,
            ("sceneChangeDetect" Lude..=) Lude.<$> sceneChangeDetect,
            ("hrdBufferInitialFillPercentage" Lude..=)
              Lude.<$> hrdBufferInitialFillPercentage,
            ("slowPal" Lude..=) Lude.<$> slowPal,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Lude..=)
              Lude.<$> numberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("hrdBufferSize" Lude..=) Lude.<$> hrdBufferSize,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("intraDcPrecision" Lude..=) Lude.<$> intraDcPrecision,
            ("dynamicSubGop" Lude..=) Lude.<$> dynamicSubGop,
            ("minIInterval" Lude..=) Lude.<$> minIInterval,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("softness" Lude..=) Lude.<$> softness,
            ("codecProfile" Lude..=) Lude.<$> codecProfile,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("codecLevel" Lude..=) Lude.<$> codecLevel,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("syntax" Lude..=) Lude.<$> syntax,
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("parDenominator" Lude..=) Lude.<$> parDenominator,
            ("spatialAdaptiveQuantization" Lude..=)
              Lude.<$> spatialAdaptiveQuantization
          ]
      )
