{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2Settings
  ( Mpeg2Settings (..)
  -- * Smart constructor
  , mkMpeg2Settings
  -- * Lenses
  , msfAdaptiveQuantization
  , msfBitrate
  , msfCodecLevel
  , msfCodecProfile
  , msfDynamicSubGop
  , msfFramerateControl
  , msfFramerateConversionAlgorithm
  , msfFramerateDenominator
  , msfFramerateNumerator
  , msfGopClosedCadence
  , msfGopSize
  , msfGopSizeUnits
  , msfHrdBufferInitialFillPercentage
  , msfHrdBufferSize
  , msfInterlaceMode
  , msfIntraDcPrecision
  , msfMaxBitrate
  , msfMinIInterval
  , msfNumberBFramesBetweenReferenceFrames
  , msfParControl
  , msfParDenominator
  , msfParNumerator
  , msfQualityTuningLevel
  , msfRateControlMode
  , msfSceneChangeDetect
  , msfSlowPal
  , msfSoftness
  , msfSpatialAdaptiveQuantization
  , msfSyntax
  , msfTelecine
  , msfTemporalAdaptiveQuantization
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2CodecLevel as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2CodecProfile as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2ParControl as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2RateControlMode as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2SlowPal as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2Syntax as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2Telecine as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /See:/ 'mkMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { adaptiveQuantization :: Core.Maybe Types.Mpeg2AdaptiveQuantization
    -- ^ Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
  , codecLevel :: Core.Maybe Types.Mpeg2CodecLevel
    -- ^ Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
  , codecProfile :: Core.Maybe Types.Mpeg2CodecProfile
    -- ^ Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
  , dynamicSubGop :: Core.Maybe Types.Mpeg2DynamicSubGop
    -- ^ Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
  , framerateControl :: Core.Maybe Types.Mpeg2FramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.Mpeg2FramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , gopClosedCadence :: Core.Maybe Core.Natural
    -- ^ Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
  , gopSize :: Core.Maybe Core.Double
    -- ^ GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
  , gopSizeUnits :: Core.Maybe Types.Mpeg2GopSizeUnits
    -- ^ Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
  , hrdBufferInitialFillPercentage :: Core.Maybe Core.Natural
    -- ^ Percentage of the buffer that should initially be filled (HRD buffer model).
  , hrdBufferSize :: Core.Maybe Core.Natural
    -- ^ Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
  , interlaceMode :: Core.Maybe Types.Mpeg2InterlaceMode
    -- ^ Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
  , intraDcPrecision :: Core.Maybe Types.Mpeg2IntraDcPrecision
    -- ^ Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
  , maxBitrate :: Core.Maybe Core.Natural
    -- ^ Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
  , minIInterval :: Core.Maybe Core.Natural
    -- ^ Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
  , numberBFramesBetweenReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Number of B-frames between reference frames.
  , parControl :: Core.Maybe Types.Mpeg2ParControl
    -- ^ Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
  , parDenominator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
  , parNumerator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
  , qualityTuningLevel :: Core.Maybe Types.Mpeg2QualityTuningLevel
    -- ^ Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
  , rateControlMode :: Core.Maybe Types.Mpeg2RateControlMode
    -- ^ Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
  , sceneChangeDetect :: Core.Maybe Types.Mpeg2SceneChangeDetect
    -- ^ Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
  , slowPal :: Core.Maybe Types.Mpeg2SlowPal
    -- ^ Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
  , softness :: Core.Maybe Core.Natural
    -- ^ Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
  , spatialAdaptiveQuantization :: Core.Maybe Types.Mpeg2SpatialAdaptiveQuantization
    -- ^ Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
  , syntax :: Core.Maybe Types.Mpeg2Syntax
    -- ^ Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
  , telecine :: Core.Maybe Types.Mpeg2Telecine
    -- ^ When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
  , temporalAdaptiveQuantization :: Core.Maybe Types.Mpeg2TemporalAdaptiveQuantization
    -- ^ Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mpeg2Settings' value with any optional fields omitted.
mkMpeg2Settings
    :: Mpeg2Settings
mkMpeg2Settings
  = Mpeg2Settings'{adaptiveQuantization = Core.Nothing,
                   bitrate = Core.Nothing, codecLevel = Core.Nothing,
                   codecProfile = Core.Nothing, dynamicSubGop = Core.Nothing,
                   framerateControl = Core.Nothing,
                   framerateConversionAlgorithm = Core.Nothing,
                   framerateDenominator = Core.Nothing,
                   framerateNumerator = Core.Nothing, gopClosedCadence = Core.Nothing,
                   gopSize = Core.Nothing, gopSizeUnits = Core.Nothing,
                   hrdBufferInitialFillPercentage = Core.Nothing,
                   hrdBufferSize = Core.Nothing, interlaceMode = Core.Nothing,
                   intraDcPrecision = Core.Nothing, maxBitrate = Core.Nothing,
                   minIInterval = Core.Nothing,
                   numberBFramesBetweenReferenceFrames = Core.Nothing,
                   parControl = Core.Nothing, parDenominator = Core.Nothing,
                   parNumerator = Core.Nothing, qualityTuningLevel = Core.Nothing,
                   rateControlMode = Core.Nothing, sceneChangeDetect = Core.Nothing,
                   slowPal = Core.Nothing, softness = Core.Nothing,
                   spatialAdaptiveQuantization = Core.Nothing, syntax = Core.Nothing,
                   telecine = Core.Nothing,
                   temporalAdaptiveQuantization = Core.Nothing}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2AdaptiveQuantization)
msfAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE msfAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfBitrate :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfBitrate = Lens.field @"bitrate"
{-# INLINEABLE msfBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfCodecLevel :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2CodecLevel)
msfCodecLevel = Lens.field @"codecLevel"
{-# INLINEABLE msfCodecLevel #-}
{-# DEPRECATED codecLevel "Use generic-lens or generic-optics with 'codecLevel' instead"  #-}

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfCodecProfile :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2CodecProfile)
msfCodecProfile = Lens.field @"codecProfile"
{-# INLINEABLE msfCodecProfile #-}
{-# DEPRECATED codecProfile "Use generic-lens or generic-optics with 'codecProfile' instead"  #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfDynamicSubGop :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2DynamicSubGop)
msfDynamicSubGop = Lens.field @"dynamicSubGop"
{-# INLINEABLE msfDynamicSubGop #-}
{-# DEPRECATED dynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfFramerateControl :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2FramerateControl)
msfFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE msfFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfFramerateConversionAlgorithm :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2FramerateConversionAlgorithm)
msfFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE msfFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfFramerateDenominator :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE msfFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfFramerateNumerator :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE msfFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfGopClosedCadence :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfGopClosedCadence = Lens.field @"gopClosedCadence"
{-# INLINEABLE msfGopClosedCadence #-}
{-# DEPRECATED gopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead"  #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfGopSize :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Double)
msfGopSize = Lens.field @"gopSize"
{-# INLINEABLE msfGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfGopSizeUnits :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2GopSizeUnits)
msfGopSizeUnits = Lens.field @"gopSizeUnits"
{-# INLINEABLE msfGopSizeUnits #-}
{-# DEPRECATED gopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead"  #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfHrdBufferInitialFillPercentage :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfHrdBufferInitialFillPercentage = Lens.field @"hrdBufferInitialFillPercentage"
{-# INLINEABLE msfHrdBufferInitialFillPercentage #-}
{-# DEPRECATED hrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead"  #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfHrdBufferSize :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfHrdBufferSize = Lens.field @"hrdBufferSize"
{-# INLINEABLE msfHrdBufferSize #-}
{-# DEPRECATED hrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead"  #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfInterlaceMode :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2InterlaceMode)
msfInterlaceMode = Lens.field @"interlaceMode"
{-# INLINEABLE msfInterlaceMode #-}
{-# DEPRECATED interlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead"  #-}

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
--
-- /Note:/ Consider using 'intraDcPrecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfIntraDcPrecision :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2IntraDcPrecision)
msfIntraDcPrecision = Lens.field @"intraDcPrecision"
{-# INLINEABLE msfIntraDcPrecision #-}
{-# DEPRECATED intraDcPrecision "Use generic-lens or generic-optics with 'intraDcPrecision' instead"  #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfMaxBitrate :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE msfMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfMinIInterval :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfMinIInterval = Lens.field @"minIInterval"
{-# INLINEABLE msfMinIInterval #-}
{-# DEPRECATED minIInterval "Use generic-lens or generic-optics with 'minIInterval' instead"  #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfNumberBFramesBetweenReferenceFrames :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfNumberBFramesBetweenReferenceFrames = Lens.field @"numberBFramesBetweenReferenceFrames"
{-# INLINEABLE msfNumberBFramesBetweenReferenceFrames #-}
{-# DEPRECATED numberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead"  #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfParControl :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2ParControl)
msfParControl = Lens.field @"parControl"
{-# INLINEABLE msfParControl #-}
{-# DEPRECATED parControl "Use generic-lens or generic-optics with 'parControl' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfParDenominator :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfParDenominator = Lens.field @"parDenominator"
{-# INLINEABLE msfParDenominator #-}
{-# DEPRECATED parDenominator "Use generic-lens or generic-optics with 'parDenominator' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfParNumerator :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfParNumerator = Lens.field @"parNumerator"
{-# INLINEABLE msfParNumerator #-}
{-# DEPRECATED parNumerator "Use generic-lens or generic-optics with 'parNumerator' instead"  #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfQualityTuningLevel :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2QualityTuningLevel)
msfQualityTuningLevel = Lens.field @"qualityTuningLevel"
{-# INLINEABLE msfQualityTuningLevel #-}
{-# DEPRECATED qualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead"  #-}

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfRateControlMode :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2RateControlMode)
msfRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE msfRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfSceneChangeDetect :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2SceneChangeDetect)
msfSceneChangeDetect = Lens.field @"sceneChangeDetect"
{-# INLINEABLE msfSceneChangeDetect #-}
{-# DEPRECATED sceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead"  #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfSlowPal :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2SlowPal)
msfSlowPal = Lens.field @"slowPal"
{-# INLINEABLE msfSlowPal #-}
{-# DEPRECATED slowPal "Use generic-lens or generic-optics with 'slowPal' instead"  #-}

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, to use the AWS Elemental default matrices. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfSoftness :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msfSoftness = Lens.field @"softness"
{-# INLINEABLE msfSoftness #-}
{-# DEPRECATED softness "Use generic-lens or generic-optics with 'softness' instead"  #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfSpatialAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2SpatialAdaptiveQuantization)
msfSpatialAdaptiveQuantization = Lens.field @"spatialAdaptiveQuantization"
{-# INLINEABLE msfSpatialAdaptiveQuantization #-}
{-# DEPRECATED spatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead"  #-}

-- | Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
--
-- /Note:/ Consider using 'syntax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfSyntax :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2Syntax)
msfSyntax = Lens.field @"syntax"
{-# INLINEABLE msfSyntax #-}
{-# DEPRECATED syntax "Use generic-lens or generic-optics with 'syntax' instead"  #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfTelecine :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2Telecine)
msfTelecine = Lens.field @"telecine"
{-# INLINEABLE msfTelecine #-}
{-# DEPRECATED telecine "Use generic-lens or generic-optics with 'telecine' instead"  #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfTemporalAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2TemporalAdaptiveQuantization)
msfTemporalAdaptiveQuantization = Lens.field @"temporalAdaptiveQuantization"
{-# INLINEABLE msfTemporalAdaptiveQuantization #-}
{-# DEPRECATED temporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead"  #-}

instance Core.FromJSON Mpeg2Settings where
        toJSON Mpeg2Settings{..}
          = Core.object
              (Core.catMaybes
                 [("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("codecLevel" Core..=) Core.<$> codecLevel,
                  ("codecProfile" Core..=) Core.<$> codecProfile,
                  ("dynamicSubGop" Core..=) Core.<$> dynamicSubGop,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateConversionAlgorithm" Core..=) Core.<$>
                    framerateConversionAlgorithm,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("gopClosedCadence" Core..=) Core.<$> gopClosedCadence,
                  ("gopSize" Core..=) Core.<$> gopSize,
                  ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
                  ("hrdBufferInitialFillPercentage" Core..=) Core.<$>
                    hrdBufferInitialFillPercentage,
                  ("hrdBufferSize" Core..=) Core.<$> hrdBufferSize,
                  ("interlaceMode" Core..=) Core.<$> interlaceMode,
                  ("intraDcPrecision" Core..=) Core.<$> intraDcPrecision,
                  ("maxBitrate" Core..=) Core.<$> maxBitrate,
                  ("minIInterval" Core..=) Core.<$> minIInterval,
                  ("numberBFramesBetweenReferenceFrames" Core..=) Core.<$>
                    numberBFramesBetweenReferenceFrames,
                  ("parControl" Core..=) Core.<$> parControl,
                  ("parDenominator" Core..=) Core.<$> parDenominator,
                  ("parNumerator" Core..=) Core.<$> parNumerator,
                  ("qualityTuningLevel" Core..=) Core.<$> qualityTuningLevel,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("sceneChangeDetect" Core..=) Core.<$> sceneChangeDetect,
                  ("slowPal" Core..=) Core.<$> slowPal,
                  ("softness" Core..=) Core.<$> softness,
                  ("spatialAdaptiveQuantization" Core..=) Core.<$>
                    spatialAdaptiveQuantization,
                  ("syntax" Core..=) Core.<$> syntax,
                  ("telecine" Core..=) Core.<$> telecine,
                  ("temporalAdaptiveQuantization" Core..=) Core.<$>
                    temporalAdaptiveQuantization])

instance Core.FromJSON Mpeg2Settings where
        parseJSON
          = Core.withObject "Mpeg2Settings" Core.$
              \ x ->
                Mpeg2Settings' Core.<$>
                  (x Core..:? "adaptiveQuantization") Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "codecLevel"
                    Core.<*> x Core..:? "codecProfile"
                    Core.<*> x Core..:? "dynamicSubGop"
                    Core.<*> x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateConversionAlgorithm"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "gopClosedCadence"
                    Core.<*> x Core..:? "gopSize"
                    Core.<*> x Core..:? "gopSizeUnits"
                    Core.<*> x Core..:? "hrdBufferInitialFillPercentage"
                    Core.<*> x Core..:? "hrdBufferSize"
                    Core.<*> x Core..:? "interlaceMode"
                    Core.<*> x Core..:? "intraDcPrecision"
                    Core.<*> x Core..:? "maxBitrate"
                    Core.<*> x Core..:? "minIInterval"
                    Core.<*> x Core..:? "numberBFramesBetweenReferenceFrames"
                    Core.<*> x Core..:? "parControl"
                    Core.<*> x Core..:? "parDenominator"
                    Core.<*> x Core..:? "parNumerator"
                    Core.<*> x Core..:? "qualityTuningLevel"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "sceneChangeDetect"
                    Core.<*> x Core..:? "slowPal"
                    Core.<*> x Core..:? "softness"
                    Core.<*> x Core..:? "spatialAdaptiveQuantization"
                    Core.<*> x Core..:? "syntax"
                    Core.<*> x Core..:? "telecine"
                    Core.<*> x Core..:? "temporalAdaptiveQuantization"
