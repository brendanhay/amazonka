{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265Settings
  ( H265Settings (..)
  -- * Smart constructor
  , mkH265Settings
  -- * Lenses
  , hAdaptiveQuantization
  , hAlternateTransferFunctionSei
  , hBitrate
  , hCodecLevel
  , hCodecProfile
  , hDynamicSubGop
  , hFlickerAdaptiveQuantization
  , hFramerateControl
  , hFramerateConversionAlgorithm
  , hFramerateDenominator
  , hFramerateNumerator
  , hGopBReference
  , hGopClosedCadence
  , hGopSize
  , hGopSizeUnits
  , hHrdBufferInitialFillPercentage
  , hHrdBufferSize
  , hInterlaceMode
  , hMaxBitrate
  , hMinIInterval
  , hNumberBFramesBetweenReferenceFrames
  , hNumberReferenceFrames
  , hParControl
  , hParDenominator
  , hParNumerator
  , hQualityTuningLevel
  , hQvbrSettings
  , hRateControlMode
  , hSampleAdaptiveOffsetFilterMode
  , hSceneChangeDetect
  , hSlices
  , hSlowPal
  , hSpatialAdaptiveQuantization
  , hTelecine
  , hTemporalAdaptiveQuantization
  , hTemporalIds
  , hTiles
  , hUnregisteredSeiTimecode
  , hWriteMp4PackagingType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.H265AdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei as Types
import qualified Network.AWS.MediaConvert.Types.H265CodecLevel as Types
import qualified Network.AWS.MediaConvert.Types.H265CodecProfile as Types
import qualified Network.AWS.MediaConvert.Types.H265DynamicSubGop as Types
import qualified Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H265FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.H265GopBReference as Types
import qualified Network.AWS.MediaConvert.Types.H265GopSizeUnits as Types
import qualified Network.AWS.MediaConvert.Types.H265InterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.H265ParControl as Types
import qualified Network.AWS.MediaConvert.Types.H265QualityTuningLevel as Types
import qualified Network.AWS.MediaConvert.Types.H265QvbrSettings as Types
import qualified Network.AWS.MediaConvert.Types.H265RateControlMode as Types
import qualified Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode as Types
import qualified Network.AWS.MediaConvert.Types.H265SceneChangeDetect as Types
import qualified Network.AWS.MediaConvert.Types.H265SlowPal as Types
import qualified Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H265Telecine as Types
import qualified Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H265TemporalIds as Types
import qualified Network.AWS.MediaConvert.Types.H265Tiles as Types
import qualified Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode as Types
import qualified Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for H265 codec
--
-- /See:/ 'mkH265Settings' smart constructor.
data H265Settings = H265Settings'
  { adaptiveQuantization :: Core.Maybe Types.H265AdaptiveQuantization
    -- ^ Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
  , alternateTransferFunctionSei :: Core.Maybe Types.H265AlternateTransferFunctionSei
    -- ^ Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
  , codecLevel :: Core.Maybe Types.H265CodecLevel
    -- ^ H.265 Level.
  , codecProfile :: Core.Maybe Types.H265CodecProfile
    -- ^ Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
  , dynamicSubGop :: Core.Maybe Types.H265DynamicSubGop
    -- ^ Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
  , flickerAdaptiveQuantization :: Core.Maybe Types.H265FlickerAdaptiveQuantization
    -- ^ Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
  , framerateControl :: Core.Maybe Types.H265FramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.H265FramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , gopBReference :: Core.Maybe Types.H265GopBReference
    -- ^ If enable, use reference B frames for GOP structures that have B frames > 1.
  , gopClosedCadence :: Core.Maybe Core.Natural
    -- ^ Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
  , gopSize :: Core.Maybe Core.Double
    -- ^ GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
  , gopSizeUnits :: Core.Maybe Types.H265GopSizeUnits
    -- ^ Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
  , hrdBufferInitialFillPercentage :: Core.Maybe Core.Natural
    -- ^ Percentage of the buffer that should initially be filled (HRD buffer model).
  , hrdBufferSize :: Core.Maybe Core.Natural
    -- ^ Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
  , interlaceMode :: Core.Maybe Types.H265InterlaceMode
    -- ^ Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
  , maxBitrate :: Core.Maybe Core.Natural
    -- ^ Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
  , minIInterval :: Core.Maybe Core.Natural
    -- ^ Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
  , numberBFramesBetweenReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Number of B-frames between reference frames.
  , numberReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
  , parControl :: Core.Maybe Types.H265ParControl
    -- ^ Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
  , parDenominator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
  , parNumerator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
  , qualityTuningLevel :: Core.Maybe Types.H265QualityTuningLevel
    -- ^ Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
  , qvbrSettings :: Core.Maybe Types.H265QvbrSettings
    -- ^ Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
  , rateControlMode :: Core.Maybe Types.H265RateControlMode
    -- ^ Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
  , sampleAdaptiveOffsetFilterMode :: Core.Maybe Types.H265SampleAdaptiveOffsetFilterMode
    -- ^ Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
  , sceneChangeDetect :: Core.Maybe Types.H265SceneChangeDetect
    -- ^ Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
  , slices :: Core.Maybe Core.Natural
    -- ^ Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
  , slowPal :: Core.Maybe Types.H265SlowPal
    -- ^ Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
  , spatialAdaptiveQuantization :: Core.Maybe Types.H265SpatialAdaptiveQuantization
    -- ^ Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
  , telecine :: Core.Maybe Types.H265Telecine
    -- ^ This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
  , temporalAdaptiveQuantization :: Core.Maybe Types.H265TemporalAdaptiveQuantization
    -- ^ Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
  , temporalIds :: Core.Maybe Types.H265TemporalIds
    -- ^ Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
  , tiles :: Core.Maybe Types.H265Tiles
    -- ^ Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
  , unregisteredSeiTimecode :: Core.Maybe Types.H265UnregisteredSeiTimecode
    -- ^ Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
  , writeMp4PackagingType :: Core.Maybe Types.H265WriteMp4PackagingType
    -- ^ If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H265Settings' value with any optional fields omitted.
mkH265Settings
    :: H265Settings
mkH265Settings
  = H265Settings'{adaptiveQuantization = Core.Nothing,
                  alternateTransferFunctionSei = Core.Nothing,
                  bitrate = Core.Nothing, codecLevel = Core.Nothing,
                  codecProfile = Core.Nothing, dynamicSubGop = Core.Nothing,
                  flickerAdaptiveQuantization = Core.Nothing,
                  framerateControl = Core.Nothing,
                  framerateConversionAlgorithm = Core.Nothing,
                  framerateDenominator = Core.Nothing,
                  framerateNumerator = Core.Nothing, gopBReference = Core.Nothing,
                  gopClosedCadence = Core.Nothing, gopSize = Core.Nothing,
                  gopSizeUnits = Core.Nothing,
                  hrdBufferInitialFillPercentage = Core.Nothing,
                  hrdBufferSize = Core.Nothing, interlaceMode = Core.Nothing,
                  maxBitrate = Core.Nothing, minIInterval = Core.Nothing,
                  numberBFramesBetweenReferenceFrames = Core.Nothing,
                  numberReferenceFrames = Core.Nothing, parControl = Core.Nothing,
                  parDenominator = Core.Nothing, parNumerator = Core.Nothing,
                  qualityTuningLevel = Core.Nothing, qvbrSettings = Core.Nothing,
                  rateControlMode = Core.Nothing,
                  sampleAdaptiveOffsetFilterMode = Core.Nothing,
                  sceneChangeDetect = Core.Nothing, slices = Core.Nothing,
                  slowPal = Core.Nothing, spatialAdaptiveQuantization = Core.Nothing,
                  telecine = Core.Nothing,
                  temporalAdaptiveQuantization = Core.Nothing,
                  temporalIds = Core.Nothing, tiles = Core.Nothing,
                  unregisteredSeiTimecode = Core.Nothing,
                  writeMp4PackagingType = Core.Nothing}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAdaptiveQuantization :: Lens.Lens' H265Settings (Core.Maybe Types.H265AdaptiveQuantization)
hAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE hAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
--
-- /Note:/ Consider using 'alternateTransferFunctionSei' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAlternateTransferFunctionSei :: Lens.Lens' H265Settings (Core.Maybe Types.H265AlternateTransferFunctionSei)
hAlternateTransferFunctionSei = Lens.field @"alternateTransferFunctionSei"
{-# INLINEABLE hAlternateTransferFunctionSei #-}
{-# DEPRECATED alternateTransferFunctionSei "Use generic-lens or generic-optics with 'alternateTransferFunctionSei' instead"  #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBitrate :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hBitrate = Lens.field @"bitrate"
{-# INLINEABLE hBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | H.265 Level.
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCodecLevel :: Lens.Lens' H265Settings (Core.Maybe Types.H265CodecLevel)
hCodecLevel = Lens.field @"codecLevel"
{-# INLINEABLE hCodecLevel #-}
{-# DEPRECATED codecLevel "Use generic-lens or generic-optics with 'codecLevel' instead"  #-}

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCodecProfile :: Lens.Lens' H265Settings (Core.Maybe Types.H265CodecProfile)
hCodecProfile = Lens.field @"codecProfile"
{-# INLINEABLE hCodecProfile #-}
{-# DEPRECATED codecProfile "Use generic-lens or generic-optics with 'codecProfile' instead"  #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hDynamicSubGop :: Lens.Lens' H265Settings (Core.Maybe Types.H265DynamicSubGop)
hDynamicSubGop = Lens.field @"dynamicSubGop"
{-# INLINEABLE hDynamicSubGop #-}
{-# DEPRECATED dynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead"  #-}

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
--
-- /Note:/ Consider using 'flickerAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFlickerAdaptiveQuantization :: Lens.Lens' H265Settings (Core.Maybe Types.H265FlickerAdaptiveQuantization)
hFlickerAdaptiveQuantization = Lens.field @"flickerAdaptiveQuantization"
{-# INLINEABLE hFlickerAdaptiveQuantization #-}
{-# DEPRECATED flickerAdaptiveQuantization "Use generic-lens or generic-optics with 'flickerAdaptiveQuantization' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateControl :: Lens.Lens' H265Settings (Core.Maybe Types.H265FramerateControl)
hFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE hFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateConversionAlgorithm :: Lens.Lens' H265Settings (Core.Maybe Types.H265FramerateConversionAlgorithm)
hFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE hFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateDenominator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE hFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateNumerator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE hFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopBReference :: Lens.Lens' H265Settings (Core.Maybe Types.H265GopBReference)
hGopBReference = Lens.field @"gopBReference"
{-# INLINEABLE hGopBReference #-}
{-# DEPRECATED gopBReference "Use generic-lens or generic-optics with 'gopBReference' instead"  #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopClosedCadence :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hGopClosedCadence = Lens.field @"gopClosedCadence"
{-# INLINEABLE hGopClosedCadence #-}
{-# DEPRECATED gopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead"  #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSize :: Lens.Lens' H265Settings (Core.Maybe Core.Double)
hGopSize = Lens.field @"gopSize"
{-# INLINEABLE hGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSizeUnits :: Lens.Lens' H265Settings (Core.Maybe Types.H265GopSizeUnits)
hGopSizeUnits = Lens.field @"gopSizeUnits"
{-# INLINEABLE hGopSizeUnits #-}
{-# DEPRECATED gopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead"  #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHrdBufferInitialFillPercentage :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hHrdBufferInitialFillPercentage = Lens.field @"hrdBufferInitialFillPercentage"
{-# INLINEABLE hHrdBufferInitialFillPercentage #-}
{-# DEPRECATED hrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead"  #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHrdBufferSize :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hHrdBufferSize = Lens.field @"hrdBufferSize"
{-# INLINEABLE hHrdBufferSize #-}
{-# DEPRECATED hrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead"  #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hInterlaceMode :: Lens.Lens' H265Settings (Core.Maybe Types.H265InterlaceMode)
hInterlaceMode = Lens.field @"interlaceMode"
{-# INLINEABLE hInterlaceMode #-}
{-# DEPRECATED interlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead"  #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMaxBitrate :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE hMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMinIInterval :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hMinIInterval = Lens.field @"minIInterval"
{-# INLINEABLE hMinIInterval #-}
{-# DEPRECATED minIInterval "Use generic-lens or generic-optics with 'minIInterval' instead"  #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hNumberBFramesBetweenReferenceFrames :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hNumberBFramesBetweenReferenceFrames = Lens.field @"numberBFramesBetweenReferenceFrames"
{-# INLINEABLE hNumberBFramesBetweenReferenceFrames #-}
{-# DEPRECATED numberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead"  #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numberReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hNumberReferenceFrames :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hNumberReferenceFrames = Lens.field @"numberReferenceFrames"
{-# INLINEABLE hNumberReferenceFrames #-}
{-# DEPRECATED numberReferenceFrames "Use generic-lens or generic-optics with 'numberReferenceFrames' instead"  #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParControl :: Lens.Lens' H265Settings (Core.Maybe Types.H265ParControl)
hParControl = Lens.field @"parControl"
{-# INLINEABLE hParControl #-}
{-# DEPRECATED parControl "Use generic-lens or generic-optics with 'parControl' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParDenominator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hParDenominator = Lens.field @"parDenominator"
{-# INLINEABLE hParDenominator #-}
{-# DEPRECATED parDenominator "Use generic-lens or generic-optics with 'parDenominator' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParNumerator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hParNumerator = Lens.field @"parNumerator"
{-# INLINEABLE hParNumerator #-}
{-# DEPRECATED parNumerator "Use generic-lens or generic-optics with 'parNumerator' instead"  #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQualityTuningLevel :: Lens.Lens' H265Settings (Core.Maybe Types.H265QualityTuningLevel)
hQualityTuningLevel = Lens.field @"qualityTuningLevel"
{-# INLINEABLE hQualityTuningLevel #-}
{-# DEPRECATED qualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead"  #-}

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrSettings :: Lens.Lens' H265Settings (Core.Maybe Types.H265QvbrSettings)
hQvbrSettings = Lens.field @"qvbrSettings"
{-# INLINEABLE hQvbrSettings #-}
{-# DEPRECATED qvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead"  #-}

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRateControlMode :: Lens.Lens' H265Settings (Core.Maybe Types.H265RateControlMode)
hRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE hRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
--
-- /Note:/ Consider using 'sampleAdaptiveOffsetFilterMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSampleAdaptiveOffsetFilterMode :: Lens.Lens' H265Settings (Core.Maybe Types.H265SampleAdaptiveOffsetFilterMode)
hSampleAdaptiveOffsetFilterMode = Lens.field @"sampleAdaptiveOffsetFilterMode"
{-# INLINEABLE hSampleAdaptiveOffsetFilterMode #-}
{-# DEPRECATED sampleAdaptiveOffsetFilterMode "Use generic-lens or generic-optics with 'sampleAdaptiveOffsetFilterMode' instead"  #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSceneChangeDetect :: Lens.Lens' H265Settings (Core.Maybe Types.H265SceneChangeDetect)
hSceneChangeDetect = Lens.field @"sceneChangeDetect"
{-# INLINEABLE hSceneChangeDetect #-}
{-# DEPRECATED sceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead"  #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlices :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hSlices = Lens.field @"slices"
{-# INLINEABLE hSlices #-}
{-# DEPRECATED slices "Use generic-lens or generic-optics with 'slices' instead"  #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlowPal :: Lens.Lens' H265Settings (Core.Maybe Types.H265SlowPal)
hSlowPal = Lens.field @"slowPal"
{-# INLINEABLE hSlowPal #-}
{-# DEPRECATED slowPal "Use generic-lens or generic-optics with 'slowPal' instead"  #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSpatialAdaptiveQuantization :: Lens.Lens' H265Settings (Core.Maybe Types.H265SpatialAdaptiveQuantization)
hSpatialAdaptiveQuantization = Lens.field @"spatialAdaptiveQuantization"
{-# INLINEABLE hSpatialAdaptiveQuantization #-}
{-# DEPRECATED spatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead"  #-}

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTelecine :: Lens.Lens' H265Settings (Core.Maybe Types.H265Telecine)
hTelecine = Lens.field @"telecine"
{-# INLINEABLE hTelecine #-}
{-# DEPRECATED telecine "Use generic-lens or generic-optics with 'telecine' instead"  #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalAdaptiveQuantization :: Lens.Lens' H265Settings (Core.Maybe Types.H265TemporalAdaptiveQuantization)
hTemporalAdaptiveQuantization = Lens.field @"temporalAdaptiveQuantization"
{-# INLINEABLE hTemporalAdaptiveQuantization #-}
{-# DEPRECATED temporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead"  #-}

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
--
-- /Note:/ Consider using 'temporalIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalIds :: Lens.Lens' H265Settings (Core.Maybe Types.H265TemporalIds)
hTemporalIds = Lens.field @"temporalIds"
{-# INLINEABLE hTemporalIds #-}
{-# DEPRECATED temporalIds "Use generic-lens or generic-optics with 'temporalIds' instead"  #-}

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
--
-- /Note:/ Consider using 'tiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTiles :: Lens.Lens' H265Settings (Core.Maybe Types.H265Tiles)
hTiles = Lens.field @"tiles"
{-# INLINEABLE hTiles #-}
{-# DEPRECATED tiles "Use generic-lens or generic-optics with 'tiles' instead"  #-}

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- /Note:/ Consider using 'unregisteredSeiTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hUnregisteredSeiTimecode :: Lens.Lens' H265Settings (Core.Maybe Types.H265UnregisteredSeiTimecode)
hUnregisteredSeiTimecode = Lens.field @"unregisteredSeiTimecode"
{-# INLINEABLE hUnregisteredSeiTimecode #-}
{-# DEPRECATED unregisteredSeiTimecode "Use generic-lens or generic-optics with 'unregisteredSeiTimecode' instead"  #-}

-- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
--
-- /Note:/ Consider using 'writeMp4PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hWriteMp4PackagingType :: Lens.Lens' H265Settings (Core.Maybe Types.H265WriteMp4PackagingType)
hWriteMp4PackagingType = Lens.field @"writeMp4PackagingType"
{-# INLINEABLE hWriteMp4PackagingType #-}
{-# DEPRECATED writeMp4PackagingType "Use generic-lens or generic-optics with 'writeMp4PackagingType' instead"  #-}

instance Core.FromJSON H265Settings where
        toJSON H265Settings{..}
          = Core.object
              (Core.catMaybes
                 [("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("alternateTransferFunctionSei" Core..=) Core.<$>
                    alternateTransferFunctionSei,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("codecLevel" Core..=) Core.<$> codecLevel,
                  ("codecProfile" Core..=) Core.<$> codecProfile,
                  ("dynamicSubGop" Core..=) Core.<$> dynamicSubGop,
                  ("flickerAdaptiveQuantization" Core..=) Core.<$>
                    flickerAdaptiveQuantization,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateConversionAlgorithm" Core..=) Core.<$>
                    framerateConversionAlgorithm,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("gopBReference" Core..=) Core.<$> gopBReference,
                  ("gopClosedCadence" Core..=) Core.<$> gopClosedCadence,
                  ("gopSize" Core..=) Core.<$> gopSize,
                  ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
                  ("hrdBufferInitialFillPercentage" Core..=) Core.<$>
                    hrdBufferInitialFillPercentage,
                  ("hrdBufferSize" Core..=) Core.<$> hrdBufferSize,
                  ("interlaceMode" Core..=) Core.<$> interlaceMode,
                  ("maxBitrate" Core..=) Core.<$> maxBitrate,
                  ("minIInterval" Core..=) Core.<$> minIInterval,
                  ("numberBFramesBetweenReferenceFrames" Core..=) Core.<$>
                    numberBFramesBetweenReferenceFrames,
                  ("numberReferenceFrames" Core..=) Core.<$> numberReferenceFrames,
                  ("parControl" Core..=) Core.<$> parControl,
                  ("parDenominator" Core..=) Core.<$> parDenominator,
                  ("parNumerator" Core..=) Core.<$> parNumerator,
                  ("qualityTuningLevel" Core..=) Core.<$> qualityTuningLevel,
                  ("qvbrSettings" Core..=) Core.<$> qvbrSettings,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("sampleAdaptiveOffsetFilterMode" Core..=) Core.<$>
                    sampleAdaptiveOffsetFilterMode,
                  ("sceneChangeDetect" Core..=) Core.<$> sceneChangeDetect,
                  ("slices" Core..=) Core.<$> slices,
                  ("slowPal" Core..=) Core.<$> slowPal,
                  ("spatialAdaptiveQuantization" Core..=) Core.<$>
                    spatialAdaptiveQuantization,
                  ("telecine" Core..=) Core.<$> telecine,
                  ("temporalAdaptiveQuantization" Core..=) Core.<$>
                    temporalAdaptiveQuantization,
                  ("temporalIds" Core..=) Core.<$> temporalIds,
                  ("tiles" Core..=) Core.<$> tiles,
                  ("unregisteredSeiTimecode" Core..=) Core.<$>
                    unregisteredSeiTimecode,
                  ("writeMp4PackagingType" Core..=) Core.<$> writeMp4PackagingType])

instance Core.FromJSON H265Settings where
        parseJSON
          = Core.withObject "H265Settings" Core.$
              \ x ->
                H265Settings' Core.<$>
                  (x Core..:? "adaptiveQuantization") Core.<*>
                    x Core..:? "alternateTransferFunctionSei"
                    Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "codecLevel"
                    Core.<*> x Core..:? "codecProfile"
                    Core.<*> x Core..:? "dynamicSubGop"
                    Core.<*> x Core..:? "flickerAdaptiveQuantization"
                    Core.<*> x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateConversionAlgorithm"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "gopBReference"
                    Core.<*> x Core..:? "gopClosedCadence"
                    Core.<*> x Core..:? "gopSize"
                    Core.<*> x Core..:? "gopSizeUnits"
                    Core.<*> x Core..:? "hrdBufferInitialFillPercentage"
                    Core.<*> x Core..:? "hrdBufferSize"
                    Core.<*> x Core..:? "interlaceMode"
                    Core.<*> x Core..:? "maxBitrate"
                    Core.<*> x Core..:? "minIInterval"
                    Core.<*> x Core..:? "numberBFramesBetweenReferenceFrames"
                    Core.<*> x Core..:? "numberReferenceFrames"
                    Core.<*> x Core..:? "parControl"
                    Core.<*> x Core..:? "parDenominator"
                    Core.<*> x Core..:? "parNumerator"
                    Core.<*> x Core..:? "qualityTuningLevel"
                    Core.<*> x Core..:? "qvbrSettings"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "sampleAdaptiveOffsetFilterMode"
                    Core.<*> x Core..:? "sceneChangeDetect"
                    Core.<*> x Core..:? "slices"
                    Core.<*> x Core..:? "slowPal"
                    Core.<*> x Core..:? "spatialAdaptiveQuantization"
                    Core.<*> x Core..:? "telecine"
                    Core.<*> x Core..:? "temporalAdaptiveQuantization"
                    Core.<*> x Core..:? "temporalIds"
                    Core.<*> x Core..:? "tiles"
                    Core.<*> x Core..:? "unregisteredSeiTimecode"
                    Core.<*> x Core..:? "writeMp4PackagingType"
