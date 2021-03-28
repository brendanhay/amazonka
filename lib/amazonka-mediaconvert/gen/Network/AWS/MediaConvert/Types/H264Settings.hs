{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264Settings
  ( H264Settings (..)
  -- * Smart constructor
  , mkH264Settings
  -- * Lenses
  , hsAdaptiveQuantization
  , hsBitrate
  , hsCodecLevel
  , hsCodecProfile
  , hsDynamicSubGop
  , hsEntropyEncoding
  , hsFieldEncoding
  , hsFlickerAdaptiveQuantization
  , hsFramerateControl
  , hsFramerateConversionAlgorithm
  , hsFramerateDenominator
  , hsFramerateNumerator
  , hsGopBReference
  , hsGopClosedCadence
  , hsGopSize
  , hsGopSizeUnits
  , hsHrdBufferInitialFillPercentage
  , hsHrdBufferSize
  , hsInterlaceMode
  , hsMaxBitrate
  , hsMinIInterval
  , hsNumberBFramesBetweenReferenceFrames
  , hsNumberReferenceFrames
  , hsParControl
  , hsParDenominator
  , hsParNumerator
  , hsQualityTuningLevel
  , hsQvbrSettings
  , hsRateControlMode
  , hsRepeatPps
  , hsSceneChangeDetect
  , hsSlices
  , hsSlowPal
  , hsSoftness
  , hsSpatialAdaptiveQuantization
  , hsSyntax
  , hsTelecine
  , hsTemporalAdaptiveQuantization
  , hsUnregisteredSeiTimecode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.H264AdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H264CodecLevel as Types
import qualified Network.AWS.MediaConvert.Types.H264CodecProfile as Types
import qualified Network.AWS.MediaConvert.Types.H264DynamicSubGop as Types
import qualified Network.AWS.MediaConvert.Types.H264EntropyEncoding as Types
import qualified Network.AWS.MediaConvert.Types.H264FieldEncoding as Types
import qualified Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H264FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.H264GopBReference as Types
import qualified Network.AWS.MediaConvert.Types.H264GopSizeUnits as Types
import qualified Network.AWS.MediaConvert.Types.H264InterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.H264ParControl as Types
import qualified Network.AWS.MediaConvert.Types.H264QualityTuningLevel as Types
import qualified Network.AWS.MediaConvert.Types.H264QvbrSettings as Types
import qualified Network.AWS.MediaConvert.Types.H264RateControlMode as Types
import qualified Network.AWS.MediaConvert.Types.H264RepeatPps as Types
import qualified Network.AWS.MediaConvert.Types.H264SceneChangeDetect as Types
import qualified Network.AWS.MediaConvert.Types.H264SlowPal as Types
import qualified Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H264Syntax as Types
import qualified Network.AWS.MediaConvert.Types.H264Telecine as Types
import qualified Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'mkH264Settings' smart constructor.
data H264Settings = H264Settings'
  { adaptiveQuantization :: Core.Maybe Types.H264AdaptiveQuantization
    -- ^ Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
  , codecLevel :: Core.Maybe Types.H264CodecLevel
    -- ^ Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
  , codecProfile :: Core.Maybe Types.H264CodecProfile
    -- ^ H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
  , dynamicSubGop :: Core.Maybe Types.H264DynamicSubGop
    -- ^ Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
  , entropyEncoding :: Core.Maybe Types.H264EntropyEncoding
    -- ^ Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
  , fieldEncoding :: Core.Maybe Types.H264FieldEncoding
    -- ^ Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
  , flickerAdaptiveQuantization :: Core.Maybe Types.H264FlickerAdaptiveQuantization
    -- ^ Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
  , framerateControl :: Core.Maybe Types.H264FramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.H264FramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , gopBReference :: Core.Maybe Types.H264GopBReference
    -- ^ If enable, use reference B frames for GOP structures that have B frames > 1.
  , gopClosedCadence :: Core.Maybe Core.Natural
    -- ^ Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
  , gopSize :: Core.Maybe Core.Double
    -- ^ GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
  , gopSizeUnits :: Core.Maybe Types.H264GopSizeUnits
    -- ^ Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
  , hrdBufferInitialFillPercentage :: Core.Maybe Core.Natural
    -- ^ Percentage of the buffer that should initially be filled (HRD buffer model).
  , hrdBufferSize :: Core.Maybe Core.Natural
    -- ^ Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
  , interlaceMode :: Core.Maybe Types.H264InterlaceMode
    -- ^ Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
  , maxBitrate :: Core.Maybe Core.Natural
    -- ^ Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
  , minIInterval :: Core.Maybe Core.Natural
    -- ^ Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
  , numberBFramesBetweenReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Number of B-frames between reference frames.
  , numberReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
  , parControl :: Core.Maybe Types.H264ParControl
    -- ^ Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
  , parDenominator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
  , parNumerator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
  , qualityTuningLevel :: Core.Maybe Types.H264QualityTuningLevel
    -- ^ Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
  , qvbrSettings :: Core.Maybe Types.H264QvbrSettings
    -- ^ Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
  , rateControlMode :: Core.Maybe Types.H264RateControlMode
    -- ^ Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
  , repeatPps :: Core.Maybe Types.H264RepeatPps
    -- ^ Places a PPS header on each encoded picture, even if repeated.
  , sceneChangeDetect :: Core.Maybe Types.H264SceneChangeDetect
    -- ^ Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
  , slices :: Core.Maybe Core.Natural
    -- ^ Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
  , slowPal :: Core.Maybe Types.H264SlowPal
    -- ^ Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
  , softness :: Core.Maybe Core.Natural
    -- ^ Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
  , spatialAdaptiveQuantization :: Core.Maybe Types.H264SpatialAdaptiveQuantization
    -- ^ Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
  , syntax :: Core.Maybe Types.H264Syntax
    -- ^ Produces a bitstream compliant with SMPTE RP-2027.
  , telecine :: Core.Maybe Types.H264Telecine
    -- ^ When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
  , temporalAdaptiveQuantization :: Core.Maybe Types.H264TemporalAdaptiveQuantization
    -- ^ Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
  , unregisteredSeiTimecode :: Core.Maybe Types.H264UnregisteredSeiTimecode
    -- ^ Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H264Settings' value with any optional fields omitted.
mkH264Settings
    :: H264Settings
mkH264Settings
  = H264Settings'{adaptiveQuantization = Core.Nothing,
                  bitrate = Core.Nothing, codecLevel = Core.Nothing,
                  codecProfile = Core.Nothing, dynamicSubGop = Core.Nothing,
                  entropyEncoding = Core.Nothing, fieldEncoding = Core.Nothing,
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
                  rateControlMode = Core.Nothing, repeatPps = Core.Nothing,
                  sceneChangeDetect = Core.Nothing, slices = Core.Nothing,
                  slowPal = Core.Nothing, softness = Core.Nothing,
                  spatialAdaptiveQuantization = Core.Nothing, syntax = Core.Nothing,
                  telecine = Core.Nothing,
                  temporalAdaptiveQuantization = Core.Nothing,
                  unregisteredSeiTimecode = Core.Nothing}

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAdaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe Types.H264AdaptiveQuantization)
hsAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE hsAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsBitrate = Lens.field @"bitrate"
{-# INLINEABLE hsBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsCodecLevel :: Lens.Lens' H264Settings (Core.Maybe Types.H264CodecLevel)
hsCodecLevel = Lens.field @"codecLevel"
{-# INLINEABLE hsCodecLevel #-}
{-# DEPRECATED codecLevel "Use generic-lens or generic-optics with 'codecLevel' instead"  #-}

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsCodecProfile :: Lens.Lens' H264Settings (Core.Maybe Types.H264CodecProfile)
hsCodecProfile = Lens.field @"codecProfile"
{-# INLINEABLE hsCodecProfile #-}
{-# DEPRECATED codecProfile "Use generic-lens or generic-optics with 'codecProfile' instead"  #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsDynamicSubGop :: Lens.Lens' H264Settings (Core.Maybe Types.H264DynamicSubGop)
hsDynamicSubGop = Lens.field @"dynamicSubGop"
{-# INLINEABLE hsDynamicSubGop #-}
{-# DEPRECATED dynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead"  #-}

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
--
-- /Note:/ Consider using 'entropyEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsEntropyEncoding :: Lens.Lens' H264Settings (Core.Maybe Types.H264EntropyEncoding)
hsEntropyEncoding = Lens.field @"entropyEncoding"
{-# INLINEABLE hsEntropyEncoding #-}
{-# DEPRECATED entropyEncoding "Use generic-lens or generic-optics with 'entropyEncoding' instead"  #-}

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
--
-- /Note:/ Consider using 'fieldEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFieldEncoding :: Lens.Lens' H264Settings (Core.Maybe Types.H264FieldEncoding)
hsFieldEncoding = Lens.field @"fieldEncoding"
{-# INLINEABLE hsFieldEncoding #-}
{-# DEPRECATED fieldEncoding "Use generic-lens or generic-optics with 'fieldEncoding' instead"  #-}

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'flickerAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFlickerAdaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe Types.H264FlickerAdaptiveQuantization)
hsFlickerAdaptiveQuantization = Lens.field @"flickerAdaptiveQuantization"
{-# INLINEABLE hsFlickerAdaptiveQuantization #-}
{-# DEPRECATED flickerAdaptiveQuantization "Use generic-lens or generic-optics with 'flickerAdaptiveQuantization' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateControl :: Lens.Lens' H264Settings (Core.Maybe Types.H264FramerateControl)
hsFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE hsFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateConversionAlgorithm :: Lens.Lens' H264Settings (Core.Maybe Types.H264FramerateConversionAlgorithm)
hsFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE hsFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE hsFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE hsFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopBReference :: Lens.Lens' H264Settings (Core.Maybe Types.H264GopBReference)
hsGopBReference = Lens.field @"gopBReference"
{-# INLINEABLE hsGopBReference #-}
{-# DEPRECATED gopBReference "Use generic-lens or generic-optics with 'gopBReference' instead"  #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopClosedCadence :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsGopClosedCadence = Lens.field @"gopClosedCadence"
{-# INLINEABLE hsGopClosedCadence #-}
{-# DEPRECATED gopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead"  #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSize :: Lens.Lens' H264Settings (Core.Maybe Core.Double)
hsGopSize = Lens.field @"gopSize"
{-# INLINEABLE hsGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSizeUnits :: Lens.Lens' H264Settings (Core.Maybe Types.H264GopSizeUnits)
hsGopSizeUnits = Lens.field @"gopSizeUnits"
{-# INLINEABLE hsGopSizeUnits #-}
{-# DEPRECATED gopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead"  #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHrdBufferInitialFillPercentage :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsHrdBufferInitialFillPercentage = Lens.field @"hrdBufferInitialFillPercentage"
{-# INLINEABLE hsHrdBufferInitialFillPercentage #-}
{-# DEPRECATED hrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead"  #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHrdBufferSize :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsHrdBufferSize = Lens.field @"hrdBufferSize"
{-# INLINEABLE hsHrdBufferSize #-}
{-# DEPRECATED hrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead"  #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsInterlaceMode :: Lens.Lens' H264Settings (Core.Maybe Types.H264InterlaceMode)
hsInterlaceMode = Lens.field @"interlaceMode"
{-# INLINEABLE hsInterlaceMode #-}
{-# DEPRECATED interlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead"  #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxBitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE hsMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMinIInterval :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsMinIInterval = Lens.field @"minIInterval"
{-# INLINEABLE hsMinIInterval #-}
{-# DEPRECATED minIInterval "Use generic-lens or generic-optics with 'minIInterval' instead"  #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsNumberBFramesBetweenReferenceFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsNumberBFramesBetweenReferenceFrames = Lens.field @"numberBFramesBetweenReferenceFrames"
{-# INLINEABLE hsNumberBFramesBetweenReferenceFrames #-}
{-# DEPRECATED numberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead"  #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numberReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsNumberReferenceFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsNumberReferenceFrames = Lens.field @"numberReferenceFrames"
{-# INLINEABLE hsNumberReferenceFrames #-}
{-# DEPRECATED numberReferenceFrames "Use generic-lens or generic-optics with 'numberReferenceFrames' instead"  #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParControl :: Lens.Lens' H264Settings (Core.Maybe Types.H264ParControl)
hsParControl = Lens.field @"parControl"
{-# INLINEABLE hsParControl #-}
{-# DEPRECATED parControl "Use generic-lens or generic-optics with 'parControl' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsParDenominator = Lens.field @"parDenominator"
{-# INLINEABLE hsParDenominator #-}
{-# DEPRECATED parDenominator "Use generic-lens or generic-optics with 'parDenominator' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsParNumerator = Lens.field @"parNumerator"
{-# INLINEABLE hsParNumerator #-}
{-# DEPRECATED parNumerator "Use generic-lens or generic-optics with 'parNumerator' instead"  #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQualityTuningLevel :: Lens.Lens' H264Settings (Core.Maybe Types.H264QualityTuningLevel)
hsQualityTuningLevel = Lens.field @"qualityTuningLevel"
{-# INLINEABLE hsQualityTuningLevel #-}
{-# DEPRECATED qualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead"  #-}

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQvbrSettings :: Lens.Lens' H264Settings (Core.Maybe Types.H264QvbrSettings)
hsQvbrSettings = Lens.field @"qvbrSettings"
{-# INLINEABLE hsQvbrSettings #-}
{-# DEPRECATED qvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead"  #-}

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsRateControlMode :: Lens.Lens' H264Settings (Core.Maybe Types.H264RateControlMode)
hsRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE hsRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Places a PPS header on each encoded picture, even if repeated.
--
-- /Note:/ Consider using 'repeatPps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsRepeatPps :: Lens.Lens' H264Settings (Core.Maybe Types.H264RepeatPps)
hsRepeatPps = Lens.field @"repeatPps"
{-# INLINEABLE hsRepeatPps #-}
{-# DEPRECATED repeatPps "Use generic-lens or generic-optics with 'repeatPps' instead"  #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSceneChangeDetect :: Lens.Lens' H264Settings (Core.Maybe Types.H264SceneChangeDetect)
hsSceneChangeDetect = Lens.field @"sceneChangeDetect"
{-# INLINEABLE hsSceneChangeDetect #-}
{-# DEPRECATED sceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead"  #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlices :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsSlices = Lens.field @"slices"
{-# INLINEABLE hsSlices #-}
{-# DEPRECATED slices "Use generic-lens or generic-optics with 'slices' instead"  #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlowPal :: Lens.Lens' H264Settings (Core.Maybe Types.H264SlowPal)
hsSlowPal = Lens.field @"slowPal"
{-# INLINEABLE hsSlowPal #-}
{-# DEPRECATED slowPal "Use generic-lens or generic-optics with 'slowPal' instead"  #-}

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSoftness :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsSoftness = Lens.field @"softness"
{-# INLINEABLE hsSoftness #-}
{-# DEPRECATED softness "Use generic-lens or generic-optics with 'softness' instead"  #-}

-- | Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSpatialAdaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe Types.H264SpatialAdaptiveQuantization)
hsSpatialAdaptiveQuantization = Lens.field @"spatialAdaptiveQuantization"
{-# INLINEABLE hsSpatialAdaptiveQuantization #-}
{-# DEPRECATED spatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead"  #-}

-- | Produces a bitstream compliant with SMPTE RP-2027.
--
-- /Note:/ Consider using 'syntax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSyntax :: Lens.Lens' H264Settings (Core.Maybe Types.H264Syntax)
hsSyntax = Lens.field @"syntax"
{-# INLINEABLE hsSyntax #-}
{-# DEPRECATED syntax "Use generic-lens or generic-optics with 'syntax' instead"  #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTelecine :: Lens.Lens' H264Settings (Core.Maybe Types.H264Telecine)
hsTelecine = Lens.field @"telecine"
{-# INLINEABLE hsTelecine #-}
{-# DEPRECATED telecine "Use generic-lens or generic-optics with 'telecine' instead"  #-}

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTemporalAdaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe Types.H264TemporalAdaptiveQuantization)
hsTemporalAdaptiveQuantization = Lens.field @"temporalAdaptiveQuantization"
{-# INLINEABLE hsTemporalAdaptiveQuantization #-}
{-# DEPRECATED temporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead"  #-}

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- /Note:/ Consider using 'unregisteredSeiTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsUnregisteredSeiTimecode :: Lens.Lens' H264Settings (Core.Maybe Types.H264UnregisteredSeiTimecode)
hsUnregisteredSeiTimecode = Lens.field @"unregisteredSeiTimecode"
{-# INLINEABLE hsUnregisteredSeiTimecode #-}
{-# DEPRECATED unregisteredSeiTimecode "Use generic-lens or generic-optics with 'unregisteredSeiTimecode' instead"  #-}

instance Core.FromJSON H264Settings where
        toJSON H264Settings{..}
          = Core.object
              (Core.catMaybes
                 [("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("codecLevel" Core..=) Core.<$> codecLevel,
                  ("codecProfile" Core..=) Core.<$> codecProfile,
                  ("dynamicSubGop" Core..=) Core.<$> dynamicSubGop,
                  ("entropyEncoding" Core..=) Core.<$> entropyEncoding,
                  ("fieldEncoding" Core..=) Core.<$> fieldEncoding,
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
                  ("repeatPps" Core..=) Core.<$> repeatPps,
                  ("sceneChangeDetect" Core..=) Core.<$> sceneChangeDetect,
                  ("slices" Core..=) Core.<$> slices,
                  ("slowPal" Core..=) Core.<$> slowPal,
                  ("softness" Core..=) Core.<$> softness,
                  ("spatialAdaptiveQuantization" Core..=) Core.<$>
                    spatialAdaptiveQuantization,
                  ("syntax" Core..=) Core.<$> syntax,
                  ("telecine" Core..=) Core.<$> telecine,
                  ("temporalAdaptiveQuantization" Core..=) Core.<$>
                    temporalAdaptiveQuantization,
                  ("unregisteredSeiTimecode" Core..=) Core.<$>
                    unregisteredSeiTimecode])

instance Core.FromJSON H264Settings where
        parseJSON
          = Core.withObject "H264Settings" Core.$
              \ x ->
                H264Settings' Core.<$>
                  (x Core..:? "adaptiveQuantization") Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "codecLevel"
                    Core.<*> x Core..:? "codecProfile"
                    Core.<*> x Core..:? "dynamicSubGop"
                    Core.<*> x Core..:? "entropyEncoding"
                    Core.<*> x Core..:? "fieldEncoding"
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
                    Core.<*> x Core..:? "repeatPps"
                    Core.<*> x Core..:? "sceneChangeDetect"
                    Core.<*> x Core..:? "slices"
                    Core.<*> x Core..:? "slowPal"
                    Core.<*> x Core..:? "softness"
                    Core.<*> x Core..:? "spatialAdaptiveQuantization"
                    Core.<*> x Core..:? "syntax"
                    Core.<*> x Core..:? "telecine"
                    Core.<*> x Core..:? "temporalAdaptiveQuantization"
                    Core.<*> x Core..:? "unregisteredSeiTimecode"
