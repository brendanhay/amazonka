{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Settings
  ( H265Settings (..),

    -- * Smart constructor
    mkH265Settings,

    -- * Lenses
    hsUnregisteredSeiTimecode,
    hsQualityTuningLevel,
    hsTemporalAdaptiveQuantization,
    hsSceneChangeDetect,
    hsHrdBufferInitialFillPercentage,
    hsTiles,
    hsSlowPal,
    hsTemporalIds,
    hsParNumerator,
    hsGopSize,
    hsNumberBFramesBetweenReferenceFrames,
    hsGopSizeUnits,
    hsHrdBufferSize,
    hsSlices,
    hsAlternateTransferFunctionSei,
    hsRateControlMode,
    hsNumberReferenceFrames,
    hsTelecine,
    hsDynamicSubGop,
    hsMinIInterval,
    hsInterlaceMode,
    hsParControl,
    hsFlickerAdaptiveQuantization,
    hsQvbrSettings,
    hsSampleAdaptiveOffsetFilterMode,
    hsCodecProfile,
    hsBitrate,
    hsFramerateDenominator,
    hsFramerateConversionAlgorithm,
    hsCodecLevel,
    hsFramerateControl,
    hsWriteMp4PackagingType,
    hsAdaptiveQuantization,
    hsFramerateNumerator,
    hsGopBReference,
    hsMaxBitrate,
    hsGopClosedCadence,
    hsParDenominator,
    hsSpatialAdaptiveQuantization,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
import Network.AWS.MediaConvert.Types.H265CodecLevel
import Network.AWS.MediaConvert.Types.H265CodecProfile
import Network.AWS.MediaConvert.Types.H265DynamicSubGop
import Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265FramerateControl
import Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H265GopBReference
import Network.AWS.MediaConvert.Types.H265GopSizeUnits
import Network.AWS.MediaConvert.Types.H265InterlaceMode
import Network.AWS.MediaConvert.Types.H265ParControl
import Network.AWS.MediaConvert.Types.H265QualityTuningLevel
import Network.AWS.MediaConvert.Types.H265QvbrSettings
import Network.AWS.MediaConvert.Types.H265RateControlMode
import Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
import Network.AWS.MediaConvert.Types.H265SceneChangeDetect
import Network.AWS.MediaConvert.Types.H265SlowPal
import Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265Telecine
import Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265TemporalIds
import Network.AWS.MediaConvert.Types.H265Tiles
import Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
import Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
import qualified Network.AWS.Prelude as Lude

-- | Settings for H265 codec
--
-- /See:/ 'mkH265Settings' smart constructor.
data H265Settings = H265Settings'
  { -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
    unregisteredSeiTimecode :: Lude.Maybe H265UnregisteredSeiTimecode,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Lude.Maybe H265QualityTuningLevel,
    -- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
    temporalAdaptiveQuantization :: Lude.Maybe H265TemporalAdaptiveQuantization,
    -- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
    sceneChangeDetect :: Lude.Maybe H265SceneChangeDetect,
    -- | Percentage of the buffer that should initially be filled (HRD buffer model).
    hrdBufferInitialFillPercentage :: Lude.Maybe Lude.Natural,
    -- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
    tiles :: Lude.Maybe H265Tiles,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Lude.Maybe H265SlowPal,
    -- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
    temporalIds :: Lude.Maybe H265TemporalIds,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
    parNumerator :: Lude.Maybe Lude.Natural,
    -- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
    gopSize :: Lude.Maybe Lude.Double,
    -- | Number of B-frames between reference frames.
    numberBFramesBetweenReferenceFrames :: Lude.Maybe Lude.Natural,
    -- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
    gopSizeUnits :: Lude.Maybe H265GopSizeUnits,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
    hrdBufferSize :: Lude.Maybe Lude.Natural,
    -- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
    slices :: Lude.Maybe Lude.Natural,
    -- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
    alternateTransferFunctionSei :: Lude.Maybe H265AlternateTransferFunctionSei,
    -- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
    rateControlMode :: Lude.Maybe H265RateControlMode,
    -- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
    numberReferenceFrames :: Lude.Maybe Lude.Natural,
    -- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
    telecine :: Lude.Maybe H265Telecine,
    -- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Lude.Maybe H265DynamicSubGop,
    -- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Lude.Maybe Lude.Natural,
    -- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
    interlaceMode :: Lude.Maybe H265InterlaceMode,
    -- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
    parControl :: Lude.Maybe H265ParControl,
    -- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
    flickerAdaptiveQuantization :: Lude.Maybe H265FlickerAdaptiveQuantization,
    -- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
    qvbrSettings :: Lude.Maybe H265QvbrSettings,
    -- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
    sampleAdaptiveOffsetFilterMode :: Lude.Maybe H265SampleAdaptiveOffsetFilterMode,
    -- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
    codecProfile :: Lude.Maybe H265CodecProfile,
    -- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Lude.Maybe Lude.Natural,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Lude.Maybe H265FramerateConversionAlgorithm,
    -- | H.265 Level.
    codecLevel :: Lude.Maybe H265CodecLevel,
    -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Lude.Maybe H265FramerateControl,
    -- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
    writeMp4PackagingType :: Lude.Maybe H265WriteMp4PackagingType,
    -- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
    adaptiveQuantization :: Lude.Maybe H265AdaptiveQuantization,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Lude.Maybe Lude.Natural,
    -- | If enable, use reference B frames for GOP structures that have B frames > 1.
    gopBReference :: Lude.Maybe H265GopBReference,
    -- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Lude.Maybe Lude.Natural,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
    parDenominator :: Lude.Maybe Lude.Natural,
    -- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
    spatialAdaptiveQuantization :: Lude.Maybe H265SpatialAdaptiveQuantization
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- * 'unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
-- * 'qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
-- * 'temporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
-- * 'sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
-- * 'hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
-- * 'tiles' - Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'temporalIds' - Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'gopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
-- * 'numberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
-- * 'gopSizeUnits' - Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
-- * 'hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
-- * 'slices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
-- * 'alternateTransferFunctionSei' - Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
-- * 'rateControlMode' - Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
-- * 'numberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
-- * 'telecine' - This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
-- * 'dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
-- * 'minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
-- * 'interlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
-- * 'flickerAdaptiveQuantization' - Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
-- * 'qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
-- * 'sampleAdaptiveOffsetFilterMode' - Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
-- * 'codecProfile' - Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
-- * 'bitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'codecLevel' - H.265 Level.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'writeMp4PackagingType' - If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
-- * 'adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'gopBReference' - If enable, use reference B frames for GOP structures that have B frames > 1.
-- * 'maxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
-- * 'gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
-- * 'spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
mkH265Settings ::
  H265Settings
mkH265Settings =
  H265Settings'
    { unregisteredSeiTimecode = Lude.Nothing,
      qualityTuningLevel = Lude.Nothing,
      temporalAdaptiveQuantization = Lude.Nothing,
      sceneChangeDetect = Lude.Nothing,
      hrdBufferInitialFillPercentage = Lude.Nothing,
      tiles = Lude.Nothing,
      slowPal = Lude.Nothing,
      temporalIds = Lude.Nothing,
      parNumerator = Lude.Nothing,
      gopSize = Lude.Nothing,
      numberBFramesBetweenReferenceFrames = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      hrdBufferSize = Lude.Nothing,
      slices = Lude.Nothing,
      alternateTransferFunctionSei = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      numberReferenceFrames = Lude.Nothing,
      telecine = Lude.Nothing,
      dynamicSubGop = Lude.Nothing,
      minIInterval = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      parControl = Lude.Nothing,
      flickerAdaptiveQuantization = Lude.Nothing,
      qvbrSettings = Lude.Nothing,
      sampleAdaptiveOffsetFilterMode = Lude.Nothing,
      codecProfile = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      codecLevel = Lude.Nothing,
      framerateControl = Lude.Nothing,
      writeMp4PackagingType = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      gopBReference = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      gopClosedCadence = Lude.Nothing,
      parDenominator = Lude.Nothing,
      spatialAdaptiveQuantization = Lude.Nothing
    }

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- /Note:/ Consider using 'unregisteredSeiTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsUnregisteredSeiTimecode :: Lens.Lens' H265Settings (Lude.Maybe H265UnregisteredSeiTimecode)
hsUnregisteredSeiTimecode = Lens.lens (unregisteredSeiTimecode :: H265Settings -> Lude.Maybe H265UnregisteredSeiTimecode) (\s a -> s {unregisteredSeiTimecode = a} :: H265Settings)
{-# DEPRECATED hsUnregisteredSeiTimecode "Use generic-lens or generic-optics with 'unregisteredSeiTimecode' instead." #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQualityTuningLevel :: Lens.Lens' H265Settings (Lude.Maybe H265QualityTuningLevel)
hsQualityTuningLevel = Lens.lens (qualityTuningLevel :: H265Settings -> Lude.Maybe H265QualityTuningLevel) (\s a -> s {qualityTuningLevel = a} :: H265Settings)
{-# DEPRECATED hsQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTemporalAdaptiveQuantization :: Lens.Lens' H265Settings (Lude.Maybe H265TemporalAdaptiveQuantization)
hsTemporalAdaptiveQuantization = Lens.lens (temporalAdaptiveQuantization :: H265Settings -> Lude.Maybe H265TemporalAdaptiveQuantization) (\s a -> s {temporalAdaptiveQuantization = a} :: H265Settings)
{-# DEPRECATED hsTemporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead." #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSceneChangeDetect :: Lens.Lens' H265Settings (Lude.Maybe H265SceneChangeDetect)
hsSceneChangeDetect = Lens.lens (sceneChangeDetect :: H265Settings -> Lude.Maybe H265SceneChangeDetect) (\s a -> s {sceneChangeDetect = a} :: H265Settings)
{-# DEPRECATED hsSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHrdBufferInitialFillPercentage :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsHrdBufferInitialFillPercentage = Lens.lens (hrdBufferInitialFillPercentage :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferInitialFillPercentage = a} :: H265Settings)
{-# DEPRECATED hsHrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead." #-}

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
--
-- /Note:/ Consider using 'tiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTiles :: Lens.Lens' H265Settings (Lude.Maybe H265Tiles)
hsTiles = Lens.lens (tiles :: H265Settings -> Lude.Maybe H265Tiles) (\s a -> s {tiles = a} :: H265Settings)
{-# DEPRECATED hsTiles "Use generic-lens or generic-optics with 'tiles' instead." #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlowPal :: Lens.Lens' H265Settings (Lude.Maybe H265SlowPal)
hsSlowPal = Lens.lens (slowPal :: H265Settings -> Lude.Maybe H265SlowPal) (\s a -> s {slowPal = a} :: H265Settings)
{-# DEPRECATED hsSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
--
-- /Note:/ Consider using 'temporalIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTemporalIds :: Lens.Lens' H265Settings (Lude.Maybe H265TemporalIds)
hsTemporalIds = Lens.lens (temporalIds :: H265Settings -> Lude.Maybe H265TemporalIds) (\s a -> s {temporalIds = a} :: H265Settings)
{-# DEPRECATED hsTemporalIds "Use generic-lens or generic-optics with 'temporalIds' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParNumerator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsParNumerator = Lens.lens (parNumerator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: H265Settings)
{-# DEPRECATED hsParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSize :: Lens.Lens' H265Settings (Lude.Maybe Lude.Double)
hsGopSize = Lens.lens (gopSize :: H265Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: H265Settings)
{-# DEPRECATED hsGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsNumberBFramesBetweenReferenceFrames :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsNumberBFramesBetweenReferenceFrames = Lens.lens (numberBFramesBetweenReferenceFrames :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberBFramesBetweenReferenceFrames = a} :: H265Settings)
{-# DEPRECATED hsNumberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead." #-}

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSizeUnits :: Lens.Lens' H265Settings (Lude.Maybe H265GopSizeUnits)
hsGopSizeUnits = Lens.lens (gopSizeUnits :: H265Settings -> Lude.Maybe H265GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: H265Settings)
{-# DEPRECATED hsGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHrdBufferSize :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsHrdBufferSize = Lens.lens (hrdBufferSize :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferSize = a} :: H265Settings)
{-# DEPRECATED hsHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlices :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsSlices = Lens.lens (slices :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {slices = a} :: H265Settings)
{-# DEPRECATED hsSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
--
-- /Note:/ Consider using 'alternateTransferFunctionSei' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAlternateTransferFunctionSei :: Lens.Lens' H265Settings (Lude.Maybe H265AlternateTransferFunctionSei)
hsAlternateTransferFunctionSei = Lens.lens (alternateTransferFunctionSei :: H265Settings -> Lude.Maybe H265AlternateTransferFunctionSei) (\s a -> s {alternateTransferFunctionSei = a} :: H265Settings)
{-# DEPRECATED hsAlternateTransferFunctionSei "Use generic-lens or generic-optics with 'alternateTransferFunctionSei' instead." #-}

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsRateControlMode :: Lens.Lens' H265Settings (Lude.Maybe H265RateControlMode)
hsRateControlMode = Lens.lens (rateControlMode :: H265Settings -> Lude.Maybe H265RateControlMode) (\s a -> s {rateControlMode = a} :: H265Settings)
{-# DEPRECATED hsRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numberReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsNumberReferenceFrames :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsNumberReferenceFrames = Lens.lens (numberReferenceFrames :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberReferenceFrames = a} :: H265Settings)
{-# DEPRECATED hsNumberReferenceFrames "Use generic-lens or generic-optics with 'numberReferenceFrames' instead." #-}

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTelecine :: Lens.Lens' H265Settings (Lude.Maybe H265Telecine)
hsTelecine = Lens.lens (telecine :: H265Settings -> Lude.Maybe H265Telecine) (\s a -> s {telecine = a} :: H265Settings)
{-# DEPRECATED hsTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsDynamicSubGop :: Lens.Lens' H265Settings (Lude.Maybe H265DynamicSubGop)
hsDynamicSubGop = Lens.lens (dynamicSubGop :: H265Settings -> Lude.Maybe H265DynamicSubGop) (\s a -> s {dynamicSubGop = a} :: H265Settings)
{-# DEPRECATED hsDynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead." #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMinIInterval :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsMinIInterval = Lens.lens (minIInterval :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {minIInterval = a} :: H265Settings)
{-# DEPRECATED hsMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsInterlaceMode :: Lens.Lens' H265Settings (Lude.Maybe H265InterlaceMode)
hsInterlaceMode = Lens.lens (interlaceMode :: H265Settings -> Lude.Maybe H265InterlaceMode) (\s a -> s {interlaceMode = a} :: H265Settings)
{-# DEPRECATED hsInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParControl :: Lens.Lens' H265Settings (Lude.Maybe H265ParControl)
hsParControl = Lens.lens (parControl :: H265Settings -> Lude.Maybe H265ParControl) (\s a -> s {parControl = a} :: H265Settings)
{-# DEPRECATED hsParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
--
-- /Note:/ Consider using 'flickerAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFlickerAdaptiveQuantization :: Lens.Lens' H265Settings (Lude.Maybe H265FlickerAdaptiveQuantization)
hsFlickerAdaptiveQuantization = Lens.lens (flickerAdaptiveQuantization :: H265Settings -> Lude.Maybe H265FlickerAdaptiveQuantization) (\s a -> s {flickerAdaptiveQuantization = a} :: H265Settings)
{-# DEPRECATED hsFlickerAdaptiveQuantization "Use generic-lens or generic-optics with 'flickerAdaptiveQuantization' instead." #-}

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQvbrSettings :: Lens.Lens' H265Settings (Lude.Maybe H265QvbrSettings)
hsQvbrSettings = Lens.lens (qvbrSettings :: H265Settings -> Lude.Maybe H265QvbrSettings) (\s a -> s {qvbrSettings = a} :: H265Settings)
{-# DEPRECATED hsQvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead." #-}

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
--
-- /Note:/ Consider using 'sampleAdaptiveOffsetFilterMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSampleAdaptiveOffsetFilterMode :: Lens.Lens' H265Settings (Lude.Maybe H265SampleAdaptiveOffsetFilterMode)
hsSampleAdaptiveOffsetFilterMode = Lens.lens (sampleAdaptiveOffsetFilterMode :: H265Settings -> Lude.Maybe H265SampleAdaptiveOffsetFilterMode) (\s a -> s {sampleAdaptiveOffsetFilterMode = a} :: H265Settings)
{-# DEPRECATED hsSampleAdaptiveOffsetFilterMode "Use generic-lens or generic-optics with 'sampleAdaptiveOffsetFilterMode' instead." #-}

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsCodecProfile :: Lens.Lens' H265Settings (Lude.Maybe H265CodecProfile)
hsCodecProfile = Lens.lens (codecProfile :: H265Settings -> Lude.Maybe H265CodecProfile) (\s a -> s {codecProfile = a} :: H265Settings)
{-# DEPRECATED hsCodecProfile "Use generic-lens or generic-optics with 'codecProfile' instead." #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBitrate :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsBitrate = Lens.lens (bitrate :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: H265Settings)
{-# DEPRECATED hsBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateDenominator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsFramerateDenominator = Lens.lens (framerateDenominator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: H265Settings)
{-# DEPRECATED hsFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateConversionAlgorithm :: Lens.Lens' H265Settings (Lude.Maybe H265FramerateConversionAlgorithm)
hsFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: H265Settings -> Lude.Maybe H265FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: H265Settings)
{-# DEPRECATED hsFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | H.265 Level.
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsCodecLevel :: Lens.Lens' H265Settings (Lude.Maybe H265CodecLevel)
hsCodecLevel = Lens.lens (codecLevel :: H265Settings -> Lude.Maybe H265CodecLevel) (\s a -> s {codecLevel = a} :: H265Settings)
{-# DEPRECATED hsCodecLevel "Use generic-lens or generic-optics with 'codecLevel' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateControl :: Lens.Lens' H265Settings (Lude.Maybe H265FramerateControl)
hsFramerateControl = Lens.lens (framerateControl :: H265Settings -> Lude.Maybe H265FramerateControl) (\s a -> s {framerateControl = a} :: H265Settings)
{-# DEPRECATED hsFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
--
-- /Note:/ Consider using 'writeMp4PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsWriteMp4PackagingType :: Lens.Lens' H265Settings (Lude.Maybe H265WriteMp4PackagingType)
hsWriteMp4PackagingType = Lens.lens (writeMp4PackagingType :: H265Settings -> Lude.Maybe H265WriteMp4PackagingType) (\s a -> s {writeMp4PackagingType = a} :: H265Settings)
{-# DEPRECATED hsWriteMp4PackagingType "Use generic-lens or generic-optics with 'writeMp4PackagingType' instead." #-}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAdaptiveQuantization :: Lens.Lens' H265Settings (Lude.Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = Lens.lens (adaptiveQuantization :: H265Settings -> Lude.Maybe H265AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: H265Settings)
{-# DEPRECATED hsAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateNumerator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsFramerateNumerator = Lens.lens (framerateNumerator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: H265Settings)
{-# DEPRECATED hsFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopBReference :: Lens.Lens' H265Settings (Lude.Maybe H265GopBReference)
hsGopBReference = Lens.lens (gopBReference :: H265Settings -> Lude.Maybe H265GopBReference) (\s a -> s {gopBReference = a} :: H265Settings)
{-# DEPRECATED hsGopBReference "Use generic-lens or generic-optics with 'gopBReference' instead." #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxBitrate :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsMaxBitrate = Lens.lens (maxBitrate :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: H265Settings)
{-# DEPRECATED hsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopClosedCadence :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsGopClosedCadence = Lens.lens (gopClosedCadence :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: H265Settings)
{-# DEPRECATED hsGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParDenominator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsParDenominator = Lens.lens (parDenominator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: H265Settings)
{-# DEPRECATED hsParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSpatialAdaptiveQuantization :: Lens.Lens' H265Settings (Lude.Maybe H265SpatialAdaptiveQuantization)
hsSpatialAdaptiveQuantization = Lens.lens (spatialAdaptiveQuantization :: H265Settings -> Lude.Maybe H265SpatialAdaptiveQuantization) (\s a -> s {spatialAdaptiveQuantization = a} :: H265Settings)
{-# DEPRECATED hsSpatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead." #-}

instance Lude.FromJSON H265Settings where
  parseJSON =
    Lude.withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            Lude.<$> (x Lude..:? "unregisteredSeiTimecode")
            Lude.<*> (x Lude..:? "qualityTuningLevel")
            Lude.<*> (x Lude..:? "temporalAdaptiveQuantization")
            Lude.<*> (x Lude..:? "sceneChangeDetect")
            Lude.<*> (x Lude..:? "hrdBufferInitialFillPercentage")
            Lude.<*> (x Lude..:? "tiles")
            Lude.<*> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "temporalIds")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "numberBFramesBetweenReferenceFrames")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "hrdBufferSize")
            Lude.<*> (x Lude..:? "slices")
            Lude.<*> (x Lude..:? "alternateTransferFunctionSei")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "numberReferenceFrames")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "dynamicSubGop")
            Lude.<*> (x Lude..:? "minIInterval")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "flickerAdaptiveQuantization")
            Lude.<*> (x Lude..:? "qvbrSettings")
            Lude.<*> (x Lude..:? "sampleAdaptiveOffsetFilterMode")
            Lude.<*> (x Lude..:? "codecProfile")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "codecLevel")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "writeMp4PackagingType")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "gopBReference")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "parDenominator")
            Lude.<*> (x Lude..:? "spatialAdaptiveQuantization")
      )

instance Lude.ToJSON H265Settings where
  toJSON H265Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("unregisteredSeiTimecode" Lude..=)
              Lude.<$> unregisteredSeiTimecode,
            ("qualityTuningLevel" Lude..=) Lude.<$> qualityTuningLevel,
            ("temporalAdaptiveQuantization" Lude..=)
              Lude.<$> temporalAdaptiveQuantization,
            ("sceneChangeDetect" Lude..=) Lude.<$> sceneChangeDetect,
            ("hrdBufferInitialFillPercentage" Lude..=)
              Lude.<$> hrdBufferInitialFillPercentage,
            ("tiles" Lude..=) Lude.<$> tiles,
            ("slowPal" Lude..=) Lude.<$> slowPal,
            ("temporalIds" Lude..=) Lude.<$> temporalIds,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Lude..=)
              Lude.<$> numberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("hrdBufferSize" Lude..=) Lude.<$> hrdBufferSize,
            ("slices" Lude..=) Lude.<$> slices,
            ("alternateTransferFunctionSei" Lude..=)
              Lude.<$> alternateTransferFunctionSei,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("numberReferenceFrames" Lude..=) Lude.<$> numberReferenceFrames,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("dynamicSubGop" Lude..=) Lude.<$> dynamicSubGop,
            ("minIInterval" Lude..=) Lude.<$> minIInterval,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("flickerAdaptiveQuantization" Lude..=)
              Lude.<$> flickerAdaptiveQuantization,
            ("qvbrSettings" Lude..=) Lude.<$> qvbrSettings,
            ("sampleAdaptiveOffsetFilterMode" Lude..=)
              Lude.<$> sampleAdaptiveOffsetFilterMode,
            ("codecProfile" Lude..=) Lude.<$> codecProfile,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("codecLevel" Lude..=) Lude.<$> codecLevel,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("writeMp4PackagingType" Lude..=) Lude.<$> writeMp4PackagingType,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("gopBReference" Lude..=) Lude.<$> gopBReference,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("parDenominator" Lude..=) Lude.<$> parDenominator,
            ("spatialAdaptiveQuantization" Lude..=)
              Lude.<$> spatialAdaptiveQuantization
          ]
      )
