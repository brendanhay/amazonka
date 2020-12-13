{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Settings
  ( H264Settings (..),

    -- * Smart constructor
    mkH264Settings,

    -- * Lenses
    hUnregisteredSeiTimecode,
    hQualityTuningLevel,
    hTemporalAdaptiveQuantization,
    hSceneChangeDetect,
    hHrdBufferInitialFillPercentage,
    hSlowPal,
    hParNumerator,
    hGopSize,
    hNumberBFramesBetweenReferenceFrames,
    hGopSizeUnits,
    hHrdBufferSize,
    hSlices,
    hRateControlMode,
    hNumberReferenceFrames,
    hTelecine,
    hDynamicSubGop,
    hMinIInterval,
    hInterlaceMode,
    hParControl,
    hRepeatPps,
    hFlickerAdaptiveQuantization,
    hQvbrSettings,
    hSoftness,
    hCodecProfile,
    hBitrate,
    hFramerateDenominator,
    hFramerateConversionAlgorithm,
    hCodecLevel,
    hEntropyEncoding,
    hFramerateControl,
    hAdaptiveQuantization,
    hFramerateNumerator,
    hGopBReference,
    hMaxBitrate,
    hSyntax,
    hFieldEncoding,
    hGopClosedCadence,
    hParDenominator,
    hSpatialAdaptiveQuantization,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264CodecLevel
import Network.AWS.MediaConvert.Types.H264CodecProfile
import Network.AWS.MediaConvert.Types.H264DynamicSubGop
import Network.AWS.MediaConvert.Types.H264EntropyEncoding
import Network.AWS.MediaConvert.Types.H264FieldEncoding
import Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264FramerateControl
import Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H264GopBReference
import Network.AWS.MediaConvert.Types.H264GopSizeUnits
import Network.AWS.MediaConvert.Types.H264InterlaceMode
import Network.AWS.MediaConvert.Types.H264ParControl
import Network.AWS.MediaConvert.Types.H264QualityTuningLevel
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264RateControlMode
import Network.AWS.MediaConvert.Types.H264RepeatPps
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
import Network.AWS.MediaConvert.Types.H264SlowPal
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264Syntax
import Network.AWS.MediaConvert.Types.H264Telecine
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'mkH264Settings' smart constructor.
data H264Settings = H264Settings'
  { -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
    unregisteredSeiTimecode :: Lude.Maybe H264UnregisteredSeiTimecode,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Lude.Maybe H264QualityTuningLevel,
    -- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
    temporalAdaptiveQuantization :: Lude.Maybe H264TemporalAdaptiveQuantization,
    -- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
    sceneChangeDetect :: Lude.Maybe H264SceneChangeDetect,
    -- | Percentage of the buffer that should initially be filled (HRD buffer model).
    hrdBufferInitialFillPercentage :: Lude.Maybe Lude.Natural,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Lude.Maybe H264SlowPal,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
    parNumerator :: Lude.Maybe Lude.Natural,
    -- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
    gopSize :: Lude.Maybe Lude.Double,
    -- | Number of B-frames between reference frames.
    numberBFramesBetweenReferenceFrames :: Lude.Maybe Lude.Natural,
    -- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
    gopSizeUnits :: Lude.Maybe H264GopSizeUnits,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
    hrdBufferSize :: Lude.Maybe Lude.Natural,
    -- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
    slices :: Lude.Maybe Lude.Natural,
    -- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
    rateControlMode :: Lude.Maybe H264RateControlMode,
    -- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
    numberReferenceFrames :: Lude.Maybe Lude.Natural,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
    telecine :: Lude.Maybe H264Telecine,
    -- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Lude.Maybe H264DynamicSubGop,
    -- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Lude.Maybe Lude.Natural,
    -- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
    interlaceMode :: Lude.Maybe H264InterlaceMode,
    -- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
    parControl :: Lude.Maybe H264ParControl,
    -- | Places a PPS header on each encoded picture, even if repeated.
    repeatPps :: Lude.Maybe H264RepeatPps,
    -- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
    flickerAdaptiveQuantization :: Lude.Maybe H264FlickerAdaptiveQuantization,
    -- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
    qvbrSettings :: Lude.Maybe H264QvbrSettings,
    -- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
    softness :: Lude.Maybe Lude.Natural,
    -- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
    codecProfile :: Lude.Maybe H264CodecProfile,
    -- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Lude.Maybe Lude.Natural,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Lude.Maybe H264FramerateConversionAlgorithm,
    -- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
    codecLevel :: Lude.Maybe H264CodecLevel,
    -- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
    entropyEncoding :: Lude.Maybe H264EntropyEncoding,
    -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Lude.Maybe H264FramerateControl,
    -- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
    adaptiveQuantization :: Lude.Maybe H264AdaptiveQuantization,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Lude.Maybe Lude.Natural,
    -- | If enable, use reference B frames for GOP structures that have B frames > 1.
    gopBReference :: Lude.Maybe H264GopBReference,
    -- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Lude.Maybe Lude.Natural,
    -- | Produces a bitstream compliant with SMPTE RP-2027.
    syntax :: Lude.Maybe H264Syntax,
    -- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
    fieldEncoding :: Lude.Maybe H264FieldEncoding,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
    parDenominator :: Lude.Maybe Lude.Natural,
    -- | Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
    spatialAdaptiveQuantization :: Lude.Maybe H264SpatialAdaptiveQuantization
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- * 'unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
-- * 'qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
-- * 'temporalAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
-- * 'sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
-- * 'hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'gopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
-- * 'numberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
-- * 'gopSizeUnits' - Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
-- * 'hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
-- * 'slices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
-- * 'rateControlMode' - Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
-- * 'numberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
-- * 'telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
-- * 'dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
-- * 'minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
-- * 'interlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
-- * 'repeatPps' - Places a PPS header on each encoded picture, even if repeated.
-- * 'flickerAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
-- * 'qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
-- * 'softness' - Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
-- * 'codecProfile' - H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
-- * 'bitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'codecLevel' - Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
-- * 'entropyEncoding' - Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'adaptiveQuantization' - Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'gopBReference' - If enable, use reference B frames for GOP structures that have B frames > 1.
-- * 'maxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
-- * 'syntax' - Produces a bitstream compliant with SMPTE RP-2027.
-- * 'fieldEncoding' - Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
-- * 'gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
-- * 'spatialAdaptiveQuantization' - Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
mkH264Settings ::
  H264Settings
mkH264Settings =
  H264Settings'
    { unregisteredSeiTimecode = Lude.Nothing,
      qualityTuningLevel = Lude.Nothing,
      temporalAdaptiveQuantization = Lude.Nothing,
      sceneChangeDetect = Lude.Nothing,
      hrdBufferInitialFillPercentage = Lude.Nothing,
      slowPal = Lude.Nothing,
      parNumerator = Lude.Nothing,
      gopSize = Lude.Nothing,
      numberBFramesBetweenReferenceFrames = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      hrdBufferSize = Lude.Nothing,
      slices = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      numberReferenceFrames = Lude.Nothing,
      telecine = Lude.Nothing,
      dynamicSubGop = Lude.Nothing,
      minIInterval = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      parControl = Lude.Nothing,
      repeatPps = Lude.Nothing,
      flickerAdaptiveQuantization = Lude.Nothing,
      qvbrSettings = Lude.Nothing,
      softness = Lude.Nothing,
      codecProfile = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      codecLevel = Lude.Nothing,
      entropyEncoding = Lude.Nothing,
      framerateControl = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      gopBReference = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      syntax = Lude.Nothing,
      fieldEncoding = Lude.Nothing,
      gopClosedCadence = Lude.Nothing,
      parDenominator = Lude.Nothing,
      spatialAdaptiveQuantization = Lude.Nothing
    }

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- /Note:/ Consider using 'unregisteredSeiTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hUnregisteredSeiTimecode :: Lens.Lens' H264Settings (Lude.Maybe H264UnregisteredSeiTimecode)
hUnregisteredSeiTimecode = Lens.lens (unregisteredSeiTimecode :: H264Settings -> Lude.Maybe H264UnregisteredSeiTimecode) (\s a -> s {unregisteredSeiTimecode = a} :: H264Settings)
{-# DEPRECATED hUnregisteredSeiTimecode "Use generic-lens or generic-optics with 'unregisteredSeiTimecode' instead." #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQualityTuningLevel :: Lens.Lens' H264Settings (Lude.Maybe H264QualityTuningLevel)
hQualityTuningLevel = Lens.lens (qualityTuningLevel :: H264Settings -> Lude.Maybe H264QualityTuningLevel) (\s a -> s {qualityTuningLevel = a} :: H264Settings)
{-# DEPRECATED hQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'temporalAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalAdaptiveQuantization :: Lens.Lens' H264Settings (Lude.Maybe H264TemporalAdaptiveQuantization)
hTemporalAdaptiveQuantization = Lens.lens (temporalAdaptiveQuantization :: H264Settings -> Lude.Maybe H264TemporalAdaptiveQuantization) (\s a -> s {temporalAdaptiveQuantization = a} :: H264Settings)
{-# DEPRECATED hTemporalAdaptiveQuantization "Use generic-lens or generic-optics with 'temporalAdaptiveQuantization' instead." #-}

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSceneChangeDetect :: Lens.Lens' H264Settings (Lude.Maybe H264SceneChangeDetect)
hSceneChangeDetect = Lens.lens (sceneChangeDetect :: H264Settings -> Lude.Maybe H264SceneChangeDetect) (\s a -> s {sceneChangeDetect = a} :: H264Settings)
{-# DEPRECATED hSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'hrdBufferInitialFillPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHrdBufferInitialFillPercentage :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hHrdBufferInitialFillPercentage = Lens.lens (hrdBufferInitialFillPercentage :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferInitialFillPercentage = a} :: H264Settings)
{-# DEPRECATED hHrdBufferInitialFillPercentage "Use generic-lens or generic-optics with 'hrdBufferInitialFillPercentage' instead." #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlowPal :: Lens.Lens' H264Settings (Lude.Maybe H264SlowPal)
hSlowPal = Lens.lens (slowPal :: H264Settings -> Lude.Maybe H264SlowPal) (\s a -> s {slowPal = a} :: H264Settings)
{-# DEPRECATED hSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParNumerator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hParNumerator = Lens.lens (parNumerator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: H264Settings)
{-# DEPRECATED hParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSize :: Lens.Lens' H264Settings (Lude.Maybe Lude.Double)
hGopSize = Lens.lens (gopSize :: H264Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: H264Settings)
{-# DEPRECATED hGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hNumberBFramesBetweenReferenceFrames :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hNumberBFramesBetweenReferenceFrames = Lens.lens (numberBFramesBetweenReferenceFrames :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberBFramesBetweenReferenceFrames = a} :: H264Settings)
{-# DEPRECATED hNumberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead." #-}

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSizeUnits :: Lens.Lens' H264Settings (Lude.Maybe H264GopSizeUnits)
hGopSizeUnits = Lens.lens (gopSizeUnits :: H264Settings -> Lude.Maybe H264GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: H264Settings)
{-# DEPRECATED hGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHrdBufferSize :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hHrdBufferSize = Lens.lens (hrdBufferSize :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferSize = a} :: H264Settings)
{-# DEPRECATED hHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlices :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hSlices = Lens.lens (slices :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {slices = a} :: H264Settings)
{-# DEPRECATED hSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRateControlMode :: Lens.Lens' H264Settings (Lude.Maybe H264RateControlMode)
hRateControlMode = Lens.lens (rateControlMode :: H264Settings -> Lude.Maybe H264RateControlMode) (\s a -> s {rateControlMode = a} :: H264Settings)
{-# DEPRECATED hRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numberReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hNumberReferenceFrames :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hNumberReferenceFrames = Lens.lens (numberReferenceFrames :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberReferenceFrames = a} :: H264Settings)
{-# DEPRECATED hNumberReferenceFrames "Use generic-lens or generic-optics with 'numberReferenceFrames' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTelecine :: Lens.Lens' H264Settings (Lude.Maybe H264Telecine)
hTelecine = Lens.lens (telecine :: H264Settings -> Lude.Maybe H264Telecine) (\s a -> s {telecine = a} :: H264Settings)
{-# DEPRECATED hTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- /Note:/ Consider using 'dynamicSubGop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hDynamicSubGop :: Lens.Lens' H264Settings (Lude.Maybe H264DynamicSubGop)
hDynamicSubGop = Lens.lens (dynamicSubGop :: H264Settings -> Lude.Maybe H264DynamicSubGop) (\s a -> s {dynamicSubGop = a} :: H264Settings)
{-# DEPRECATED hDynamicSubGop "Use generic-lens or generic-optics with 'dynamicSubGop' instead." #-}

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMinIInterval :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hMinIInterval = Lens.lens (minIInterval :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {minIInterval = a} :: H264Settings)
{-# DEPRECATED hMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hInterlaceMode :: Lens.Lens' H264Settings (Lude.Maybe H264InterlaceMode)
hInterlaceMode = Lens.lens (interlaceMode :: H264Settings -> Lude.Maybe H264InterlaceMode) (\s a -> s {interlaceMode = a} :: H264Settings)
{-# DEPRECATED hInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParControl :: Lens.Lens' H264Settings (Lude.Maybe H264ParControl)
hParControl = Lens.lens (parControl :: H264Settings -> Lude.Maybe H264ParControl) (\s a -> s {parControl = a} :: H264Settings)
{-# DEPRECATED hParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Places a PPS header on each encoded picture, even if repeated.
--
-- /Note:/ Consider using 'repeatPps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRepeatPps :: Lens.Lens' H264Settings (Lude.Maybe H264RepeatPps)
hRepeatPps = Lens.lens (repeatPps :: H264Settings -> Lude.Maybe H264RepeatPps) (\s a -> s {repeatPps = a} :: H264Settings)
{-# DEPRECATED hRepeatPps "Use generic-lens or generic-optics with 'repeatPps' instead." #-}

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'flickerAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFlickerAdaptiveQuantization :: Lens.Lens' H264Settings (Lude.Maybe H264FlickerAdaptiveQuantization)
hFlickerAdaptiveQuantization = Lens.lens (flickerAdaptiveQuantization :: H264Settings -> Lude.Maybe H264FlickerAdaptiveQuantization) (\s a -> s {flickerAdaptiveQuantization = a} :: H264Settings)
{-# DEPRECATED hFlickerAdaptiveQuantization "Use generic-lens or generic-optics with 'flickerAdaptiveQuantization' instead." #-}

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrSettings :: Lens.Lens' H264Settings (Lude.Maybe H264QvbrSettings)
hQvbrSettings = Lens.lens (qvbrSettings :: H264Settings -> Lude.Maybe H264QvbrSettings) (\s a -> s {qvbrSettings = a} :: H264Settings)
{-# DEPRECATED hQvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead." #-}

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSoftness :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hSoftness = Lens.lens (softness :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {softness = a} :: H264Settings)
{-# DEPRECATED hSoftness "Use generic-lens or generic-optics with 'softness' instead." #-}

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCodecProfile :: Lens.Lens' H264Settings (Lude.Maybe H264CodecProfile)
hCodecProfile = Lens.lens (codecProfile :: H264Settings -> Lude.Maybe H264CodecProfile) (\s a -> s {codecProfile = a} :: H264Settings)
{-# DEPRECATED hCodecProfile "Use generic-lens or generic-optics with 'codecProfile' instead." #-}

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBitrate :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hBitrate = Lens.lens (bitrate :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: H264Settings)
{-# DEPRECATED hBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateDenominator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hFramerateDenominator = Lens.lens (framerateDenominator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: H264Settings)
{-# DEPRECATED hFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateConversionAlgorithm :: Lens.Lens' H264Settings (Lude.Maybe H264FramerateConversionAlgorithm)
hFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: H264Settings -> Lude.Maybe H264FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: H264Settings)
{-# DEPRECATED hFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
--
-- /Note:/ Consider using 'codecLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCodecLevel :: Lens.Lens' H264Settings (Lude.Maybe H264CodecLevel)
hCodecLevel = Lens.lens (codecLevel :: H264Settings -> Lude.Maybe H264CodecLevel) (\s a -> s {codecLevel = a} :: H264Settings)
{-# DEPRECATED hCodecLevel "Use generic-lens or generic-optics with 'codecLevel' instead." #-}

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
--
-- /Note:/ Consider using 'entropyEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hEntropyEncoding :: Lens.Lens' H264Settings (Lude.Maybe H264EntropyEncoding)
hEntropyEncoding = Lens.lens (entropyEncoding :: H264Settings -> Lude.Maybe H264EntropyEncoding) (\s a -> s {entropyEncoding = a} :: H264Settings)
{-# DEPRECATED hEntropyEncoding "Use generic-lens or generic-optics with 'entropyEncoding' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateControl :: Lens.Lens' H264Settings (Lude.Maybe H264FramerateControl)
hFramerateControl = Lens.lens (framerateControl :: H264Settings -> Lude.Maybe H264FramerateControl) (\s a -> s {framerateControl = a} :: H264Settings)
{-# DEPRECATED hFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAdaptiveQuantization :: Lens.Lens' H264Settings (Lude.Maybe H264AdaptiveQuantization)
hAdaptiveQuantization = Lens.lens (adaptiveQuantization :: H264Settings -> Lude.Maybe H264AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: H264Settings)
{-# DEPRECATED hAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateNumerator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hFramerateNumerator = Lens.lens (framerateNumerator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: H264Settings)
{-# DEPRECATED hFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopBReference :: Lens.Lens' H264Settings (Lude.Maybe H264GopBReference)
hGopBReference = Lens.lens (gopBReference :: H264Settings -> Lude.Maybe H264GopBReference) (\s a -> s {gopBReference = a} :: H264Settings)
{-# DEPRECATED hGopBReference "Use generic-lens or generic-optics with 'gopBReference' instead." #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMaxBitrate :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hMaxBitrate = Lens.lens (maxBitrate :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: H264Settings)
{-# DEPRECATED hMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Produces a bitstream compliant with SMPTE RP-2027.
--
-- /Note:/ Consider using 'syntax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSyntax :: Lens.Lens' H264Settings (Lude.Maybe H264Syntax)
hSyntax = Lens.lens (syntax :: H264Settings -> Lude.Maybe H264Syntax) (\s a -> s {syntax = a} :: H264Settings)
{-# DEPRECATED hSyntax "Use generic-lens or generic-optics with 'syntax' instead." #-}

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
--
-- /Note:/ Consider using 'fieldEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFieldEncoding :: Lens.Lens' H264Settings (Lude.Maybe H264FieldEncoding)
hFieldEncoding = Lens.lens (fieldEncoding :: H264Settings -> Lude.Maybe H264FieldEncoding) (\s a -> s {fieldEncoding = a} :: H264Settings)
{-# DEPRECATED hFieldEncoding "Use generic-lens or generic-optics with 'fieldEncoding' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopClosedCadence :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hGopClosedCadence = Lens.lens (gopClosedCadence :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: H264Settings)
{-# DEPRECATED hGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParDenominator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hParDenominator = Lens.lens (parDenominator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: H264Settings)
{-# DEPRECATED hParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

-- | Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSpatialAdaptiveQuantization :: Lens.Lens' H264Settings (Lude.Maybe H264SpatialAdaptiveQuantization)
hSpatialAdaptiveQuantization = Lens.lens (spatialAdaptiveQuantization :: H264Settings -> Lude.Maybe H264SpatialAdaptiveQuantization) (\s a -> s {spatialAdaptiveQuantization = a} :: H264Settings)
{-# DEPRECATED hSpatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead." #-}

instance Lude.FromJSON H264Settings where
  parseJSON =
    Lude.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Lude.<$> (x Lude..:? "unregisteredSeiTimecode")
            Lude.<*> (x Lude..:? "qualityTuningLevel")
            Lude.<*> (x Lude..:? "temporalAdaptiveQuantization")
            Lude.<*> (x Lude..:? "sceneChangeDetect")
            Lude.<*> (x Lude..:? "hrdBufferInitialFillPercentage")
            Lude.<*> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "numberBFramesBetweenReferenceFrames")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "hrdBufferSize")
            Lude.<*> (x Lude..:? "slices")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "numberReferenceFrames")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "dynamicSubGop")
            Lude.<*> (x Lude..:? "minIInterval")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "repeatPps")
            Lude.<*> (x Lude..:? "flickerAdaptiveQuantization")
            Lude.<*> (x Lude..:? "qvbrSettings")
            Lude.<*> (x Lude..:? "softness")
            Lude.<*> (x Lude..:? "codecProfile")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "codecLevel")
            Lude.<*> (x Lude..:? "entropyEncoding")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "gopBReference")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "syntax")
            Lude.<*> (x Lude..:? "fieldEncoding")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "parDenominator")
            Lude.<*> (x Lude..:? "spatialAdaptiveQuantization")
      )

instance Lude.ToJSON H264Settings where
  toJSON H264Settings' {..} =
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
            ("slowPal" Lude..=) Lude.<$> slowPal,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Lude..=)
              Lude.<$> numberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("hrdBufferSize" Lude..=) Lude.<$> hrdBufferSize,
            ("slices" Lude..=) Lude.<$> slices,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("numberReferenceFrames" Lude..=) Lude.<$> numberReferenceFrames,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("dynamicSubGop" Lude..=) Lude.<$> dynamicSubGop,
            ("minIInterval" Lude..=) Lude.<$> minIInterval,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("repeatPps" Lude..=) Lude.<$> repeatPps,
            ("flickerAdaptiveQuantization" Lude..=)
              Lude.<$> flickerAdaptiveQuantization,
            ("qvbrSettings" Lude..=) Lude.<$> qvbrSettings,
            ("softness" Lude..=) Lude.<$> softness,
            ("codecProfile" Lude..=) Lude.<$> codecProfile,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("codecLevel" Lude..=) Lude.<$> codecLevel,
            ("entropyEncoding" Lude..=) Lude.<$> entropyEncoding,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("gopBReference" Lude..=) Lude.<$> gopBReference,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("syntax" Lude..=) Lude.<$> syntax,
            ("fieldEncoding" Lude..=) Lude.<$> fieldEncoding,
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("parDenominator" Lude..=) Lude.<$> parDenominator,
            ("spatialAdaptiveQuantization" Lude..=)
              Lude.<$> spatialAdaptiveQuantization
          ]
      )
