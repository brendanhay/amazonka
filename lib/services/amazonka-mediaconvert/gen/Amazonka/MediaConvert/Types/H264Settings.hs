{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.H264AdaptiveQuantization
import Amazonka.MediaConvert.Types.H264CodecLevel
import Amazonka.MediaConvert.Types.H264CodecProfile
import Amazonka.MediaConvert.Types.H264DynamicSubGop
import Amazonka.MediaConvert.Types.H264EntropyEncoding
import Amazonka.MediaConvert.Types.H264FieldEncoding
import Amazonka.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264FramerateControl
import Amazonka.MediaConvert.Types.H264FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.H264GopBReference
import Amazonka.MediaConvert.Types.H264GopSizeUnits
import Amazonka.MediaConvert.Types.H264InterlaceMode
import Amazonka.MediaConvert.Types.H264ParControl
import Amazonka.MediaConvert.Types.H264QualityTuningLevel
import Amazonka.MediaConvert.Types.H264QvbrSettings
import Amazonka.MediaConvert.Types.H264RateControlMode
import Amazonka.MediaConvert.Types.H264RepeatPps
import Amazonka.MediaConvert.Types.H264ScanTypeConversionMode
import Amazonka.MediaConvert.Types.H264SceneChangeDetect
import Amazonka.MediaConvert.Types.H264SlowPal
import Amazonka.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264Syntax
import Amazonka.MediaConvert.Types.H264Telecine
import Amazonka.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264UnregisteredSeiTimecode
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
--
-- /See:/ 'newH264Settings' smart constructor.
data H264Settings = H264Settings'
  { -- | Keep the default value, Auto (AUTO), for this setting to have
    -- MediaConvert automatically apply the best types of quantization for your
    -- video content. When you want to apply your quantization settings
    -- manually, you must set H264AdaptiveQuantization to a value other than
    -- Auto (AUTO). Use this setting to specify the strength of any adaptive
    -- quantization filters that you enable. If you don\'t want MediaConvert to
    -- do any adaptive quantization in this transcode, set Adaptive
    -- quantization (H264AdaptiveQuantization) to Off (OFF). Related settings:
    -- The value that you choose here applies to the following settings:
    -- H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and
    -- H264TemporalAdaptiveQuantization.
    adaptiveQuantization :: Prelude.Maybe H264AdaptiveQuantization,
    -- | Specify the average bitrate in bits per second. Required for VBR and
    -- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
    -- the nearest multiple of 1000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify an H.264 level that is consistent with your output video
    -- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
    codecLevel :: Prelude.Maybe H264CodecLevel,
    -- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
    -- the AVC-I License.
    codecProfile :: Prelude.Maybe H264CodecProfile,
    -- | Choose Adaptive to improve subjective video quality for high-motion
    -- content. This will cause the service to use fewer B-frames (which infer
    -- information based on other frames) for high-motion portions of the video
    -- and more B-frames for low-motion portions. The maximum number of
    -- B-frames is limited by the value you provide for the setting B frames
    -- between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Prelude.Maybe H264DynamicSubGop,
    -- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
    -- CAVLC.
    entropyEncoding :: Prelude.Maybe H264EntropyEncoding,
    -- | The video encoding method for your MPEG-4 AVC output. Keep the default
    -- value, PAFF, to have MediaConvert use PAFF encoding for interlaced
    -- outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and
    -- create separate interlaced fields. Choose MBAFF to disable PAFF and have
    -- MediaConvert use MBAFF encoding for interlaced outputs.
    fieldEncoding :: Prelude.Maybe H264FieldEncoding,
    -- | Only use this setting when you change the default value, AUTO, for the
    -- setting H264AdaptiveQuantization. When you keep all defaults, excluding
    -- H264AdaptiveQuantization and all other adaptive quantization from your
    -- JSON job specification, MediaConvert automatically applies the best
    -- types of quantization for your video content. When you set
    -- H264AdaptiveQuantization to a value other than AUTO, the default value
    -- for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this
    -- value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as
    -- a visual flicker that can arise when the encoder saves bits by copying
    -- some macroblocks many times from frame to frame, and then refreshes them
    -- at the I-frame. When you enable this setting, the encoder updates these
    -- macroblocks slightly more often to smooth out the flicker. To manually
    -- enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive
    -- quantization (H264AdaptiveQuantization) to a value other than AUTO.
    flickerAdaptiveQuantization :: Prelude.Maybe H264FlickerAdaptiveQuantization,
    -- | If you are using the console, use the Framerate setting to specify the
    -- frame rate for this output. If you want to keep the same frame rate as
    -- the input video, choose Follow source. If you want to do frame rate
    -- conversion, choose a frame rate from the dropdown list or choose Custom.
    -- The framerates shown in the dropdown list are decimal approximations of
    -- fractions. If you choose Custom, specify your frame rate as a fraction.
    -- If you are creating your transcoding job specification as a JSON file
    -- without the console, use FramerateControl to specify which value the
    -- service uses for the frame rate for this output. Choose
    -- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
    -- from the input. Choose SPECIFIED if you want the service to use the
    -- frame rate you specify in the settings FramerateNumerator and
    -- FramerateDenominator.
    framerateControl :: Prelude.Maybe H264FramerateControl,
    -- | Choose the method that you want MediaConvert to use when increasing or
    -- decreasing the frame rate. We recommend using drop duplicate
    -- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
    -- 30 fps. For numerically complex conversions, you can use interpolate
    -- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
    -- might introduce undesirable video artifacts. For complex frame rate
    -- conversions, especially if your source video has already been converted
    -- from its original cadence, use FrameFormer (FRAMEFORMER) to do
    -- motion-compensated interpolation. FrameFormer chooses the best
    -- conversion method frame by frame. Note that using FrameFormer increases
    -- the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Prelude.Maybe H264FramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | If enable, use reference B frames for GOP structures that have B frames
    -- > 1.
    gopBReference :: Prelude.Maybe H264GopBReference,
    -- | Specify the relative frequency of open to closed GOPs in this output.
    -- For example, if you want to allow four open GOPs and then require a
    -- closed GOP, set this value to 5. We recommend that you have the
    -- transcoder automatically choose this value for you based on
    -- characteristics of your input video. To enable this automatic behavior,
    -- keep the default value by leaving this setting out of your JSON job
    -- specification. In the console, do this by keeping the default empty
    -- value. If you do explicitly specify a value, for segmented outputs,
    -- don\'t set this value to 0.
    gopClosedCadence :: Prelude.Maybe Prelude.Natural,
    -- | Use this setting only when you set GOP mode control (GopSizeUnits) to
    -- Specified, frames (FRAMES) or Specified, seconds (SECONDS). Specify the
    -- GOP length using a whole number of frames or a decimal value of seconds.
    -- MediaConvert will interpret this value as frames or seconds depending on
    -- the value you choose for GOP mode control (GopSizeUnits). If you want to
    -- allow MediaConvert to automatically determine GOP size, leave GOP size
    -- blank and set GOP mode control to Auto (AUTO). If your output group
    -- specifies HLS, DASH, or CMAF, leave GOP size blank and set GOP mode
    -- control to Auto in each output in your output group.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Specify how the transcoder determines GOP size for this output. We
    -- recommend that you have the transcoder automatically choose this value
    -- for you based on characteristics of your input video. To enable this
    -- automatic behavior, choose Auto (AUTO) and and leave GOP size (GopSize)
    -- blank. By default, if you don\'t specify GOP mode control
    -- (GopSizeUnits), MediaConvert will use automatic behavior. If your output
    -- group specifies HLS, DASH, or CMAF, set GOP mode control to Auto and
    -- leave GOP size blank in each output in your output group. To explicitly
    -- specify the GOP length, choose Specified, frames (FRAMES) or Specified,
    -- seconds (SECONDS) and then provide the GOP length in the related setting
    -- GOP size (GopSize).
    gopSizeUnits :: Prelude.Maybe H264GopSizeUnits,
    -- | If your downstream systems have strict buffer requirements: Specify the
    -- minimum percentage of the HRD buffer that\'s available at the end of
    -- each encoded video segment. For the best video quality: Set to 0 or
    -- leave blank to automatically determine the final buffer fill percentage.
    hrdBufferFinalFillPercentage :: Prelude.Maybe Prelude.Natural,
    -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    hrdBufferInitialFillPercentage :: Prelude.Maybe Prelude.Natural,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five
    -- megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
    -- | Choose the scan line type for the output. Keep the default value,
    -- Progressive (PROGRESSIVE) to create a progressive output, regardless of
    -- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
    -- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
    -- the same field polarity throughout. Use Follow, default top
    -- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
    -- produce outputs with the same field polarity as the source. For jobs
    -- that have multiple inputs, the output field polarity might change over
    -- the course of the output. Follow behavior depends on the input scan
    -- type. If the source is interlaced, the output will be interlaced with
    -- the same polarity as the source. If the source is progressive, the
    -- output will be interlaced with top field bottom field first, depending
    -- on which of the Follow options you choose.
    interlaceMode :: Prelude.Maybe H264InterlaceMode,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Use this setting only when you also enable Scene change detection
    -- (SceneChangeDetect). This setting determines how the encoder manages the
    -- spacing between I-frames that it inserts as part of the I-frame cadence
    -- and the I-frames that it inserts for Scene change detection. We
    -- recommend that you have the transcoder automatically choose this value
    -- for you based on characteristics of your input video. To enable this
    -- automatic behavior, keep the default value by leaving this setting out
    -- of your JSON job specification. In the console, do this by keeping the
    -- default empty value. When you explicitly specify a value for this
    -- setting, the encoder determines whether to skip a cadence-driven I-frame
    -- by the value you set. For example, if you set Min I interval
    -- (minIInterval) to 5 and a cadence-driven I-frame would fall within 5
    -- frames of a scene-change I-frame, then the encoder skips the
    -- cadence-driven I-frame. In this way, one GOP is shrunk slightly and one
    -- GOP is stretched slightly. When the cadence-driven I-frames are farther
    -- from the scene-change I-frame than the value you set, then the encoder
    -- leaves all I-frames in place and the GOPs surrounding the scene change
    -- are smaller than the usual cadence GOPs.
    minIInterval :: Prelude.Maybe Prelude.Natural,
    -- | This setting to determines the number of B-frames that MediaConvert puts
    -- between reference frames in this output. We recommend that you use
    -- automatic behavior to allow the transcoder to choose the best value
    -- based on characteristics of your input video. In the console, choose
    -- AUTO to select this automatic behavior. When you manually edit your JSON
    -- job specification, leave this setting out to choose automatic behavior.
    -- When you want to specify this number explicitly, choose a whole number
    -- from 0 through 7.
    numberBFramesBetweenReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Number of reference frames to use. The encoder may use more than
    -- requested if using B-frames and\/or interlaced encoding.
    numberReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify how the service determines the pixel aspect ratio
    -- (PAR) for this output. The default behavior, Follow source
    -- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
    -- output. To specify a different PAR in the console, choose any value
    -- other than Follow source. To specify a different PAR by editing the JSON
    -- job specification, choose SPECIFIED. When you choose SPECIFIED for this
    -- setting, you must also specify values for the parNumerator and
    -- parDenominator settings.
    parControl :: Prelude.Maybe H264ParControl,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parDenominator is 33.
    parDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe H264QualityTuningLevel,
    -- | Settings for quality-defined variable bitrate encoding with the H.265
    -- codec. Use these settings only when you set QVBR for Rate control mode
    -- (RateControlMode).
    qvbrSettings :: Prelude.Maybe H264QvbrSettings,
    -- | Use this setting to specify whether this output has a variable bitrate
    -- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
    -- (QVBR).
    rateControlMode :: Prelude.Maybe H264RateControlMode,
    -- | Places a PPS header on each encoded picture, even if repeated.
    repeatPps :: Prelude.Maybe H264RepeatPps,
    -- | Use this setting for interlaced outputs, when your output frame rate is
    -- half of your input frame rate. In this situation, choose Optimized
    -- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
    -- output. In this case, each progressive frame from the input corresponds
    -- to an interlaced field in the output. Keep the default value, Basic
    -- interlacing (INTERLACED), for all other output frame rates. With basic
    -- interlacing, MediaConvert performs any frame rate conversion first and
    -- then interlaces the frames. When you choose Optimized interlacing and
    -- you set your output frame rate to a value that isn\'t suitable for
    -- optimized interlacing, MediaConvert automatically falls back to basic
    -- interlacing. Required settings: To use optimized interlacing, you must
    -- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
    -- optimized interlacing for hard telecine outputs. You must also set
    -- Interlace mode (interlaceMode) to a value other than Progressive
    -- (PROGRESSIVE).
    scanTypeConversionMode :: Prelude.Maybe H264ScanTypeConversionMode,
    -- | Enable this setting to insert I-frames at scene changes that the service
    -- automatically detects. This improves video quality and is enabled by
    -- default. If this output uses QVBR, choose Transition detection
    -- (TRANSITION_DETECTION) for further video quality improvement. For more
    -- information about QVBR, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
    sceneChangeDetect :: Prelude.Maybe H264SceneChangeDetect,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output. When you
    -- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
    -- resamples your audio to keep it synchronized with the video. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Required settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe H264SlowPal,
    -- | Ignore this setting unless you need to comply with a specification that
    -- requires a specific value. If you don\'t have a specification
    -- requirement, we recommend that you adjust the softness of your output by
    -- using a lower value for the setting Sharpness (sharpness) or by enabling
    -- a noise reducer filter (noiseReducerFilter). The Softness (softness)
    -- setting specifies the quantization matrices that the encoder uses. Keep
    -- the default value, 0, for flat quantization. Choose the value 1 or 16 to
    -- use the default JVT softening quantization matricies from the H.264
    -- specification. Choose a value from 17 to 128 to use planar
    -- interpolation. Increasing values from 17 to 128 result in increasing
    -- reduction of high-frequency data. The value 128 results in the softest
    -- video.
    softness :: Prelude.Maybe Prelude.Natural,
    -- | Only use this setting when you change the default value, Auto (AUTO),
    -- for the setting H264AdaptiveQuantization. When you keep all defaults,
    -- excluding H264AdaptiveQuantization and all other adaptive quantization
    -- from your JSON job specification, MediaConvert automatically applies the
    -- best types of quantization for your video content. When you set
    -- H264AdaptiveQuantization to a value other than AUTO, the default value
    -- for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this
    -- default value to adjust quantization within each frame based on spatial
    -- variation of content complexity. When you enable this feature, the
    -- encoder uses fewer bits on areas that can sustain more distortion with
    -- no noticeable visual degradation and uses more bits on areas where any
    -- small distortion will be noticeable. For example, complex textured
    -- blocks are encoded with fewer bits and smooth textured blocks are
    -- encoded with more bits. Enabling this feature will almost always improve
    -- your video quality. Note, though, that this feature doesn\'t take into
    -- account where the viewer\'s attention is likely to be. If viewers are
    -- likely to be focusing their attention on a part of the screen with a lot
    -- of complex texture, you might choose to set
    -- H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting:
    -- When you enable spatial adaptive quantization, set the value for
    -- Adaptive quantization (H264AdaptiveQuantization) depending on your
    -- content. For homogeneous content, such as cartoons and video games, set
    -- it to Low. For content with a wider variety of textures, set it to High
    -- or Higher. To manually enable or disable
    -- H264SpatialAdaptiveQuantization, you must set Adaptive quantization
    -- (H264AdaptiveQuantization) to a value other than AUTO.
    spatialAdaptiveQuantization :: Prelude.Maybe H264SpatialAdaptiveQuantization,
    -- | Produces a bitstream compliant with SMPTE RP-2027.
    syntax :: Prelude.Maybe H264Syntax,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to
    -- 29.97 fps, and your output scan type is interlaced, you can optionally
    -- enable hard or soft telecine to create a smoother picture. Hard telecine
    -- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
    -- with a 23.976 output that signals to the video player device to do the
    -- conversion during play back. When you keep the default value, None
    -- (NONE), MediaConvert does a standard frame rate conversion to 29.97
    -- without doing anything with the field polarity to create a smoother
    -- picture.
    telecine :: Prelude.Maybe H264Telecine,
    -- | Only use this setting when you change the default value, AUTO, for the
    -- setting H264AdaptiveQuantization. When you keep all defaults, excluding
    -- H264AdaptiveQuantization and all other adaptive quantization from your
    -- JSON job specification, MediaConvert automatically applies the best
    -- types of quantization for your video content. When you set
    -- H264AdaptiveQuantization to a value other than AUTO, the default value
    -- for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this
    -- default value to adjust quantization within each frame based on temporal
    -- variation of content complexity. When you enable this feature, the
    -- encoder uses fewer bits on areas of the frame that aren\'t moving and
    -- uses more bits on complex objects with sharp edges that move a lot. For
    -- example, this feature improves the readability of text tickers on
    -- newscasts and scoreboards on sports matches. Enabling this feature will
    -- almost always improve your video quality. Note, though, that this
    -- feature doesn\'t take into account where the viewer\'s attention is
    -- likely to be. If viewers are likely to be focusing their attention on a
    -- part of the screen that doesn\'t have moving objects with sharp edges,
    -- such as sports athletes\' faces, you might choose to set
    -- H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related
    -- setting: When you enable temporal quantization, adjust the strength of
    -- the filter with the setting Adaptive quantization
    -- (adaptiveQuantization). To manually enable or disable
    -- H264TemporalAdaptiveQuantization, you must set Adaptive quantization
    -- (H264AdaptiveQuantization) to a value other than AUTO.
    temporalAdaptiveQuantization :: Prelude.Maybe H264TemporalAdaptiveQuantization,
    -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
    -- message.
    unregisteredSeiTimecode :: Prelude.Maybe H264UnregisteredSeiTimecode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H264Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adaptiveQuantization', 'h264Settings_adaptiveQuantization' - Keep the default value, Auto (AUTO), for this setting to have
-- MediaConvert automatically apply the best types of quantization for your
-- video content. When you want to apply your quantization settings
-- manually, you must set H264AdaptiveQuantization to a value other than
-- Auto (AUTO). Use this setting to specify the strength of any adaptive
-- quantization filters that you enable. If you don\'t want MediaConvert to
-- do any adaptive quantization in this transcode, set Adaptive
-- quantization (H264AdaptiveQuantization) to Off (OFF). Related settings:
-- The value that you choose here applies to the following settings:
-- H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and
-- H264TemporalAdaptiveQuantization.
--
-- 'bitrate', 'h264Settings_bitrate' - Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
--
-- 'codecLevel', 'h264Settings_codecLevel' - Specify an H.264 level that is consistent with your output video
-- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
--
-- 'codecProfile', 'h264Settings_codecProfile' - H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
-- the AVC-I License.
--
-- 'dynamicSubGop', 'h264Settings_dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
--
-- 'entropyEncoding', 'h264Settings_entropyEncoding' - Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
--
-- 'fieldEncoding', 'h264Settings_fieldEncoding' - The video encoding method for your MPEG-4 AVC output. Keep the default
-- value, PAFF, to have MediaConvert use PAFF encoding for interlaced
-- outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and
-- create separate interlaced fields. Choose MBAFF to disable PAFF and have
-- MediaConvert use MBAFF encoding for interlaced outputs.
--
-- 'flickerAdaptiveQuantization', 'h264Settings_flickerAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the
-- setting H264AdaptiveQuantization. When you keep all defaults, excluding
-- H264AdaptiveQuantization and all other adaptive quantization from your
-- JSON job specification, MediaConvert automatically applies the best
-- types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this
-- value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as
-- a visual flicker that can arise when the encoder saves bits by copying
-- some macroblocks many times from frame to frame, and then refreshes them
-- at the I-frame. When you enable this setting, the encoder updates these
-- macroblocks slightly more often to smooth out the flicker. To manually
-- enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive
-- quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- 'framerateControl', 'h264Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
--
-- 'framerateConversionAlgorithm', 'h264Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
--
-- 'framerateDenominator', 'h264Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateNumerator', 'h264Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'gopBReference', 'h264Settings_gopBReference' - If enable, use reference B frames for GOP structures that have B frames
-- > 1.
--
-- 'gopClosedCadence', 'h264Settings_gopClosedCadence' - Specify the relative frequency of open to closed GOPs in this output.
-- For example, if you want to allow four open GOPs and then require a
-- closed GOP, set this value to 5. We recommend that you have the
-- transcoder automatically choose this value for you based on
-- characteristics of your input video. To enable this automatic behavior,
-- keep the default value by leaving this setting out of your JSON job
-- specification. In the console, do this by keeping the default empty
-- value. If you do explicitly specify a value, for segmented outputs,
-- don\'t set this value to 0.
--
-- 'gopSize', 'h264Settings_gopSize' - Use this setting only when you set GOP mode control (GopSizeUnits) to
-- Specified, frames (FRAMES) or Specified, seconds (SECONDS). Specify the
-- GOP length using a whole number of frames or a decimal value of seconds.
-- MediaConvert will interpret this value as frames or seconds depending on
-- the value you choose for GOP mode control (GopSizeUnits). If you want to
-- allow MediaConvert to automatically determine GOP size, leave GOP size
-- blank and set GOP mode control to Auto (AUTO). If your output group
-- specifies HLS, DASH, or CMAF, leave GOP size blank and set GOP mode
-- control to Auto in each output in your output group.
--
-- 'gopSizeUnits', 'h264Settings_gopSizeUnits' - Specify how the transcoder determines GOP size for this output. We
-- recommend that you have the transcoder automatically choose this value
-- for you based on characteristics of your input video. To enable this
-- automatic behavior, choose Auto (AUTO) and and leave GOP size (GopSize)
-- blank. By default, if you don\'t specify GOP mode control
-- (GopSizeUnits), MediaConvert will use automatic behavior. If your output
-- group specifies HLS, DASH, or CMAF, set GOP mode control to Auto and
-- leave GOP size blank in each output in your output group. To explicitly
-- specify the GOP length, choose Specified, frames (FRAMES) or Specified,
-- seconds (SECONDS) and then provide the GOP length in the related setting
-- GOP size (GopSize).
--
-- 'hrdBufferFinalFillPercentage', 'h264Settings_hrdBufferFinalFillPercentage' - If your downstream systems have strict buffer requirements: Specify the
-- minimum percentage of the HRD buffer that\'s available at the end of
-- each encoded video segment. For the best video quality: Set to 0 or
-- leave blank to automatically determine the final buffer fill percentage.
--
-- 'hrdBufferInitialFillPercentage', 'h264Settings_hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'hrdBufferSize', 'h264Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
--
-- 'interlaceMode', 'h264Settings_interlaceMode' - Choose the scan line type for the output. Keep the default value,
-- Progressive (PROGRESSIVE) to create a progressive output, regardless of
-- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
-- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
-- the same field polarity throughout. Use Follow, default top
-- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
-- produce outputs with the same field polarity as the source. For jobs
-- that have multiple inputs, the output field polarity might change over
-- the course of the output. Follow behavior depends on the input scan
-- type. If the source is interlaced, the output will be interlaced with
-- the same polarity as the source. If the source is progressive, the
-- output will be interlaced with top field bottom field first, depending
-- on which of the Follow options you choose.
--
-- 'maxBitrate', 'h264Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'minIInterval', 'h264Settings_minIInterval' - Use this setting only when you also enable Scene change detection
-- (SceneChangeDetect). This setting determines how the encoder manages the
-- spacing between I-frames that it inserts as part of the I-frame cadence
-- and the I-frames that it inserts for Scene change detection. We
-- recommend that you have the transcoder automatically choose this value
-- for you based on characteristics of your input video. To enable this
-- automatic behavior, keep the default value by leaving this setting out
-- of your JSON job specification. In the console, do this by keeping the
-- default empty value. When you explicitly specify a value for this
-- setting, the encoder determines whether to skip a cadence-driven I-frame
-- by the value you set. For example, if you set Min I interval
-- (minIInterval) to 5 and a cadence-driven I-frame would fall within 5
-- frames of a scene-change I-frame, then the encoder skips the
-- cadence-driven I-frame. In this way, one GOP is shrunk slightly and one
-- GOP is stretched slightly. When the cadence-driven I-frames are farther
-- from the scene-change I-frame than the value you set, then the encoder
-- leaves all I-frames in place and the GOPs surrounding the scene change
-- are smaller than the usual cadence GOPs.
--
-- 'numberBFramesBetweenReferenceFrames', 'h264Settings_numberBFramesBetweenReferenceFrames' - This setting to determines the number of B-frames that MediaConvert puts
-- between reference frames in this output. We recommend that you use
-- automatic behavior to allow the transcoder to choose the best value
-- based on characteristics of your input video. In the console, choose
-- AUTO to select this automatic behavior. When you manually edit your JSON
-- job specification, leave this setting out to choose automatic behavior.
-- When you want to specify this number explicitly, choose a whole number
-- from 0 through 7.
--
-- 'numberReferenceFrames', 'h264Settings_numberReferenceFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
--
-- 'parControl', 'h264Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
--
-- 'parDenominator', 'h264Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'parNumerator', 'h264Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'qualityTuningLevel', 'h264Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'qvbrSettings', 'h264Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- 'rateControlMode', 'h264Settings_rateControlMode' - Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
--
-- 'repeatPps', 'h264Settings_repeatPps' - Places a PPS header on each encoded picture, even if repeated.
--
-- 'scanTypeConversionMode', 'h264Settings_scanTypeConversionMode' - Use this setting for interlaced outputs, when your output frame rate is
-- half of your input frame rate. In this situation, choose Optimized
-- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
-- output. In this case, each progressive frame from the input corresponds
-- to an interlaced field in the output. Keep the default value, Basic
-- interlacing (INTERLACED), for all other output frame rates. With basic
-- interlacing, MediaConvert performs any frame rate conversion first and
-- then interlaces the frames. When you choose Optimized interlacing and
-- you set your output frame rate to a value that isn\'t suitable for
-- optimized interlacing, MediaConvert automatically falls back to basic
-- interlacing. Required settings: To use optimized interlacing, you must
-- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
-- optimized interlacing for hard telecine outputs. You must also set
-- Interlace mode (interlaceMode) to a value other than Progressive
-- (PROGRESSIVE).
--
-- 'sceneChangeDetect', 'h264Settings_sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
--
-- 'slices', 'h264Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
--
-- 'slowPal', 'h264Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'softness', 'h264Settings_softness' - Ignore this setting unless you need to comply with a specification that
-- requires a specific value. If you don\'t have a specification
-- requirement, we recommend that you adjust the softness of your output by
-- using a lower value for the setting Sharpness (sharpness) or by enabling
-- a noise reducer filter (noiseReducerFilter). The Softness (softness)
-- setting specifies the quantization matrices that the encoder uses. Keep
-- the default value, 0, for flat quantization. Choose the value 1 or 16 to
-- use the default JVT softening quantization matricies from the H.264
-- specification. Choose a value from 17 to 128 to use planar
-- interpolation. Increasing values from 17 to 128 result in increasing
-- reduction of high-frequency data. The value 128 results in the softest
-- video.
--
-- 'spatialAdaptiveQuantization', 'h264Settings_spatialAdaptiveQuantization' - Only use this setting when you change the default value, Auto (AUTO),
-- for the setting H264AdaptiveQuantization. When you keep all defaults,
-- excluding H264AdaptiveQuantization and all other adaptive quantization
-- from your JSON job specification, MediaConvert automatically applies the
-- best types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this
-- default value to adjust quantization within each frame based on spatial
-- variation of content complexity. When you enable this feature, the
-- encoder uses fewer bits on areas that can sustain more distortion with
-- no noticeable visual degradation and uses more bits on areas where any
-- small distortion will be noticeable. For example, complex textured
-- blocks are encoded with fewer bits and smooth textured blocks are
-- encoded with more bits. Enabling this feature will almost always improve
-- your video quality. Note, though, that this feature doesn\'t take into
-- account where the viewer\'s attention is likely to be. If viewers are
-- likely to be focusing their attention on a part of the screen with a lot
-- of complex texture, you might choose to set
-- H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting:
-- When you enable spatial adaptive quantization, set the value for
-- Adaptive quantization (H264AdaptiveQuantization) depending on your
-- content. For homogeneous content, such as cartoons and video games, set
-- it to Low. For content with a wider variety of textures, set it to High
-- or Higher. To manually enable or disable
-- H264SpatialAdaptiveQuantization, you must set Adaptive quantization
-- (H264AdaptiveQuantization) to a value other than AUTO.
--
-- 'syntax', 'h264Settings_syntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- 'telecine', 'h264Settings_telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
--
-- 'temporalAdaptiveQuantization', 'h264Settings_temporalAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the
-- setting H264AdaptiveQuantization. When you keep all defaults, excluding
-- H264AdaptiveQuantization and all other adaptive quantization from your
-- JSON job specification, MediaConvert automatically applies the best
-- types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this
-- default value to adjust quantization within each frame based on temporal
-- variation of content complexity. When you enable this feature, the
-- encoder uses fewer bits on areas of the frame that aren\'t moving and
-- uses more bits on complex objects with sharp edges that move a lot. For
-- example, this feature improves the readability of text tickers on
-- newscasts and scoreboards on sports matches. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen that doesn\'t have moving objects with sharp edges,
-- such as sports athletes\' faces, you might choose to set
-- H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related
-- setting: When you enable temporal quantization, adjust the strength of
-- the filter with the setting Adaptive quantization
-- (adaptiveQuantization). To manually enable or disable
-- H264TemporalAdaptiveQuantization, you must set Adaptive quantization
-- (H264AdaptiveQuantization) to a value other than AUTO.
--
-- 'unregisteredSeiTimecode', 'h264Settings_unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
newH264Settings ::
  H264Settings
newH264Settings =
  H264Settings'
    { adaptiveQuantization =
        Prelude.Nothing,
      bitrate = Prelude.Nothing,
      codecLevel = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      dynamicSubGop = Prelude.Nothing,
      entropyEncoding = Prelude.Nothing,
      fieldEncoding = Prelude.Nothing,
      flickerAdaptiveQuantization = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      gopSizeUnits = Prelude.Nothing,
      hrdBufferFinalFillPercentage = Prelude.Nothing,
      hrdBufferInitialFillPercentage = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      minIInterval = Prelude.Nothing,
      numberBFramesBetweenReferenceFrames =
        Prelude.Nothing,
      numberReferenceFrames = Prelude.Nothing,
      parControl = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      qvbrSettings = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      repeatPps = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      slices = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      softness = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing,
      syntax = Prelude.Nothing,
      telecine = Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      unregisteredSeiTimecode = Prelude.Nothing
    }

-- | Keep the default value, Auto (AUTO), for this setting to have
-- MediaConvert automatically apply the best types of quantization for your
-- video content. When you want to apply your quantization settings
-- manually, you must set H264AdaptiveQuantization to a value other than
-- Auto (AUTO). Use this setting to specify the strength of any adaptive
-- quantization filters that you enable. If you don\'t want MediaConvert to
-- do any adaptive quantization in this transcode, set Adaptive
-- quantization (H264AdaptiveQuantization) to Off (OFF). Related settings:
-- The value that you choose here applies to the following settings:
-- H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and
-- H264TemporalAdaptiveQuantization.
h264Settings_adaptiveQuantization :: Lens.Lens' H264Settings (Prelude.Maybe H264AdaptiveQuantization)
h264Settings_adaptiveQuantization = Lens.lens (\H264Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@H264Settings' {} a -> s {adaptiveQuantization = a} :: H264Settings)

-- | Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
h264Settings_bitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_bitrate = Lens.lens (\H264Settings' {bitrate} -> bitrate) (\s@H264Settings' {} a -> s {bitrate = a} :: H264Settings)

-- | Specify an H.264 level that is consistent with your output video
-- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
h264Settings_codecLevel :: Lens.Lens' H264Settings (Prelude.Maybe H264CodecLevel)
h264Settings_codecLevel = Lens.lens (\H264Settings' {codecLevel} -> codecLevel) (\s@H264Settings' {} a -> s {codecLevel = a} :: H264Settings)

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
-- the AVC-I License.
h264Settings_codecProfile :: Lens.Lens' H264Settings (Prelude.Maybe H264CodecProfile)
h264Settings_codecProfile = Lens.lens (\H264Settings' {codecProfile} -> codecProfile) (\s@H264Settings' {} a -> s {codecProfile = a} :: H264Settings)

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
h264Settings_dynamicSubGop :: Lens.Lens' H264Settings (Prelude.Maybe H264DynamicSubGop)
h264Settings_dynamicSubGop = Lens.lens (\H264Settings' {dynamicSubGop} -> dynamicSubGop) (\s@H264Settings' {} a -> s {dynamicSubGop = a} :: H264Settings)

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
h264Settings_entropyEncoding :: Lens.Lens' H264Settings (Prelude.Maybe H264EntropyEncoding)
h264Settings_entropyEncoding = Lens.lens (\H264Settings' {entropyEncoding} -> entropyEncoding) (\s@H264Settings' {} a -> s {entropyEncoding = a} :: H264Settings)

-- | The video encoding method for your MPEG-4 AVC output. Keep the default
-- value, PAFF, to have MediaConvert use PAFF encoding for interlaced
-- outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and
-- create separate interlaced fields. Choose MBAFF to disable PAFF and have
-- MediaConvert use MBAFF encoding for interlaced outputs.
h264Settings_fieldEncoding :: Lens.Lens' H264Settings (Prelude.Maybe H264FieldEncoding)
h264Settings_fieldEncoding = Lens.lens (\H264Settings' {fieldEncoding} -> fieldEncoding) (\s@H264Settings' {} a -> s {fieldEncoding = a} :: H264Settings)

-- | Only use this setting when you change the default value, AUTO, for the
-- setting H264AdaptiveQuantization. When you keep all defaults, excluding
-- H264AdaptiveQuantization and all other adaptive quantization from your
-- JSON job specification, MediaConvert automatically applies the best
-- types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this
-- value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as
-- a visual flicker that can arise when the encoder saves bits by copying
-- some macroblocks many times from frame to frame, and then refreshes them
-- at the I-frame. When you enable this setting, the encoder updates these
-- macroblocks slightly more often to smooth out the flicker. To manually
-- enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive
-- quantization (H264AdaptiveQuantization) to a value other than AUTO.
h264Settings_flickerAdaptiveQuantization :: Lens.Lens' H264Settings (Prelude.Maybe H264FlickerAdaptiveQuantization)
h264Settings_flickerAdaptiveQuantization = Lens.lens (\H264Settings' {flickerAdaptiveQuantization} -> flickerAdaptiveQuantization) (\s@H264Settings' {} a -> s {flickerAdaptiveQuantization = a} :: H264Settings)

-- | If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
h264Settings_framerateControl :: Lens.Lens' H264Settings (Prelude.Maybe H264FramerateControl)
h264Settings_framerateControl = Lens.lens (\H264Settings' {framerateControl} -> framerateControl) (\s@H264Settings' {} a -> s {framerateControl = a} :: H264Settings)

-- | Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
h264Settings_framerateConversionAlgorithm :: Lens.Lens' H264Settings (Prelude.Maybe H264FramerateConversionAlgorithm)
h264Settings_framerateConversionAlgorithm = Lens.lens (\H264Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@H264Settings' {} a -> s {framerateConversionAlgorithm = a} :: H264Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h264Settings_framerateDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateDenominator = Lens.lens (\H264Settings' {framerateDenominator} -> framerateDenominator) (\s@H264Settings' {} a -> s {framerateDenominator = a} :: H264Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h264Settings_framerateNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateNumerator = Lens.lens (\H264Settings' {framerateNumerator} -> framerateNumerator) (\s@H264Settings' {} a -> s {framerateNumerator = a} :: H264Settings)

-- | If enable, use reference B frames for GOP structures that have B frames
-- > 1.
h264Settings_gopBReference :: Lens.Lens' H264Settings (Prelude.Maybe H264GopBReference)
h264Settings_gopBReference = Lens.lens (\H264Settings' {gopBReference} -> gopBReference) (\s@H264Settings' {} a -> s {gopBReference = a} :: H264Settings)

-- | Specify the relative frequency of open to closed GOPs in this output.
-- For example, if you want to allow four open GOPs and then require a
-- closed GOP, set this value to 5. We recommend that you have the
-- transcoder automatically choose this value for you based on
-- characteristics of your input video. To enable this automatic behavior,
-- keep the default value by leaving this setting out of your JSON job
-- specification. In the console, do this by keeping the default empty
-- value. If you do explicitly specify a value, for segmented outputs,
-- don\'t set this value to 0.
h264Settings_gopClosedCadence :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_gopClosedCadence = Lens.lens (\H264Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H264Settings' {} a -> s {gopClosedCadence = a} :: H264Settings)

-- | Use this setting only when you set GOP mode control (GopSizeUnits) to
-- Specified, frames (FRAMES) or Specified, seconds (SECONDS). Specify the
-- GOP length using a whole number of frames or a decimal value of seconds.
-- MediaConvert will interpret this value as frames or seconds depending on
-- the value you choose for GOP mode control (GopSizeUnits). If you want to
-- allow MediaConvert to automatically determine GOP size, leave GOP size
-- blank and set GOP mode control to Auto (AUTO). If your output group
-- specifies HLS, DASH, or CMAF, leave GOP size blank and set GOP mode
-- control to Auto in each output in your output group.
h264Settings_gopSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Double)
h264Settings_gopSize = Lens.lens (\H264Settings' {gopSize} -> gopSize) (\s@H264Settings' {} a -> s {gopSize = a} :: H264Settings)

-- | Specify how the transcoder determines GOP size for this output. We
-- recommend that you have the transcoder automatically choose this value
-- for you based on characteristics of your input video. To enable this
-- automatic behavior, choose Auto (AUTO) and and leave GOP size (GopSize)
-- blank. By default, if you don\'t specify GOP mode control
-- (GopSizeUnits), MediaConvert will use automatic behavior. If your output
-- group specifies HLS, DASH, or CMAF, set GOP mode control to Auto and
-- leave GOP size blank in each output in your output group. To explicitly
-- specify the GOP length, choose Specified, frames (FRAMES) or Specified,
-- seconds (SECONDS) and then provide the GOP length in the related setting
-- GOP size (GopSize).
h264Settings_gopSizeUnits :: Lens.Lens' H264Settings (Prelude.Maybe H264GopSizeUnits)
h264Settings_gopSizeUnits = Lens.lens (\H264Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H264Settings' {} a -> s {gopSizeUnits = a} :: H264Settings)

-- | If your downstream systems have strict buffer requirements: Specify the
-- minimum percentage of the HRD buffer that\'s available at the end of
-- each encoded video segment. For the best video quality: Set to 0 or
-- leave blank to automatically determine the final buffer fill percentage.
h264Settings_hrdBufferFinalFillPercentage :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_hrdBufferFinalFillPercentage = Lens.lens (\H264Settings' {hrdBufferFinalFillPercentage} -> hrdBufferFinalFillPercentage) (\s@H264Settings' {} a -> s {hrdBufferFinalFillPercentage = a} :: H264Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h264Settings_hrdBufferInitialFillPercentage :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_hrdBufferInitialFillPercentage = Lens.lens (\H264Settings' {hrdBufferInitialFillPercentage} -> hrdBufferInitialFillPercentage) (\s@H264Settings' {} a -> s {hrdBufferInitialFillPercentage = a} :: H264Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
h264Settings_hrdBufferSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_hrdBufferSize = Lens.lens (\H264Settings' {hrdBufferSize} -> hrdBufferSize) (\s@H264Settings' {} a -> s {hrdBufferSize = a} :: H264Settings)

-- | Choose the scan line type for the output. Keep the default value,
-- Progressive (PROGRESSIVE) to create a progressive output, regardless of
-- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
-- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
-- the same field polarity throughout. Use Follow, default top
-- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
-- produce outputs with the same field polarity as the source. For jobs
-- that have multiple inputs, the output field polarity might change over
-- the course of the output. Follow behavior depends on the input scan
-- type. If the source is interlaced, the output will be interlaced with
-- the same polarity as the source. If the source is progressive, the
-- output will be interlaced with top field bottom field first, depending
-- on which of the Follow options you choose.
h264Settings_interlaceMode :: Lens.Lens' H264Settings (Prelude.Maybe H264InterlaceMode)
h264Settings_interlaceMode = Lens.lens (\H264Settings' {interlaceMode} -> interlaceMode) (\s@H264Settings' {} a -> s {interlaceMode = a} :: H264Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
h264Settings_maxBitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_maxBitrate = Lens.lens (\H264Settings' {maxBitrate} -> maxBitrate) (\s@H264Settings' {} a -> s {maxBitrate = a} :: H264Settings)

-- | Use this setting only when you also enable Scene change detection
-- (SceneChangeDetect). This setting determines how the encoder manages the
-- spacing between I-frames that it inserts as part of the I-frame cadence
-- and the I-frames that it inserts for Scene change detection. We
-- recommend that you have the transcoder automatically choose this value
-- for you based on characteristics of your input video. To enable this
-- automatic behavior, keep the default value by leaving this setting out
-- of your JSON job specification. In the console, do this by keeping the
-- default empty value. When you explicitly specify a value for this
-- setting, the encoder determines whether to skip a cadence-driven I-frame
-- by the value you set. For example, if you set Min I interval
-- (minIInterval) to 5 and a cadence-driven I-frame would fall within 5
-- frames of a scene-change I-frame, then the encoder skips the
-- cadence-driven I-frame. In this way, one GOP is shrunk slightly and one
-- GOP is stretched slightly. When the cadence-driven I-frames are farther
-- from the scene-change I-frame than the value you set, then the encoder
-- leaves all I-frames in place and the GOPs surrounding the scene change
-- are smaller than the usual cadence GOPs.
h264Settings_minIInterval :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_minIInterval = Lens.lens (\H264Settings' {minIInterval} -> minIInterval) (\s@H264Settings' {} a -> s {minIInterval = a} :: H264Settings)

-- | This setting to determines the number of B-frames that MediaConvert puts
-- between reference frames in this output. We recommend that you use
-- automatic behavior to allow the transcoder to choose the best value
-- based on characteristics of your input video. In the console, choose
-- AUTO to select this automatic behavior. When you manually edit your JSON
-- job specification, leave this setting out to choose automatic behavior.
-- When you want to specify this number explicitly, choose a whole number
-- from 0 through 7.
h264Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\H264Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@H264Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: H264Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h264Settings_numberReferenceFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_numberReferenceFrames = Lens.lens (\H264Settings' {numberReferenceFrames} -> numberReferenceFrames) (\s@H264Settings' {} a -> s {numberReferenceFrames = a} :: H264Settings)

-- | Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
h264Settings_parControl :: Lens.Lens' H264Settings (Prelude.Maybe H264ParControl)
h264Settings_parControl = Lens.lens (\H264Settings' {parControl} -> parControl) (\s@H264Settings' {} a -> s {parControl = a} :: H264Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
h264Settings_parDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parDenominator = Lens.lens (\H264Settings' {parDenominator} -> parDenominator) (\s@H264Settings' {} a -> s {parDenominator = a} :: H264Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
h264Settings_parNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parNumerator = Lens.lens (\H264Settings' {parNumerator} -> parNumerator) (\s@H264Settings' {} a -> s {parNumerator = a} :: H264Settings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
h264Settings_qualityTuningLevel :: Lens.Lens' H264Settings (Prelude.Maybe H264QualityTuningLevel)
h264Settings_qualityTuningLevel = Lens.lens (\H264Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@H264Settings' {} a -> s {qualityTuningLevel = a} :: H264Settings)

-- | Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
h264Settings_qvbrSettings :: Lens.Lens' H264Settings (Prelude.Maybe H264QvbrSettings)
h264Settings_qvbrSettings = Lens.lens (\H264Settings' {qvbrSettings} -> qvbrSettings) (\s@H264Settings' {} a -> s {qvbrSettings = a} :: H264Settings)

-- | Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
h264Settings_rateControlMode :: Lens.Lens' H264Settings (Prelude.Maybe H264RateControlMode)
h264Settings_rateControlMode = Lens.lens (\H264Settings' {rateControlMode} -> rateControlMode) (\s@H264Settings' {} a -> s {rateControlMode = a} :: H264Settings)

-- | Places a PPS header on each encoded picture, even if repeated.
h264Settings_repeatPps :: Lens.Lens' H264Settings (Prelude.Maybe H264RepeatPps)
h264Settings_repeatPps = Lens.lens (\H264Settings' {repeatPps} -> repeatPps) (\s@H264Settings' {} a -> s {repeatPps = a} :: H264Settings)

-- | Use this setting for interlaced outputs, when your output frame rate is
-- half of your input frame rate. In this situation, choose Optimized
-- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
-- output. In this case, each progressive frame from the input corresponds
-- to an interlaced field in the output. Keep the default value, Basic
-- interlacing (INTERLACED), for all other output frame rates. With basic
-- interlacing, MediaConvert performs any frame rate conversion first and
-- then interlaces the frames. When you choose Optimized interlacing and
-- you set your output frame rate to a value that isn\'t suitable for
-- optimized interlacing, MediaConvert automatically falls back to basic
-- interlacing. Required settings: To use optimized interlacing, you must
-- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
-- optimized interlacing for hard telecine outputs. You must also set
-- Interlace mode (interlaceMode) to a value other than Progressive
-- (PROGRESSIVE).
h264Settings_scanTypeConversionMode :: Lens.Lens' H264Settings (Prelude.Maybe H264ScanTypeConversionMode)
h264Settings_scanTypeConversionMode = Lens.lens (\H264Settings' {scanTypeConversionMode} -> scanTypeConversionMode) (\s@H264Settings' {} a -> s {scanTypeConversionMode = a} :: H264Settings)

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
h264Settings_sceneChangeDetect :: Lens.Lens' H264Settings (Prelude.Maybe H264SceneChangeDetect)
h264Settings_sceneChangeDetect = Lens.lens (\H264Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H264Settings' {} a -> s {sceneChangeDetect = a} :: H264Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
h264Settings_slices :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_slices = Lens.lens (\H264Settings' {slices} -> slices) (\s@H264Settings' {} a -> s {slices = a} :: H264Settings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
h264Settings_slowPal :: Lens.Lens' H264Settings (Prelude.Maybe H264SlowPal)
h264Settings_slowPal = Lens.lens (\H264Settings' {slowPal} -> slowPal) (\s@H264Settings' {} a -> s {slowPal = a} :: H264Settings)

-- | Ignore this setting unless you need to comply with a specification that
-- requires a specific value. If you don\'t have a specification
-- requirement, we recommend that you adjust the softness of your output by
-- using a lower value for the setting Sharpness (sharpness) or by enabling
-- a noise reducer filter (noiseReducerFilter). The Softness (softness)
-- setting specifies the quantization matrices that the encoder uses. Keep
-- the default value, 0, for flat quantization. Choose the value 1 or 16 to
-- use the default JVT softening quantization matricies from the H.264
-- specification. Choose a value from 17 to 128 to use planar
-- interpolation. Increasing values from 17 to 128 result in increasing
-- reduction of high-frequency data. The value 128 results in the softest
-- video.
h264Settings_softness :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_softness = Lens.lens (\H264Settings' {softness} -> softness) (\s@H264Settings' {} a -> s {softness = a} :: H264Settings)

-- | Only use this setting when you change the default value, Auto (AUTO),
-- for the setting H264AdaptiveQuantization. When you keep all defaults,
-- excluding H264AdaptiveQuantization and all other adaptive quantization
-- from your JSON job specification, MediaConvert automatically applies the
-- best types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this
-- default value to adjust quantization within each frame based on spatial
-- variation of content complexity. When you enable this feature, the
-- encoder uses fewer bits on areas that can sustain more distortion with
-- no noticeable visual degradation and uses more bits on areas where any
-- small distortion will be noticeable. For example, complex textured
-- blocks are encoded with fewer bits and smooth textured blocks are
-- encoded with more bits. Enabling this feature will almost always improve
-- your video quality. Note, though, that this feature doesn\'t take into
-- account where the viewer\'s attention is likely to be. If viewers are
-- likely to be focusing their attention on a part of the screen with a lot
-- of complex texture, you might choose to set
-- H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting:
-- When you enable spatial adaptive quantization, set the value for
-- Adaptive quantization (H264AdaptiveQuantization) depending on your
-- content. For homogeneous content, such as cartoons and video games, set
-- it to Low. For content with a wider variety of textures, set it to High
-- or Higher. To manually enable or disable
-- H264SpatialAdaptiveQuantization, you must set Adaptive quantization
-- (H264AdaptiveQuantization) to a value other than AUTO.
h264Settings_spatialAdaptiveQuantization :: Lens.Lens' H264Settings (Prelude.Maybe H264SpatialAdaptiveQuantization)
h264Settings_spatialAdaptiveQuantization = Lens.lens (\H264Settings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@H264Settings' {} a -> s {spatialAdaptiveQuantization = a} :: H264Settings)

-- | Produces a bitstream compliant with SMPTE RP-2027.
h264Settings_syntax :: Lens.Lens' H264Settings (Prelude.Maybe H264Syntax)
h264Settings_syntax = Lens.lens (\H264Settings' {syntax} -> syntax) (\s@H264Settings' {} a -> s {syntax = a} :: H264Settings)

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
h264Settings_telecine :: Lens.Lens' H264Settings (Prelude.Maybe H264Telecine)
h264Settings_telecine = Lens.lens (\H264Settings' {telecine} -> telecine) (\s@H264Settings' {} a -> s {telecine = a} :: H264Settings)

-- | Only use this setting when you change the default value, AUTO, for the
-- setting H264AdaptiveQuantization. When you keep all defaults, excluding
-- H264AdaptiveQuantization and all other adaptive quantization from your
-- JSON job specification, MediaConvert automatically applies the best
-- types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this
-- default value to adjust quantization within each frame based on temporal
-- variation of content complexity. When you enable this feature, the
-- encoder uses fewer bits on areas of the frame that aren\'t moving and
-- uses more bits on complex objects with sharp edges that move a lot. For
-- example, this feature improves the readability of text tickers on
-- newscasts and scoreboards on sports matches. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen that doesn\'t have moving objects with sharp edges,
-- such as sports athletes\' faces, you might choose to set
-- H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related
-- setting: When you enable temporal quantization, adjust the strength of
-- the filter with the setting Adaptive quantization
-- (adaptiveQuantization). To manually enable or disable
-- H264TemporalAdaptiveQuantization, you must set Adaptive quantization
-- (H264AdaptiveQuantization) to a value other than AUTO.
h264Settings_temporalAdaptiveQuantization :: Lens.Lens' H264Settings (Prelude.Maybe H264TemporalAdaptiveQuantization)
h264Settings_temporalAdaptiveQuantization = Lens.lens (\H264Settings' {temporalAdaptiveQuantization} -> temporalAdaptiveQuantization) (\s@H264Settings' {} a -> s {temporalAdaptiveQuantization = a} :: H264Settings)

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
h264Settings_unregisteredSeiTimecode :: Lens.Lens' H264Settings (Prelude.Maybe H264UnregisteredSeiTimecode)
h264Settings_unregisteredSeiTimecode = Lens.lens (\H264Settings' {unregisteredSeiTimecode} -> unregisteredSeiTimecode) (\s@H264Settings' {} a -> s {unregisteredSeiTimecode = a} :: H264Settings)

instance Data.FromJSON H264Settings where
  parseJSON =
    Data.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Prelude.<$> (x Data..:? "adaptiveQuantization")
            Prelude.<*> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "codecLevel")
            Prelude.<*> (x Data..:? "codecProfile")
            Prelude.<*> (x Data..:? "dynamicSubGop")
            Prelude.<*> (x Data..:? "entropyEncoding")
            Prelude.<*> (x Data..:? "fieldEncoding")
            Prelude.<*> (x Data..:? "flickerAdaptiveQuantization")
            Prelude.<*> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "gopBReference")
            Prelude.<*> (x Data..:? "gopClosedCadence")
            Prelude.<*> (x Data..:? "gopSize")
            Prelude.<*> (x Data..:? "gopSizeUnits")
            Prelude.<*> (x Data..:? "hrdBufferFinalFillPercentage")
            Prelude.<*> (x Data..:? "hrdBufferInitialFillPercentage")
            Prelude.<*> (x Data..:? "hrdBufferSize")
            Prelude.<*> (x Data..:? "interlaceMode")
            Prelude.<*> (x Data..:? "maxBitrate")
            Prelude.<*> (x Data..:? "minIInterval")
            Prelude.<*> (x Data..:? "numberBFramesBetweenReferenceFrames")
            Prelude.<*> (x Data..:? "numberReferenceFrames")
            Prelude.<*> (x Data..:? "parControl")
            Prelude.<*> (x Data..:? "parDenominator")
            Prelude.<*> (x Data..:? "parNumerator")
            Prelude.<*> (x Data..:? "qualityTuningLevel")
            Prelude.<*> (x Data..:? "qvbrSettings")
            Prelude.<*> (x Data..:? "rateControlMode")
            Prelude.<*> (x Data..:? "repeatPps")
            Prelude.<*> (x Data..:? "scanTypeConversionMode")
            Prelude.<*> (x Data..:? "sceneChangeDetect")
            Prelude.<*> (x Data..:? "slices")
            Prelude.<*> (x Data..:? "slowPal")
            Prelude.<*> (x Data..:? "softness")
            Prelude.<*> (x Data..:? "spatialAdaptiveQuantization")
            Prelude.<*> (x Data..:? "syntax")
            Prelude.<*> (x Data..:? "telecine")
            Prelude.<*> (x Data..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Data..:? "unregisteredSeiTimecode")
      )

instance Prelude.Hashable H264Settings where
  hashWithSalt _salt H264Settings' {..} =
    _salt
      `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codecLevel
      `Prelude.hashWithSalt` codecProfile
      `Prelude.hashWithSalt` dynamicSubGop
      `Prelude.hashWithSalt` entropyEncoding
      `Prelude.hashWithSalt` fieldEncoding
      `Prelude.hashWithSalt` flickerAdaptiveQuantization
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` gopBReference
      `Prelude.hashWithSalt` gopClosedCadence
      `Prelude.hashWithSalt` gopSize
      `Prelude.hashWithSalt` gopSizeUnits
      `Prelude.hashWithSalt` hrdBufferFinalFillPercentage
      `Prelude.hashWithSalt` hrdBufferInitialFillPercentage
      `Prelude.hashWithSalt` hrdBufferSize
      `Prelude.hashWithSalt` interlaceMode
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` minIInterval
      `Prelude.hashWithSalt` numberBFramesBetweenReferenceFrames
      `Prelude.hashWithSalt` numberReferenceFrames
      `Prelude.hashWithSalt` parControl
      `Prelude.hashWithSalt` parDenominator
      `Prelude.hashWithSalt` parNumerator
      `Prelude.hashWithSalt` qualityTuningLevel
      `Prelude.hashWithSalt` qvbrSettings
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` repeatPps
      `Prelude.hashWithSalt` scanTypeConversionMode
      `Prelude.hashWithSalt` sceneChangeDetect
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` softness
      `Prelude.hashWithSalt` spatialAdaptiveQuantization
      `Prelude.hashWithSalt` syntax
      `Prelude.hashWithSalt` telecine
      `Prelude.hashWithSalt` temporalAdaptiveQuantization
      `Prelude.hashWithSalt` unregisteredSeiTimecode

instance Prelude.NFData H264Settings where
  rnf H264Settings' {..} =
    Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codecLevel
      `Prelude.seq` Prelude.rnf codecProfile
      `Prelude.seq` Prelude.rnf dynamicSubGop
      `Prelude.seq` Prelude.rnf entropyEncoding
      `Prelude.seq` Prelude.rnf fieldEncoding
      `Prelude.seq` Prelude.rnf flickerAdaptiveQuantization
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf gopBReference
      `Prelude.seq` Prelude.rnf gopClosedCadence
      `Prelude.seq` Prelude.rnf gopSize
      `Prelude.seq` Prelude.rnf gopSizeUnits
      `Prelude.seq` Prelude.rnf
        hrdBufferFinalFillPercentage
      `Prelude.seq` Prelude.rnf
        hrdBufferInitialFillPercentage
      `Prelude.seq` Prelude.rnf hrdBufferSize
      `Prelude.seq` Prelude.rnf interlaceMode
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf minIInterval
      `Prelude.seq` Prelude.rnf
        numberBFramesBetweenReferenceFrames
      `Prelude.seq` Prelude.rnf
        numberReferenceFrames
      `Prelude.seq` Prelude.rnf
        parControl
      `Prelude.seq` Prelude.rnf
        parDenominator
      `Prelude.seq` Prelude.rnf
        parNumerator
      `Prelude.seq` Prelude.rnf
        qualityTuningLevel
      `Prelude.seq` Prelude.rnf
        qvbrSettings
      `Prelude.seq` Prelude.rnf
        rateControlMode
      `Prelude.seq` Prelude.rnf
        repeatPps
      `Prelude.seq` Prelude.rnf
        scanTypeConversionMode
      `Prelude.seq` Prelude.rnf
        sceneChangeDetect
      `Prelude.seq` Prelude.rnf
        slices
      `Prelude.seq` Prelude.rnf
        slowPal
      `Prelude.seq` Prelude.rnf
        softness
      `Prelude.seq` Prelude.rnf
        spatialAdaptiveQuantization
      `Prelude.seq` Prelude.rnf
        syntax
      `Prelude.seq` Prelude.rnf
        telecine
      `Prelude.seq` Prelude.rnf
        temporalAdaptiveQuantization
      `Prelude.seq` Prelude.rnf
        unregisteredSeiTimecode

instance Data.ToJSON H264Settings where
  toJSON H264Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adaptiveQuantization" Data..=)
              Prelude.<$> adaptiveQuantization,
            ("bitrate" Data..=) Prelude.<$> bitrate,
            ("codecLevel" Data..=) Prelude.<$> codecLevel,
            ("codecProfile" Data..=) Prelude.<$> codecProfile,
            ("dynamicSubGop" Data..=) Prelude.<$> dynamicSubGop,
            ("entropyEncoding" Data..=)
              Prelude.<$> entropyEncoding,
            ("fieldEncoding" Data..=) Prelude.<$> fieldEncoding,
            ("flickerAdaptiveQuantization" Data..=)
              Prelude.<$> flickerAdaptiveQuantization,
            ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Data..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("gopBReference" Data..=) Prelude.<$> gopBReference,
            ("gopClosedCadence" Data..=)
              Prelude.<$> gopClosedCadence,
            ("gopSize" Data..=) Prelude.<$> gopSize,
            ("gopSizeUnits" Data..=) Prelude.<$> gopSizeUnits,
            ("hrdBufferFinalFillPercentage" Data..=)
              Prelude.<$> hrdBufferFinalFillPercentage,
            ("hrdBufferInitialFillPercentage" Data..=)
              Prelude.<$> hrdBufferInitialFillPercentage,
            ("hrdBufferSize" Data..=) Prelude.<$> hrdBufferSize,
            ("interlaceMode" Data..=) Prelude.<$> interlaceMode,
            ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("minIInterval" Data..=) Prelude.<$> minIInterval,
            ("numberBFramesBetweenReferenceFrames" Data..=)
              Prelude.<$> numberBFramesBetweenReferenceFrames,
            ("numberReferenceFrames" Data..=)
              Prelude.<$> numberReferenceFrames,
            ("parControl" Data..=) Prelude.<$> parControl,
            ("parDenominator" Data..=)
              Prelude.<$> parDenominator,
            ("parNumerator" Data..=) Prelude.<$> parNumerator,
            ("qualityTuningLevel" Data..=)
              Prelude.<$> qualityTuningLevel,
            ("qvbrSettings" Data..=) Prelude.<$> qvbrSettings,
            ("rateControlMode" Data..=)
              Prelude.<$> rateControlMode,
            ("repeatPps" Data..=) Prelude.<$> repeatPps,
            ("scanTypeConversionMode" Data..=)
              Prelude.<$> scanTypeConversionMode,
            ("sceneChangeDetect" Data..=)
              Prelude.<$> sceneChangeDetect,
            ("slices" Data..=) Prelude.<$> slices,
            ("slowPal" Data..=) Prelude.<$> slowPal,
            ("softness" Data..=) Prelude.<$> softness,
            ("spatialAdaptiveQuantization" Data..=)
              Prelude.<$> spatialAdaptiveQuantization,
            ("syntax" Data..=) Prelude.<$> syntax,
            ("telecine" Data..=) Prelude.<$> telecine,
            ("temporalAdaptiveQuantization" Data..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("unregisteredSeiTimecode" Data..=)
              Prelude.<$> unregisteredSeiTimecode
          ]
      )
