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
-- Module      : Network.AWS.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Settings where

import qualified Network.AWS.Core as Core
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
import Network.AWS.MediaConvert.Types.H264ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
import Network.AWS.MediaConvert.Types.H264SlowPal
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264Syntax
import Network.AWS.MediaConvert.Types.H264Telecine
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
--
-- /See:/ 'newH264Settings' smart constructor.
data H264Settings = H264Settings'
  { -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    hrdBufferInitialFillPercentage :: Prelude.Maybe Prelude.Natural,
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
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe H264QualityTuningLevel,
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
    -- | Places a PPS header on each encoded picture, even if repeated.
    repeatPps :: Prelude.Maybe H264RepeatPps,
    -- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for
    -- interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF
    -- encoding and create separate interlaced fields.
    fieldEncoding :: Prelude.Maybe H264FieldEncoding,
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
    -- | If enable, use reference B frames for GOP structures that have B frames
    -- > 1.
    gopBReference :: Prelude.Maybe H264GopBReference,
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
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Use this setting to specify whether this output has a variable bitrate
    -- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
    -- (QVBR).
    rateControlMode :: Prelude.Maybe H264RateControlMode,
    -- | Number of reference frames to use. The encoder may use more than
    -- requested if using B-frames and\/or interlaced encoding.
    numberReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
    -- CAVLC.
    entropyEncoding :: Prelude.Maybe H264EntropyEncoding,
    -- | Indicates if the GOP Size in H264 is specified in frames or seconds. If
    -- seconds the system will convert the GOP Size into a frame count at run
    -- time.
    gopSizeUnits :: Prelude.Maybe H264GopSizeUnits,
    -- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
    -- the AVC-I License.
    codecProfile :: Prelude.Maybe H264CodecProfile,
    -- | GOP Length (keyframe interval) in frames or seconds. Must be greater
    -- than zero.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
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
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Enable this setting to insert I-frames at scene changes that the service
    -- automatically detects. This improves video quality and is enabled by
    -- default. If this output uses QVBR, choose Transition detection
    -- (TRANSITION_DETECTION) for further video quality improvement. For more
    -- information about QVBR, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
    sceneChangeDetect :: Prelude.Maybe H264SceneChangeDetect,
    -- | Enforces separation between repeated (cadence) I-frames and I-frames
    -- inserted by Scene Change Detection. If a scene change I-frame is within
    -- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
    -- stretched to the scene change I-frame. GOP stretch requires enabling
    -- lookahead as well as setting I-interval. The normal cadence resumes for
    -- the next GOP. This setting is only used when Scene Change Detect is
    -- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Prelude.Maybe Prelude.Natural,
    -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
    -- message.
    unregisteredSeiTimecode :: Prelude.Maybe H264UnregisteredSeiTimecode,
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
    -- | Optional. Specify how the service determines the pixel aspect ratio
    -- (PAR) for this output. The default behavior, Follow source
    -- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
    -- output. To specify a different PAR in the console, choose any value
    -- other than Follow source. To specify a different PAR by editing the JSON
    -- job specification, choose SPECIFIED. When you choose SPECIFIED for this
    -- setting, you must also specify values for the parNumerator and
    -- parDenominator settings.
    parControl :: Prelude.Maybe H264ParControl,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended
    -- that this be set to 1 so a decoder joining mid-stream will receive an
    -- IDR frame as quickly as possible. Setting this value to 0 will break
    -- output segmenting.
    gopClosedCadence :: Prelude.Maybe Prelude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parDenominator is 33.
    parDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Choose Adaptive to improve subjective video quality for high-motion
    -- content. This will cause the service to use fewer B-frames (which infer
    -- information based on other frames) for high-motion portions of the video
    -- and more B-frames for low-motion portions. The maximum number of
    -- B-frames is limited by the value you provide for the setting B frames
    -- between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Prelude.Maybe H264DynamicSubGop,
    -- | Produces a bitstream compliant with SMPTE RP-2027.
    syntax :: Prelude.Maybe H264Syntax,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five
    -- megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
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
    adaptiveQuantization :: Prelude.Maybe H264AdaptiveQuantization,
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
    -- | Number of B-frames between reference frames.
    numberBFramesBetweenReferenceFrames :: Prelude.Maybe Prelude.Natural,
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
    -- | Specify an H.264 level that is consistent with your output video
    -- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
    codecLevel :: Prelude.Maybe H264CodecLevel,
    -- | Specify the average bitrate in bits per second. Required for VBR and
    -- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
    -- the nearest multiple of 1000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Settings for quality-defined variable bitrate encoding with the H.264
    -- codec. Required when you set Rate control mode to QVBR. Not valid when
    -- you set Rate control mode to a value other than QVBR, or when you don\'t
    -- define Rate control mode.
    qvbrSettings :: Prelude.Maybe H264QvbrSettings,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output. When you
    -- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
    -- resamples your audio to keep it synchronized with the video. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Required settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe H264SlowPal
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
-- 'hrdBufferInitialFillPercentage', 'h264Settings_hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
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
-- 'qualityTuningLevel', 'h264Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
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
-- 'repeatPps', 'h264Settings_repeatPps' - Places a PPS header on each encoded picture, even if repeated.
--
-- 'fieldEncoding', 'h264Settings_fieldEncoding' - Keep the default value, PAFF, to have MediaConvert use PAFF encoding for
-- interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF
-- encoding and create separate interlaced fields.
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
-- 'gopBReference', 'h264Settings_gopBReference' - If enable, use reference B frames for GOP structures that have B frames
-- > 1.
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
-- 'framerateNumerator', 'h264Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'rateControlMode', 'h264Settings_rateControlMode' - Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
--
-- 'numberReferenceFrames', 'h264Settings_numberReferenceFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
--
-- 'slices', 'h264Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
--
-- 'entropyEncoding', 'h264Settings_entropyEncoding' - Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
--
-- 'gopSizeUnits', 'h264Settings_gopSizeUnits' - Indicates if the GOP Size in H264 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
--
-- 'codecProfile', 'h264Settings_codecProfile' - H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
-- the AVC-I License.
--
-- 'gopSize', 'h264Settings_gopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater
-- than zero.
--
-- 'framerateDenominator', 'h264Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
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
-- 'parNumerator', 'h264Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'sceneChangeDetect', 'h264Settings_sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
--
-- 'minIInterval', 'h264Settings_minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- 'unregisteredSeiTimecode', 'h264Settings_unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
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
-- 'parControl', 'h264Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
--
-- 'gopClosedCadence', 'h264Settings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'parDenominator', 'h264Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'maxBitrate', 'h264Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'dynamicSubGop', 'h264Settings_dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
--
-- 'syntax', 'h264Settings_syntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- 'hrdBufferSize', 'h264Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
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
-- 'numberBFramesBetweenReferenceFrames', 'h264Settings_numberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
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
-- 'codecLevel', 'h264Settings_codecLevel' - Specify an H.264 level that is consistent with your output video
-- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
--
-- 'bitrate', 'h264Settings_bitrate' - Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
--
-- 'qvbrSettings', 'h264Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.264
-- codec. Required when you set Rate control mode to QVBR. Not valid when
-- you set Rate control mode to a value other than QVBR, or when you don\'t
-- define Rate control mode.
--
-- 'slowPal', 'h264Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
newH264Settings ::
  H264Settings
newH264Settings =
  H264Settings'
    { hrdBufferInitialFillPercentage =
        Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      flickerAdaptiveQuantization = Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      repeatPps = Prelude.Nothing,
      fieldEncoding = Prelude.Nothing,
      telecine = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      numberReferenceFrames = Prelude.Nothing,
      slices = Prelude.Nothing,
      entropyEncoding = Prelude.Nothing,
      gopSizeUnits = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      softness = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      minIInterval = Prelude.Nothing,
      unregisteredSeiTimecode = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      parControl = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      dynamicSubGop = Prelude.Nothing,
      syntax = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      adaptiveQuantization = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      numberBFramesBetweenReferenceFrames =
        Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      codecLevel = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      qvbrSettings = Prelude.Nothing,
      slowPal = Prelude.Nothing
    }

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h264Settings_hrdBufferInitialFillPercentage :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_hrdBufferInitialFillPercentage = Lens.lens (\H264Settings' {hrdBufferInitialFillPercentage} -> hrdBufferInitialFillPercentage) (\s@H264Settings' {} a -> s {hrdBufferInitialFillPercentage = a} :: H264Settings)

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

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
h264Settings_qualityTuningLevel :: Lens.Lens' H264Settings (Prelude.Maybe H264QualityTuningLevel)
h264Settings_qualityTuningLevel = Lens.lens (\H264Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@H264Settings' {} a -> s {qualityTuningLevel = a} :: H264Settings)

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

-- | Places a PPS header on each encoded picture, even if repeated.
h264Settings_repeatPps :: Lens.Lens' H264Settings (Prelude.Maybe H264RepeatPps)
h264Settings_repeatPps = Lens.lens (\H264Settings' {repeatPps} -> repeatPps) (\s@H264Settings' {} a -> s {repeatPps = a} :: H264Settings)

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for
-- interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF
-- encoding and create separate interlaced fields.
h264Settings_fieldEncoding :: Lens.Lens' H264Settings (Prelude.Maybe H264FieldEncoding)
h264Settings_fieldEncoding = Lens.lens (\H264Settings' {fieldEncoding} -> fieldEncoding) (\s@H264Settings' {} a -> s {fieldEncoding = a} :: H264Settings)

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

-- | If enable, use reference B frames for GOP structures that have B frames
-- > 1.
h264Settings_gopBReference :: Lens.Lens' H264Settings (Prelude.Maybe H264GopBReference)
h264Settings_gopBReference = Lens.lens (\H264Settings' {gopBReference} -> gopBReference) (\s@H264Settings' {} a -> s {gopBReference = a} :: H264Settings)

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

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h264Settings_framerateNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateNumerator = Lens.lens (\H264Settings' {framerateNumerator} -> framerateNumerator) (\s@H264Settings' {} a -> s {framerateNumerator = a} :: H264Settings)

-- | Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
h264Settings_rateControlMode :: Lens.Lens' H264Settings (Prelude.Maybe H264RateControlMode)
h264Settings_rateControlMode = Lens.lens (\H264Settings' {rateControlMode} -> rateControlMode) (\s@H264Settings' {} a -> s {rateControlMode = a} :: H264Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h264Settings_numberReferenceFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_numberReferenceFrames = Lens.lens (\H264Settings' {numberReferenceFrames} -> numberReferenceFrames) (\s@H264Settings' {} a -> s {numberReferenceFrames = a} :: H264Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
h264Settings_slices :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_slices = Lens.lens (\H264Settings' {slices} -> slices) (\s@H264Settings' {} a -> s {slices = a} :: H264Settings)

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
h264Settings_entropyEncoding :: Lens.Lens' H264Settings (Prelude.Maybe H264EntropyEncoding)
h264Settings_entropyEncoding = Lens.lens (\H264Settings' {entropyEncoding} -> entropyEncoding) (\s@H264Settings' {} a -> s {entropyEncoding = a} :: H264Settings)

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
h264Settings_gopSizeUnits :: Lens.Lens' H264Settings (Prelude.Maybe H264GopSizeUnits)
h264Settings_gopSizeUnits = Lens.lens (\H264Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H264Settings' {} a -> s {gopSizeUnits = a} :: H264Settings)

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with
-- the AVC-I License.
h264Settings_codecProfile :: Lens.Lens' H264Settings (Prelude.Maybe H264CodecProfile)
h264Settings_codecProfile = Lens.lens (\H264Settings' {codecProfile} -> codecProfile) (\s@H264Settings' {} a -> s {codecProfile = a} :: H264Settings)

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater
-- than zero.
h264Settings_gopSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Double)
h264Settings_gopSize = Lens.lens (\H264Settings' {gopSize} -> gopSize) (\s@H264Settings' {} a -> s {gopSize = a} :: H264Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h264Settings_framerateDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateDenominator = Lens.lens (\H264Settings' {framerateDenominator} -> framerateDenominator) (\s@H264Settings' {} a -> s {framerateDenominator = a} :: H264Settings)

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

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
h264Settings_parNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parNumerator = Lens.lens (\H264Settings' {parNumerator} -> parNumerator) (\s@H264Settings' {} a -> s {parNumerator = a} :: H264Settings)

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
h264Settings_sceneChangeDetect :: Lens.Lens' H264Settings (Prelude.Maybe H264SceneChangeDetect)
h264Settings_sceneChangeDetect = Lens.lens (\H264Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H264Settings' {} a -> s {sceneChangeDetect = a} :: H264Settings)

-- | Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
h264Settings_minIInterval :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_minIInterval = Lens.lens (\H264Settings' {minIInterval} -> minIInterval) (\s@H264Settings' {} a -> s {minIInterval = a} :: H264Settings)

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
h264Settings_unregisteredSeiTimecode :: Lens.Lens' H264Settings (Prelude.Maybe H264UnregisteredSeiTimecode)
h264Settings_unregisteredSeiTimecode = Lens.lens (\H264Settings' {unregisteredSeiTimecode} -> unregisteredSeiTimecode) (\s@H264Settings' {} a -> s {unregisteredSeiTimecode = a} :: H264Settings)

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

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
h264Settings_gopClosedCadence :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_gopClosedCadence = Lens.lens (\H264Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H264Settings' {} a -> s {gopClosedCadence = a} :: H264Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
h264Settings_parDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parDenominator = Lens.lens (\H264Settings' {parDenominator} -> parDenominator) (\s@H264Settings' {} a -> s {parDenominator = a} :: H264Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
h264Settings_maxBitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_maxBitrate = Lens.lens (\H264Settings' {maxBitrate} -> maxBitrate) (\s@H264Settings' {} a -> s {maxBitrate = a} :: H264Settings)

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
h264Settings_dynamicSubGop :: Lens.Lens' H264Settings (Prelude.Maybe H264DynamicSubGop)
h264Settings_dynamicSubGop = Lens.lens (\H264Settings' {dynamicSubGop} -> dynamicSubGop) (\s@H264Settings' {} a -> s {dynamicSubGop = a} :: H264Settings)

-- | Produces a bitstream compliant with SMPTE RP-2027.
h264Settings_syntax :: Lens.Lens' H264Settings (Prelude.Maybe H264Syntax)
h264Settings_syntax = Lens.lens (\H264Settings' {syntax} -> syntax) (\s@H264Settings' {} a -> s {syntax = a} :: H264Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
h264Settings_hrdBufferSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_hrdBufferSize = Lens.lens (\H264Settings' {hrdBufferSize} -> hrdBufferSize) (\s@H264Settings' {} a -> s {hrdBufferSize = a} :: H264Settings)

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

-- | Number of B-frames between reference frames.
h264Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\H264Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@H264Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: H264Settings)

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

-- | Specify an H.264 level that is consistent with your output video
-- settings. If you aren\'t sure what level to specify, choose Auto (AUTO).
h264Settings_codecLevel :: Lens.Lens' H264Settings (Prelude.Maybe H264CodecLevel)
h264Settings_codecLevel = Lens.lens (\H264Settings' {codecLevel} -> codecLevel) (\s@H264Settings' {} a -> s {codecLevel = a} :: H264Settings)

-- | Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
h264Settings_bitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_bitrate = Lens.lens (\H264Settings' {bitrate} -> bitrate) (\s@H264Settings' {} a -> s {bitrate = a} :: H264Settings)

-- | Settings for quality-defined variable bitrate encoding with the H.264
-- codec. Required when you set Rate control mode to QVBR. Not valid when
-- you set Rate control mode to a value other than QVBR, or when you don\'t
-- define Rate control mode.
h264Settings_qvbrSettings :: Lens.Lens' H264Settings (Prelude.Maybe H264QvbrSettings)
h264Settings_qvbrSettings = Lens.lens (\H264Settings' {qvbrSettings} -> qvbrSettings) (\s@H264Settings' {} a -> s {qvbrSettings = a} :: H264Settings)

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

instance Core.FromJSON H264Settings where
  parseJSON =
    Core.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Prelude.<$> (x Core..:? "hrdBufferInitialFillPercentage")
            Prelude.<*> (x Core..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Core..:? "flickerAdaptiveQuantization")
            Prelude.<*> (x Core..:? "qualityTuningLevel")
            Prelude.<*> (x Core..:? "interlaceMode")
            Prelude.<*> (x Core..:? "repeatPps")
            Prelude.<*> (x Core..:? "fieldEncoding")
            Prelude.<*> (x Core..:? "telecine")
            Prelude.<*> (x Core..:? "gopBReference")
            Prelude.<*> (x Core..:? "spatialAdaptiveQuantization")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "numberReferenceFrames")
            Prelude.<*> (x Core..:? "slices")
            Prelude.<*> (x Core..:? "entropyEncoding")
            Prelude.<*> (x Core..:? "gopSizeUnits")
            Prelude.<*> (x Core..:? "codecProfile")
            Prelude.<*> (x Core..:? "gopSize")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "softness")
            Prelude.<*> (x Core..:? "parNumerator")
            Prelude.<*> (x Core..:? "sceneChangeDetect")
            Prelude.<*> (x Core..:? "minIInterval")
            Prelude.<*> (x Core..:? "unregisteredSeiTimecode")
            Prelude.<*> (x Core..:? "scanTypeConversionMode")
            Prelude.<*> (x Core..:? "parControl")
            Prelude.<*> (x Core..:? "gopClosedCadence")
            Prelude.<*> (x Core..:? "parDenominator")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "dynamicSubGop")
            Prelude.<*> (x Core..:? "syntax")
            Prelude.<*> (x Core..:? "hrdBufferSize")
            Prelude.<*> (x Core..:? "adaptiveQuantization")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "numberBFramesBetweenReferenceFrames")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "codecLevel")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "qvbrSettings")
            Prelude.<*> (x Core..:? "slowPal")
      )

instance Prelude.Hashable H264Settings

instance Prelude.NFData H264Settings

instance Core.ToJSON H264Settings where
  toJSON H264Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hrdBufferInitialFillPercentage" Core..=)
              Prelude.<$> hrdBufferInitialFillPercentage,
            ("temporalAdaptiveQuantization" Core..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("flickerAdaptiveQuantization" Core..=)
              Prelude.<$> flickerAdaptiveQuantization,
            ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel,
            ("interlaceMode" Core..=) Prelude.<$> interlaceMode,
            ("repeatPps" Core..=) Prelude.<$> repeatPps,
            ("fieldEncoding" Core..=) Prelude.<$> fieldEncoding,
            ("telecine" Core..=) Prelude.<$> telecine,
            ("gopBReference" Core..=) Prelude.<$> gopBReference,
            ("spatialAdaptiveQuantization" Core..=)
              Prelude.<$> spatialAdaptiveQuantization,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("numberReferenceFrames" Core..=)
              Prelude.<$> numberReferenceFrames,
            ("slices" Core..=) Prelude.<$> slices,
            ("entropyEncoding" Core..=)
              Prelude.<$> entropyEncoding,
            ("gopSizeUnits" Core..=) Prelude.<$> gopSizeUnits,
            ("codecProfile" Core..=) Prelude.<$> codecProfile,
            ("gopSize" Core..=) Prelude.<$> gopSize,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("softness" Core..=) Prelude.<$> softness,
            ("parNumerator" Core..=) Prelude.<$> parNumerator,
            ("sceneChangeDetect" Core..=)
              Prelude.<$> sceneChangeDetect,
            ("minIInterval" Core..=) Prelude.<$> minIInterval,
            ("unregisteredSeiTimecode" Core..=)
              Prelude.<$> unregisteredSeiTimecode,
            ("scanTypeConversionMode" Core..=)
              Prelude.<$> scanTypeConversionMode,
            ("parControl" Core..=) Prelude.<$> parControl,
            ("gopClosedCadence" Core..=)
              Prelude.<$> gopClosedCadence,
            ("parDenominator" Core..=)
              Prelude.<$> parDenominator,
            ("maxBitrate" Core..=) Prelude.<$> maxBitrate,
            ("dynamicSubGop" Core..=) Prelude.<$> dynamicSubGop,
            ("syntax" Core..=) Prelude.<$> syntax,
            ("hrdBufferSize" Core..=) Prelude.<$> hrdBufferSize,
            ("adaptiveQuantization" Core..=)
              Prelude.<$> adaptiveQuantization,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("numberBFramesBetweenReferenceFrames" Core..=)
              Prelude.<$> numberBFramesBetweenReferenceFrames,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("codecLevel" Core..=) Prelude.<$> codecLevel,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("qvbrSettings" Core..=) Prelude.<$> qvbrSettings,
            ("slowPal" Core..=) Prelude.<$> slowPal
          ]
      )
