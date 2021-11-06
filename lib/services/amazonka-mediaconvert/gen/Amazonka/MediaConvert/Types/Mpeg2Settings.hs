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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.Mpeg2AdaptiveQuantization
import Amazonka.MediaConvert.Types.Mpeg2CodecLevel
import Amazonka.MediaConvert.Types.Mpeg2CodecProfile
import Amazonka.MediaConvert.Types.Mpeg2DynamicSubGop
import Amazonka.MediaConvert.Types.Mpeg2FramerateControl
import Amazonka.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Mpeg2GopSizeUnits
import Amazonka.MediaConvert.Types.Mpeg2InterlaceMode
import Amazonka.MediaConvert.Types.Mpeg2IntraDcPrecision
import Amazonka.MediaConvert.Types.Mpeg2ParControl
import Amazonka.MediaConvert.Types.Mpeg2QualityTuningLevel
import Amazonka.MediaConvert.Types.Mpeg2RateControlMode
import Amazonka.MediaConvert.Types.Mpeg2ScanTypeConversionMode
import Amazonka.MediaConvert.Types.Mpeg2SceneChangeDetect
import Amazonka.MediaConvert.Types.Mpeg2SlowPal
import Amazonka.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.Mpeg2Syntax
import Amazonka.MediaConvert.Types.Mpeg2Telecine
import Amazonka.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value MPEG2.
--
-- /See:/ 'newMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe Mpeg2QualityTuningLevel,
    -- | Keep the default value, Enabled (ENABLED), to adjust quantization within
    -- each frame based on temporal variation of content complexity. When you
    -- enable this feature, the encoder uses fewer bits on areas of the frame
    -- that aren\'t moving and uses more bits on complex objects with sharp
    -- edges that move a lot. For example, this feature improves the
    -- readability of text tickers on newscasts and scoreboards on sports
    -- matches. Enabling this feature will almost always improve your video
    -- quality. Note, though, that this feature doesn\'t take into account
    -- where the viewer\'s attention is likely to be. If viewers are likely to
    -- be focusing their attention on a part of the screen that doesn\'t have
    -- moving objects with sharp edges, such as sports athletes\' faces, you
    -- might choose to disable this feature. Related setting: When you enable
    -- temporal quantization, adjust the strength of the filter with the
    -- setting Adaptive quantization (adaptiveQuantization).
    temporalAdaptiveQuantization :: Prelude.Maybe Mpeg2TemporalAdaptiveQuantization,
    -- | Enable this setting to insert I-frames at scene changes that the service
    -- automatically detects. This improves video quality and is enabled by
    -- default.
    sceneChangeDetect :: Prelude.Maybe Mpeg2SceneChangeDetect,
    -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    hrdBufferInitialFillPercentage :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output. When you
    -- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
    -- resamples your audio to keep it synchronized with the video. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Required settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe Mpeg2SlowPal,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Specify the interval between keyframes, in seconds or frames, for this
    -- output. Default: 12 Related settings: When you specify the GOP size in
    -- seconds, set GOP mode control (GopSizeUnits) to Specified, seconds
    -- (SECONDS). The default value for GOP mode control (GopSizeUnits) is
    -- Frames (FRAMES).
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Specify the number of B-frames that MediaConvert puts between reference
    -- frames in this output. Valid values are whole numbers from 0 through 7.
    -- When you don\'t specify a value, MediaConvert defaults to 2.
    numberBFramesBetweenReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Specify the units for GOP size (GopSize). If you don\'t specify a value
    -- here, by default the encoder measures GOP size in frames.
    gopSizeUnits :: Prelude.Maybe Mpeg2GopSizeUnits,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five
    -- megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
    -- | Use Rate control mode (Mpeg2RateControlMode) to specify whether the
    -- bitrate is variable (vbr) or constant (cbr).
    rateControlMode :: Prelude.Maybe Mpeg2RateControlMode,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to
    -- 29.97 fps, and your output scan type is interlaced, you can optionally
    -- enable hard or soft telecine to create a smoother picture. Hard telecine
    -- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
    -- with a 23.976 output that signals to the video player device to do the
    -- conversion during play back. When you keep the default value, None
    -- (NONE), MediaConvert does a standard frame rate conversion to 29.97
    -- without doing anything with the field polarity to create a smoother
    -- picture.
    telecine :: Prelude.Maybe Mpeg2Telecine,
    -- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization
    -- precision for intra-block DC coefficients. If you choose the value auto,
    -- the service will automatically select the precision based on the
    -- per-frame compression ratio.
    intraDcPrecision :: Prelude.Maybe Mpeg2IntraDcPrecision,
    -- | Choose Adaptive to improve subjective video quality for high-motion
    -- content. This will cause the service to use fewer B-frames (which infer
    -- information based on other frames) for high-motion portions of the video
    -- and more B-frames for low-motion portions. The maximum number of
    -- B-frames is limited by the value you provide for the setting B frames
    -- between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Prelude.Maybe Mpeg2DynamicSubGop,
    -- | Enforces separation between repeated (cadence) I-frames and I-frames
    -- inserted by Scene Change Detection. If a scene change I-frame is within
    -- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
    -- stretched to the scene change I-frame. GOP stretch requires enabling
    -- lookahead as well as setting I-interval. The normal cadence resumes for
    -- the next GOP. This setting is only used when Scene Change Detect is
    -- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Prelude.Maybe Prelude.Natural,
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
    interlaceMode :: Prelude.Maybe Mpeg2InterlaceMode,
    -- | Optional. Specify how the service determines the pixel aspect ratio
    -- (PAR) for this output. The default behavior, Follow source
    -- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
    -- output. To specify a different PAR in the console, choose any value
    -- other than Follow source. To specify a different PAR by editing the JSON
    -- job specification, choose SPECIFIED. When you choose SPECIFIED for this
    -- setting, you must also specify values for the parNumerator and
    -- parDenominator settings.
    parControl :: Prelude.Maybe Mpeg2ParControl,
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
    scanTypeConversionMode :: Prelude.Maybe Mpeg2ScanTypeConversionMode,
    -- | Ignore this setting unless you need to comply with a specification that
    -- requires a specific value. If you don\'t have a specification
    -- requirement, we recommend that you adjust the softness of your output by
    -- using a lower value for the setting Sharpness (sharpness) or by enabling
    -- a noise reducer filter (noiseReducerFilter). The Softness (softness)
    -- setting specifies the quantization matrices that the encoder uses. Keep
    -- the default value, 0, to use the AWS Elemental default matrices. Choose
    -- a value from 17 to 128 to use planar interpolation. Increasing values
    -- from 17 to 128 result in increasing reduction of high-frequency data.
    -- The value 128 results in the softest video.
    softness :: Prelude.Maybe Prelude.Natural,
    -- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
    -- output.
    codecProfile :: Prelude.Maybe Mpeg2CodecProfile,
    -- | Specify the average bitrate in bits per second. Required for VBR and
    -- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
    -- the nearest multiple of 1000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
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
    framerateConversionAlgorithm :: Prelude.Maybe Mpeg2FramerateConversionAlgorithm,
    -- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video
    -- output.
    codecLevel :: Prelude.Maybe Mpeg2CodecLevel,
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
    framerateControl :: Prelude.Maybe Mpeg2FramerateControl,
    -- | Specify the strength of any adaptive quantization filters that you
    -- enable. The value that you choose here applies to the following
    -- settings: Spatial adaptive quantization (spatialAdaptiveQuantization),
    -- and Temporal adaptive quantization (temporalAdaptiveQuantization).
    adaptiveQuantization :: Prelude.Maybe Mpeg2AdaptiveQuantization,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether this output\'s video uses the D10 syntax. Keep the
    -- default value to not use the syntax. Related settings: When you choose
    -- D10 (D_10) for your MXF profile (profile), you must also set this value
    -- to to D10 (D_10).
    syntax :: Prelude.Maybe Mpeg2Syntax,
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
    -- | Keep the default value, Enabled (ENABLED), to adjust quantization within
    -- each frame based on spatial variation of content complexity. When you
    -- enable this feature, the encoder uses fewer bits on areas that can
    -- sustain more distortion with no noticeable visual degradation and uses
    -- more bits on areas where any small distortion will be noticeable. For
    -- example, complex textured blocks are encoded with fewer bits and smooth
    -- textured blocks are encoded with more bits. Enabling this feature will
    -- almost always improve your video quality. Note, though, that this
    -- feature doesn\'t take into account where the viewer\'s attention is
    -- likely to be. If viewers are likely to be focusing their attention on a
    -- part of the screen with a lot of complex texture, you might choose to
    -- disable this feature. Related setting: When you enable spatial adaptive
    -- quantization, set the value for Adaptive quantization
    -- (adaptiveQuantization) depending on your content. For homogeneous
    -- content, such as cartoons and video games, set it to Low. For content
    -- with a wider variety of textures, set it to High or Higher.
    spatialAdaptiveQuantization :: Prelude.Maybe Mpeg2SpatialAdaptiveQuantization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mpeg2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityTuningLevel', 'mpeg2Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'temporalAdaptiveQuantization', 'mpeg2Settings_temporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on temporal variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas of the frame
-- that aren\'t moving and uses more bits on complex objects with sharp
-- edges that move a lot. For example, this feature improves the
-- readability of text tickers on newscasts and scoreboards on sports
-- matches. Enabling this feature will almost always improve your video
-- quality. Note, though, that this feature doesn\'t take into account
-- where the viewer\'s attention is likely to be. If viewers are likely to
-- be focusing their attention on a part of the screen that doesn\'t have
-- moving objects with sharp edges, such as sports athletes\' faces, you
-- might choose to disable this feature. Related setting: When you enable
-- temporal quantization, adjust the strength of the filter with the
-- setting Adaptive quantization (adaptiveQuantization).
--
-- 'sceneChangeDetect', 'mpeg2Settings_sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default.
--
-- 'hrdBufferInitialFillPercentage', 'mpeg2Settings_hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'slowPal', 'mpeg2Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'parNumerator', 'mpeg2Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'gopSize', 'mpeg2Settings_gopSize' - Specify the interval between keyframes, in seconds or frames, for this
-- output. Default: 12 Related settings: When you specify the GOP size in
-- seconds, set GOP mode control (GopSizeUnits) to Specified, seconds
-- (SECONDS). The default value for GOP mode control (GopSizeUnits) is
-- Frames (FRAMES).
--
-- 'numberBFramesBetweenReferenceFrames', 'mpeg2Settings_numberBFramesBetweenReferenceFrames' - Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
--
-- 'gopSizeUnits', 'mpeg2Settings_gopSizeUnits' - Specify the units for GOP size (GopSize). If you don\'t specify a value
-- here, by default the encoder measures GOP size in frames.
--
-- 'hrdBufferSize', 'mpeg2Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
--
-- 'rateControlMode', 'mpeg2Settings_rateControlMode' - Use Rate control mode (Mpeg2RateControlMode) to specify whether the
-- bitrate is variable (vbr) or constant (cbr).
--
-- 'telecine', 'mpeg2Settings_telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
--
-- 'intraDcPrecision', 'mpeg2Settings_intraDcPrecision' - Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization
-- precision for intra-block DC coefficients. If you choose the value auto,
-- the service will automatically select the precision based on the
-- per-frame compression ratio.
--
-- 'dynamicSubGop', 'mpeg2Settings_dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
--
-- 'minIInterval', 'mpeg2Settings_minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- 'interlaceMode', 'mpeg2Settings_interlaceMode' - Choose the scan line type for the output. Keep the default value,
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
-- 'parControl', 'mpeg2Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
--
-- 'scanTypeConversionMode', 'mpeg2Settings_scanTypeConversionMode' - Use this setting for interlaced outputs, when your output frame rate is
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
-- 'softness', 'mpeg2Settings_softness' - Ignore this setting unless you need to comply with a specification that
-- requires a specific value. If you don\'t have a specification
-- requirement, we recommend that you adjust the softness of your output by
-- using a lower value for the setting Sharpness (sharpness) or by enabling
-- a noise reducer filter (noiseReducerFilter). The Softness (softness)
-- setting specifies the quantization matrices that the encoder uses. Keep
-- the default value, 0, to use the AWS Elemental default matrices. Choose
-- a value from 17 to 128 to use planar interpolation. Increasing values
-- from 17 to 128 result in increasing reduction of high-frequency data.
-- The value 128 results in the softest video.
--
-- 'codecProfile', 'mpeg2Settings_codecProfile' - Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
-- output.
--
-- 'bitrate', 'mpeg2Settings_bitrate' - Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
--
-- 'framerateDenominator', 'mpeg2Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateConversionAlgorithm', 'mpeg2Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'codecLevel', 'mpeg2Settings_codecLevel' - Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video
-- output.
--
-- 'framerateControl', 'mpeg2Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
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
-- 'adaptiveQuantization', 'mpeg2Settings_adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Spatial adaptive quantization (spatialAdaptiveQuantization),
-- and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- 'framerateNumerator', 'mpeg2Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'maxBitrate', 'mpeg2Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
--
-- 'syntax', 'mpeg2Settings_syntax' - Specify whether this output\'s video uses the D10 syntax. Keep the
-- default value to not use the syntax. Related settings: When you choose
-- D10 (D_10) for your MXF profile (profile), you must also set this value
-- to to D10 (D_10).
--
-- 'gopClosedCadence', 'mpeg2Settings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'parDenominator', 'mpeg2Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'spatialAdaptiveQuantization', 'mpeg2Settings_spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
newMpeg2Settings ::
  Mpeg2Settings
newMpeg2Settings =
  Mpeg2Settings'
    { qualityTuningLevel =
        Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      hrdBufferInitialFillPercentage = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      numberBFramesBetweenReferenceFrames =
        Prelude.Nothing,
      gopSizeUnits = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      telecine = Prelude.Nothing,
      intraDcPrecision = Prelude.Nothing,
      dynamicSubGop = Prelude.Nothing,
      minIInterval = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      parControl = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      softness = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      codecLevel = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      adaptiveQuantization = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      syntax = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
mpeg2Settings_qualityTuningLevel :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2QualityTuningLevel)
mpeg2Settings_qualityTuningLevel = Lens.lens (\Mpeg2Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@Mpeg2Settings' {} a -> s {qualityTuningLevel = a} :: Mpeg2Settings)

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on temporal variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas of the frame
-- that aren\'t moving and uses more bits on complex objects with sharp
-- edges that move a lot. For example, this feature improves the
-- readability of text tickers on newscasts and scoreboards on sports
-- matches. Enabling this feature will almost always improve your video
-- quality. Note, though, that this feature doesn\'t take into account
-- where the viewer\'s attention is likely to be. If viewers are likely to
-- be focusing their attention on a part of the screen that doesn\'t have
-- moving objects with sharp edges, such as sports athletes\' faces, you
-- might choose to disable this feature. Related setting: When you enable
-- temporal quantization, adjust the strength of the filter with the
-- setting Adaptive quantization (adaptiveQuantization).
mpeg2Settings_temporalAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2TemporalAdaptiveQuantization)
mpeg2Settings_temporalAdaptiveQuantization = Lens.lens (\Mpeg2Settings' {temporalAdaptiveQuantization} -> temporalAdaptiveQuantization) (\s@Mpeg2Settings' {} a -> s {temporalAdaptiveQuantization = a} :: Mpeg2Settings)

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default.
mpeg2Settings_sceneChangeDetect :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2SceneChangeDetect)
mpeg2Settings_sceneChangeDetect = Lens.lens (\Mpeg2Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@Mpeg2Settings' {} a -> s {sceneChangeDetect = a} :: Mpeg2Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
mpeg2Settings_hrdBufferInitialFillPercentage :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_hrdBufferInitialFillPercentage = Lens.lens (\Mpeg2Settings' {hrdBufferInitialFillPercentage} -> hrdBufferInitialFillPercentage) (\s@Mpeg2Settings' {} a -> s {hrdBufferInitialFillPercentage = a} :: Mpeg2Settings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
mpeg2Settings_slowPal :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2SlowPal)
mpeg2Settings_slowPal = Lens.lens (\Mpeg2Settings' {slowPal} -> slowPal) (\s@Mpeg2Settings' {} a -> s {slowPal = a} :: Mpeg2Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
mpeg2Settings_parNumerator :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_parNumerator = Lens.lens (\Mpeg2Settings' {parNumerator} -> parNumerator) (\s@Mpeg2Settings' {} a -> s {parNumerator = a} :: Mpeg2Settings)

-- | Specify the interval between keyframes, in seconds or frames, for this
-- output. Default: 12 Related settings: When you specify the GOP size in
-- seconds, set GOP mode control (GopSizeUnits) to Specified, seconds
-- (SECONDS). The default value for GOP mode control (GopSizeUnits) is
-- Frames (FRAMES).
mpeg2Settings_gopSize :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Double)
mpeg2Settings_gopSize = Lens.lens (\Mpeg2Settings' {gopSize} -> gopSize) (\s@Mpeg2Settings' {} a -> s {gopSize = a} :: Mpeg2Settings)

-- | Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
mpeg2Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\Mpeg2Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@Mpeg2Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: Mpeg2Settings)

-- | Specify the units for GOP size (GopSize). If you don\'t specify a value
-- here, by default the encoder measures GOP size in frames.
mpeg2Settings_gopSizeUnits :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2GopSizeUnits)
mpeg2Settings_gopSizeUnits = Lens.lens (\Mpeg2Settings' {gopSizeUnits} -> gopSizeUnits) (\s@Mpeg2Settings' {} a -> s {gopSizeUnits = a} :: Mpeg2Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
mpeg2Settings_hrdBufferSize :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_hrdBufferSize = Lens.lens (\Mpeg2Settings' {hrdBufferSize} -> hrdBufferSize) (\s@Mpeg2Settings' {} a -> s {hrdBufferSize = a} :: Mpeg2Settings)

-- | Use Rate control mode (Mpeg2RateControlMode) to specify whether the
-- bitrate is variable (vbr) or constant (cbr).
mpeg2Settings_rateControlMode :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2RateControlMode)
mpeg2Settings_rateControlMode = Lens.lens (\Mpeg2Settings' {rateControlMode} -> rateControlMode) (\s@Mpeg2Settings' {} a -> s {rateControlMode = a} :: Mpeg2Settings)

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
mpeg2Settings_telecine :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2Telecine)
mpeg2Settings_telecine = Lens.lens (\Mpeg2Settings' {telecine} -> telecine) (\s@Mpeg2Settings' {} a -> s {telecine = a} :: Mpeg2Settings)

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization
-- precision for intra-block DC coefficients. If you choose the value auto,
-- the service will automatically select the precision based on the
-- per-frame compression ratio.
mpeg2Settings_intraDcPrecision :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2IntraDcPrecision)
mpeg2Settings_intraDcPrecision = Lens.lens (\Mpeg2Settings' {intraDcPrecision} -> intraDcPrecision) (\s@Mpeg2Settings' {} a -> s {intraDcPrecision = a} :: Mpeg2Settings)

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
mpeg2Settings_dynamicSubGop :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2DynamicSubGop)
mpeg2Settings_dynamicSubGop = Lens.lens (\Mpeg2Settings' {dynamicSubGop} -> dynamicSubGop) (\s@Mpeg2Settings' {} a -> s {dynamicSubGop = a} :: Mpeg2Settings)

-- | Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
mpeg2Settings_minIInterval :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_minIInterval = Lens.lens (\Mpeg2Settings' {minIInterval} -> minIInterval) (\s@Mpeg2Settings' {} a -> s {minIInterval = a} :: Mpeg2Settings)

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
mpeg2Settings_interlaceMode :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2InterlaceMode)
mpeg2Settings_interlaceMode = Lens.lens (\Mpeg2Settings' {interlaceMode} -> interlaceMode) (\s@Mpeg2Settings' {} a -> s {interlaceMode = a} :: Mpeg2Settings)

-- | Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
mpeg2Settings_parControl :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2ParControl)
mpeg2Settings_parControl = Lens.lens (\Mpeg2Settings' {parControl} -> parControl) (\s@Mpeg2Settings' {} a -> s {parControl = a} :: Mpeg2Settings)

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
mpeg2Settings_scanTypeConversionMode :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2ScanTypeConversionMode)
mpeg2Settings_scanTypeConversionMode = Lens.lens (\Mpeg2Settings' {scanTypeConversionMode} -> scanTypeConversionMode) (\s@Mpeg2Settings' {} a -> s {scanTypeConversionMode = a} :: Mpeg2Settings)

-- | Ignore this setting unless you need to comply with a specification that
-- requires a specific value. If you don\'t have a specification
-- requirement, we recommend that you adjust the softness of your output by
-- using a lower value for the setting Sharpness (sharpness) or by enabling
-- a noise reducer filter (noiseReducerFilter). The Softness (softness)
-- setting specifies the quantization matrices that the encoder uses. Keep
-- the default value, 0, to use the AWS Elemental default matrices. Choose
-- a value from 17 to 128 to use planar interpolation. Increasing values
-- from 17 to 128 result in increasing reduction of high-frequency data.
-- The value 128 results in the softest video.
mpeg2Settings_softness :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_softness = Lens.lens (\Mpeg2Settings' {softness} -> softness) (\s@Mpeg2Settings' {} a -> s {softness = a} :: Mpeg2Settings)

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
-- output.
mpeg2Settings_codecProfile :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2CodecProfile)
mpeg2Settings_codecProfile = Lens.lens (\Mpeg2Settings' {codecProfile} -> codecProfile) (\s@Mpeg2Settings' {} a -> s {codecProfile = a} :: Mpeg2Settings)

-- | Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
mpeg2Settings_bitrate :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_bitrate = Lens.lens (\Mpeg2Settings' {bitrate} -> bitrate) (\s@Mpeg2Settings' {} a -> s {bitrate = a} :: Mpeg2Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
mpeg2Settings_framerateDenominator :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_framerateDenominator = Lens.lens (\Mpeg2Settings' {framerateDenominator} -> framerateDenominator) (\s@Mpeg2Settings' {} a -> s {framerateDenominator = a} :: Mpeg2Settings)

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
mpeg2Settings_framerateConversionAlgorithm :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2FramerateConversionAlgorithm)
mpeg2Settings_framerateConversionAlgorithm = Lens.lens (\Mpeg2Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Mpeg2Settings' {} a -> s {framerateConversionAlgorithm = a} :: Mpeg2Settings)

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video
-- output.
mpeg2Settings_codecLevel :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2CodecLevel)
mpeg2Settings_codecLevel = Lens.lens (\Mpeg2Settings' {codecLevel} -> codecLevel) (\s@Mpeg2Settings' {} a -> s {codecLevel = a} :: Mpeg2Settings)

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
mpeg2Settings_framerateControl :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2FramerateControl)
mpeg2Settings_framerateControl = Lens.lens (\Mpeg2Settings' {framerateControl} -> framerateControl) (\s@Mpeg2Settings' {} a -> s {framerateControl = a} :: Mpeg2Settings)

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Spatial adaptive quantization (spatialAdaptiveQuantization),
-- and Temporal adaptive quantization (temporalAdaptiveQuantization).
mpeg2Settings_adaptiveQuantization :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2AdaptiveQuantization)
mpeg2Settings_adaptiveQuantization = Lens.lens (\Mpeg2Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@Mpeg2Settings' {} a -> s {adaptiveQuantization = a} :: Mpeg2Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
mpeg2Settings_framerateNumerator :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_framerateNumerator = Lens.lens (\Mpeg2Settings' {framerateNumerator} -> framerateNumerator) (\s@Mpeg2Settings' {} a -> s {framerateNumerator = a} :: Mpeg2Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
mpeg2Settings_maxBitrate :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_maxBitrate = Lens.lens (\Mpeg2Settings' {maxBitrate} -> maxBitrate) (\s@Mpeg2Settings' {} a -> s {maxBitrate = a} :: Mpeg2Settings)

-- | Specify whether this output\'s video uses the D10 syntax. Keep the
-- default value to not use the syntax. Related settings: When you choose
-- D10 (D_10) for your MXF profile (profile), you must also set this value
-- to to D10 (D_10).
mpeg2Settings_syntax :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2Syntax)
mpeg2Settings_syntax = Lens.lens (\Mpeg2Settings' {syntax} -> syntax) (\s@Mpeg2Settings' {} a -> s {syntax = a} :: Mpeg2Settings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
mpeg2Settings_gopClosedCadence :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_gopClosedCadence = Lens.lens (\Mpeg2Settings' {gopClosedCadence} -> gopClosedCadence) (\s@Mpeg2Settings' {} a -> s {gopClosedCadence = a} :: Mpeg2Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
mpeg2Settings_parDenominator :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_parDenominator = Lens.lens (\Mpeg2Settings' {parDenominator} -> parDenominator) (\s@Mpeg2Settings' {} a -> s {parDenominator = a} :: Mpeg2Settings)

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
mpeg2Settings_spatialAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2SpatialAdaptiveQuantization)
mpeg2Settings_spatialAdaptiveQuantization = Lens.lens (\Mpeg2Settings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@Mpeg2Settings' {} a -> s {spatialAdaptiveQuantization = a} :: Mpeg2Settings)

instance Core.FromJSON Mpeg2Settings where
  parseJSON =
    Core.withObject
      "Mpeg2Settings"
      ( \x ->
          Mpeg2Settings'
            Prelude.<$> (x Core..:? "qualityTuningLevel")
            Prelude.<*> (x Core..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Core..:? "sceneChangeDetect")
            Prelude.<*> (x Core..:? "hrdBufferInitialFillPercentage")
            Prelude.<*> (x Core..:? "slowPal")
            Prelude.<*> (x Core..:? "parNumerator")
            Prelude.<*> (x Core..:? "gopSize")
            Prelude.<*> (x Core..:? "numberBFramesBetweenReferenceFrames")
            Prelude.<*> (x Core..:? "gopSizeUnits")
            Prelude.<*> (x Core..:? "hrdBufferSize")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "telecine")
            Prelude.<*> (x Core..:? "intraDcPrecision")
            Prelude.<*> (x Core..:? "dynamicSubGop")
            Prelude.<*> (x Core..:? "minIInterval")
            Prelude.<*> (x Core..:? "interlaceMode")
            Prelude.<*> (x Core..:? "parControl")
            Prelude.<*> (x Core..:? "scanTypeConversionMode")
            Prelude.<*> (x Core..:? "softness")
            Prelude.<*> (x Core..:? "codecProfile")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "codecLevel")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "adaptiveQuantization")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "syntax")
            Prelude.<*> (x Core..:? "gopClosedCadence")
            Prelude.<*> (x Core..:? "parDenominator")
            Prelude.<*> (x Core..:? "spatialAdaptiveQuantization")
      )

instance Prelude.Hashable Mpeg2Settings

instance Prelude.NFData Mpeg2Settings

instance Core.ToJSON Mpeg2Settings where
  toJSON Mpeg2Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel,
            ("temporalAdaptiveQuantization" Core..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("sceneChangeDetect" Core..=)
              Prelude.<$> sceneChangeDetect,
            ("hrdBufferInitialFillPercentage" Core..=)
              Prelude.<$> hrdBufferInitialFillPercentage,
            ("slowPal" Core..=) Prelude.<$> slowPal,
            ("parNumerator" Core..=) Prelude.<$> parNumerator,
            ("gopSize" Core..=) Prelude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Core..=)
              Prelude.<$> numberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" Core..=) Prelude.<$> gopSizeUnits,
            ("hrdBufferSize" Core..=) Prelude.<$> hrdBufferSize,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("telecine" Core..=) Prelude.<$> telecine,
            ("intraDcPrecision" Core..=)
              Prelude.<$> intraDcPrecision,
            ("dynamicSubGop" Core..=) Prelude.<$> dynamicSubGop,
            ("minIInterval" Core..=) Prelude.<$> minIInterval,
            ("interlaceMode" Core..=) Prelude.<$> interlaceMode,
            ("parControl" Core..=) Prelude.<$> parControl,
            ("scanTypeConversionMode" Core..=)
              Prelude.<$> scanTypeConversionMode,
            ("softness" Core..=) Prelude.<$> softness,
            ("codecProfile" Core..=) Prelude.<$> codecProfile,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("codecLevel" Core..=) Prelude.<$> codecLevel,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("adaptiveQuantization" Core..=)
              Prelude.<$> adaptiveQuantization,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("maxBitrate" Core..=) Prelude.<$> maxBitrate,
            ("syntax" Core..=) Prelude.<$> syntax,
            ("gopClosedCadence" Core..=)
              Prelude.<$> gopClosedCadence,
            ("parDenominator" Core..=)
              Prelude.<$> parDenominator,
            ("spatialAdaptiveQuantization" Core..=)
              Prelude.<$> spatialAdaptiveQuantization
          ]
      )
