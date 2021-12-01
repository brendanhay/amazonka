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
-- Module      : Amazonka.MediaConvert.Types.H265Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.H265AdaptiveQuantization
import Amazonka.MediaConvert.Types.H265AlternateTransferFunctionSei
import Amazonka.MediaConvert.Types.H265CodecLevel
import Amazonka.MediaConvert.Types.H265CodecProfile
import Amazonka.MediaConvert.Types.H265DynamicSubGop
import Amazonka.MediaConvert.Types.H265FlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265FramerateControl
import Amazonka.MediaConvert.Types.H265FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.H265GopBReference
import Amazonka.MediaConvert.Types.H265GopSizeUnits
import Amazonka.MediaConvert.Types.H265InterlaceMode
import Amazonka.MediaConvert.Types.H265ParControl
import Amazonka.MediaConvert.Types.H265QualityTuningLevel
import Amazonka.MediaConvert.Types.H265QvbrSettings
import Amazonka.MediaConvert.Types.H265RateControlMode
import Amazonka.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
import Amazonka.MediaConvert.Types.H265ScanTypeConversionMode
import Amazonka.MediaConvert.Types.H265SceneChangeDetect
import Amazonka.MediaConvert.Types.H265SlowPal
import Amazonka.MediaConvert.Types.H265SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265Telecine
import Amazonka.MediaConvert.Types.H265TemporalAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265TemporalIds
import Amazonka.MediaConvert.Types.H265Tiles
import Amazonka.MediaConvert.Types.H265UnregisteredSeiTimecode
import Amazonka.MediaConvert.Types.H265WriteMp4PackagingType
import qualified Amazonka.Prelude as Prelude

-- | Settings for H265 codec
--
-- /See:/ 'newH265Settings' smart constructor.
data H265Settings = H265Settings'
  { -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
    -- message.
    unregisteredSeiTimecode :: Prelude.Maybe H265UnregisteredSeiTimecode,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe H265QualityTuningLevel,
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
    temporalAdaptiveQuantization :: Prelude.Maybe H265TemporalAdaptiveQuantization,
    -- | Enable this setting to insert I-frames at scene changes that the service
    -- automatically detects. This improves video quality and is enabled by
    -- default. If this output uses QVBR, choose Transition detection
    -- (TRANSITION_DETECTION) for further video quality improvement. For more
    -- information about QVBR, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
    sceneChangeDetect :: Prelude.Maybe H265SceneChangeDetect,
    -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    hrdBufferInitialFillPercentage :: Prelude.Maybe Prelude.Natural,
    -- | Enable use of tiles, allowing horizontal as well as vertical subdivision
    -- of the encoded pictures.
    tiles :: Prelude.Maybe H265Tiles,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output. When you
    -- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
    -- resamples your audio to keep it synchronized with the video. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Required settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe H265SlowPal,
    -- | Enables temporal layer identifiers in the encoded bitstream. Up to 3
    -- layers are supported depending on GOP structure: I- and P-frames form
    -- one layer, reference B-frames can form a second layer and non-reference
    -- b-frames can form a third layer. Decoders can optionally decode only the
    -- lower temporal layers to generate a lower frame rate output. For
    -- example, given a bitstream with temporal IDs and with b-frames = 1 (i.e.
    -- IbPbPb display order), a decoder could decode all the frames for full
    -- frame rate output or only the I and P frames (lowest temporal layer) for
    -- a half frame rate output.
    temporalIds :: Prelude.Maybe H265TemporalIds,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | GOP Length (keyframe interval) in frames or seconds. Must be greater
    -- than zero.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Specify the number of B-frames that MediaConvert puts between reference
    -- frames in this output. Valid values are whole numbers from 0 through 7.
    -- When you don\'t specify a value, MediaConvert defaults to 2.
    numberBFramesBetweenReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the GOP Size in H265 is specified in frames or seconds. If
    -- seconds the system will convert the GOP Size into a frame count at run
    -- time.
    gopSizeUnits :: Prelude.Maybe H265GopSizeUnits,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five
    -- megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Enables Alternate Transfer Function SEI message for outputs using Hybrid
    -- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
    alternateTransferFunctionSei :: Prelude.Maybe H265AlternateTransferFunctionSei,
    -- | Use this setting to specify whether this output has a variable bitrate
    -- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
    -- (QVBR).
    rateControlMode :: Prelude.Maybe H265RateControlMode,
    -- | Number of reference frames to use. The encoder may use more than
    -- requested if using B-frames and\/or interlaced encoding.
    numberReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | This field applies only if the Streams > Advanced > Framerate
    -- (framerate) field is set to 29.970. This field works with the Streams >
    -- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
    -- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
    -- the scan type for the output: Progressive, Interlaced, Hard Telecine or
    -- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
    -- produces 23.976; the player converts this output to 29.97i.
    telecine :: Prelude.Maybe H265Telecine,
    -- | Choose Adaptive to improve subjective video quality for high-motion
    -- content. This will cause the service to use fewer B-frames (which infer
    -- information based on other frames) for high-motion portions of the video
    -- and more B-frames for low-motion portions. The maximum number of
    -- B-frames is limited by the value you provide for the setting B frames
    -- between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Prelude.Maybe H265DynamicSubGop,
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
    interlaceMode :: Prelude.Maybe H265InterlaceMode,
    -- | Optional. Specify how the service determines the pixel aspect ratio
    -- (PAR) for this output. The default behavior, Follow source
    -- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
    -- output. To specify a different PAR in the console, choose any value
    -- other than Follow source. To specify a different PAR by editing the JSON
    -- job specification, choose SPECIFIED. When you choose SPECIFIED for this
    -- setting, you must also specify values for the parNumerator and
    -- parDenominator settings.
    parControl :: Prelude.Maybe H265ParControl,
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
    scanTypeConversionMode :: Prelude.Maybe H265ScanTypeConversionMode,
    -- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop
    -- appears as a visual flicker that can arise when the encoder saves bits
    -- by copying some macroblocks many times from frame to frame, and then
    -- refreshes them at the I-frame. When you enable this setting, the encoder
    -- updates these macroblocks slightly more often to smooth out the flicker.
    -- This setting is disabled by default. Related setting: In addition to
    -- enabling this setting, you must also set adaptiveQuantization to a value
    -- other than Off (OFF).
    flickerAdaptiveQuantization :: Prelude.Maybe H265FlickerAdaptiveQuantization,
    -- | Settings for quality-defined variable bitrate encoding with the H.265
    -- codec. Use these settings only when you set QVBR for Rate control mode
    -- (RateControlMode).
    qvbrSettings :: Prelude.Maybe H265QvbrSettings,
    -- | Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
    -- dynamically selects best strength based on content
    sampleAdaptiveOffsetFilterMode :: Prelude.Maybe H265SampleAdaptiveOffsetFilterMode,
    -- | Represents the Profile and Tier, per the HEVC (H.265) specification.
    -- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
    -- represents Main Profile with High Tier. 4:2:2 profiles are only
    -- available with the HEVC 4:2:2 License.
    codecProfile :: Prelude.Maybe H265CodecProfile,
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
    framerateConversionAlgorithm :: Prelude.Maybe H265FramerateConversionAlgorithm,
    -- | H.265 Level.
    codecLevel :: Prelude.Maybe H265CodecLevel,
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
    framerateControl :: Prelude.Maybe H265FramerateControl,
    -- | If the location of parameter set NAL units doesn\'t matter in your
    -- workflow, ignore this setting. Use this setting only with CMAF or DASH
    -- outputs, or with standalone file outputs in an MPEG-4 container (MP4
    -- outputs). Choose HVC1 to mark your output as HVC1. This makes your
    -- output compliant with the following specification: ISO IECJTC1 SC29
    -- N13798 Text ISO\/IEC FDIS 14496-15 3rd Edition. For these outputs, the
    -- service stores parameter set NAL units in the sample headers but not in
    -- the samples directly. For MP4 outputs, when you choose HVC1, your output
    -- video might not work properly with some downstream systems and video
    -- players. The service defaults to marking your output as HEV1. For these
    -- outputs, the service writes parameter set NAL units directly into the
    -- samples.
    writeMp4PackagingType :: Prelude.Maybe H265WriteMp4PackagingType,
    -- | Specify the strength of any adaptive quantization filters that you
    -- enable. The value that you choose here applies to the following
    -- settings: Flicker adaptive quantization (flickerAdaptiveQuantization),
    -- Spatial adaptive quantization (spatialAdaptiveQuantization), and
    -- Temporal adaptive quantization (temporalAdaptiveQuantization).
    adaptiveQuantization :: Prelude.Maybe H265AdaptiveQuantization,
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
    gopBReference :: Prelude.Maybe H265GopBReference,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
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
    spatialAdaptiveQuantization :: Prelude.Maybe H265SpatialAdaptiveQuantization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H265Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unregisteredSeiTimecode', 'h265Settings_unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
--
-- 'qualityTuningLevel', 'h265Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'temporalAdaptiveQuantization', 'h265Settings_temporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within
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
-- 'sceneChangeDetect', 'h265Settings_sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
--
-- 'hrdBufferInitialFillPercentage', 'h265Settings_hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'tiles', 'h265Settings_tiles' - Enable use of tiles, allowing horizontal as well as vertical subdivision
-- of the encoded pictures.
--
-- 'slowPal', 'h265Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'temporalIds', 'h265Settings_temporalIds' - Enables temporal layer identifiers in the encoded bitstream. Up to 3
-- layers are supported depending on GOP structure: I- and P-frames form
-- one layer, reference B-frames can form a second layer and non-reference
-- b-frames can form a third layer. Decoders can optionally decode only the
-- lower temporal layers to generate a lower frame rate output. For
-- example, given a bitstream with temporal IDs and with b-frames = 1 (i.e.
-- IbPbPb display order), a decoder could decode all the frames for full
-- frame rate output or only the I and P frames (lowest temporal layer) for
-- a half frame rate output.
--
-- 'parNumerator', 'h265Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'gopSize', 'h265Settings_gopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater
-- than zero.
--
-- 'numberBFramesBetweenReferenceFrames', 'h265Settings_numberBFramesBetweenReferenceFrames' - Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
--
-- 'gopSizeUnits', 'h265Settings_gopSizeUnits' - Indicates if the GOP Size in H265 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
--
-- 'hrdBufferSize', 'h265Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
--
-- 'slices', 'h265Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
--
-- 'alternateTransferFunctionSei', 'h265Settings_alternateTransferFunctionSei' - Enables Alternate Transfer Function SEI message for outputs using Hybrid
-- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
--
-- 'rateControlMode', 'h265Settings_rateControlMode' - Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
--
-- 'numberReferenceFrames', 'h265Settings_numberReferenceFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
--
-- 'telecine', 'h265Settings_telecine' - This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
--
-- 'dynamicSubGop', 'h265Settings_dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
--
-- 'minIInterval', 'h265Settings_minIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- 'interlaceMode', 'h265Settings_interlaceMode' - Choose the scan line type for the output. Keep the default value,
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
-- 'parControl', 'h265Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
--
-- 'scanTypeConversionMode', 'h265Settings_scanTypeConversionMode' - Use this setting for interlaced outputs, when your output frame rate is
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
-- 'flickerAdaptiveQuantization', 'h265Settings_flickerAdaptiveQuantization' - Enable this setting to have the encoder reduce I-frame pop. I-frame pop
-- appears as a visual flicker that can arise when the encoder saves bits
-- by copying some macroblocks many times from frame to frame, and then
-- refreshes them at the I-frame. When you enable this setting, the encoder
-- updates these macroblocks slightly more often to smooth out the flicker.
-- This setting is disabled by default. Related setting: In addition to
-- enabling this setting, you must also set adaptiveQuantization to a value
-- other than Off (OFF).
--
-- 'qvbrSettings', 'h265Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- 'sampleAdaptiveOffsetFilterMode', 'h265Settings_sampleAdaptiveOffsetFilterMode' - Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
-- dynamically selects best strength based on content
--
-- 'codecProfile', 'h265Settings_codecProfile' - Represents the Profile and Tier, per the HEVC (H.265) specification.
-- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
-- represents Main Profile with High Tier. 4:2:2 profiles are only
-- available with the HEVC 4:2:2 License.
--
-- 'bitrate', 'h265Settings_bitrate' - Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
--
-- 'framerateDenominator', 'h265Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateConversionAlgorithm', 'h265Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'codecLevel', 'h265Settings_codecLevel' - H.265 Level.
--
-- 'framerateControl', 'h265Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
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
-- 'writeMp4PackagingType', 'h265Settings_writeMp4PackagingType' - If the location of parameter set NAL units doesn\'t matter in your
-- workflow, ignore this setting. Use this setting only with CMAF or DASH
-- outputs, or with standalone file outputs in an MPEG-4 container (MP4
-- outputs). Choose HVC1 to mark your output as HVC1. This makes your
-- output compliant with the following specification: ISO IECJTC1 SC29
-- N13798 Text ISO\/IEC FDIS 14496-15 3rd Edition. For these outputs, the
-- service stores parameter set NAL units in the sample headers but not in
-- the samples directly. For MP4 outputs, when you choose HVC1, your output
-- video might not work properly with some downstream systems and video
-- players. The service defaults to marking your output as HEV1. For these
-- outputs, the service writes parameter set NAL units directly into the
-- samples.
--
-- 'adaptiveQuantization', 'h265Settings_adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Flicker adaptive quantization (flickerAdaptiveQuantization),
-- Spatial adaptive quantization (spatialAdaptiveQuantization), and
-- Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- 'framerateNumerator', 'h265Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'gopBReference', 'h265Settings_gopBReference' - If enable, use reference B frames for GOP structures that have B frames
-- > 1.
--
-- 'maxBitrate', 'h265Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'gopClosedCadence', 'h265Settings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'parDenominator', 'h265Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'spatialAdaptiveQuantization', 'h265Settings_spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within
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
newH265Settings ::
  H265Settings
newH265Settings =
  H265Settings'
    { unregisteredSeiTimecode =
        Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      hrdBufferInitialFillPercentage = Prelude.Nothing,
      tiles = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      temporalIds = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      numberBFramesBetweenReferenceFrames =
        Prelude.Nothing,
      gopSizeUnits = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      slices = Prelude.Nothing,
      alternateTransferFunctionSei = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      numberReferenceFrames = Prelude.Nothing,
      telecine = Prelude.Nothing,
      dynamicSubGop = Prelude.Nothing,
      minIInterval = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      parControl = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      flickerAdaptiveQuantization = Prelude.Nothing,
      qvbrSettings = Prelude.Nothing,
      sampleAdaptiveOffsetFilterMode = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      codecLevel = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      writeMp4PackagingType = Prelude.Nothing,
      adaptiveQuantization = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing
    }

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
h265Settings_unregisteredSeiTimecode :: Lens.Lens' H265Settings (Prelude.Maybe H265UnregisteredSeiTimecode)
h265Settings_unregisteredSeiTimecode = Lens.lens (\H265Settings' {unregisteredSeiTimecode} -> unregisteredSeiTimecode) (\s@H265Settings' {} a -> s {unregisteredSeiTimecode = a} :: H265Settings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
h265Settings_qualityTuningLevel :: Lens.Lens' H265Settings (Prelude.Maybe H265QualityTuningLevel)
h265Settings_qualityTuningLevel = Lens.lens (\H265Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@H265Settings' {} a -> s {qualityTuningLevel = a} :: H265Settings)

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
h265Settings_temporalAdaptiveQuantization :: Lens.Lens' H265Settings (Prelude.Maybe H265TemporalAdaptiveQuantization)
h265Settings_temporalAdaptiveQuantization = Lens.lens (\H265Settings' {temporalAdaptiveQuantization} -> temporalAdaptiveQuantization) (\s@H265Settings' {} a -> s {temporalAdaptiveQuantization = a} :: H265Settings)

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
h265Settings_sceneChangeDetect :: Lens.Lens' H265Settings (Prelude.Maybe H265SceneChangeDetect)
h265Settings_sceneChangeDetect = Lens.lens (\H265Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H265Settings' {} a -> s {sceneChangeDetect = a} :: H265Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h265Settings_hrdBufferInitialFillPercentage :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_hrdBufferInitialFillPercentage = Lens.lens (\H265Settings' {hrdBufferInitialFillPercentage} -> hrdBufferInitialFillPercentage) (\s@H265Settings' {} a -> s {hrdBufferInitialFillPercentage = a} :: H265Settings)

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision
-- of the encoded pictures.
h265Settings_tiles :: Lens.Lens' H265Settings (Prelude.Maybe H265Tiles)
h265Settings_tiles = Lens.lens (\H265Settings' {tiles} -> tiles) (\s@H265Settings' {} a -> s {tiles = a} :: H265Settings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
h265Settings_slowPal :: Lens.Lens' H265Settings (Prelude.Maybe H265SlowPal)
h265Settings_slowPal = Lens.lens (\H265Settings' {slowPal} -> slowPal) (\s@H265Settings' {} a -> s {slowPal = a} :: H265Settings)

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3
-- layers are supported depending on GOP structure: I- and P-frames form
-- one layer, reference B-frames can form a second layer and non-reference
-- b-frames can form a third layer. Decoders can optionally decode only the
-- lower temporal layers to generate a lower frame rate output. For
-- example, given a bitstream with temporal IDs and with b-frames = 1 (i.e.
-- IbPbPb display order), a decoder could decode all the frames for full
-- frame rate output or only the I and P frames (lowest temporal layer) for
-- a half frame rate output.
h265Settings_temporalIds :: Lens.Lens' H265Settings (Prelude.Maybe H265TemporalIds)
h265Settings_temporalIds = Lens.lens (\H265Settings' {temporalIds} -> temporalIds) (\s@H265Settings' {} a -> s {temporalIds = a} :: H265Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
h265Settings_parNumerator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_parNumerator = Lens.lens (\H265Settings' {parNumerator} -> parNumerator) (\s@H265Settings' {} a -> s {parNumerator = a} :: H265Settings)

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater
-- than zero.
h265Settings_gopSize :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Double)
h265Settings_gopSize = Lens.lens (\H265Settings' {gopSize} -> gopSize) (\s@H265Settings' {} a -> s {gopSize = a} :: H265Settings)

-- | Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
h265Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\H265Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@H265Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: H265Settings)

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
h265Settings_gopSizeUnits :: Lens.Lens' H265Settings (Prelude.Maybe H265GopSizeUnits)
h265Settings_gopSizeUnits = Lens.lens (\H265Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H265Settings' {} a -> s {gopSizeUnits = a} :: H265Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
h265Settings_hrdBufferSize :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_hrdBufferSize = Lens.lens (\H265Settings' {hrdBufferSize} -> hrdBufferSize) (\s@H265Settings' {} a -> s {hrdBufferSize = a} :: H265Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
h265Settings_slices :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_slices = Lens.lens (\H265Settings' {slices} -> slices) (\s@H265Settings' {} a -> s {slices = a} :: H265Settings)

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid
-- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
h265Settings_alternateTransferFunctionSei :: Lens.Lens' H265Settings (Prelude.Maybe H265AlternateTransferFunctionSei)
h265Settings_alternateTransferFunctionSei = Lens.lens (\H265Settings' {alternateTransferFunctionSei} -> alternateTransferFunctionSei) (\s@H265Settings' {} a -> s {alternateTransferFunctionSei = a} :: H265Settings)

-- | Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
h265Settings_rateControlMode :: Lens.Lens' H265Settings (Prelude.Maybe H265RateControlMode)
h265Settings_rateControlMode = Lens.lens (\H265Settings' {rateControlMode} -> rateControlMode) (\s@H265Settings' {} a -> s {rateControlMode = a} :: H265Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h265Settings_numberReferenceFrames :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_numberReferenceFrames = Lens.lens (\H265Settings' {numberReferenceFrames} -> numberReferenceFrames) (\s@H265Settings' {} a -> s {numberReferenceFrames = a} :: H265Settings)

-- | This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
h265Settings_telecine :: Lens.Lens' H265Settings (Prelude.Maybe H265Telecine)
h265Settings_telecine = Lens.lens (\H265Settings' {telecine} -> telecine) (\s@H265Settings' {} a -> s {telecine = a} :: H265Settings)

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
h265Settings_dynamicSubGop :: Lens.Lens' H265Settings (Prelude.Maybe H265DynamicSubGop)
h265Settings_dynamicSubGop = Lens.lens (\H265Settings' {dynamicSubGop} -> dynamicSubGop) (\s@H265Settings' {} a -> s {dynamicSubGop = a} :: H265Settings)

-- | Enforces separation between repeated (cadence) I-frames and I-frames
-- inserted by Scene Change Detection. If a scene change I-frame is within
-- I-interval frames of a cadence I-frame, the GOP is shrunk and\/or
-- stretched to the scene change I-frame. GOP stretch requires enabling
-- lookahead as well as setting I-interval. The normal cadence resumes for
-- the next GOP. This setting is only used when Scene Change Detect is
-- enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
h265Settings_minIInterval :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_minIInterval = Lens.lens (\H265Settings' {minIInterval} -> minIInterval) (\s@H265Settings' {} a -> s {minIInterval = a} :: H265Settings)

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
h265Settings_interlaceMode :: Lens.Lens' H265Settings (Prelude.Maybe H265InterlaceMode)
h265Settings_interlaceMode = Lens.lens (\H265Settings' {interlaceMode} -> interlaceMode) (\s@H265Settings' {} a -> s {interlaceMode = a} :: H265Settings)

-- | Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
h265Settings_parControl :: Lens.Lens' H265Settings (Prelude.Maybe H265ParControl)
h265Settings_parControl = Lens.lens (\H265Settings' {parControl} -> parControl) (\s@H265Settings' {} a -> s {parControl = a} :: H265Settings)

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
h265Settings_scanTypeConversionMode :: Lens.Lens' H265Settings (Prelude.Maybe H265ScanTypeConversionMode)
h265Settings_scanTypeConversionMode = Lens.lens (\H265Settings' {scanTypeConversionMode} -> scanTypeConversionMode) (\s@H265Settings' {} a -> s {scanTypeConversionMode = a} :: H265Settings)

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop
-- appears as a visual flicker that can arise when the encoder saves bits
-- by copying some macroblocks many times from frame to frame, and then
-- refreshes them at the I-frame. When you enable this setting, the encoder
-- updates these macroblocks slightly more often to smooth out the flicker.
-- This setting is disabled by default. Related setting: In addition to
-- enabling this setting, you must also set adaptiveQuantization to a value
-- other than Off (OFF).
h265Settings_flickerAdaptiveQuantization :: Lens.Lens' H265Settings (Prelude.Maybe H265FlickerAdaptiveQuantization)
h265Settings_flickerAdaptiveQuantization = Lens.lens (\H265Settings' {flickerAdaptiveQuantization} -> flickerAdaptiveQuantization) (\s@H265Settings' {} a -> s {flickerAdaptiveQuantization = a} :: H265Settings)

-- | Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
h265Settings_qvbrSettings :: Lens.Lens' H265Settings (Prelude.Maybe H265QvbrSettings)
h265Settings_qvbrSettings = Lens.lens (\H265Settings' {qvbrSettings} -> qvbrSettings) (\s@H265Settings' {} a -> s {qvbrSettings = a} :: H265Settings)

-- | Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
-- dynamically selects best strength based on content
h265Settings_sampleAdaptiveOffsetFilterMode :: Lens.Lens' H265Settings (Prelude.Maybe H265SampleAdaptiveOffsetFilterMode)
h265Settings_sampleAdaptiveOffsetFilterMode = Lens.lens (\H265Settings' {sampleAdaptiveOffsetFilterMode} -> sampleAdaptiveOffsetFilterMode) (\s@H265Settings' {} a -> s {sampleAdaptiveOffsetFilterMode = a} :: H265Settings)

-- | Represents the Profile and Tier, per the HEVC (H.265) specification.
-- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
-- represents Main Profile with High Tier. 4:2:2 profiles are only
-- available with the HEVC 4:2:2 License.
h265Settings_codecProfile :: Lens.Lens' H265Settings (Prelude.Maybe H265CodecProfile)
h265Settings_codecProfile = Lens.lens (\H265Settings' {codecProfile} -> codecProfile) (\s@H265Settings' {} a -> s {codecProfile = a} :: H265Settings)

-- | Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
h265Settings_bitrate :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_bitrate = Lens.lens (\H265Settings' {bitrate} -> bitrate) (\s@H265Settings' {} a -> s {bitrate = a} :: H265Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h265Settings_framerateDenominator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_framerateDenominator = Lens.lens (\H265Settings' {framerateDenominator} -> framerateDenominator) (\s@H265Settings' {} a -> s {framerateDenominator = a} :: H265Settings)

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
h265Settings_framerateConversionAlgorithm :: Lens.Lens' H265Settings (Prelude.Maybe H265FramerateConversionAlgorithm)
h265Settings_framerateConversionAlgorithm = Lens.lens (\H265Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@H265Settings' {} a -> s {framerateConversionAlgorithm = a} :: H265Settings)

-- | H.265 Level.
h265Settings_codecLevel :: Lens.Lens' H265Settings (Prelude.Maybe H265CodecLevel)
h265Settings_codecLevel = Lens.lens (\H265Settings' {codecLevel} -> codecLevel) (\s@H265Settings' {} a -> s {codecLevel = a} :: H265Settings)

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
h265Settings_framerateControl :: Lens.Lens' H265Settings (Prelude.Maybe H265FramerateControl)
h265Settings_framerateControl = Lens.lens (\H265Settings' {framerateControl} -> framerateControl) (\s@H265Settings' {} a -> s {framerateControl = a} :: H265Settings)

-- | If the location of parameter set NAL units doesn\'t matter in your
-- workflow, ignore this setting. Use this setting only with CMAF or DASH
-- outputs, or with standalone file outputs in an MPEG-4 container (MP4
-- outputs). Choose HVC1 to mark your output as HVC1. This makes your
-- output compliant with the following specification: ISO IECJTC1 SC29
-- N13798 Text ISO\/IEC FDIS 14496-15 3rd Edition. For these outputs, the
-- service stores parameter set NAL units in the sample headers but not in
-- the samples directly. For MP4 outputs, when you choose HVC1, your output
-- video might not work properly with some downstream systems and video
-- players. The service defaults to marking your output as HEV1. For these
-- outputs, the service writes parameter set NAL units directly into the
-- samples.
h265Settings_writeMp4PackagingType :: Lens.Lens' H265Settings (Prelude.Maybe H265WriteMp4PackagingType)
h265Settings_writeMp4PackagingType = Lens.lens (\H265Settings' {writeMp4PackagingType} -> writeMp4PackagingType) (\s@H265Settings' {} a -> s {writeMp4PackagingType = a} :: H265Settings)

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Flicker adaptive quantization (flickerAdaptiveQuantization),
-- Spatial adaptive quantization (spatialAdaptiveQuantization), and
-- Temporal adaptive quantization (temporalAdaptiveQuantization).
h265Settings_adaptiveQuantization :: Lens.Lens' H265Settings (Prelude.Maybe H265AdaptiveQuantization)
h265Settings_adaptiveQuantization = Lens.lens (\H265Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@H265Settings' {} a -> s {adaptiveQuantization = a} :: H265Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h265Settings_framerateNumerator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_framerateNumerator = Lens.lens (\H265Settings' {framerateNumerator} -> framerateNumerator) (\s@H265Settings' {} a -> s {framerateNumerator = a} :: H265Settings)

-- | If enable, use reference B frames for GOP structures that have B frames
-- > 1.
h265Settings_gopBReference :: Lens.Lens' H265Settings (Prelude.Maybe H265GopBReference)
h265Settings_gopBReference = Lens.lens (\H265Settings' {gopBReference} -> gopBReference) (\s@H265Settings' {} a -> s {gopBReference = a} :: H265Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
h265Settings_maxBitrate :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_maxBitrate = Lens.lens (\H265Settings' {maxBitrate} -> maxBitrate) (\s@H265Settings' {} a -> s {maxBitrate = a} :: H265Settings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
h265Settings_gopClosedCadence :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_gopClosedCadence = Lens.lens (\H265Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H265Settings' {} a -> s {gopClosedCadence = a} :: H265Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
h265Settings_parDenominator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_parDenominator = Lens.lens (\H265Settings' {parDenominator} -> parDenominator) (\s@H265Settings' {} a -> s {parDenominator = a} :: H265Settings)

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
h265Settings_spatialAdaptiveQuantization :: Lens.Lens' H265Settings (Prelude.Maybe H265SpatialAdaptiveQuantization)
h265Settings_spatialAdaptiveQuantization = Lens.lens (\H265Settings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@H265Settings' {} a -> s {spatialAdaptiveQuantization = a} :: H265Settings)

instance Core.FromJSON H265Settings where
  parseJSON =
    Core.withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            Prelude.<$> (x Core..:? "unregisteredSeiTimecode")
            Prelude.<*> (x Core..:? "qualityTuningLevel")
            Prelude.<*> (x Core..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Core..:? "sceneChangeDetect")
            Prelude.<*> (x Core..:? "hrdBufferInitialFillPercentage")
            Prelude.<*> (x Core..:? "tiles")
            Prelude.<*> (x Core..:? "slowPal")
            Prelude.<*> (x Core..:? "temporalIds")
            Prelude.<*> (x Core..:? "parNumerator")
            Prelude.<*> (x Core..:? "gopSize")
            Prelude.<*> (x Core..:? "numberBFramesBetweenReferenceFrames")
            Prelude.<*> (x Core..:? "gopSizeUnits")
            Prelude.<*> (x Core..:? "hrdBufferSize")
            Prelude.<*> (x Core..:? "slices")
            Prelude.<*> (x Core..:? "alternateTransferFunctionSei")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "numberReferenceFrames")
            Prelude.<*> (x Core..:? "telecine")
            Prelude.<*> (x Core..:? "dynamicSubGop")
            Prelude.<*> (x Core..:? "minIInterval")
            Prelude.<*> (x Core..:? "interlaceMode")
            Prelude.<*> (x Core..:? "parControl")
            Prelude.<*> (x Core..:? "scanTypeConversionMode")
            Prelude.<*> (x Core..:? "flickerAdaptiveQuantization")
            Prelude.<*> (x Core..:? "qvbrSettings")
            Prelude.<*> (x Core..:? "sampleAdaptiveOffsetFilterMode")
            Prelude.<*> (x Core..:? "codecProfile")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "codecLevel")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "writeMp4PackagingType")
            Prelude.<*> (x Core..:? "adaptiveQuantization")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "gopBReference")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "gopClosedCadence")
            Prelude.<*> (x Core..:? "parDenominator")
            Prelude.<*> (x Core..:? "spatialAdaptiveQuantization")
      )

instance Prelude.Hashable H265Settings where
  hashWithSalt salt' H265Settings' {..} =
    salt'
      `Prelude.hashWithSalt` spatialAdaptiveQuantization
      `Prelude.hashWithSalt` parDenominator
      `Prelude.hashWithSalt` gopClosedCadence
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` gopBReference
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` writeMp4PackagingType
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` codecLevel
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codecProfile
      `Prelude.hashWithSalt` sampleAdaptiveOffsetFilterMode
      `Prelude.hashWithSalt` qvbrSettings
      `Prelude.hashWithSalt` flickerAdaptiveQuantization
      `Prelude.hashWithSalt` scanTypeConversionMode
      `Prelude.hashWithSalt` parControl
      `Prelude.hashWithSalt` interlaceMode
      `Prelude.hashWithSalt` minIInterval
      `Prelude.hashWithSalt` dynamicSubGop
      `Prelude.hashWithSalt` telecine
      `Prelude.hashWithSalt` numberReferenceFrames
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` alternateTransferFunctionSei
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` hrdBufferSize
      `Prelude.hashWithSalt` gopSizeUnits
      `Prelude.hashWithSalt` numberBFramesBetweenReferenceFrames
      `Prelude.hashWithSalt` gopSize
      `Prelude.hashWithSalt` parNumerator
      `Prelude.hashWithSalt` temporalIds
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` tiles
      `Prelude.hashWithSalt` hrdBufferInitialFillPercentage
      `Prelude.hashWithSalt` sceneChangeDetect
      `Prelude.hashWithSalt` temporalAdaptiveQuantization
      `Prelude.hashWithSalt` qualityTuningLevel
      `Prelude.hashWithSalt` unregisteredSeiTimecode

instance Prelude.NFData H265Settings where
  rnf H265Settings' {..} =
    Prelude.rnf unregisteredSeiTimecode
      `Prelude.seq` Prelude.rnf spatialAdaptiveQuantization
      `Prelude.seq` Prelude.rnf parDenominator
      `Prelude.seq` Prelude.rnf gopClosedCadence
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf gopBReference
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf writeMp4PackagingType
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf codecLevel
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codecProfile
      `Prelude.seq` Prelude.rnf sampleAdaptiveOffsetFilterMode
      `Prelude.seq` Prelude.rnf qvbrSettings
      `Prelude.seq` Prelude.rnf flickerAdaptiveQuantization
      `Prelude.seq` Prelude.rnf scanTypeConversionMode
      `Prelude.seq` Prelude.rnf parControl
      `Prelude.seq` Prelude.rnf interlaceMode
      `Prelude.seq` Prelude.rnf minIInterval
      `Prelude.seq` Prelude.rnf dynamicSubGop
      `Prelude.seq` Prelude.rnf telecine
      `Prelude.seq` Prelude.rnf numberReferenceFrames
      `Prelude.seq` Prelude.rnf rateControlMode
      `Prelude.seq` Prelude.rnf alternateTransferFunctionSei
      `Prelude.seq` Prelude.rnf slices
      `Prelude.seq` Prelude.rnf hrdBufferSize
      `Prelude.seq` Prelude.rnf gopSizeUnits
      `Prelude.seq` Prelude.rnf numberBFramesBetweenReferenceFrames
      `Prelude.seq` Prelude.rnf gopSize
      `Prelude.seq` Prelude.rnf parNumerator
      `Prelude.seq` Prelude.rnf temporalIds
      `Prelude.seq` Prelude.rnf slowPal
      `Prelude.seq` Prelude.rnf tiles
      `Prelude.seq` Prelude.rnf hrdBufferInitialFillPercentage
      `Prelude.seq` Prelude.rnf sceneChangeDetect
      `Prelude.seq` Prelude.rnf temporalAdaptiveQuantization
      `Prelude.seq` Prelude.rnf qualityTuningLevel

instance Core.ToJSON H265Settings where
  toJSON H265Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unregisteredSeiTimecode" Core..=)
              Prelude.<$> unregisteredSeiTimecode,
            ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel,
            ("temporalAdaptiveQuantization" Core..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("sceneChangeDetect" Core..=)
              Prelude.<$> sceneChangeDetect,
            ("hrdBufferInitialFillPercentage" Core..=)
              Prelude.<$> hrdBufferInitialFillPercentage,
            ("tiles" Core..=) Prelude.<$> tiles,
            ("slowPal" Core..=) Prelude.<$> slowPal,
            ("temporalIds" Core..=) Prelude.<$> temporalIds,
            ("parNumerator" Core..=) Prelude.<$> parNumerator,
            ("gopSize" Core..=) Prelude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Core..=)
              Prelude.<$> numberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" Core..=) Prelude.<$> gopSizeUnits,
            ("hrdBufferSize" Core..=) Prelude.<$> hrdBufferSize,
            ("slices" Core..=) Prelude.<$> slices,
            ("alternateTransferFunctionSei" Core..=)
              Prelude.<$> alternateTransferFunctionSei,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("numberReferenceFrames" Core..=)
              Prelude.<$> numberReferenceFrames,
            ("telecine" Core..=) Prelude.<$> telecine,
            ("dynamicSubGop" Core..=) Prelude.<$> dynamicSubGop,
            ("minIInterval" Core..=) Prelude.<$> minIInterval,
            ("interlaceMode" Core..=) Prelude.<$> interlaceMode,
            ("parControl" Core..=) Prelude.<$> parControl,
            ("scanTypeConversionMode" Core..=)
              Prelude.<$> scanTypeConversionMode,
            ("flickerAdaptiveQuantization" Core..=)
              Prelude.<$> flickerAdaptiveQuantization,
            ("qvbrSettings" Core..=) Prelude.<$> qvbrSettings,
            ("sampleAdaptiveOffsetFilterMode" Core..=)
              Prelude.<$> sampleAdaptiveOffsetFilterMode,
            ("codecProfile" Core..=) Prelude.<$> codecProfile,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("codecLevel" Core..=) Prelude.<$> codecLevel,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("writeMp4PackagingType" Core..=)
              Prelude.<$> writeMp4PackagingType,
            ("adaptiveQuantization" Core..=)
              Prelude.<$> adaptiveQuantization,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("gopBReference" Core..=) Prelude.<$> gopBReference,
            ("maxBitrate" Core..=) Prelude.<$> maxBitrate,
            ("gopClosedCadence" Core..=)
              Prelude.<$> gopClosedCadence,
            ("parDenominator" Core..=)
              Prelude.<$> parDenominator,
            ("spatialAdaptiveQuantization" Core..=)
              Prelude.<$> spatialAdaptiveQuantization
          ]
      )
