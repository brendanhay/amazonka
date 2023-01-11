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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | When you set Adaptive Quantization (H265AdaptiveQuantization) to Auto
    -- (AUTO), or leave blank, MediaConvert automatically applies quantization
    -- to improve the video quality of your output. Set Adaptive Quantization
    -- to Low (LOW), Medium (MEDIUM), High (HIGH), Higher (HIGHER), or Max
    -- (MAX) to manually control the strength of the quantization filter. When
    -- you do, you can specify a value for Spatial Adaptive Quantization
    -- (H265SpatialAdaptiveQuantization), Temporal Adaptive Quantization
    -- (H265TemporalAdaptiveQuantization), and Flicker Adaptive Quantization
    -- (H265FlickerAdaptiveQuantization), to further control the quantization
    -- filter. Set Adaptive Quantization to Off (OFF) to apply no quantization
    -- to your output.
    adaptiveQuantization :: Prelude.Maybe H265AdaptiveQuantization,
    -- | Enables Alternate Transfer Function SEI message for outputs using Hybrid
    -- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
    alternateTransferFunctionSei :: Prelude.Maybe H265AlternateTransferFunctionSei,
    -- | Specify the average bitrate in bits per second. Required for VBR and
    -- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
    -- the nearest multiple of 1000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | H.265 Level.
    codecLevel :: Prelude.Maybe H265CodecLevel,
    -- | Represents the Profile and Tier, per the HEVC (H.265) specification.
    -- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
    -- represents Main Profile with High Tier. 4:2:2 profiles are only
    -- available with the HEVC 4:2:2 License.
    codecProfile :: Prelude.Maybe H265CodecProfile,
    -- | Choose Adaptive to improve subjective video quality for high-motion
    -- content. This will cause the service to use fewer B-frames (which infer
    -- information based on other frames) for high-motion portions of the video
    -- and more B-frames for low-motion portions. The maximum number of
    -- B-frames is limited by the value you provide for the setting B frames
    -- between reference frames (numberBFramesBetweenReferenceFrames).
    dynamicSubGop :: Prelude.Maybe H265DynamicSubGop,
    -- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop
    -- appears as a visual flicker that can arise when the encoder saves bits
    -- by copying some macroblocks many times from frame to frame, and then
    -- refreshes them at the I-frame. When you enable this setting, the encoder
    -- updates these macroblocks slightly more often to smooth out the flicker.
    -- This setting is disabled by default. Related setting: In addition to
    -- enabling this setting, you must also set adaptiveQuantization to a value
    -- other than Off (OFF).
    flickerAdaptiveQuantization :: Prelude.Maybe H265FlickerAdaptiveQuantization,
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
    gopBReference :: Prelude.Maybe H265GopBReference,
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
    gopSizeUnits :: Prelude.Maybe H265GopSizeUnits,
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
    interlaceMode :: Prelude.Maybe H265InterlaceMode,
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
    -- | Specify the number of B-frames that MediaConvert puts between reference
    -- frames in this output. Valid values are whole numbers from 0 through 7.
    -- When you don\'t specify a value, MediaConvert defaults to 2.
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
    parControl :: Prelude.Maybe H265ParControl,
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
    qualityTuningLevel :: Prelude.Maybe H265QualityTuningLevel,
    -- | Settings for quality-defined variable bitrate encoding with the H.265
    -- codec. Use these settings only when you set QVBR for Rate control mode
    -- (RateControlMode).
    qvbrSettings :: Prelude.Maybe H265QvbrSettings,
    -- | Use this setting to specify whether this output has a variable bitrate
    -- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
    -- (QVBR).
    rateControlMode :: Prelude.Maybe H265RateControlMode,
    -- | Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
    -- dynamically selects best strength based on content
    sampleAdaptiveOffsetFilterMode :: Prelude.Maybe H265SampleAdaptiveOffsetFilterMode,
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
    -- | Enable this setting to insert I-frames at scene changes that the service
    -- automatically detects. This improves video quality and is enabled by
    -- default. If this output uses QVBR, choose Transition detection
    -- (TRANSITION_DETECTION) for further video quality improvement. For more
    -- information about QVBR, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
    sceneChangeDetect :: Prelude.Maybe H265SceneChangeDetect,
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
    slowPal :: Prelude.Maybe H265SlowPal,
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
    spatialAdaptiveQuantization :: Prelude.Maybe H265SpatialAdaptiveQuantization,
    -- | This field applies only if the Streams > Advanced > Framerate
    -- (framerate) field is set to 29.970. This field works with the Streams >
    -- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
    -- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
    -- the scan type for the output: Progressive, Interlaced, Hard Telecine or
    -- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
    -- produces 23.976; the player converts this output to 29.97i.
    telecine :: Prelude.Maybe H265Telecine,
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
    -- | Enable use of tiles, allowing horizontal as well as vertical subdivision
    -- of the encoded pictures.
    tiles :: Prelude.Maybe H265Tiles,
    -- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
    -- message.
    unregisteredSeiTimecode :: Prelude.Maybe H265UnregisteredSeiTimecode,
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
    writeMp4PackagingType :: Prelude.Maybe H265WriteMp4PackagingType
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
-- 'adaptiveQuantization', 'h265Settings_adaptiveQuantization' - When you set Adaptive Quantization (H265AdaptiveQuantization) to Auto
-- (AUTO), or leave blank, MediaConvert automatically applies quantization
-- to improve the video quality of your output. Set Adaptive Quantization
-- to Low (LOW), Medium (MEDIUM), High (HIGH), Higher (HIGHER), or Max
-- (MAX) to manually control the strength of the quantization filter. When
-- you do, you can specify a value for Spatial Adaptive Quantization
-- (H265SpatialAdaptiveQuantization), Temporal Adaptive Quantization
-- (H265TemporalAdaptiveQuantization), and Flicker Adaptive Quantization
-- (H265FlickerAdaptiveQuantization), to further control the quantization
-- filter. Set Adaptive Quantization to Off (OFF) to apply no quantization
-- to your output.
--
-- 'alternateTransferFunctionSei', 'h265Settings_alternateTransferFunctionSei' - Enables Alternate Transfer Function SEI message for outputs using Hybrid
-- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
--
-- 'bitrate', 'h265Settings_bitrate' - Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
--
-- 'codecLevel', 'h265Settings_codecLevel' - H.265 Level.
--
-- 'codecProfile', 'h265Settings_codecProfile' - Represents the Profile and Tier, per the HEVC (H.265) specification.
-- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
-- represents Main Profile with High Tier. 4:2:2 profiles are only
-- available with the HEVC 4:2:2 License.
--
-- 'dynamicSubGop', 'h265Settings_dynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
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
-- 'framerateDenominator', 'h265Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
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
-- 'gopClosedCadence', 'h265Settings_gopClosedCadence' - Specify the relative frequency of open to closed GOPs in this output.
-- For example, if you want to allow four open GOPs and then require a
-- closed GOP, set this value to 5. We recommend that you have the
-- transcoder automatically choose this value for you based on
-- characteristics of your input video. To enable this automatic behavior,
-- keep the default value by leaving this setting out of your JSON job
-- specification. In the console, do this by keeping the default empty
-- value. If you do explicitly specify a value, for segmented outputs,
-- don\'t set this value to 0.
--
-- 'gopSize', 'h265Settings_gopSize' - Use this setting only when you set GOP mode control (GopSizeUnits) to
-- Specified, frames (FRAMES) or Specified, seconds (SECONDS). Specify the
-- GOP length using a whole number of frames or a decimal value of seconds.
-- MediaConvert will interpret this value as frames or seconds depending on
-- the value you choose for GOP mode control (GopSizeUnits). If you want to
-- allow MediaConvert to automatically determine GOP size, leave GOP size
-- blank and set GOP mode control to Auto (AUTO). If your output group
-- specifies HLS, DASH, or CMAF, leave GOP size blank and set GOP mode
-- control to Auto in each output in your output group.
--
-- 'gopSizeUnits', 'h265Settings_gopSizeUnits' - Specify how the transcoder determines GOP size for this output. We
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
-- 'hrdBufferFinalFillPercentage', 'h265Settings_hrdBufferFinalFillPercentage' - If your downstream systems have strict buffer requirements: Specify the
-- minimum percentage of the HRD buffer that\'s available at the end of
-- each encoded video segment. For the best video quality: Set to 0 or
-- leave blank to automatically determine the final buffer fill percentage.
--
-- 'hrdBufferInitialFillPercentage', 'h265Settings_hrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'hrdBufferSize', 'h265Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
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
-- 'maxBitrate', 'h265Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'minIInterval', 'h265Settings_minIInterval' - Use this setting only when you also enable Scene change detection
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
-- 'numberBFramesBetweenReferenceFrames', 'h265Settings_numberBFramesBetweenReferenceFrames' - Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
--
-- 'numberReferenceFrames', 'h265Settings_numberReferenceFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
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
-- 'parDenominator', 'h265Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'parNumerator', 'h265Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'qualityTuningLevel', 'h265Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'qvbrSettings', 'h265Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- 'rateControlMode', 'h265Settings_rateControlMode' - Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
--
-- 'sampleAdaptiveOffsetFilterMode', 'h265Settings_sampleAdaptiveOffsetFilterMode' - Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
-- dynamically selects best strength based on content
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
-- 'sceneChangeDetect', 'h265Settings_sceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
--
-- 'slices', 'h265Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
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
--
-- 'telecine', 'h265Settings_telecine' - This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
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
-- 'tiles', 'h265Settings_tiles' - Enable use of tiles, allowing horizontal as well as vertical subdivision
-- of the encoded pictures.
--
-- 'unregisteredSeiTimecode', 'h265Settings_unregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
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
newH265Settings ::
  H265Settings
newH265Settings =
  H265Settings'
    { adaptiveQuantization =
        Prelude.Nothing,
      alternateTransferFunctionSei = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      codecLevel = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      dynamicSubGop = Prelude.Nothing,
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
      sampleAdaptiveOffsetFilterMode = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      slices = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing,
      telecine = Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      temporalIds = Prelude.Nothing,
      tiles = Prelude.Nothing,
      unregisteredSeiTimecode = Prelude.Nothing,
      writeMp4PackagingType = Prelude.Nothing
    }

-- | When you set Adaptive Quantization (H265AdaptiveQuantization) to Auto
-- (AUTO), or leave blank, MediaConvert automatically applies quantization
-- to improve the video quality of your output. Set Adaptive Quantization
-- to Low (LOW), Medium (MEDIUM), High (HIGH), Higher (HIGHER), or Max
-- (MAX) to manually control the strength of the quantization filter. When
-- you do, you can specify a value for Spatial Adaptive Quantization
-- (H265SpatialAdaptiveQuantization), Temporal Adaptive Quantization
-- (H265TemporalAdaptiveQuantization), and Flicker Adaptive Quantization
-- (H265FlickerAdaptiveQuantization), to further control the quantization
-- filter. Set Adaptive Quantization to Off (OFF) to apply no quantization
-- to your output.
h265Settings_adaptiveQuantization :: Lens.Lens' H265Settings (Prelude.Maybe H265AdaptiveQuantization)
h265Settings_adaptiveQuantization = Lens.lens (\H265Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@H265Settings' {} a -> s {adaptiveQuantization = a} :: H265Settings)

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid
-- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
h265Settings_alternateTransferFunctionSei :: Lens.Lens' H265Settings (Prelude.Maybe H265AlternateTransferFunctionSei)
h265Settings_alternateTransferFunctionSei = Lens.lens (\H265Settings' {alternateTransferFunctionSei} -> alternateTransferFunctionSei) (\s@H265Settings' {} a -> s {alternateTransferFunctionSei = a} :: H265Settings)

-- | Specify the average bitrate in bits per second. Required for VBR and
-- CBR. For MS Smooth outputs, bitrates must be unique when rounded down to
-- the nearest multiple of 1000.
h265Settings_bitrate :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_bitrate = Lens.lens (\H265Settings' {bitrate} -> bitrate) (\s@H265Settings' {} a -> s {bitrate = a} :: H265Settings)

-- | H.265 Level.
h265Settings_codecLevel :: Lens.Lens' H265Settings (Prelude.Maybe H265CodecLevel)
h265Settings_codecLevel = Lens.lens (\H265Settings' {codecLevel} -> codecLevel) (\s@H265Settings' {} a -> s {codecLevel = a} :: H265Settings)

-- | Represents the Profile and Tier, per the HEVC (H.265) specification.
-- Selections are grouped as [Profile] \/ [Tier], so \"Main\/High\"
-- represents Main Profile with High Tier. 4:2:2 profiles are only
-- available with the HEVC 4:2:2 License.
h265Settings_codecProfile :: Lens.Lens' H265Settings (Prelude.Maybe H265CodecProfile)
h265Settings_codecProfile = Lens.lens (\H265Settings' {codecProfile} -> codecProfile) (\s@H265Settings' {} a -> s {codecProfile = a} :: H265Settings)

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
h265Settings_dynamicSubGop :: Lens.Lens' H265Settings (Prelude.Maybe H265DynamicSubGop)
h265Settings_dynamicSubGop = Lens.lens (\H265Settings' {dynamicSubGop} -> dynamicSubGop) (\s@H265Settings' {} a -> s {dynamicSubGop = a} :: H265Settings)

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

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
h265Settings_framerateDenominator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_framerateDenominator = Lens.lens (\H265Settings' {framerateDenominator} -> framerateDenominator) (\s@H265Settings' {} a -> s {framerateDenominator = a} :: H265Settings)

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

-- | Specify the relative frequency of open to closed GOPs in this output.
-- For example, if you want to allow four open GOPs and then require a
-- closed GOP, set this value to 5. We recommend that you have the
-- transcoder automatically choose this value for you based on
-- characteristics of your input video. To enable this automatic behavior,
-- keep the default value by leaving this setting out of your JSON job
-- specification. In the console, do this by keeping the default empty
-- value. If you do explicitly specify a value, for segmented outputs,
-- don\'t set this value to 0.
h265Settings_gopClosedCadence :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_gopClosedCadence = Lens.lens (\H265Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H265Settings' {} a -> s {gopClosedCadence = a} :: H265Settings)

-- | Use this setting only when you set GOP mode control (GopSizeUnits) to
-- Specified, frames (FRAMES) or Specified, seconds (SECONDS). Specify the
-- GOP length using a whole number of frames or a decimal value of seconds.
-- MediaConvert will interpret this value as frames or seconds depending on
-- the value you choose for GOP mode control (GopSizeUnits). If you want to
-- allow MediaConvert to automatically determine GOP size, leave GOP size
-- blank and set GOP mode control to Auto (AUTO). If your output group
-- specifies HLS, DASH, or CMAF, leave GOP size blank and set GOP mode
-- control to Auto in each output in your output group.
h265Settings_gopSize :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Double)
h265Settings_gopSize = Lens.lens (\H265Settings' {gopSize} -> gopSize) (\s@H265Settings' {} a -> s {gopSize = a} :: H265Settings)

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
h265Settings_gopSizeUnits :: Lens.Lens' H265Settings (Prelude.Maybe H265GopSizeUnits)
h265Settings_gopSizeUnits = Lens.lens (\H265Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H265Settings' {} a -> s {gopSizeUnits = a} :: H265Settings)

-- | If your downstream systems have strict buffer requirements: Specify the
-- minimum percentage of the HRD buffer that\'s available at the end of
-- each encoded video segment. For the best video quality: Set to 0 or
-- leave blank to automatically determine the final buffer fill percentage.
h265Settings_hrdBufferFinalFillPercentage :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_hrdBufferFinalFillPercentage = Lens.lens (\H265Settings' {hrdBufferFinalFillPercentage} -> hrdBufferFinalFillPercentage) (\s@H265Settings' {} a -> s {hrdBufferFinalFillPercentage = a} :: H265Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h265Settings_hrdBufferInitialFillPercentage :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_hrdBufferInitialFillPercentage = Lens.lens (\H265Settings' {hrdBufferInitialFillPercentage} -> hrdBufferInitialFillPercentage) (\s@H265Settings' {} a -> s {hrdBufferInitialFillPercentage = a} :: H265Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
h265Settings_hrdBufferSize :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_hrdBufferSize = Lens.lens (\H265Settings' {hrdBufferSize} -> hrdBufferSize) (\s@H265Settings' {} a -> s {hrdBufferSize = a} :: H265Settings)

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

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
h265Settings_maxBitrate :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_maxBitrate = Lens.lens (\H265Settings' {maxBitrate} -> maxBitrate) (\s@H265Settings' {} a -> s {maxBitrate = a} :: H265Settings)

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
h265Settings_minIInterval :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_minIInterval = Lens.lens (\H265Settings' {minIInterval} -> minIInterval) (\s@H265Settings' {} a -> s {minIInterval = a} :: H265Settings)

-- | Specify the number of B-frames that MediaConvert puts between reference
-- frames in this output. Valid values are whole numbers from 0 through 7.
-- When you don\'t specify a value, MediaConvert defaults to 2.
h265Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\H265Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@H265Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: H265Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h265Settings_numberReferenceFrames :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_numberReferenceFrames = Lens.lens (\H265Settings' {numberReferenceFrames} -> numberReferenceFrames) (\s@H265Settings' {} a -> s {numberReferenceFrames = a} :: H265Settings)

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

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
h265Settings_parDenominator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_parDenominator = Lens.lens (\H265Settings' {parDenominator} -> parDenominator) (\s@H265Settings' {} a -> s {parDenominator = a} :: H265Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
h265Settings_parNumerator :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_parNumerator = Lens.lens (\H265Settings' {parNumerator} -> parNumerator) (\s@H265Settings' {} a -> s {parNumerator = a} :: H265Settings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
h265Settings_qualityTuningLevel :: Lens.Lens' H265Settings (Prelude.Maybe H265QualityTuningLevel)
h265Settings_qualityTuningLevel = Lens.lens (\H265Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@H265Settings' {} a -> s {qualityTuningLevel = a} :: H265Settings)

-- | Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
h265Settings_qvbrSettings :: Lens.Lens' H265Settings (Prelude.Maybe H265QvbrSettings)
h265Settings_qvbrSettings = Lens.lens (\H265Settings' {qvbrSettings} -> qvbrSettings) (\s@H265Settings' {} a -> s {qvbrSettings = a} :: H265Settings)

-- | Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
h265Settings_rateControlMode :: Lens.Lens' H265Settings (Prelude.Maybe H265RateControlMode)
h265Settings_rateControlMode = Lens.lens (\H265Settings' {rateControlMode} -> rateControlMode) (\s@H265Settings' {} a -> s {rateControlMode = a} :: H265Settings)

-- | Specify Sample Adaptive Offset (SAO) filter strength. Adaptive mode
-- dynamically selects best strength based on content
h265Settings_sampleAdaptiveOffsetFilterMode :: Lens.Lens' H265Settings (Prelude.Maybe H265SampleAdaptiveOffsetFilterMode)
h265Settings_sampleAdaptiveOffsetFilterMode = Lens.lens (\H265Settings' {sampleAdaptiveOffsetFilterMode} -> sampleAdaptiveOffsetFilterMode) (\s@H265Settings' {} a -> s {sampleAdaptiveOffsetFilterMode = a} :: H265Settings)

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

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
h265Settings_sceneChangeDetect :: Lens.Lens' H265Settings (Prelude.Maybe H265SceneChangeDetect)
h265Settings_sceneChangeDetect = Lens.lens (\H265Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H265Settings' {} a -> s {sceneChangeDetect = a} :: H265Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
h265Settings_slices :: Lens.Lens' H265Settings (Prelude.Maybe Prelude.Natural)
h265Settings_slices = Lens.lens (\H265Settings' {slices} -> slices) (\s@H265Settings' {} a -> s {slices = a} :: H265Settings)

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

-- | This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
h265Settings_telecine :: Lens.Lens' H265Settings (Prelude.Maybe H265Telecine)
h265Settings_telecine = Lens.lens (\H265Settings' {telecine} -> telecine) (\s@H265Settings' {} a -> s {telecine = a} :: H265Settings)

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

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision
-- of the encoded pictures.
h265Settings_tiles :: Lens.Lens' H265Settings (Prelude.Maybe H265Tiles)
h265Settings_tiles = Lens.lens (\H265Settings' {tiles} -> tiles) (\s@H265Settings' {} a -> s {tiles = a} :: H265Settings)

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
h265Settings_unregisteredSeiTimecode :: Lens.Lens' H265Settings (Prelude.Maybe H265UnregisteredSeiTimecode)
h265Settings_unregisteredSeiTimecode = Lens.lens (\H265Settings' {unregisteredSeiTimecode} -> unregisteredSeiTimecode) (\s@H265Settings' {} a -> s {unregisteredSeiTimecode = a} :: H265Settings)

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

instance Data.FromJSON H265Settings where
  parseJSON =
    Data.withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            Prelude.<$> (x Data..:? "adaptiveQuantization")
            Prelude.<*> (x Data..:? "alternateTransferFunctionSei")
            Prelude.<*> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "codecLevel")
            Prelude.<*> (x Data..:? "codecProfile")
            Prelude.<*> (x Data..:? "dynamicSubGop")
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
            Prelude.<*> (x Data..:? "sampleAdaptiveOffsetFilterMode")
            Prelude.<*> (x Data..:? "scanTypeConversionMode")
            Prelude.<*> (x Data..:? "sceneChangeDetect")
            Prelude.<*> (x Data..:? "slices")
            Prelude.<*> (x Data..:? "slowPal")
            Prelude.<*> (x Data..:? "spatialAdaptiveQuantization")
            Prelude.<*> (x Data..:? "telecine")
            Prelude.<*> (x Data..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Data..:? "temporalIds")
            Prelude.<*> (x Data..:? "tiles")
            Prelude.<*> (x Data..:? "unregisteredSeiTimecode")
            Prelude.<*> (x Data..:? "writeMp4PackagingType")
      )

instance Prelude.Hashable H265Settings where
  hashWithSalt _salt H265Settings' {..} =
    _salt `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` alternateTransferFunctionSei
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codecLevel
      `Prelude.hashWithSalt` codecProfile
      `Prelude.hashWithSalt` dynamicSubGop
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
      `Prelude.hashWithSalt` sampleAdaptiveOffsetFilterMode
      `Prelude.hashWithSalt` scanTypeConversionMode
      `Prelude.hashWithSalt` sceneChangeDetect
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` spatialAdaptiveQuantization
      `Prelude.hashWithSalt` telecine
      `Prelude.hashWithSalt` temporalAdaptiveQuantization
      `Prelude.hashWithSalt` temporalIds
      `Prelude.hashWithSalt` tiles
      `Prelude.hashWithSalt` unregisteredSeiTimecode
      `Prelude.hashWithSalt` writeMp4PackagingType

instance Prelude.NFData H265Settings where
  rnf H265Settings' {..} =
    Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf alternateTransferFunctionSei
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codecLevel
      `Prelude.seq` Prelude.rnf codecProfile
      `Prelude.seq` Prelude.rnf dynamicSubGop
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
      `Prelude.seq` Prelude.rnf parControl
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
        sampleAdaptiveOffsetFilterMode
      `Prelude.seq` Prelude.rnf
        scanTypeConversionMode
      `Prelude.seq` Prelude.rnf
        sceneChangeDetect
      `Prelude.seq` Prelude.rnf
        slices
      `Prelude.seq` Prelude.rnf
        slowPal
      `Prelude.seq` Prelude.rnf
        spatialAdaptiveQuantization
      `Prelude.seq` Prelude.rnf
        telecine
      `Prelude.seq` Prelude.rnf
        temporalAdaptiveQuantization
      `Prelude.seq` Prelude.rnf
        temporalIds
      `Prelude.seq` Prelude.rnf
        tiles
      `Prelude.seq` Prelude.rnf
        unregisteredSeiTimecode
      `Prelude.seq` Prelude.rnf
        writeMp4PackagingType

instance Data.ToJSON H265Settings where
  toJSON H265Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adaptiveQuantization" Data..=)
              Prelude.<$> adaptiveQuantization,
            ("alternateTransferFunctionSei" Data..=)
              Prelude.<$> alternateTransferFunctionSei,
            ("bitrate" Data..=) Prelude.<$> bitrate,
            ("codecLevel" Data..=) Prelude.<$> codecLevel,
            ("codecProfile" Data..=) Prelude.<$> codecProfile,
            ("dynamicSubGop" Data..=) Prelude.<$> dynamicSubGop,
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
            ("sampleAdaptiveOffsetFilterMode" Data..=)
              Prelude.<$> sampleAdaptiveOffsetFilterMode,
            ("scanTypeConversionMode" Data..=)
              Prelude.<$> scanTypeConversionMode,
            ("sceneChangeDetect" Data..=)
              Prelude.<$> sceneChangeDetect,
            ("slices" Data..=) Prelude.<$> slices,
            ("slowPal" Data..=) Prelude.<$> slowPal,
            ("spatialAdaptiveQuantization" Data..=)
              Prelude.<$> spatialAdaptiveQuantization,
            ("telecine" Data..=) Prelude.<$> telecine,
            ("temporalAdaptiveQuantization" Data..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("temporalIds" Data..=) Prelude.<$> temporalIds,
            ("tiles" Data..=) Prelude.<$> tiles,
            ("unregisteredSeiTimecode" Data..=)
              Prelude.<$> unregisteredSeiTimecode,
            ("writeMp4PackagingType" Data..=)
              Prelude.<$> writeMp4PackagingType
          ]
      )
