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
-- Module      : Amazonka.MediaLive.Types.H264Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AfdSignaling
import Amazonka.MediaLive.Types.FixedAfd
import Amazonka.MediaLive.Types.H264AdaptiveQuantization
import Amazonka.MediaLive.Types.H264ColorMetadata
import Amazonka.MediaLive.Types.H264ColorSpaceSettings
import Amazonka.MediaLive.Types.H264EntropyEncoding
import Amazonka.MediaLive.Types.H264FilterSettings
import Amazonka.MediaLive.Types.H264FlickerAq
import Amazonka.MediaLive.Types.H264ForceFieldPictures
import Amazonka.MediaLive.Types.H264FramerateControl
import Amazonka.MediaLive.Types.H264GopBReference
import Amazonka.MediaLive.Types.H264GopSizeUnits
import Amazonka.MediaLive.Types.H264Level
import Amazonka.MediaLive.Types.H264LookAheadRateControl
import Amazonka.MediaLive.Types.H264ParControl
import Amazonka.MediaLive.Types.H264Profile
import Amazonka.MediaLive.Types.H264QualityLevel
import Amazonka.MediaLive.Types.H264RateControlMode
import Amazonka.MediaLive.Types.H264ScanType
import Amazonka.MediaLive.Types.H264SceneChangeDetect
import Amazonka.MediaLive.Types.H264SpatialAq
import Amazonka.MediaLive.Types.H264SubGopLength
import Amazonka.MediaLive.Types.H264Syntax
import Amazonka.MediaLive.Types.H264TemporalAq
import Amazonka.MediaLive.Types.H264TimecodeInsertionBehavior
import Amazonka.MediaLive.Types.TimecodeBurninSettings
import qualified Amazonka.Prelude as Prelude

-- | H264 Settings
--
-- /See:/ 'newH264Settings' smart constructor.
data H264Settings = H264Settings'
  { -- | Enables or disables adaptive quantization, which is a technique
    -- MediaLive can apply to video on a frame-by-frame basis to produce more
    -- compression without losing quality. There are three types of adaptive
    -- quantization: flicker, spatial, and temporal. Set the field in one of
    -- these ways: Set to Auto. Recommended. For each type of AQ, MediaLive
    -- will determine if AQ is needed, and if so, the appropriate strength. Set
    -- a strength (a value other than Auto or Disable). This strength will
    -- apply to any of the AQ fields that you choose to enable. Set to Disabled
    -- to disable all types of adaptive quantization.
    adaptiveQuantization :: Prelude.Maybe H264AdaptiveQuantization,
    -- | Indicates that AFD values will be written into the output stream. If
    -- afdSignaling is \"auto\", the system will try to preserve the input AFD
    -- value (in cases where multiple AFD values are valid). If set to
    -- \"fixed\", the AFD value will be the value configured in the fixedAfd
    -- parameter.
    afdSignaling :: Prelude.Maybe AfdSignaling,
    -- | Average bitrate in bits\/second. Required when the rate control mode is
    -- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
    -- must have a unique value when its bitrate is rounded down to the nearest
    -- multiple of 1000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    bufFillPct :: Prelude.Maybe Prelude.Natural,
    -- | Size of buffer (HRD buffer model) in bits.
    bufSize :: Prelude.Maybe Prelude.Natural,
    -- | Includes colorspace metadata in the output.
    colorMetadata :: Prelude.Maybe H264ColorMetadata,
    -- | Color Space settings
    colorSpaceSettings :: Prelude.Maybe H264ColorSpaceSettings,
    -- | Entropy encoding mode. Use cabac (must be in Main or High profile) or
    -- cavlc.
    entropyEncoding :: Prelude.Maybe H264EntropyEncoding,
    -- | Optional filters that you can apply to an encode.
    filterSettings :: Prelude.Maybe H264FilterSettings,
    -- | Four bit AFD value to write on all frames of video in the output stream.
    -- Only valid when afdSignaling is set to \'Fixed\'.
    fixedAfd :: Prelude.Maybe FixedAfd,
    -- | Flicker AQ makes adjustments within each frame to reduce flicker or
    -- \'pop\' on I-frames. The value to enter in this field depends on the
    -- value in the Adaptive quantization field: If you have set the Adaptive
    -- quantization field to Auto, MediaLive ignores any value in this field.
    -- MediaLive will determine if flicker AQ is appropriate and will apply the
    -- appropriate strength. If you have set the Adaptive quantization field to
    -- a strength, you can set this field to Enabled or Disabled. Enabled:
    -- MediaLive will apply flicker AQ using the specified strength. Disabled:
    -- MediaLive won\'t apply flicker AQ. If you have set the Adaptive
    -- quantization to Disabled, MediaLive ignores any value in this field and
    -- doesn\'t apply flicker AQ.
    flickerAq :: Prelude.Maybe H264FlickerAq,
    -- | This setting applies only when scan type is \"interlaced.\" It controls
    -- whether coding is performed on a field basis or on a frame basis. (When
    -- the video is progressive, the coding is always performed on a frame
    -- basis.) enabled: Force MediaLive to code on a field basis, so that odd
    -- and even sets of fields are coded separately. disabled: Code the two
    -- sets of fields separately (on a field basis) or together (on a frame
    -- basis using PAFF), depending on what is most appropriate for the
    -- content.
    forceFieldPictures :: Prelude.Maybe H264ForceFieldPictures,
    -- | This field indicates how the output video frame rate is specified. If
    -- \"specified\" is selected then the output video frame rate is determined
    -- by framerateNumerator and framerateDenominator, else if
    -- \"initializeFromSource\" is selected then the output video frame rate
    -- will be set equal to the input video frame rate of the first input.
    framerateControl :: Prelude.Maybe H264FramerateControl,
    -- | Framerate denominator.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
    -- 23.976 fps.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Documentation update needed
    gopBReference :: Prelude.Maybe H264GopBReference,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended
    -- that this be set to 1 so a decoder joining mid-stream will receive an
    -- IDR frame as quickly as possible. Setting this value to 0 will break
    -- output segmenting.
    gopClosedCadence :: Prelude.Maybe Prelude.Natural,
    -- | Number of B-frames between reference frames.
    gopNumBFrames :: Prelude.Maybe Prelude.Natural,
    -- | GOP size (keyframe interval) in units of either frames or seconds per
    -- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
    -- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
    -- must be greater than 0, but need not be an integer.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Indicates if the gopSize is specified in frames or seconds. If seconds
    -- the system will convert the gopSize into a frame count at run time.
    gopSizeUnits :: Prelude.Maybe H264GopSizeUnits,
    -- | H.264 Level.
    level :: Prelude.Maybe H264Level,
    -- | Amount of lookahead. A value of low can decrease latency and memory
    -- usage, while high can produce better quality for certain content.
    lookAheadRateControl :: Prelude.Maybe H264LookAheadRateControl,
    -- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum
    -- bitrate in order to accommodate expected spikes in the complexity of the
    -- video.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Only meaningful if sceneChangeDetect is set to enabled. Defaults to 5 if
    -- multiplex rate control is used. Enforces separation between repeated
    -- (cadence) I-frames and I-frames inserted by Scene Change Detection. If a
    -- scene change I-frame is within I-interval frames of a cadence I-frame,
    -- the GOP is shrunk and\/or stretched to the scene change I-frame. GOP
    -- stretch requires enabling lookahead as well as setting I-interval. The
    -- normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP
    -- size + Min-I-interval - 1
    minIInterval :: Prelude.Maybe Prelude.Natural,
    -- | Number of reference frames to use. The encoder may use more than
    -- requested if using B-frames and\/or interlaced encoding.
    numRefFrames :: Prelude.Maybe Prelude.Natural,
    -- | This field indicates how the output pixel aspect ratio is specified. If
    -- \"specified\" is selected then the output video pixel aspect ratio is
    -- determined by parNumerator and parDenominator, else if
    -- \"initializeFromSource\" is selected then the output pixsel aspect ratio
    -- will be set equal to the input video pixel aspect ratio of the first
    -- input.
    parControl :: Prelude.Maybe H264ParControl,
    -- | Pixel Aspect Ratio denominator.
    parDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Pixel Aspect Ratio numerator.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | H.264 Profile.
    profile :: Prelude.Maybe H264Profile,
    -- | Leave as STANDARD_QUALITY or choose a different value (which might
    -- result in additional costs to run the channel). - ENHANCED_QUALITY:
    -- Produces a slightly better video quality without an increase in the
    -- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
    -- If this channel is in a MediaLive multiplex, the value must be
    -- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
    qualityLevel :: Prelude.Maybe H264QualityLevel,
    -- | Controls the target quality for the video encode. Applies only when the
    -- rate control mode is QVBR. You can set a target quality or you can let
    -- MediaLive determine the best quality. To set a target quality, enter
    -- values in the QVBR quality level field and the Max bitrate field. Enter
    -- values that suit your most important viewing devices. Recommended values
    -- are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or
    -- tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality
    -- level: 6. Max bitrate: 1M to 1.5M To let MediaLive decide, leave the
    -- QVBR quality level field empty, and in Max bitrate enter the maximum
    -- rate you want in the video. For more information, see the section called
    -- \"Video - rate control mode\" in the MediaLive user guide
    qvbrQualityLevel :: Prelude.Maybe Prelude.Natural,
    -- | Rate control mode. QVBR: Quality will match the specified quality level
    -- except when it is constrained by the maximum bitrate. Recommended if you
    -- or your viewers pay for bandwidth. VBR: Quality and bitrate vary,
    -- depending on the video complexity. Recommended instead of QVBR if you
    -- want to maintain a specific average bitrate over the duration of the
    -- channel. CBR: Quality varies, depending on the video complexity.
    -- Recommended only if you distribute your assets to devices that cannot
    -- handle variable bitrates. Multiplex: This rate control mode is only
    -- supported (and is required) when the video is being delivered to a
    -- MediaLive Multiplex in which case the rate control configuration is
    -- controlled by the properties within the Multiplex Program.
    rateControlMode :: Prelude.Maybe H264RateControlMode,
    -- | Sets the scan type of the output to progressive or top-field-first
    -- interlaced.
    scanType :: Prelude.Maybe H264ScanType,
    -- | Scene change detection. - On: inserts I-frames when scene change is
    -- detected. - Off: does not force an I-frame when scene change is
    -- detected.
    sceneChangeDetect :: Prelude.Maybe H264SceneChangeDetect,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures. This field
    -- is optional; when no value is specified the encoder will choose the
    -- number of slices based on encode resolution.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Softness. Selects quantizer matrix, larger values reduce high-frequency
    -- content in the encoded image. If not set to zero, must be greater than
    -- 15.
    softness :: Prelude.Maybe Prelude.Natural,
    -- | Spatial AQ makes adjustments within each frame based on spatial
    -- variation of content complexity. The value to enter in this field
    -- depends on the value in the Adaptive quantization field: If you have set
    -- the Adaptive quantization field to Auto, MediaLive ignores any value in
    -- this field. MediaLive will determine if spatial AQ is appropriate and
    -- will apply the appropriate strength. If you have set the Adaptive
    -- quantization field to a strength, you can set this field to Enabled or
    -- Disabled. Enabled: MediaLive will apply spatial AQ using the specified
    -- strength. Disabled: MediaLive won\'t apply spatial AQ. If you have set
    -- the Adaptive quantization to Disabled, MediaLive ignores any value in
    -- this field and doesn\'t apply spatial AQ.
    spatialAq :: Prelude.Maybe H264SpatialAq,
    -- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
    -- dynamic, optimize the number of B-frames used for each sub-GOP to
    -- improve visual quality.
    subgopLength :: Prelude.Maybe H264SubGopLength,
    -- | Produces a bitstream compliant with SMPTE RP-2027.
    syntax :: Prelude.Maybe H264Syntax,
    -- | Temporal makes adjustments within each frame based on temporal variation
    -- of content complexity. The value to enter in this field depends on the
    -- value in the Adaptive quantization field: If you have set the Adaptive
    -- quantization field to Auto, MediaLive ignores any value in this field.
    -- MediaLive will determine if temporal AQ is appropriate and will apply
    -- the appropriate strength. If you have set the Adaptive quantization
    -- field to a strength, you can set this field to Enabled or Disabled.
    -- Enabled: MediaLive will apply temporal AQ using the specified strength.
    -- Disabled: MediaLive won\'t apply temporal AQ. If you have set the
    -- Adaptive quantization to Disabled, MediaLive ignores any value in this
    -- field and doesn\'t apply temporal AQ.
    temporalAq :: Prelude.Maybe H264TemporalAq,
    -- | Timecode burn-in settings
    timecodeBurninSettings :: Prelude.Maybe TimecodeBurninSettings,
    -- | Determines how timecodes should be inserted into the video elementary
    -- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
    -- Pass through picture timing SEI messages from the source specified in
    -- Timecode Config
    timecodeInsertion :: Prelude.Maybe H264TimecodeInsertionBehavior
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
-- 'adaptiveQuantization', 'h264Settings_adaptiveQuantization' - Enables or disables adaptive quantization, which is a technique
-- MediaLive can apply to video on a frame-by-frame basis to produce more
-- compression without losing quality. There are three types of adaptive
-- quantization: flicker, spatial, and temporal. Set the field in one of
-- these ways: Set to Auto. Recommended. For each type of AQ, MediaLive
-- will determine if AQ is needed, and if so, the appropriate strength. Set
-- a strength (a value other than Auto or Disable). This strength will
-- apply to any of the AQ fields that you choose to enable. Set to Disabled
-- to disable all types of adaptive quantization.
--
-- 'afdSignaling', 'h264Settings_afdSignaling' - Indicates that AFD values will be written into the output stream. If
-- afdSignaling is \"auto\", the system will try to preserve the input AFD
-- value (in cases where multiple AFD values are valid). If set to
-- \"fixed\", the AFD value will be the value configured in the fixedAfd
-- parameter.
--
-- 'bitrate', 'h264Settings_bitrate' - Average bitrate in bits\/second. Required when the rate control mode is
-- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
-- must have a unique value when its bitrate is rounded down to the nearest
-- multiple of 1000.
--
-- 'bufFillPct', 'h264Settings_bufFillPct' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'bufSize', 'h264Settings_bufSize' - Size of buffer (HRD buffer model) in bits.
--
-- 'colorMetadata', 'h264Settings_colorMetadata' - Includes colorspace metadata in the output.
--
-- 'colorSpaceSettings', 'h264Settings_colorSpaceSettings' - Color Space settings
--
-- 'entropyEncoding', 'h264Settings_entropyEncoding' - Entropy encoding mode. Use cabac (must be in Main or High profile) or
-- cavlc.
--
-- 'filterSettings', 'h264Settings_filterSettings' - Optional filters that you can apply to an encode.
--
-- 'fixedAfd', 'h264Settings_fixedAfd' - Four bit AFD value to write on all frames of video in the output stream.
-- Only valid when afdSignaling is set to \'Fixed\'.
--
-- 'flickerAq', 'h264Settings_flickerAq' - Flicker AQ makes adjustments within each frame to reduce flicker or
-- \'pop\' on I-frames. The value to enter in this field depends on the
-- value in the Adaptive quantization field: If you have set the Adaptive
-- quantization field to Auto, MediaLive ignores any value in this field.
-- MediaLive will determine if flicker AQ is appropriate and will apply the
-- appropriate strength. If you have set the Adaptive quantization field to
-- a strength, you can set this field to Enabled or Disabled. Enabled:
-- MediaLive will apply flicker AQ using the specified strength. Disabled:
-- MediaLive won\'t apply flicker AQ. If you have set the Adaptive
-- quantization to Disabled, MediaLive ignores any value in this field and
-- doesn\'t apply flicker AQ.
--
-- 'forceFieldPictures', 'h264Settings_forceFieldPictures' - This setting applies only when scan type is \"interlaced.\" It controls
-- whether coding is performed on a field basis or on a frame basis. (When
-- the video is progressive, the coding is always performed on a frame
-- basis.) enabled: Force MediaLive to code on a field basis, so that odd
-- and even sets of fields are coded separately. disabled: Code the two
-- sets of fields separately (on a field basis) or together (on a frame
-- basis using PAFF), depending on what is most appropriate for the
-- content.
--
-- 'framerateControl', 'h264Settings_framerateControl' - This field indicates how the output video frame rate is specified. If
-- \"specified\" is selected then the output video frame rate is determined
-- by framerateNumerator and framerateDenominator, else if
-- \"initializeFromSource\" is selected then the output video frame rate
-- will be set equal to the input video frame rate of the first input.
--
-- 'framerateDenominator', 'h264Settings_framerateDenominator' - Framerate denominator.
--
-- 'framerateNumerator', 'h264Settings_framerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
-- 23.976 fps.
--
-- 'gopBReference', 'h264Settings_gopBReference' - Documentation update needed
--
-- 'gopClosedCadence', 'h264Settings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'gopNumBFrames', 'h264Settings_gopNumBFrames' - Number of B-frames between reference frames.
--
-- 'gopSize', 'h264Settings_gopSize' - GOP size (keyframe interval) in units of either frames or seconds per
-- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
-- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
-- must be greater than 0, but need not be an integer.
--
-- 'gopSizeUnits', 'h264Settings_gopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds
-- the system will convert the gopSize into a frame count at run time.
--
-- 'level', 'h264Settings_level' - H.264 Level.
--
-- 'lookAheadRateControl', 'h264Settings_lookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory
-- usage, while high can produce better quality for certain content.
--
-- 'maxBitrate', 'h264Settings_maxBitrate' - For QVBR: See the tooltip for Quality level For VBR: Set the maximum
-- bitrate in order to accommodate expected spikes in the complexity of the
-- video.
--
-- 'minIInterval', 'h264Settings_minIInterval' - Only meaningful if sceneChangeDetect is set to enabled. Defaults to 5 if
-- multiplex rate control is used. Enforces separation between repeated
-- (cadence) I-frames and I-frames inserted by Scene Change Detection. If a
-- scene change I-frame is within I-interval frames of a cadence I-frame,
-- the GOP is shrunk and\/or stretched to the scene change I-frame. GOP
-- stretch requires enabling lookahead as well as setting I-interval. The
-- normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP
-- size + Min-I-interval - 1
--
-- 'numRefFrames', 'h264Settings_numRefFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
--
-- 'parControl', 'h264Settings_parControl' - This field indicates how the output pixel aspect ratio is specified. If
-- \"specified\" is selected then the output video pixel aspect ratio is
-- determined by parNumerator and parDenominator, else if
-- \"initializeFromSource\" is selected then the output pixsel aspect ratio
-- will be set equal to the input video pixel aspect ratio of the first
-- input.
--
-- 'parDenominator', 'h264Settings_parDenominator' - Pixel Aspect Ratio denominator.
--
-- 'parNumerator', 'h264Settings_parNumerator' - Pixel Aspect Ratio numerator.
--
-- 'profile', 'h264Settings_profile' - H.264 Profile.
--
-- 'qualityLevel', 'h264Settings_qualityLevel' - Leave as STANDARD_QUALITY or choose a different value (which might
-- result in additional costs to run the channel). - ENHANCED_QUALITY:
-- Produces a slightly better video quality without an increase in the
-- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
-- If this channel is in a MediaLive multiplex, the value must be
-- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- 'qvbrQualityLevel', 'h264Settings_qvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the
-- rate control mode is QVBR. You can set a target quality or you can let
-- MediaLive determine the best quality. To set a target quality, enter
-- values in the QVBR quality level field and the Max bitrate field. Enter
-- values that suit your most important viewing devices. Recommended values
-- are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or
-- tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality
-- level: 6. Max bitrate: 1M to 1.5M To let MediaLive decide, leave the
-- QVBR quality level field empty, and in Max bitrate enter the maximum
-- rate you want in the video. For more information, see the section called
-- \"Video - rate control mode\" in the MediaLive user guide
--
-- 'rateControlMode', 'h264Settings_rateControlMode' - Rate control mode. QVBR: Quality will match the specified quality level
-- except when it is constrained by the maximum bitrate. Recommended if you
-- or your viewers pay for bandwidth. VBR: Quality and bitrate vary,
-- depending on the video complexity. Recommended instead of QVBR if you
-- want to maintain a specific average bitrate over the duration of the
-- channel. CBR: Quality varies, depending on the video complexity.
-- Recommended only if you distribute your assets to devices that cannot
-- handle variable bitrates. Multiplex: This rate control mode is only
-- supported (and is required) when the video is being delivered to a
-- MediaLive Multiplex in which case the rate control configuration is
-- controlled by the properties within the Multiplex Program.
--
-- 'scanType', 'h264Settings_scanType' - Sets the scan type of the output to progressive or top-field-first
-- interlaced.
--
-- 'sceneChangeDetect', 'h264Settings_sceneChangeDetect' - Scene change detection. - On: inserts I-frames when scene change is
-- detected. - Off: does not force an I-frame when scene change is
-- detected.
--
-- 'slices', 'h264Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures. This field
-- is optional; when no value is specified the encoder will choose the
-- number of slices based on encode resolution.
--
-- 'softness', 'h264Settings_softness' - Softness. Selects quantizer matrix, larger values reduce high-frequency
-- content in the encoded image. If not set to zero, must be greater than
-- 15.
--
-- 'spatialAq', 'h264Settings_spatialAq' - Spatial AQ makes adjustments within each frame based on spatial
-- variation of content complexity. The value to enter in this field
-- depends on the value in the Adaptive quantization field: If you have set
-- the Adaptive quantization field to Auto, MediaLive ignores any value in
-- this field. MediaLive will determine if spatial AQ is appropriate and
-- will apply the appropriate strength. If you have set the Adaptive
-- quantization field to a strength, you can set this field to Enabled or
-- Disabled. Enabled: MediaLive will apply spatial AQ using the specified
-- strength. Disabled: MediaLive won\'t apply spatial AQ. If you have set
-- the Adaptive quantization to Disabled, MediaLive ignores any value in
-- this field and doesn\'t apply spatial AQ.
--
-- 'subgopLength', 'h264Settings_subgopLength' - If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
-- dynamic, optimize the number of B-frames used for each sub-GOP to
-- improve visual quality.
--
-- 'syntax', 'h264Settings_syntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- 'temporalAq', 'h264Settings_temporalAq' - Temporal makes adjustments within each frame based on temporal variation
-- of content complexity. The value to enter in this field depends on the
-- value in the Adaptive quantization field: If you have set the Adaptive
-- quantization field to Auto, MediaLive ignores any value in this field.
-- MediaLive will determine if temporal AQ is appropriate and will apply
-- the appropriate strength. If you have set the Adaptive quantization
-- field to a strength, you can set this field to Enabled or Disabled.
-- Enabled: MediaLive will apply temporal AQ using the specified strength.
-- Disabled: MediaLive won\'t apply temporal AQ. If you have set the
-- Adaptive quantization to Disabled, MediaLive ignores any value in this
-- field and doesn\'t apply temporal AQ.
--
-- 'timecodeBurninSettings', 'h264Settings_timecodeBurninSettings' - Timecode burn-in settings
--
-- 'timecodeInsertion', 'h264Settings_timecodeInsertion' - Determines how timecodes should be inserted into the video elementary
-- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
-- Pass through picture timing SEI messages from the source specified in
-- Timecode Config
newH264Settings ::
  H264Settings
newH264Settings =
  H264Settings'
    { adaptiveQuantization =
        Prelude.Nothing,
      afdSignaling = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      bufFillPct = Prelude.Nothing,
      bufSize = Prelude.Nothing,
      colorMetadata = Prelude.Nothing,
      colorSpaceSettings = Prelude.Nothing,
      entropyEncoding = Prelude.Nothing,
      filterSettings = Prelude.Nothing,
      fixedAfd = Prelude.Nothing,
      flickerAq = Prelude.Nothing,
      forceFieldPictures = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      gopNumBFrames = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      gopSizeUnits = Prelude.Nothing,
      level = Prelude.Nothing,
      lookAheadRateControl = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      minIInterval = Prelude.Nothing,
      numRefFrames = Prelude.Nothing,
      parControl = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      profile = Prelude.Nothing,
      qualityLevel = Prelude.Nothing,
      qvbrQualityLevel = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      scanType = Prelude.Nothing,
      sceneChangeDetect = Prelude.Nothing,
      slices = Prelude.Nothing,
      softness = Prelude.Nothing,
      spatialAq = Prelude.Nothing,
      subgopLength = Prelude.Nothing,
      syntax = Prelude.Nothing,
      temporalAq = Prelude.Nothing,
      timecodeBurninSettings = Prelude.Nothing,
      timecodeInsertion = Prelude.Nothing
    }

-- | Enables or disables adaptive quantization, which is a technique
-- MediaLive can apply to video on a frame-by-frame basis to produce more
-- compression without losing quality. There are three types of adaptive
-- quantization: flicker, spatial, and temporal. Set the field in one of
-- these ways: Set to Auto. Recommended. For each type of AQ, MediaLive
-- will determine if AQ is needed, and if so, the appropriate strength. Set
-- a strength (a value other than Auto or Disable). This strength will
-- apply to any of the AQ fields that you choose to enable. Set to Disabled
-- to disable all types of adaptive quantization.
h264Settings_adaptiveQuantization :: Lens.Lens' H264Settings (Prelude.Maybe H264AdaptiveQuantization)
h264Settings_adaptiveQuantization = Lens.lens (\H264Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@H264Settings' {} a -> s {adaptiveQuantization = a} :: H264Settings)

-- | Indicates that AFD values will be written into the output stream. If
-- afdSignaling is \"auto\", the system will try to preserve the input AFD
-- value (in cases where multiple AFD values are valid). If set to
-- \"fixed\", the AFD value will be the value configured in the fixedAfd
-- parameter.
h264Settings_afdSignaling :: Lens.Lens' H264Settings (Prelude.Maybe AfdSignaling)
h264Settings_afdSignaling = Lens.lens (\H264Settings' {afdSignaling} -> afdSignaling) (\s@H264Settings' {} a -> s {afdSignaling = a} :: H264Settings)

-- | Average bitrate in bits\/second. Required when the rate control mode is
-- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
-- must have a unique value when its bitrate is rounded down to the nearest
-- multiple of 1000.
h264Settings_bitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_bitrate = Lens.lens (\H264Settings' {bitrate} -> bitrate) (\s@H264Settings' {} a -> s {bitrate = a} :: H264Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h264Settings_bufFillPct :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_bufFillPct = Lens.lens (\H264Settings' {bufFillPct} -> bufFillPct) (\s@H264Settings' {} a -> s {bufFillPct = a} :: H264Settings)

-- | Size of buffer (HRD buffer model) in bits.
h264Settings_bufSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_bufSize = Lens.lens (\H264Settings' {bufSize} -> bufSize) (\s@H264Settings' {} a -> s {bufSize = a} :: H264Settings)

-- | Includes colorspace metadata in the output.
h264Settings_colorMetadata :: Lens.Lens' H264Settings (Prelude.Maybe H264ColorMetadata)
h264Settings_colorMetadata = Lens.lens (\H264Settings' {colorMetadata} -> colorMetadata) (\s@H264Settings' {} a -> s {colorMetadata = a} :: H264Settings)

-- | Color Space settings
h264Settings_colorSpaceSettings :: Lens.Lens' H264Settings (Prelude.Maybe H264ColorSpaceSettings)
h264Settings_colorSpaceSettings = Lens.lens (\H264Settings' {colorSpaceSettings} -> colorSpaceSettings) (\s@H264Settings' {} a -> s {colorSpaceSettings = a} :: H264Settings)

-- | Entropy encoding mode. Use cabac (must be in Main or High profile) or
-- cavlc.
h264Settings_entropyEncoding :: Lens.Lens' H264Settings (Prelude.Maybe H264EntropyEncoding)
h264Settings_entropyEncoding = Lens.lens (\H264Settings' {entropyEncoding} -> entropyEncoding) (\s@H264Settings' {} a -> s {entropyEncoding = a} :: H264Settings)

-- | Optional filters that you can apply to an encode.
h264Settings_filterSettings :: Lens.Lens' H264Settings (Prelude.Maybe H264FilterSettings)
h264Settings_filterSettings = Lens.lens (\H264Settings' {filterSettings} -> filterSettings) (\s@H264Settings' {} a -> s {filterSettings = a} :: H264Settings)

-- | Four bit AFD value to write on all frames of video in the output stream.
-- Only valid when afdSignaling is set to \'Fixed\'.
h264Settings_fixedAfd :: Lens.Lens' H264Settings (Prelude.Maybe FixedAfd)
h264Settings_fixedAfd = Lens.lens (\H264Settings' {fixedAfd} -> fixedAfd) (\s@H264Settings' {} a -> s {fixedAfd = a} :: H264Settings)

-- | Flicker AQ makes adjustments within each frame to reduce flicker or
-- \'pop\' on I-frames. The value to enter in this field depends on the
-- value in the Adaptive quantization field: If you have set the Adaptive
-- quantization field to Auto, MediaLive ignores any value in this field.
-- MediaLive will determine if flicker AQ is appropriate and will apply the
-- appropriate strength. If you have set the Adaptive quantization field to
-- a strength, you can set this field to Enabled or Disabled. Enabled:
-- MediaLive will apply flicker AQ using the specified strength. Disabled:
-- MediaLive won\'t apply flicker AQ. If you have set the Adaptive
-- quantization to Disabled, MediaLive ignores any value in this field and
-- doesn\'t apply flicker AQ.
h264Settings_flickerAq :: Lens.Lens' H264Settings (Prelude.Maybe H264FlickerAq)
h264Settings_flickerAq = Lens.lens (\H264Settings' {flickerAq} -> flickerAq) (\s@H264Settings' {} a -> s {flickerAq = a} :: H264Settings)

-- | This setting applies only when scan type is \"interlaced.\" It controls
-- whether coding is performed on a field basis or on a frame basis. (When
-- the video is progressive, the coding is always performed on a frame
-- basis.) enabled: Force MediaLive to code on a field basis, so that odd
-- and even sets of fields are coded separately. disabled: Code the two
-- sets of fields separately (on a field basis) or together (on a frame
-- basis using PAFF), depending on what is most appropriate for the
-- content.
h264Settings_forceFieldPictures :: Lens.Lens' H264Settings (Prelude.Maybe H264ForceFieldPictures)
h264Settings_forceFieldPictures = Lens.lens (\H264Settings' {forceFieldPictures} -> forceFieldPictures) (\s@H264Settings' {} a -> s {forceFieldPictures = a} :: H264Settings)

-- | This field indicates how the output video frame rate is specified. If
-- \"specified\" is selected then the output video frame rate is determined
-- by framerateNumerator and framerateDenominator, else if
-- \"initializeFromSource\" is selected then the output video frame rate
-- will be set equal to the input video frame rate of the first input.
h264Settings_framerateControl :: Lens.Lens' H264Settings (Prelude.Maybe H264FramerateControl)
h264Settings_framerateControl = Lens.lens (\H264Settings' {framerateControl} -> framerateControl) (\s@H264Settings' {} a -> s {framerateControl = a} :: H264Settings)

-- | Framerate denominator.
h264Settings_framerateDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateDenominator = Lens.lens (\H264Settings' {framerateDenominator} -> framerateDenominator) (\s@H264Settings' {} a -> s {framerateDenominator = a} :: H264Settings)

-- | Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
-- 23.976 fps.
h264Settings_framerateNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_framerateNumerator = Lens.lens (\H264Settings' {framerateNumerator} -> framerateNumerator) (\s@H264Settings' {} a -> s {framerateNumerator = a} :: H264Settings)

-- | Documentation update needed
h264Settings_gopBReference :: Lens.Lens' H264Settings (Prelude.Maybe H264GopBReference)
h264Settings_gopBReference = Lens.lens (\H264Settings' {gopBReference} -> gopBReference) (\s@H264Settings' {} a -> s {gopBReference = a} :: H264Settings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
h264Settings_gopClosedCadence :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_gopClosedCadence = Lens.lens (\H264Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H264Settings' {} a -> s {gopClosedCadence = a} :: H264Settings)

-- | Number of B-frames between reference frames.
h264Settings_gopNumBFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_gopNumBFrames = Lens.lens (\H264Settings' {gopNumBFrames} -> gopNumBFrames) (\s@H264Settings' {} a -> s {gopNumBFrames = a} :: H264Settings)

-- | GOP size (keyframe interval) in units of either frames or seconds per
-- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
-- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
-- must be greater than 0, but need not be an integer.
h264Settings_gopSize :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Double)
h264Settings_gopSize = Lens.lens (\H264Settings' {gopSize} -> gopSize) (\s@H264Settings' {} a -> s {gopSize = a} :: H264Settings)

-- | Indicates if the gopSize is specified in frames or seconds. If seconds
-- the system will convert the gopSize into a frame count at run time.
h264Settings_gopSizeUnits :: Lens.Lens' H264Settings (Prelude.Maybe H264GopSizeUnits)
h264Settings_gopSizeUnits = Lens.lens (\H264Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H264Settings' {} a -> s {gopSizeUnits = a} :: H264Settings)

-- | H.264 Level.
h264Settings_level :: Lens.Lens' H264Settings (Prelude.Maybe H264Level)
h264Settings_level = Lens.lens (\H264Settings' {level} -> level) (\s@H264Settings' {} a -> s {level = a} :: H264Settings)

-- | Amount of lookahead. A value of low can decrease latency and memory
-- usage, while high can produce better quality for certain content.
h264Settings_lookAheadRateControl :: Lens.Lens' H264Settings (Prelude.Maybe H264LookAheadRateControl)
h264Settings_lookAheadRateControl = Lens.lens (\H264Settings' {lookAheadRateControl} -> lookAheadRateControl) (\s@H264Settings' {} a -> s {lookAheadRateControl = a} :: H264Settings)

-- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum
-- bitrate in order to accommodate expected spikes in the complexity of the
-- video.
h264Settings_maxBitrate :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_maxBitrate = Lens.lens (\H264Settings' {maxBitrate} -> maxBitrate) (\s@H264Settings' {} a -> s {maxBitrate = a} :: H264Settings)

-- | Only meaningful if sceneChangeDetect is set to enabled. Defaults to 5 if
-- multiplex rate control is used. Enforces separation between repeated
-- (cadence) I-frames and I-frames inserted by Scene Change Detection. If a
-- scene change I-frame is within I-interval frames of a cadence I-frame,
-- the GOP is shrunk and\/or stretched to the scene change I-frame. GOP
-- stretch requires enabling lookahead as well as setting I-interval. The
-- normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP
-- size + Min-I-interval - 1
h264Settings_minIInterval :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_minIInterval = Lens.lens (\H264Settings' {minIInterval} -> minIInterval) (\s@H264Settings' {} a -> s {minIInterval = a} :: H264Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h264Settings_numRefFrames :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_numRefFrames = Lens.lens (\H264Settings' {numRefFrames} -> numRefFrames) (\s@H264Settings' {} a -> s {numRefFrames = a} :: H264Settings)

-- | This field indicates how the output pixel aspect ratio is specified. If
-- \"specified\" is selected then the output video pixel aspect ratio is
-- determined by parNumerator and parDenominator, else if
-- \"initializeFromSource\" is selected then the output pixsel aspect ratio
-- will be set equal to the input video pixel aspect ratio of the first
-- input.
h264Settings_parControl :: Lens.Lens' H264Settings (Prelude.Maybe H264ParControl)
h264Settings_parControl = Lens.lens (\H264Settings' {parControl} -> parControl) (\s@H264Settings' {} a -> s {parControl = a} :: H264Settings)

-- | Pixel Aspect Ratio denominator.
h264Settings_parDenominator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parDenominator = Lens.lens (\H264Settings' {parDenominator} -> parDenominator) (\s@H264Settings' {} a -> s {parDenominator = a} :: H264Settings)

-- | Pixel Aspect Ratio numerator.
h264Settings_parNumerator :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_parNumerator = Lens.lens (\H264Settings' {parNumerator} -> parNumerator) (\s@H264Settings' {} a -> s {parNumerator = a} :: H264Settings)

-- | H.264 Profile.
h264Settings_profile :: Lens.Lens' H264Settings (Prelude.Maybe H264Profile)
h264Settings_profile = Lens.lens (\H264Settings' {profile} -> profile) (\s@H264Settings' {} a -> s {profile = a} :: H264Settings)

-- | Leave as STANDARD_QUALITY or choose a different value (which might
-- result in additional costs to run the channel). - ENHANCED_QUALITY:
-- Produces a slightly better video quality without an increase in the
-- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
-- If this channel is in a MediaLive multiplex, the value must be
-- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
h264Settings_qualityLevel :: Lens.Lens' H264Settings (Prelude.Maybe H264QualityLevel)
h264Settings_qualityLevel = Lens.lens (\H264Settings' {qualityLevel} -> qualityLevel) (\s@H264Settings' {} a -> s {qualityLevel = a} :: H264Settings)

-- | Controls the target quality for the video encode. Applies only when the
-- rate control mode is QVBR. You can set a target quality or you can let
-- MediaLive determine the best quality. To set a target quality, enter
-- values in the QVBR quality level field and the Max bitrate field. Enter
-- values that suit your most important viewing devices. Recommended values
-- are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or
-- tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality
-- level: 6. Max bitrate: 1M to 1.5M To let MediaLive decide, leave the
-- QVBR quality level field empty, and in Max bitrate enter the maximum
-- rate you want in the video. For more information, see the section called
-- \"Video - rate control mode\" in the MediaLive user guide
h264Settings_qvbrQualityLevel :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_qvbrQualityLevel = Lens.lens (\H264Settings' {qvbrQualityLevel} -> qvbrQualityLevel) (\s@H264Settings' {} a -> s {qvbrQualityLevel = a} :: H264Settings)

-- | Rate control mode. QVBR: Quality will match the specified quality level
-- except when it is constrained by the maximum bitrate. Recommended if you
-- or your viewers pay for bandwidth. VBR: Quality and bitrate vary,
-- depending on the video complexity. Recommended instead of QVBR if you
-- want to maintain a specific average bitrate over the duration of the
-- channel. CBR: Quality varies, depending on the video complexity.
-- Recommended only if you distribute your assets to devices that cannot
-- handle variable bitrates. Multiplex: This rate control mode is only
-- supported (and is required) when the video is being delivered to a
-- MediaLive Multiplex in which case the rate control configuration is
-- controlled by the properties within the Multiplex Program.
h264Settings_rateControlMode :: Lens.Lens' H264Settings (Prelude.Maybe H264RateControlMode)
h264Settings_rateControlMode = Lens.lens (\H264Settings' {rateControlMode} -> rateControlMode) (\s@H264Settings' {} a -> s {rateControlMode = a} :: H264Settings)

-- | Sets the scan type of the output to progressive or top-field-first
-- interlaced.
h264Settings_scanType :: Lens.Lens' H264Settings (Prelude.Maybe H264ScanType)
h264Settings_scanType = Lens.lens (\H264Settings' {scanType} -> scanType) (\s@H264Settings' {} a -> s {scanType = a} :: H264Settings)

-- | Scene change detection. - On: inserts I-frames when scene change is
-- detected. - Off: does not force an I-frame when scene change is
-- detected.
h264Settings_sceneChangeDetect :: Lens.Lens' H264Settings (Prelude.Maybe H264SceneChangeDetect)
h264Settings_sceneChangeDetect = Lens.lens (\H264Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H264Settings' {} a -> s {sceneChangeDetect = a} :: H264Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures. This field
-- is optional; when no value is specified the encoder will choose the
-- number of slices based on encode resolution.
h264Settings_slices :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_slices = Lens.lens (\H264Settings' {slices} -> slices) (\s@H264Settings' {} a -> s {slices = a} :: H264Settings)

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency
-- content in the encoded image. If not set to zero, must be greater than
-- 15.
h264Settings_softness :: Lens.Lens' H264Settings (Prelude.Maybe Prelude.Natural)
h264Settings_softness = Lens.lens (\H264Settings' {softness} -> softness) (\s@H264Settings' {} a -> s {softness = a} :: H264Settings)

-- | Spatial AQ makes adjustments within each frame based on spatial
-- variation of content complexity. The value to enter in this field
-- depends on the value in the Adaptive quantization field: If you have set
-- the Adaptive quantization field to Auto, MediaLive ignores any value in
-- this field. MediaLive will determine if spatial AQ is appropriate and
-- will apply the appropriate strength. If you have set the Adaptive
-- quantization field to a strength, you can set this field to Enabled or
-- Disabled. Enabled: MediaLive will apply spatial AQ using the specified
-- strength. Disabled: MediaLive won\'t apply spatial AQ. If you have set
-- the Adaptive quantization to Disabled, MediaLive ignores any value in
-- this field and doesn\'t apply spatial AQ.
h264Settings_spatialAq :: Lens.Lens' H264Settings (Prelude.Maybe H264SpatialAq)
h264Settings_spatialAq = Lens.lens (\H264Settings' {spatialAq} -> spatialAq) (\s@H264Settings' {} a -> s {spatialAq = a} :: H264Settings)

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
-- dynamic, optimize the number of B-frames used for each sub-GOP to
-- improve visual quality.
h264Settings_subgopLength :: Lens.Lens' H264Settings (Prelude.Maybe H264SubGopLength)
h264Settings_subgopLength = Lens.lens (\H264Settings' {subgopLength} -> subgopLength) (\s@H264Settings' {} a -> s {subgopLength = a} :: H264Settings)

-- | Produces a bitstream compliant with SMPTE RP-2027.
h264Settings_syntax :: Lens.Lens' H264Settings (Prelude.Maybe H264Syntax)
h264Settings_syntax = Lens.lens (\H264Settings' {syntax} -> syntax) (\s@H264Settings' {} a -> s {syntax = a} :: H264Settings)

-- | Temporal makes adjustments within each frame based on temporal variation
-- of content complexity. The value to enter in this field depends on the
-- value in the Adaptive quantization field: If you have set the Adaptive
-- quantization field to Auto, MediaLive ignores any value in this field.
-- MediaLive will determine if temporal AQ is appropriate and will apply
-- the appropriate strength. If you have set the Adaptive quantization
-- field to a strength, you can set this field to Enabled or Disabled.
-- Enabled: MediaLive will apply temporal AQ using the specified strength.
-- Disabled: MediaLive won\'t apply temporal AQ. If you have set the
-- Adaptive quantization to Disabled, MediaLive ignores any value in this
-- field and doesn\'t apply temporal AQ.
h264Settings_temporalAq :: Lens.Lens' H264Settings (Prelude.Maybe H264TemporalAq)
h264Settings_temporalAq = Lens.lens (\H264Settings' {temporalAq} -> temporalAq) (\s@H264Settings' {} a -> s {temporalAq = a} :: H264Settings)

-- | Timecode burn-in settings
h264Settings_timecodeBurninSettings :: Lens.Lens' H264Settings (Prelude.Maybe TimecodeBurninSettings)
h264Settings_timecodeBurninSettings = Lens.lens (\H264Settings' {timecodeBurninSettings} -> timecodeBurninSettings) (\s@H264Settings' {} a -> s {timecodeBurninSettings = a} :: H264Settings)

-- | Determines how timecodes should be inserted into the video elementary
-- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
-- Pass through picture timing SEI messages from the source specified in
-- Timecode Config
h264Settings_timecodeInsertion :: Lens.Lens' H264Settings (Prelude.Maybe H264TimecodeInsertionBehavior)
h264Settings_timecodeInsertion = Lens.lens (\H264Settings' {timecodeInsertion} -> timecodeInsertion) (\s@H264Settings' {} a -> s {timecodeInsertion = a} :: H264Settings)

instance Data.FromJSON H264Settings where
  parseJSON =
    Data.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Prelude.<$> (x Data..:? "adaptiveQuantization")
            Prelude.<*> (x Data..:? "afdSignaling")
            Prelude.<*> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "bufFillPct")
            Prelude.<*> (x Data..:? "bufSize")
            Prelude.<*> (x Data..:? "colorMetadata")
            Prelude.<*> (x Data..:? "colorSpaceSettings")
            Prelude.<*> (x Data..:? "entropyEncoding")
            Prelude.<*> (x Data..:? "filterSettings")
            Prelude.<*> (x Data..:? "fixedAfd")
            Prelude.<*> (x Data..:? "flickerAq")
            Prelude.<*> (x Data..:? "forceFieldPictures")
            Prelude.<*> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "gopBReference")
            Prelude.<*> (x Data..:? "gopClosedCadence")
            Prelude.<*> (x Data..:? "gopNumBFrames")
            Prelude.<*> (x Data..:? "gopSize")
            Prelude.<*> (x Data..:? "gopSizeUnits")
            Prelude.<*> (x Data..:? "level")
            Prelude.<*> (x Data..:? "lookAheadRateControl")
            Prelude.<*> (x Data..:? "maxBitrate")
            Prelude.<*> (x Data..:? "minIInterval")
            Prelude.<*> (x Data..:? "numRefFrames")
            Prelude.<*> (x Data..:? "parControl")
            Prelude.<*> (x Data..:? "parDenominator")
            Prelude.<*> (x Data..:? "parNumerator")
            Prelude.<*> (x Data..:? "profile")
            Prelude.<*> (x Data..:? "qualityLevel")
            Prelude.<*> (x Data..:? "qvbrQualityLevel")
            Prelude.<*> (x Data..:? "rateControlMode")
            Prelude.<*> (x Data..:? "scanType")
            Prelude.<*> (x Data..:? "sceneChangeDetect")
            Prelude.<*> (x Data..:? "slices")
            Prelude.<*> (x Data..:? "softness")
            Prelude.<*> (x Data..:? "spatialAq")
            Prelude.<*> (x Data..:? "subgopLength")
            Prelude.<*> (x Data..:? "syntax")
            Prelude.<*> (x Data..:? "temporalAq")
            Prelude.<*> (x Data..:? "timecodeBurninSettings")
            Prelude.<*> (x Data..:? "timecodeInsertion")
      )

instance Prelude.Hashable H264Settings where
  hashWithSalt _salt H264Settings' {..} =
    _salt
      `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` afdSignaling
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` bufFillPct
      `Prelude.hashWithSalt` bufSize
      `Prelude.hashWithSalt` colorMetadata
      `Prelude.hashWithSalt` colorSpaceSettings
      `Prelude.hashWithSalt` entropyEncoding
      `Prelude.hashWithSalt` filterSettings
      `Prelude.hashWithSalt` fixedAfd
      `Prelude.hashWithSalt` flickerAq
      `Prelude.hashWithSalt` forceFieldPictures
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` gopBReference
      `Prelude.hashWithSalt` gopClosedCadence
      `Prelude.hashWithSalt` gopNumBFrames
      `Prelude.hashWithSalt` gopSize
      `Prelude.hashWithSalt` gopSizeUnits
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` lookAheadRateControl
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` minIInterval
      `Prelude.hashWithSalt` numRefFrames
      `Prelude.hashWithSalt` parControl
      `Prelude.hashWithSalt` parDenominator
      `Prelude.hashWithSalt` parNumerator
      `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` qualityLevel
      `Prelude.hashWithSalt` qvbrQualityLevel
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` scanType
      `Prelude.hashWithSalt` sceneChangeDetect
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` softness
      `Prelude.hashWithSalt` spatialAq
      `Prelude.hashWithSalt` subgopLength
      `Prelude.hashWithSalt` syntax
      `Prelude.hashWithSalt` temporalAq
      `Prelude.hashWithSalt` timecodeBurninSettings
      `Prelude.hashWithSalt` timecodeInsertion

instance Prelude.NFData H264Settings where
  rnf H264Settings' {..} =
    Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf afdSignaling
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf bufFillPct
      `Prelude.seq` Prelude.rnf bufSize
      `Prelude.seq` Prelude.rnf colorMetadata
      `Prelude.seq` Prelude.rnf colorSpaceSettings
      `Prelude.seq` Prelude.rnf entropyEncoding
      `Prelude.seq` Prelude.rnf filterSettings
      `Prelude.seq` Prelude.rnf fixedAfd
      `Prelude.seq` Prelude.rnf flickerAq
      `Prelude.seq` Prelude.rnf forceFieldPictures
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf gopBReference
      `Prelude.seq` Prelude.rnf gopClosedCadence
      `Prelude.seq` Prelude.rnf gopNumBFrames
      `Prelude.seq` Prelude.rnf gopSize
      `Prelude.seq` Prelude.rnf gopSizeUnits
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf
        lookAheadRateControl
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf
        minIInterval
      `Prelude.seq` Prelude.rnf
        numRefFrames
      `Prelude.seq` Prelude.rnf
        parControl
      `Prelude.seq` Prelude.rnf
        parDenominator
      `Prelude.seq` Prelude.rnf
        parNumerator
      `Prelude.seq` Prelude.rnf
        profile
      `Prelude.seq` Prelude.rnf
        qualityLevel
      `Prelude.seq` Prelude.rnf
        qvbrQualityLevel
      `Prelude.seq` Prelude.rnf
        rateControlMode
      `Prelude.seq` Prelude.rnf
        scanType
      `Prelude.seq` Prelude.rnf
        sceneChangeDetect
      `Prelude.seq` Prelude.rnf
        slices
      `Prelude.seq` Prelude.rnf
        softness
      `Prelude.seq` Prelude.rnf
        spatialAq
      `Prelude.seq` Prelude.rnf
        subgopLength
      `Prelude.seq` Prelude.rnf
        syntax
      `Prelude.seq` Prelude.rnf
        temporalAq
      `Prelude.seq` Prelude.rnf
        timecodeBurninSettings
      `Prelude.seq` Prelude.rnf
        timecodeInsertion

instance Data.ToJSON H264Settings where
  toJSON H264Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adaptiveQuantization" Data..=)
              Prelude.<$> adaptiveQuantization,
            ("afdSignaling" Data..=) Prelude.<$> afdSignaling,
            ("bitrate" Data..=) Prelude.<$> bitrate,
            ("bufFillPct" Data..=) Prelude.<$> bufFillPct,
            ("bufSize" Data..=) Prelude.<$> bufSize,
            ("colorMetadata" Data..=) Prelude.<$> colorMetadata,
            ("colorSpaceSettings" Data..=)
              Prelude.<$> colorSpaceSettings,
            ("entropyEncoding" Data..=)
              Prelude.<$> entropyEncoding,
            ("filterSettings" Data..=)
              Prelude.<$> filterSettings,
            ("fixedAfd" Data..=) Prelude.<$> fixedAfd,
            ("flickerAq" Data..=) Prelude.<$> flickerAq,
            ("forceFieldPictures" Data..=)
              Prelude.<$> forceFieldPictures,
            ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("gopBReference" Data..=) Prelude.<$> gopBReference,
            ("gopClosedCadence" Data..=)
              Prelude.<$> gopClosedCadence,
            ("gopNumBFrames" Data..=) Prelude.<$> gopNumBFrames,
            ("gopSize" Data..=) Prelude.<$> gopSize,
            ("gopSizeUnits" Data..=) Prelude.<$> gopSizeUnits,
            ("level" Data..=) Prelude.<$> level,
            ("lookAheadRateControl" Data..=)
              Prelude.<$> lookAheadRateControl,
            ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("minIInterval" Data..=) Prelude.<$> minIInterval,
            ("numRefFrames" Data..=) Prelude.<$> numRefFrames,
            ("parControl" Data..=) Prelude.<$> parControl,
            ("parDenominator" Data..=)
              Prelude.<$> parDenominator,
            ("parNumerator" Data..=) Prelude.<$> parNumerator,
            ("profile" Data..=) Prelude.<$> profile,
            ("qualityLevel" Data..=) Prelude.<$> qualityLevel,
            ("qvbrQualityLevel" Data..=)
              Prelude.<$> qvbrQualityLevel,
            ("rateControlMode" Data..=)
              Prelude.<$> rateControlMode,
            ("scanType" Data..=) Prelude.<$> scanType,
            ("sceneChangeDetect" Data..=)
              Prelude.<$> sceneChangeDetect,
            ("slices" Data..=) Prelude.<$> slices,
            ("softness" Data..=) Prelude.<$> softness,
            ("spatialAq" Data..=) Prelude.<$> spatialAq,
            ("subgopLength" Data..=) Prelude.<$> subgopLength,
            ("syntax" Data..=) Prelude.<$> syntax,
            ("temporalAq" Data..=) Prelude.<$> temporalAq,
            ("timecodeBurninSettings" Data..=)
              Prelude.<$> timecodeBurninSettings,
            ("timecodeInsertion" Data..=)
              Prelude.<$> timecodeInsertion
          ]
      )
