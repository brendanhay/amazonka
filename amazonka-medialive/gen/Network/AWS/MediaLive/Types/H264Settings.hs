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
-- Module      : Network.AWS.MediaLive.Types.H264Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.H264AdaptiveQuantization
import Network.AWS.MediaLive.Types.H264ColorMetadata
import Network.AWS.MediaLive.Types.H264ColorSpaceSettings
import Network.AWS.MediaLive.Types.H264EntropyEncoding
import Network.AWS.MediaLive.Types.H264FilterSettings
import Network.AWS.MediaLive.Types.H264FlickerAq
import Network.AWS.MediaLive.Types.H264ForceFieldPictures
import Network.AWS.MediaLive.Types.H264FramerateControl
import Network.AWS.MediaLive.Types.H264GopBReference
import Network.AWS.MediaLive.Types.H264GopSizeUnits
import Network.AWS.MediaLive.Types.H264Level
import Network.AWS.MediaLive.Types.H264LookAheadRateControl
import Network.AWS.MediaLive.Types.H264ParControl
import Network.AWS.MediaLive.Types.H264Profile
import Network.AWS.MediaLive.Types.H264QualityLevel
import Network.AWS.MediaLive.Types.H264RateControlMode
import Network.AWS.MediaLive.Types.H264ScanType
import Network.AWS.MediaLive.Types.H264SceneChangeDetect
import Network.AWS.MediaLive.Types.H264SpatialAq
import Network.AWS.MediaLive.Types.H264SubGopLength
import Network.AWS.MediaLive.Types.H264Syntax
import Network.AWS.MediaLive.Types.H264TemporalAq
import Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior

-- | H264 Settings
--
-- /See:/ 'newH264Settings' smart constructor.
data H264Settings = H264Settings'
  { -- | Sets the scan type of the output to progressive or top-field-first
    -- interlaced.
    scanType :: Core.Maybe H264ScanType,
    -- | If set to enabled, adjust quantization within each frame based on
    -- temporal variation of content complexity.
    temporalAq :: Core.Maybe H264TemporalAq,
    -- | Size of buffer (HRD buffer model) in bits.
    bufSize :: Core.Maybe Core.Natural,
    -- | If set to enabled, adjust quantization within each frame to reduce
    -- flicker or \'pop\' on I-frames.
    flickerAq :: Core.Maybe H264FlickerAq,
    -- | Documentation update needed
    gopBReference :: Core.Maybe H264GopBReference,
    -- | Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
    -- 23.976 fps.
    framerateNumerator :: Core.Maybe Core.Natural,
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
    rateControlMode :: Core.Maybe H264RateControlMode,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures. This field
    -- is optional; when no value is specified the encoder will choose the
    -- number of slices based on encode resolution.
    slices :: Core.Maybe Core.Natural,
    -- | Leave as STANDARD_QUALITY or choose a different value (which might
    -- result in additional costs to run the channel). - ENHANCED_QUALITY:
    -- Produces a slightly better video quality without an increase in the
    -- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
    -- If this channel is in a MediaLive multiplex, the value must be
    -- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
    qualityLevel :: Core.Maybe H264QualityLevel,
    -- | Includes colorspace metadata in the output.
    colorMetadata :: Core.Maybe H264ColorMetadata,
    -- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
    -- dynamic, optimize the number of B-frames used for each sub-GOP to
    -- improve visual quality.
    subgopLength :: Core.Maybe H264SubGopLength,
    -- | Entropy encoding mode. Use cabac (must be in Main or High profile) or
    -- cavlc.
    entropyEncoding :: Core.Maybe H264EntropyEncoding,
    -- | Indicates if the gopSize is specified in frames or seconds. If seconds
    -- the system will convert the gopSize into a frame count at run time.
    gopSizeUnits :: Core.Maybe H264GopSizeUnits,
    -- | GOP size (keyframe interval) in units of either frames or seconds per
    -- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
    -- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
    -- must be greater than 0, but need not be an integer.
    gopSize :: Core.Maybe Core.Double,
    -- | Framerate denominator.
    framerateDenominator :: Core.Maybe Core.Natural,
    -- | Four bit AFD value to write on all frames of video in the output stream.
    -- Only valid when afdSignaling is set to \'Fixed\'.
    fixedAfd :: Core.Maybe FixedAfd,
    -- | Softness. Selects quantizer matrix, larger values reduce high-frequency
    -- content in the encoded image.
    softness :: Core.Maybe Core.Natural,
    -- | Optional filters that you can apply to an encode.
    filterSettings :: Core.Maybe H264FilterSettings,
    -- | Pixel Aspect Ratio numerator.
    parNumerator :: Core.Maybe Core.Natural,
    -- | If set to enabled, adjust quantization within each frame based on
    -- spatial variation of content complexity.
    spatialAq :: Core.Maybe H264SpatialAq,
    -- | Number of B-frames between reference frames.
    gopNumBFrames :: Core.Maybe Core.Natural,
    -- | Scene change detection. - On: inserts I-frames when scene change is
    -- detected. - Off: does not force an I-frame when scene change is
    -- detected.
    sceneChangeDetect :: Core.Maybe H264SceneChangeDetect,
    -- | Determines how timecodes should be inserted into the video elementary
    -- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
    -- Pass through picture timing SEI messages from the source specified in
    -- Timecode Config
    timecodeInsertion :: Core.Maybe H264TimecodeInsertionBehavior,
    -- | Color Space settings
    colorSpaceSettings :: Core.Maybe H264ColorSpaceSettings,
    -- | Only meaningful if sceneChangeDetect is set to enabled. Defaults to 5 if
    -- multiplex rate control is used. Enforces separation between repeated
    -- (cadence) I-frames and I-frames inserted by Scene Change Detection. If a
    -- scene change I-frame is within I-interval frames of a cadence I-frame,
    -- the GOP is shrunk and\/or stretched to the scene change I-frame. GOP
    -- stretch requires enabling lookahead as well as setting I-interval. The
    -- normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP
    -- size + Min-I-interval - 1
    minIInterval :: Core.Maybe Core.Natural,
    -- | Controls the target quality for the video encode. Applies only when the
    -- rate control mode is QVBR. Set values for the QVBR quality level field
    -- and Max bitrate field that suit your most important viewing devices.
    -- Recommended values are: - Primary screen: Quality level: 8 to 10. Max
    -- bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M -
    -- Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
    qvbrQualityLevel :: Core.Maybe Core.Natural,
    -- | This field indicates how the output pixel aspect ratio is specified. If
    -- \"specified\" is selected then the output video pixel aspect ratio is
    -- determined by parNumerator and parDenominator, else if
    -- \"initializeFromSource\" is selected then the output pixsel aspect ratio
    -- will be set equal to the input video pixel aspect ratio of the first
    -- input.
    parControl :: Core.Maybe H264ParControl,
    -- | Percentage of the buffer that should initially be filled (HRD buffer
    -- model).
    bufFillPct :: Core.Maybe Core.Natural,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended
    -- that this be set to 1 so a decoder joining mid-stream will receive an
    -- IDR frame as quickly as possible. Setting this value to 0 will break
    -- output segmenting.
    gopClosedCadence :: Core.Maybe Core.Natural,
    -- | Pixel Aspect Ratio denominator.
    parDenominator :: Core.Maybe Core.Natural,
    -- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum
    -- bitrate in order to accommodate expected spikes in the complexity of the
    -- video.
    maxBitrate :: Core.Maybe Core.Natural,
    -- | Produces a bitstream compliant with SMPTE RP-2027.
    syntax :: Core.Maybe H264Syntax,
    -- | Number of reference frames to use. The encoder may use more than
    -- requested if using B-frames and\/or interlaced encoding.
    numRefFrames :: Core.Maybe Core.Natural,
    -- | H.264 Level.
    level :: Core.Maybe H264Level,
    -- | H.264 Profile.
    profile :: Core.Maybe H264Profile,
    -- | Adaptive quantization. Allows intra-frame quantizers to vary to improve
    -- visual quality.
    adaptiveQuantization :: Core.Maybe H264AdaptiveQuantization,
    -- | Amount of lookahead. A value of low can decrease latency and memory
    -- usage, while high can produce better quality for certain content.
    lookAheadRateControl :: Core.Maybe H264LookAheadRateControl,
    -- | This field indicates how the output video frame rate is specified. If
    -- \"specified\" is selected then the output video frame rate is determined
    -- by framerateNumerator and framerateDenominator, else if
    -- \"initializeFromSource\" is selected then the output video frame rate
    -- will be set equal to the input video frame rate of the first input.
    framerateControl :: Core.Maybe H264FramerateControl,
    -- | This setting applies only when scan type is \"interlaced.\" It controls
    -- whether coding is performed on a field basis or on a frame basis. (When
    -- the video is progressive, the coding is always performed on a frame
    -- basis.) enabled: Force MediaLive to code on a field basis, so that odd
    -- and even sets of fields are coded separately. disabled: Code the two
    -- sets of fields separately (on a field basis) or together (on a frame
    -- basis using PAFF), depending on what is most appropriate for the
    -- content.
    forceFieldPictures :: Core.Maybe H264ForceFieldPictures,
    -- | Average bitrate in bits\/second. Required when the rate control mode is
    -- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
    -- must have a unique value when its bitrate is rounded down to the nearest
    -- multiple of 1000.
    bitrate :: Core.Maybe Core.Natural,
    -- | Indicates that AFD values will be written into the output stream. If
    -- afdSignaling is \"auto\", the system will try to preserve the input AFD
    -- value (in cases where multiple AFD values are valid). If set to
    -- \"fixed\", the AFD value will be the value configured in the fixedAfd
    -- parameter.
    afdSignaling :: Core.Maybe AfdSignaling
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'H264Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanType', 'h264Settings_scanType' - Sets the scan type of the output to progressive or top-field-first
-- interlaced.
--
-- 'temporalAq', 'h264Settings_temporalAq' - If set to enabled, adjust quantization within each frame based on
-- temporal variation of content complexity.
--
-- 'bufSize', 'h264Settings_bufSize' - Size of buffer (HRD buffer model) in bits.
--
-- 'flickerAq', 'h264Settings_flickerAq' - If set to enabled, adjust quantization within each frame to reduce
-- flicker or \'pop\' on I-frames.
--
-- 'gopBReference', 'h264Settings_gopBReference' - Documentation update needed
--
-- 'framerateNumerator', 'h264Settings_framerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
-- 23.976 fps.
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
-- 'slices', 'h264Settings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures. This field
-- is optional; when no value is specified the encoder will choose the
-- number of slices based on encode resolution.
--
-- 'qualityLevel', 'h264Settings_qualityLevel' - Leave as STANDARD_QUALITY or choose a different value (which might
-- result in additional costs to run the channel). - ENHANCED_QUALITY:
-- Produces a slightly better video quality without an increase in the
-- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
-- If this channel is in a MediaLive multiplex, the value must be
-- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- 'colorMetadata', 'h264Settings_colorMetadata' - Includes colorspace metadata in the output.
--
-- 'subgopLength', 'h264Settings_subgopLength' - If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
-- dynamic, optimize the number of B-frames used for each sub-GOP to
-- improve visual quality.
--
-- 'entropyEncoding', 'h264Settings_entropyEncoding' - Entropy encoding mode. Use cabac (must be in Main or High profile) or
-- cavlc.
--
-- 'gopSizeUnits', 'h264Settings_gopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds
-- the system will convert the gopSize into a frame count at run time.
--
-- 'gopSize', 'h264Settings_gopSize' - GOP size (keyframe interval) in units of either frames or seconds per
-- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
-- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
-- must be greater than 0, but need not be an integer.
--
-- 'framerateDenominator', 'h264Settings_framerateDenominator' - Framerate denominator.
--
-- 'fixedAfd', 'h264Settings_fixedAfd' - Four bit AFD value to write on all frames of video in the output stream.
-- Only valid when afdSignaling is set to \'Fixed\'.
--
-- 'softness', 'h264Settings_softness' - Softness. Selects quantizer matrix, larger values reduce high-frequency
-- content in the encoded image.
--
-- 'filterSettings', 'h264Settings_filterSettings' - Optional filters that you can apply to an encode.
--
-- 'parNumerator', 'h264Settings_parNumerator' - Pixel Aspect Ratio numerator.
--
-- 'spatialAq', 'h264Settings_spatialAq' - If set to enabled, adjust quantization within each frame based on
-- spatial variation of content complexity.
--
-- 'gopNumBFrames', 'h264Settings_gopNumBFrames' - Number of B-frames between reference frames.
--
-- 'sceneChangeDetect', 'h264Settings_sceneChangeDetect' - Scene change detection. - On: inserts I-frames when scene change is
-- detected. - Off: does not force an I-frame when scene change is
-- detected.
--
-- 'timecodeInsertion', 'h264Settings_timecodeInsertion' - Determines how timecodes should be inserted into the video elementary
-- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
-- Pass through picture timing SEI messages from the source specified in
-- Timecode Config
--
-- 'colorSpaceSettings', 'h264Settings_colorSpaceSettings' - Color Space settings
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
-- 'qvbrQualityLevel', 'h264Settings_qvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the
-- rate control mode is QVBR. Set values for the QVBR quality level field
-- and Max bitrate field that suit your most important viewing devices.
-- Recommended values are: - Primary screen: Quality level: 8 to 10. Max
-- bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M -
-- Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- 'parControl', 'h264Settings_parControl' - This field indicates how the output pixel aspect ratio is specified. If
-- \"specified\" is selected then the output video pixel aspect ratio is
-- determined by parNumerator and parDenominator, else if
-- \"initializeFromSource\" is selected then the output pixsel aspect ratio
-- will be set equal to the input video pixel aspect ratio of the first
-- input.
--
-- 'bufFillPct', 'h264Settings_bufFillPct' - Percentage of the buffer that should initially be filled (HRD buffer
-- model).
--
-- 'gopClosedCadence', 'h264Settings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'parDenominator', 'h264Settings_parDenominator' - Pixel Aspect Ratio denominator.
--
-- 'maxBitrate', 'h264Settings_maxBitrate' - For QVBR: See the tooltip for Quality level For VBR: Set the maximum
-- bitrate in order to accommodate expected spikes in the complexity of the
-- video.
--
-- 'syntax', 'h264Settings_syntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- 'numRefFrames', 'h264Settings_numRefFrames' - Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
--
-- 'level', 'h264Settings_level' - H.264 Level.
--
-- 'profile', 'h264Settings_profile' - H.264 Profile.
--
-- 'adaptiveQuantization', 'h264Settings_adaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve
-- visual quality.
--
-- 'lookAheadRateControl', 'h264Settings_lookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory
-- usage, while high can produce better quality for certain content.
--
-- 'framerateControl', 'h264Settings_framerateControl' - This field indicates how the output video frame rate is specified. If
-- \"specified\" is selected then the output video frame rate is determined
-- by framerateNumerator and framerateDenominator, else if
-- \"initializeFromSource\" is selected then the output video frame rate
-- will be set equal to the input video frame rate of the first input.
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
-- 'bitrate', 'h264Settings_bitrate' - Average bitrate in bits\/second. Required when the rate control mode is
-- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
-- must have a unique value when its bitrate is rounded down to the nearest
-- multiple of 1000.
--
-- 'afdSignaling', 'h264Settings_afdSignaling' - Indicates that AFD values will be written into the output stream. If
-- afdSignaling is \"auto\", the system will try to preserve the input AFD
-- value (in cases where multiple AFD values are valid). If set to
-- \"fixed\", the AFD value will be the value configured in the fixedAfd
-- parameter.
newH264Settings ::
  H264Settings
newH264Settings =
  H264Settings'
    { scanType = Core.Nothing,
      temporalAq = Core.Nothing,
      bufSize = Core.Nothing,
      flickerAq = Core.Nothing,
      gopBReference = Core.Nothing,
      framerateNumerator = Core.Nothing,
      rateControlMode = Core.Nothing,
      slices = Core.Nothing,
      qualityLevel = Core.Nothing,
      colorMetadata = Core.Nothing,
      subgopLength = Core.Nothing,
      entropyEncoding = Core.Nothing,
      gopSizeUnits = Core.Nothing,
      gopSize = Core.Nothing,
      framerateDenominator = Core.Nothing,
      fixedAfd = Core.Nothing,
      softness = Core.Nothing,
      filterSettings = Core.Nothing,
      parNumerator = Core.Nothing,
      spatialAq = Core.Nothing,
      gopNumBFrames = Core.Nothing,
      sceneChangeDetect = Core.Nothing,
      timecodeInsertion = Core.Nothing,
      colorSpaceSettings = Core.Nothing,
      minIInterval = Core.Nothing,
      qvbrQualityLevel = Core.Nothing,
      parControl = Core.Nothing,
      bufFillPct = Core.Nothing,
      gopClosedCadence = Core.Nothing,
      parDenominator = Core.Nothing,
      maxBitrate = Core.Nothing,
      syntax = Core.Nothing,
      numRefFrames = Core.Nothing,
      level = Core.Nothing,
      profile = Core.Nothing,
      adaptiveQuantization = Core.Nothing,
      lookAheadRateControl = Core.Nothing,
      framerateControl = Core.Nothing,
      forceFieldPictures = Core.Nothing,
      bitrate = Core.Nothing,
      afdSignaling = Core.Nothing
    }

-- | Sets the scan type of the output to progressive or top-field-first
-- interlaced.
h264Settings_scanType :: Lens.Lens' H264Settings (Core.Maybe H264ScanType)
h264Settings_scanType = Lens.lens (\H264Settings' {scanType} -> scanType) (\s@H264Settings' {} a -> s {scanType = a} :: H264Settings)

-- | If set to enabled, adjust quantization within each frame based on
-- temporal variation of content complexity.
h264Settings_temporalAq :: Lens.Lens' H264Settings (Core.Maybe H264TemporalAq)
h264Settings_temporalAq = Lens.lens (\H264Settings' {temporalAq} -> temporalAq) (\s@H264Settings' {} a -> s {temporalAq = a} :: H264Settings)

-- | Size of buffer (HRD buffer model) in bits.
h264Settings_bufSize :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_bufSize = Lens.lens (\H264Settings' {bufSize} -> bufSize) (\s@H264Settings' {} a -> s {bufSize = a} :: H264Settings)

-- | If set to enabled, adjust quantization within each frame to reduce
-- flicker or \'pop\' on I-frames.
h264Settings_flickerAq :: Lens.Lens' H264Settings (Core.Maybe H264FlickerAq)
h264Settings_flickerAq = Lens.lens (\H264Settings' {flickerAq} -> flickerAq) (\s@H264Settings' {} a -> s {flickerAq = a} :: H264Settings)

-- | Documentation update needed
h264Settings_gopBReference :: Lens.Lens' H264Settings (Core.Maybe H264GopBReference)
h264Settings_gopBReference = Lens.lens (\H264Settings' {gopBReference} -> gopBReference) (\s@H264Settings' {} a -> s {gopBReference = a} :: H264Settings)

-- | Framerate numerator - framerate is a fraction, e.g. 24000 \/ 1001 =
-- 23.976 fps.
h264Settings_framerateNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_framerateNumerator = Lens.lens (\H264Settings' {framerateNumerator} -> framerateNumerator) (\s@H264Settings' {} a -> s {framerateNumerator = a} :: H264Settings)

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
h264Settings_rateControlMode :: Lens.Lens' H264Settings (Core.Maybe H264RateControlMode)
h264Settings_rateControlMode = Lens.lens (\H264Settings' {rateControlMode} -> rateControlMode) (\s@H264Settings' {} a -> s {rateControlMode = a} :: H264Settings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures. This field
-- is optional; when no value is specified the encoder will choose the
-- number of slices based on encode resolution.
h264Settings_slices :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_slices = Lens.lens (\H264Settings' {slices} -> slices) (\s@H264Settings' {} a -> s {slices = a} :: H264Settings)

-- | Leave as STANDARD_QUALITY or choose a different value (which might
-- result in additional costs to run the channel). - ENHANCED_QUALITY:
-- Produces a slightly better video quality without an increase in the
-- bitrate. Has an effect only when the Rate control mode is QVBR or CBR.
-- If this channel is in a MediaLive multiplex, the value must be
-- ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
h264Settings_qualityLevel :: Lens.Lens' H264Settings (Core.Maybe H264QualityLevel)
h264Settings_qualityLevel = Lens.lens (\H264Settings' {qualityLevel} -> qualityLevel) (\s@H264Settings' {} a -> s {qualityLevel = a} :: H264Settings)

-- | Includes colorspace metadata in the output.
h264Settings_colorMetadata :: Lens.Lens' H264Settings (Core.Maybe H264ColorMetadata)
h264Settings_colorMetadata = Lens.lens (\H264Settings' {colorMetadata} -> colorMetadata) (\s@H264Settings' {} a -> s {colorMetadata = a} :: H264Settings)

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to
-- dynamic, optimize the number of B-frames used for each sub-GOP to
-- improve visual quality.
h264Settings_subgopLength :: Lens.Lens' H264Settings (Core.Maybe H264SubGopLength)
h264Settings_subgopLength = Lens.lens (\H264Settings' {subgopLength} -> subgopLength) (\s@H264Settings' {} a -> s {subgopLength = a} :: H264Settings)

-- | Entropy encoding mode. Use cabac (must be in Main or High profile) or
-- cavlc.
h264Settings_entropyEncoding :: Lens.Lens' H264Settings (Core.Maybe H264EntropyEncoding)
h264Settings_entropyEncoding = Lens.lens (\H264Settings' {entropyEncoding} -> entropyEncoding) (\s@H264Settings' {} a -> s {entropyEncoding = a} :: H264Settings)

-- | Indicates if the gopSize is specified in frames or seconds. If seconds
-- the system will convert the gopSize into a frame count at run time.
h264Settings_gopSizeUnits :: Lens.Lens' H264Settings (Core.Maybe H264GopSizeUnits)
h264Settings_gopSizeUnits = Lens.lens (\H264Settings' {gopSizeUnits} -> gopSizeUnits) (\s@H264Settings' {} a -> s {gopSizeUnits = a} :: H264Settings)

-- | GOP size (keyframe interval) in units of either frames or seconds per
-- gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and
-- must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize
-- must be greater than 0, but need not be an integer.
h264Settings_gopSize :: Lens.Lens' H264Settings (Core.Maybe Core.Double)
h264Settings_gopSize = Lens.lens (\H264Settings' {gopSize} -> gopSize) (\s@H264Settings' {} a -> s {gopSize = a} :: H264Settings)

-- | Framerate denominator.
h264Settings_framerateDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_framerateDenominator = Lens.lens (\H264Settings' {framerateDenominator} -> framerateDenominator) (\s@H264Settings' {} a -> s {framerateDenominator = a} :: H264Settings)

-- | Four bit AFD value to write on all frames of video in the output stream.
-- Only valid when afdSignaling is set to \'Fixed\'.
h264Settings_fixedAfd :: Lens.Lens' H264Settings (Core.Maybe FixedAfd)
h264Settings_fixedAfd = Lens.lens (\H264Settings' {fixedAfd} -> fixedAfd) (\s@H264Settings' {} a -> s {fixedAfd = a} :: H264Settings)

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency
-- content in the encoded image.
h264Settings_softness :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_softness = Lens.lens (\H264Settings' {softness} -> softness) (\s@H264Settings' {} a -> s {softness = a} :: H264Settings)

-- | Optional filters that you can apply to an encode.
h264Settings_filterSettings :: Lens.Lens' H264Settings (Core.Maybe H264FilterSettings)
h264Settings_filterSettings = Lens.lens (\H264Settings' {filterSettings} -> filterSettings) (\s@H264Settings' {} a -> s {filterSettings = a} :: H264Settings)

-- | Pixel Aspect Ratio numerator.
h264Settings_parNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_parNumerator = Lens.lens (\H264Settings' {parNumerator} -> parNumerator) (\s@H264Settings' {} a -> s {parNumerator = a} :: H264Settings)

-- | If set to enabled, adjust quantization within each frame based on
-- spatial variation of content complexity.
h264Settings_spatialAq :: Lens.Lens' H264Settings (Core.Maybe H264SpatialAq)
h264Settings_spatialAq = Lens.lens (\H264Settings' {spatialAq} -> spatialAq) (\s@H264Settings' {} a -> s {spatialAq = a} :: H264Settings)

-- | Number of B-frames between reference frames.
h264Settings_gopNumBFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_gopNumBFrames = Lens.lens (\H264Settings' {gopNumBFrames} -> gopNumBFrames) (\s@H264Settings' {} a -> s {gopNumBFrames = a} :: H264Settings)

-- | Scene change detection. - On: inserts I-frames when scene change is
-- detected. - Off: does not force an I-frame when scene change is
-- detected.
h264Settings_sceneChangeDetect :: Lens.Lens' H264Settings (Core.Maybe H264SceneChangeDetect)
h264Settings_sceneChangeDetect = Lens.lens (\H264Settings' {sceneChangeDetect} -> sceneChangeDetect) (\s@H264Settings' {} a -> s {sceneChangeDetect = a} :: H264Settings)

-- | Determines how timecodes should be inserted into the video elementary
-- stream. - \'disabled\': Do not include timecodes - \'picTimingSei\':
-- Pass through picture timing SEI messages from the source specified in
-- Timecode Config
h264Settings_timecodeInsertion :: Lens.Lens' H264Settings (Core.Maybe H264TimecodeInsertionBehavior)
h264Settings_timecodeInsertion = Lens.lens (\H264Settings' {timecodeInsertion} -> timecodeInsertion) (\s@H264Settings' {} a -> s {timecodeInsertion = a} :: H264Settings)

-- | Color Space settings
h264Settings_colorSpaceSettings :: Lens.Lens' H264Settings (Core.Maybe H264ColorSpaceSettings)
h264Settings_colorSpaceSettings = Lens.lens (\H264Settings' {colorSpaceSettings} -> colorSpaceSettings) (\s@H264Settings' {} a -> s {colorSpaceSettings = a} :: H264Settings)

-- | Only meaningful if sceneChangeDetect is set to enabled. Defaults to 5 if
-- multiplex rate control is used. Enforces separation between repeated
-- (cadence) I-frames and I-frames inserted by Scene Change Detection. If a
-- scene change I-frame is within I-interval frames of a cadence I-frame,
-- the GOP is shrunk and\/or stretched to the scene change I-frame. GOP
-- stretch requires enabling lookahead as well as setting I-interval. The
-- normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP
-- size + Min-I-interval - 1
h264Settings_minIInterval :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_minIInterval = Lens.lens (\H264Settings' {minIInterval} -> minIInterval) (\s@H264Settings' {} a -> s {minIInterval = a} :: H264Settings)

-- | Controls the target quality for the video encode. Applies only when the
-- rate control mode is QVBR. Set values for the QVBR quality level field
-- and Max bitrate field that suit your most important viewing devices.
-- Recommended values are: - Primary screen: Quality level: 8 to 10. Max
-- bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M -
-- Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
h264Settings_qvbrQualityLevel :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_qvbrQualityLevel = Lens.lens (\H264Settings' {qvbrQualityLevel} -> qvbrQualityLevel) (\s@H264Settings' {} a -> s {qvbrQualityLevel = a} :: H264Settings)

-- | This field indicates how the output pixel aspect ratio is specified. If
-- \"specified\" is selected then the output video pixel aspect ratio is
-- determined by parNumerator and parDenominator, else if
-- \"initializeFromSource\" is selected then the output pixsel aspect ratio
-- will be set equal to the input video pixel aspect ratio of the first
-- input.
h264Settings_parControl :: Lens.Lens' H264Settings (Core.Maybe H264ParControl)
h264Settings_parControl = Lens.lens (\H264Settings' {parControl} -> parControl) (\s@H264Settings' {} a -> s {parControl = a} :: H264Settings)

-- | Percentage of the buffer that should initially be filled (HRD buffer
-- model).
h264Settings_bufFillPct :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_bufFillPct = Lens.lens (\H264Settings' {bufFillPct} -> bufFillPct) (\s@H264Settings' {} a -> s {bufFillPct = a} :: H264Settings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
h264Settings_gopClosedCadence :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_gopClosedCadence = Lens.lens (\H264Settings' {gopClosedCadence} -> gopClosedCadence) (\s@H264Settings' {} a -> s {gopClosedCadence = a} :: H264Settings)

-- | Pixel Aspect Ratio denominator.
h264Settings_parDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_parDenominator = Lens.lens (\H264Settings' {parDenominator} -> parDenominator) (\s@H264Settings' {} a -> s {parDenominator = a} :: H264Settings)

-- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum
-- bitrate in order to accommodate expected spikes in the complexity of the
-- video.
h264Settings_maxBitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_maxBitrate = Lens.lens (\H264Settings' {maxBitrate} -> maxBitrate) (\s@H264Settings' {} a -> s {maxBitrate = a} :: H264Settings)

-- | Produces a bitstream compliant with SMPTE RP-2027.
h264Settings_syntax :: Lens.Lens' H264Settings (Core.Maybe H264Syntax)
h264Settings_syntax = Lens.lens (\H264Settings' {syntax} -> syntax) (\s@H264Settings' {} a -> s {syntax = a} :: H264Settings)

-- | Number of reference frames to use. The encoder may use more than
-- requested if using B-frames and\/or interlaced encoding.
h264Settings_numRefFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_numRefFrames = Lens.lens (\H264Settings' {numRefFrames} -> numRefFrames) (\s@H264Settings' {} a -> s {numRefFrames = a} :: H264Settings)

-- | H.264 Level.
h264Settings_level :: Lens.Lens' H264Settings (Core.Maybe H264Level)
h264Settings_level = Lens.lens (\H264Settings' {level} -> level) (\s@H264Settings' {} a -> s {level = a} :: H264Settings)

-- | H.264 Profile.
h264Settings_profile :: Lens.Lens' H264Settings (Core.Maybe H264Profile)
h264Settings_profile = Lens.lens (\H264Settings' {profile} -> profile) (\s@H264Settings' {} a -> s {profile = a} :: H264Settings)

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve
-- visual quality.
h264Settings_adaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe H264AdaptiveQuantization)
h264Settings_adaptiveQuantization = Lens.lens (\H264Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@H264Settings' {} a -> s {adaptiveQuantization = a} :: H264Settings)

-- | Amount of lookahead. A value of low can decrease latency and memory
-- usage, while high can produce better quality for certain content.
h264Settings_lookAheadRateControl :: Lens.Lens' H264Settings (Core.Maybe H264LookAheadRateControl)
h264Settings_lookAheadRateControl = Lens.lens (\H264Settings' {lookAheadRateControl} -> lookAheadRateControl) (\s@H264Settings' {} a -> s {lookAheadRateControl = a} :: H264Settings)

-- | This field indicates how the output video frame rate is specified. If
-- \"specified\" is selected then the output video frame rate is determined
-- by framerateNumerator and framerateDenominator, else if
-- \"initializeFromSource\" is selected then the output video frame rate
-- will be set equal to the input video frame rate of the first input.
h264Settings_framerateControl :: Lens.Lens' H264Settings (Core.Maybe H264FramerateControl)
h264Settings_framerateControl = Lens.lens (\H264Settings' {framerateControl} -> framerateControl) (\s@H264Settings' {} a -> s {framerateControl = a} :: H264Settings)

-- | This setting applies only when scan type is \"interlaced.\" It controls
-- whether coding is performed on a field basis or on a frame basis. (When
-- the video is progressive, the coding is always performed on a frame
-- basis.) enabled: Force MediaLive to code on a field basis, so that odd
-- and even sets of fields are coded separately. disabled: Code the two
-- sets of fields separately (on a field basis) or together (on a frame
-- basis using PAFF), depending on what is most appropriate for the
-- content.
h264Settings_forceFieldPictures :: Lens.Lens' H264Settings (Core.Maybe H264ForceFieldPictures)
h264Settings_forceFieldPictures = Lens.lens (\H264Settings' {forceFieldPictures} -> forceFieldPictures) (\s@H264Settings' {} a -> s {forceFieldPictures = a} :: H264Settings)

-- | Average bitrate in bits\/second. Required when the rate control mode is
-- VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output
-- must have a unique value when its bitrate is rounded down to the nearest
-- multiple of 1000.
h264Settings_bitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
h264Settings_bitrate = Lens.lens (\H264Settings' {bitrate} -> bitrate) (\s@H264Settings' {} a -> s {bitrate = a} :: H264Settings)

-- | Indicates that AFD values will be written into the output stream. If
-- afdSignaling is \"auto\", the system will try to preserve the input AFD
-- value (in cases where multiple AFD values are valid). If set to
-- \"fixed\", the AFD value will be the value configured in the fixedAfd
-- parameter.
h264Settings_afdSignaling :: Lens.Lens' H264Settings (Core.Maybe AfdSignaling)
h264Settings_afdSignaling = Lens.lens (\H264Settings' {afdSignaling} -> afdSignaling) (\s@H264Settings' {} a -> s {afdSignaling = a} :: H264Settings)

instance Core.FromJSON H264Settings where
  parseJSON =
    Core.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Core.<$> (x Core..:? "scanType")
            Core.<*> (x Core..:? "temporalAq")
            Core.<*> (x Core..:? "bufSize")
            Core.<*> (x Core..:? "flickerAq")
            Core.<*> (x Core..:? "gopBReference")
            Core.<*> (x Core..:? "framerateNumerator")
            Core.<*> (x Core..:? "rateControlMode")
            Core.<*> (x Core..:? "slices")
            Core.<*> (x Core..:? "qualityLevel")
            Core.<*> (x Core..:? "colorMetadata")
            Core.<*> (x Core..:? "subgopLength")
            Core.<*> (x Core..:? "entropyEncoding")
            Core.<*> (x Core..:? "gopSizeUnits")
            Core.<*> (x Core..:? "gopSize")
            Core.<*> (x Core..:? "framerateDenominator")
            Core.<*> (x Core..:? "fixedAfd")
            Core.<*> (x Core..:? "softness")
            Core.<*> (x Core..:? "filterSettings")
            Core.<*> (x Core..:? "parNumerator")
            Core.<*> (x Core..:? "spatialAq")
            Core.<*> (x Core..:? "gopNumBFrames")
            Core.<*> (x Core..:? "sceneChangeDetect")
            Core.<*> (x Core..:? "timecodeInsertion")
            Core.<*> (x Core..:? "colorSpaceSettings")
            Core.<*> (x Core..:? "minIInterval")
            Core.<*> (x Core..:? "qvbrQualityLevel")
            Core.<*> (x Core..:? "parControl")
            Core.<*> (x Core..:? "bufFillPct")
            Core.<*> (x Core..:? "gopClosedCadence")
            Core.<*> (x Core..:? "parDenominator")
            Core.<*> (x Core..:? "maxBitrate")
            Core.<*> (x Core..:? "syntax")
            Core.<*> (x Core..:? "numRefFrames")
            Core.<*> (x Core..:? "level")
            Core.<*> (x Core..:? "profile")
            Core.<*> (x Core..:? "adaptiveQuantization")
            Core.<*> (x Core..:? "lookAheadRateControl")
            Core.<*> (x Core..:? "framerateControl")
            Core.<*> (x Core..:? "forceFieldPictures")
            Core.<*> (x Core..:? "bitrate")
            Core.<*> (x Core..:? "afdSignaling")
      )

instance Core.Hashable H264Settings

instance Core.NFData H264Settings

instance Core.ToJSON H264Settings where
  toJSON H264Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("scanType" Core..=) Core.<$> scanType,
            ("temporalAq" Core..=) Core.<$> temporalAq,
            ("bufSize" Core..=) Core.<$> bufSize,
            ("flickerAq" Core..=) Core.<$> flickerAq,
            ("gopBReference" Core..=) Core.<$> gopBReference,
            ("framerateNumerator" Core..=)
              Core.<$> framerateNumerator,
            ("rateControlMode" Core..=) Core.<$> rateControlMode,
            ("slices" Core..=) Core.<$> slices,
            ("qualityLevel" Core..=) Core.<$> qualityLevel,
            ("colorMetadata" Core..=) Core.<$> colorMetadata,
            ("subgopLength" Core..=) Core.<$> subgopLength,
            ("entropyEncoding" Core..=) Core.<$> entropyEncoding,
            ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
            ("gopSize" Core..=) Core.<$> gopSize,
            ("framerateDenominator" Core..=)
              Core.<$> framerateDenominator,
            ("fixedAfd" Core..=) Core.<$> fixedAfd,
            ("softness" Core..=) Core.<$> softness,
            ("filterSettings" Core..=) Core.<$> filterSettings,
            ("parNumerator" Core..=) Core.<$> parNumerator,
            ("spatialAq" Core..=) Core.<$> spatialAq,
            ("gopNumBFrames" Core..=) Core.<$> gopNumBFrames,
            ("sceneChangeDetect" Core..=)
              Core.<$> sceneChangeDetect,
            ("timecodeInsertion" Core..=)
              Core.<$> timecodeInsertion,
            ("colorSpaceSettings" Core..=)
              Core.<$> colorSpaceSettings,
            ("minIInterval" Core..=) Core.<$> minIInterval,
            ("qvbrQualityLevel" Core..=)
              Core.<$> qvbrQualityLevel,
            ("parControl" Core..=) Core.<$> parControl,
            ("bufFillPct" Core..=) Core.<$> bufFillPct,
            ("gopClosedCadence" Core..=)
              Core.<$> gopClosedCadence,
            ("parDenominator" Core..=) Core.<$> parDenominator,
            ("maxBitrate" Core..=) Core.<$> maxBitrate,
            ("syntax" Core..=) Core.<$> syntax,
            ("numRefFrames" Core..=) Core.<$> numRefFrames,
            ("level" Core..=) Core.<$> level,
            ("profile" Core..=) Core.<$> profile,
            ("adaptiveQuantization" Core..=)
              Core.<$> adaptiveQuantization,
            ("lookAheadRateControl" Core..=)
              Core.<$> lookAheadRateControl,
            ("framerateControl" Core..=)
              Core.<$> framerateControl,
            ("forceFieldPictures" Core..=)
              Core.<$> forceFieldPictures,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("afdSignaling" Core..=) Core.<$> afdSignaling
          ]
      )
