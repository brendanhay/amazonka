{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Settings
  ( H264Settings (..),

    -- * Smart constructor
    mkH264Settings,

    -- * Lenses
    hTemporalAq,
    hSceneChangeDetect,
    hScanType,
    hTimecodeInsertion,
    hParNumerator,
    hAfdSignaling,
    hGopSize,
    hGopSizeUnits,
    hSubgopLength,
    hQualityLevel,
    hSlices,
    hProfile,
    hRateControlMode,
    hMinIInterval,
    hQvbrQualityLevel,
    hColorSpaceSettings,
    hParControl,
    hFlickerAq,
    hBufSize,
    hSpatialAq,
    hGopNumBFrames,
    hFixedAfd,
    hSoftness,
    hFilterSettings,
    hBitrate,
    hFramerateDenominator,
    hForceFieldPictures,
    hEntropyEncoding,
    hFramerateControl,
    hColorMetadata,
    hLookAheadRateControl,
    hAdaptiveQuantization,
    hFramerateNumerator,
    hLevel,
    hGopBReference,
    hMaxBitrate,
    hSyntax,
    hBufFillPct,
    hGopClosedCadence,
    hNumRefFrames,
    hParDenominator,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | H264 Settings
--
-- /See:/ 'mkH264Settings' smart constructor.
data H264Settings = H264Settings'
  { temporalAq ::
      Lude.Maybe H264TemporalAq,
    sceneChangeDetect :: Lude.Maybe H264SceneChangeDetect,
    scanType :: Lude.Maybe H264ScanType,
    timecodeInsertion :: Lude.Maybe H264TimecodeInsertionBehavior,
    parNumerator :: Lude.Maybe Lude.Natural,
    afdSignaling :: Lude.Maybe AfdSignaling,
    gopSize :: Lude.Maybe Lude.Double,
    gopSizeUnits :: Lude.Maybe H264GopSizeUnits,
    subgopLength :: Lude.Maybe H264SubGopLength,
    qualityLevel :: Lude.Maybe H264QualityLevel,
    slices :: Lude.Maybe Lude.Natural,
    profile :: Lude.Maybe H264Profile,
    rateControlMode :: Lude.Maybe H264RateControlMode,
    minIInterval :: Lude.Maybe Lude.Natural,
    qvbrQualityLevel :: Lude.Maybe Lude.Natural,
    colorSpaceSettings :: Lude.Maybe H264ColorSpaceSettings,
    parControl :: Lude.Maybe H264ParControl,
    flickerAq :: Lude.Maybe H264FlickerAq,
    bufSize :: Lude.Maybe Lude.Natural,
    spatialAq :: Lude.Maybe H264SpatialAq,
    gopNumBFrames :: Lude.Maybe Lude.Natural,
    fixedAfd :: Lude.Maybe FixedAfd,
    softness :: Lude.Maybe Lude.Natural,
    filterSettings :: Lude.Maybe H264FilterSettings,
    bitrate :: Lude.Maybe Lude.Natural,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    forceFieldPictures :: Lude.Maybe H264ForceFieldPictures,
    entropyEncoding :: Lude.Maybe H264EntropyEncoding,
    framerateControl :: Lude.Maybe H264FramerateControl,
    colorMetadata :: Lude.Maybe H264ColorMetadata,
    lookAheadRateControl :: Lude.Maybe H264LookAheadRateControl,
    adaptiveQuantization :: Lude.Maybe H264AdaptiveQuantization,
    framerateNumerator :: Lude.Maybe Lude.Natural,
    level :: Lude.Maybe H264Level,
    gopBReference :: Lude.Maybe H264GopBReference,
    maxBitrate :: Lude.Maybe Lude.Natural,
    syntax :: Lude.Maybe H264Syntax,
    bufFillPct :: Lude.Maybe Lude.Natural,
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    numRefFrames :: Lude.Maybe Lude.Natural,
    parDenominator :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- * 'adaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
-- * 'afdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
-- * 'bitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
-- * 'bufFillPct' - Percentage of the buffer that should initially be filled (HRD buffer model).
-- * 'bufSize' - Size of buffer (HRD buffer model) in bits.
-- * 'colorMetadata' - Includes colorspace metadata in the output.
-- * 'colorSpaceSettings' - Color Space settings
-- * 'entropyEncoding' - Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
-- * 'filterSettings' - Optional filters that you can apply to an encode.
-- * 'fixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
-- * 'flickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
-- * 'forceFieldPictures' - This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.)
--
-- enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately.
-- disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
-- * 'framerateControl' - This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
-- * 'framerateDenominator' - Framerate denominator.
-- * 'framerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
-- * 'gopBReference' - Documentation update needed
-- * 'gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
-- * 'gopNumBFrames' - Number of B-frames between reference frames.
-- * 'gopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
-- * 'gopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
-- * 'level' - H.264 Level.
-- * 'lookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
-- * 'maxBitrate' - For QVBR: See the tooltip for Quality level
--
--
-- For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
-- * 'minIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
-- * 'numRefFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
-- * 'parControl' - This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
-- * 'parDenominator' - Pixel Aspect Ratio denominator.
-- * 'parNumerator' - Pixel Aspect Ratio numerator.
-- * 'profile' - H.264 Profile.
-- * 'qualityLevel' - Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel).
--
-- - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY.
-- - STANDARD_QUALITY: Valid for any Rate control mode.
-- * 'qvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
-- * 'rateControlMode' - Rate control mode.
--
--
-- QVBR: Quality will match the specified quality level except when it is constrained by the
-- maximum bitrate.  Recommended if you or your viewers pay for bandwidth.
--
-- VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR
-- if you want to maintain a specific average bitrate over the duration of the channel.
--
-- CBR: Quality varies, depending on the video complexity. Recommended only if you distribute
-- your assets to devices that cannot handle variable bitrates.
--
-- Multiplex: This rate control mode is only supported (and is required) when the video is being
-- delivered to a MediaLive Multiplex in which case the rate control configuration is controlled
-- by the properties within the Multiplex Program.
-- * 'scanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
-- * 'sceneChangeDetect' - Scene change detection.
--
--
-- - On: inserts I-frames when scene change is detected.
-- - Off: does not force an I-frame when scene change is detected.
-- * 'slices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
-- * 'softness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
-- * 'spatialAq' - If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
-- * 'subgopLength' - If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
-- * 'syntax' - Produces a bitstream compliant with SMPTE RP-2027.
-- * 'temporalAq' - If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
-- * 'timecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
mkH264Settings ::
  H264Settings
mkH264Settings =
  H264Settings'
    { temporalAq = Lude.Nothing,
      sceneChangeDetect = Lude.Nothing,
      scanType = Lude.Nothing,
      timecodeInsertion = Lude.Nothing,
      parNumerator = Lude.Nothing,
      afdSignaling = Lude.Nothing,
      gopSize = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      subgopLength = Lude.Nothing,
      qualityLevel = Lude.Nothing,
      slices = Lude.Nothing,
      profile = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      minIInterval = Lude.Nothing,
      qvbrQualityLevel = Lude.Nothing,
      colorSpaceSettings = Lude.Nothing,
      parControl = Lude.Nothing,
      flickerAq = Lude.Nothing,
      bufSize = Lude.Nothing,
      spatialAq = Lude.Nothing,
      gopNumBFrames = Lude.Nothing,
      fixedAfd = Lude.Nothing,
      softness = Lude.Nothing,
      filterSettings = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      forceFieldPictures = Lude.Nothing,
      entropyEncoding = Lude.Nothing,
      framerateControl = Lude.Nothing,
      colorMetadata = Lude.Nothing,
      lookAheadRateControl = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      level = Lude.Nothing,
      gopBReference = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      syntax = Lude.Nothing,
      bufFillPct = Lude.Nothing,
      gopClosedCadence = Lude.Nothing,
      numRefFrames = Lude.Nothing,
      parDenominator = Lude.Nothing
    }

-- | If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
--
-- /Note:/ Consider using 'temporalAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalAq :: Lens.Lens' H264Settings (Lude.Maybe H264TemporalAq)
hTemporalAq = Lens.lens (temporalAq :: H264Settings -> Lude.Maybe H264TemporalAq) (\s a -> s {temporalAq = a} :: H264Settings)
{-# DEPRECATED hTemporalAq "Use generic-lens or generic-optics with 'temporalAq' instead." #-}

-- | Scene change detection.
--
--
-- - On: inserts I-frames when scene change is detected.
-- - Off: does not force an I-frame when scene change is detected.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSceneChangeDetect :: Lens.Lens' H264Settings (Lude.Maybe H264SceneChangeDetect)
hSceneChangeDetect = Lens.lens (sceneChangeDetect :: H264Settings -> Lude.Maybe H264SceneChangeDetect) (\s a -> s {sceneChangeDetect = a} :: H264Settings)
{-# DEPRECATED hSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hScanType :: Lens.Lens' H264Settings (Lude.Maybe H264ScanType)
hScanType = Lens.lens (scanType :: H264Settings -> Lude.Maybe H264ScanType) (\s a -> s {scanType = a} :: H264Settings)
{-# DEPRECATED hScanType "Use generic-lens or generic-optics with 'scanType' instead." #-}

-- | Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTimecodeInsertion :: Lens.Lens' H264Settings (Lude.Maybe H264TimecodeInsertionBehavior)
hTimecodeInsertion = Lens.lens (timecodeInsertion :: H264Settings -> Lude.Maybe H264TimecodeInsertionBehavior) (\s a -> s {timecodeInsertion = a} :: H264Settings)
{-# DEPRECATED hTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

-- | Pixel Aspect Ratio numerator.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParNumerator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hParNumerator = Lens.lens (parNumerator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: H264Settings)
{-# DEPRECATED hParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAfdSignaling :: Lens.Lens' H264Settings (Lude.Maybe AfdSignaling)
hAfdSignaling = Lens.lens (afdSignaling :: H264Settings -> Lude.Maybe AfdSignaling) (\s a -> s {afdSignaling = a} :: H264Settings)
{-# DEPRECATED hAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSize :: Lens.Lens' H264Settings (Lude.Maybe Lude.Double)
hGopSize = Lens.lens (gopSize :: H264Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: H264Settings)
{-# DEPRECATED hGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSizeUnits :: Lens.Lens' H264Settings (Lude.Maybe H264GopSizeUnits)
hGopSizeUnits = Lens.lens (gopSizeUnits :: H264Settings -> Lude.Maybe H264GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: H264Settings)
{-# DEPRECATED hGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
--
-- /Note:/ Consider using 'subgopLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSubgopLength :: Lens.Lens' H264Settings (Lude.Maybe H264SubGopLength)
hSubgopLength = Lens.lens (subgopLength :: H264Settings -> Lude.Maybe H264SubGopLength) (\s a -> s {subgopLength = a} :: H264Settings)
{-# DEPRECATED hSubgopLength "Use generic-lens or generic-optics with 'subgopLength' instead." #-}

-- | Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel).
--
-- - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY.
-- - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- /Note:/ Consider using 'qualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQualityLevel :: Lens.Lens' H264Settings (Lude.Maybe H264QualityLevel)
hQualityLevel = Lens.lens (qualityLevel :: H264Settings -> Lude.Maybe H264QualityLevel) (\s a -> s {qualityLevel = a} :: H264Settings)
{-# DEPRECATED hQualityLevel "Use generic-lens or generic-optics with 'qualityLevel' instead." #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlices :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hSlices = Lens.lens (slices :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {slices = a} :: H264Settings)
{-# DEPRECATED hSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | H.264 Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hProfile :: Lens.Lens' H264Settings (Lude.Maybe H264Profile)
hProfile = Lens.lens (profile :: H264Settings -> Lude.Maybe H264Profile) (\s a -> s {profile = a} :: H264Settings)
{-# DEPRECATED hProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Rate control mode.
--
--
-- QVBR: Quality will match the specified quality level except when it is constrained by the
-- maximum bitrate.  Recommended if you or your viewers pay for bandwidth.
--
-- VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR
-- if you want to maintain a specific average bitrate over the duration of the channel.
--
-- CBR: Quality varies, depending on the video complexity. Recommended only if you distribute
-- your assets to devices that cannot handle variable bitrates.
--
-- Multiplex: This rate control mode is only supported (and is required) when the video is being
-- delivered to a MediaLive Multiplex in which case the rate control configuration is controlled
-- by the properties within the Multiplex Program.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRateControlMode :: Lens.Lens' H264Settings (Lude.Maybe H264RateControlMode)
hRateControlMode = Lens.lens (rateControlMode :: H264Settings -> Lude.Maybe H264RateControlMode) (\s a -> s {rateControlMode = a} :: H264Settings)
{-# DEPRECATED hRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMinIInterval :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hMinIInterval = Lens.lens (minIInterval :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {minIInterval = a} :: H264Settings)
{-# DEPRECATED hMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrQualityLevel :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hQvbrQualityLevel = Lens.lens (qvbrQualityLevel :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {qvbrQualityLevel = a} :: H264Settings)
{-# DEPRECATED hQvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead." #-}

-- | Color Space settings
--
-- /Note:/ Consider using 'colorSpaceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorSpaceSettings :: Lens.Lens' H264Settings (Lude.Maybe H264ColorSpaceSettings)
hColorSpaceSettings = Lens.lens (colorSpaceSettings :: H264Settings -> Lude.Maybe H264ColorSpaceSettings) (\s a -> s {colorSpaceSettings = a} :: H264Settings)
{-# DEPRECATED hColorSpaceSettings "Use generic-lens or generic-optics with 'colorSpaceSettings' instead." #-}

-- | This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParControl :: Lens.Lens' H264Settings (Lude.Maybe H264ParControl)
hParControl = Lens.lens (parControl :: H264Settings -> Lude.Maybe H264ParControl) (\s a -> s {parControl = a} :: H264Settings)
{-# DEPRECATED hParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- /Note:/ Consider using 'flickerAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFlickerAq :: Lens.Lens' H264Settings (Lude.Maybe H264FlickerAq)
hFlickerAq = Lens.lens (flickerAq :: H264Settings -> Lude.Maybe H264FlickerAq) (\s a -> s {flickerAq = a} :: H264Settings)
{-# DEPRECATED hFlickerAq "Use generic-lens or generic-optics with 'flickerAq' instead." #-}

-- | Size of buffer (HRD buffer model) in bits.
--
-- /Note:/ Consider using 'bufSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBufSize :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hBufSize = Lens.lens (bufSize :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bufSize = a} :: H264Settings)
{-# DEPRECATED hBufSize "Use generic-lens or generic-optics with 'bufSize' instead." #-}

-- | If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
--
-- /Note:/ Consider using 'spatialAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSpatialAq :: Lens.Lens' H264Settings (Lude.Maybe H264SpatialAq)
hSpatialAq = Lens.lens (spatialAq :: H264Settings -> Lude.Maybe H264SpatialAq) (\s a -> s {spatialAq = a} :: H264Settings)
{-# DEPRECATED hSpatialAq "Use generic-lens or generic-optics with 'spatialAq' instead." #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'gopNumBFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopNumBFrames :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hGopNumBFrames = Lens.lens (gopNumBFrames :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopNumBFrames = a} :: H264Settings)
{-# DEPRECATED hGopNumBFrames "Use generic-lens or generic-optics with 'gopNumBFrames' instead." #-}

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFixedAfd :: Lens.Lens' H264Settings (Lude.Maybe FixedAfd)
hFixedAfd = Lens.lens (fixedAfd :: H264Settings -> Lude.Maybe FixedAfd) (\s a -> s {fixedAfd = a} :: H264Settings)
{-# DEPRECATED hFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSoftness :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hSoftness = Lens.lens (softness :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {softness = a} :: H264Settings)
{-# DEPRECATED hSoftness "Use generic-lens or generic-optics with 'softness' instead." #-}

-- | Optional filters that you can apply to an encode.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFilterSettings :: Lens.Lens' H264Settings (Lude.Maybe H264FilterSettings)
hFilterSettings = Lens.lens (filterSettings :: H264Settings -> Lude.Maybe H264FilterSettings) (\s a -> s {filterSettings = a} :: H264Settings)
{-# DEPRECATED hFilterSettings "Use generic-lens or generic-optics with 'filterSettings' instead." #-}

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBitrate :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hBitrate = Lens.lens (bitrate :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: H264Settings)
{-# DEPRECATED hBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Framerate denominator.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateDenominator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hFramerateDenominator = Lens.lens (framerateDenominator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: H264Settings)
{-# DEPRECATED hFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.)
--
-- enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately.
-- disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
--
-- /Note:/ Consider using 'forceFieldPictures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hForceFieldPictures :: Lens.Lens' H264Settings (Lude.Maybe H264ForceFieldPictures)
hForceFieldPictures = Lens.lens (forceFieldPictures :: H264Settings -> Lude.Maybe H264ForceFieldPictures) (\s a -> s {forceFieldPictures = a} :: H264Settings)
{-# DEPRECATED hForceFieldPictures "Use generic-lens or generic-optics with 'forceFieldPictures' instead." #-}

-- | Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
--
-- /Note:/ Consider using 'entropyEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hEntropyEncoding :: Lens.Lens' H264Settings (Lude.Maybe H264EntropyEncoding)
hEntropyEncoding = Lens.lens (entropyEncoding :: H264Settings -> Lude.Maybe H264EntropyEncoding) (\s a -> s {entropyEncoding = a} :: H264Settings)
{-# DEPRECATED hEntropyEncoding "Use generic-lens or generic-optics with 'entropyEncoding' instead." #-}

-- | This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateControl :: Lens.Lens' H264Settings (Lude.Maybe H264FramerateControl)
hFramerateControl = Lens.lens (framerateControl :: H264Settings -> Lude.Maybe H264FramerateControl) (\s a -> s {framerateControl = a} :: H264Settings)
{-# DEPRECATED hFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Includes colorspace metadata in the output.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorMetadata :: Lens.Lens' H264Settings (Lude.Maybe H264ColorMetadata)
hColorMetadata = Lens.lens (colorMetadata :: H264Settings -> Lude.Maybe H264ColorMetadata) (\s a -> s {colorMetadata = a} :: H264Settings)
{-# DEPRECATED hColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- /Note:/ Consider using 'lookAheadRateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLookAheadRateControl :: Lens.Lens' H264Settings (Lude.Maybe H264LookAheadRateControl)
hLookAheadRateControl = Lens.lens (lookAheadRateControl :: H264Settings -> Lude.Maybe H264LookAheadRateControl) (\s a -> s {lookAheadRateControl = a} :: H264Settings)
{-# DEPRECATED hLookAheadRateControl "Use generic-lens or generic-optics with 'lookAheadRateControl' instead." #-}

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAdaptiveQuantization :: Lens.Lens' H264Settings (Lude.Maybe H264AdaptiveQuantization)
hAdaptiveQuantization = Lens.lens (adaptiveQuantization :: H264Settings -> Lude.Maybe H264AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: H264Settings)
{-# DEPRECATED hAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateNumerator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hFramerateNumerator = Lens.lens (framerateNumerator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: H264Settings)
{-# DEPRECATED hFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | H.264 Level.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLevel :: Lens.Lens' H264Settings (Lude.Maybe H264Level)
hLevel = Lens.lens (level :: H264Settings -> Lude.Maybe H264Level) (\s a -> s {level = a} :: H264Settings)
{-# DEPRECATED hLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopBReference :: Lens.Lens' H264Settings (Lude.Maybe H264GopBReference)
hGopBReference = Lens.lens (gopBReference :: H264Settings -> Lude.Maybe H264GopBReference) (\s a -> s {gopBReference = a} :: H264Settings)
{-# DEPRECATED hGopBReference "Use generic-lens or generic-optics with 'gopBReference' instead." #-}

-- | For QVBR: See the tooltip for Quality level
--
--
-- For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
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

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'bufFillPct' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBufFillPct :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hBufFillPct = Lens.lens (bufFillPct :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bufFillPct = a} :: H264Settings)
{-# DEPRECATED hBufFillPct "Use generic-lens or generic-optics with 'bufFillPct' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopClosedCadence :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hGopClosedCadence = Lens.lens (gopClosedCadence :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: H264Settings)
{-# DEPRECATED hGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numRefFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hNumRefFrames :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hNumRefFrames = Lens.lens (numRefFrames :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numRefFrames = a} :: H264Settings)
{-# DEPRECATED hNumRefFrames "Use generic-lens or generic-optics with 'numRefFrames' instead." #-}

-- | Pixel Aspect Ratio denominator.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParDenominator :: Lens.Lens' H264Settings (Lude.Maybe Lude.Natural)
hParDenominator = Lens.lens (parDenominator :: H264Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: H264Settings)
{-# DEPRECATED hParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

instance Lude.FromJSON H264Settings where
  parseJSON =
    Lude.withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            Lude.<$> (x Lude..:? "temporalAq")
            Lude.<*> (x Lude..:? "sceneChangeDetect")
            Lude.<*> (x Lude..:? "scanType")
            Lude.<*> (x Lude..:? "timecodeInsertion")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "afdSignaling")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "subgopLength")
            Lude.<*> (x Lude..:? "qualityLevel")
            Lude.<*> (x Lude..:? "slices")
            Lude.<*> (x Lude..:? "profile")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "minIInterval")
            Lude.<*> (x Lude..:? "qvbrQualityLevel")
            Lude.<*> (x Lude..:? "colorSpaceSettings")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "flickerAq")
            Lude.<*> (x Lude..:? "bufSize")
            Lude.<*> (x Lude..:? "spatialAq")
            Lude.<*> (x Lude..:? "gopNumBFrames")
            Lude.<*> (x Lude..:? "fixedAfd")
            Lude.<*> (x Lude..:? "softness")
            Lude.<*> (x Lude..:? "filterSettings")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "forceFieldPictures")
            Lude.<*> (x Lude..:? "entropyEncoding")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "colorMetadata")
            Lude.<*> (x Lude..:? "lookAheadRateControl")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "level")
            Lude.<*> (x Lude..:? "gopBReference")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "syntax")
            Lude.<*> (x Lude..:? "bufFillPct")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "numRefFrames")
            Lude.<*> (x Lude..:? "parDenominator")
      )

instance Lude.ToJSON H264Settings where
  toJSON H264Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("temporalAq" Lude..=) Lude.<$> temporalAq,
            ("sceneChangeDetect" Lude..=) Lude.<$> sceneChangeDetect,
            ("scanType" Lude..=) Lude.<$> scanType,
            ("timecodeInsertion" Lude..=) Lude.<$> timecodeInsertion,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("afdSignaling" Lude..=) Lude.<$> afdSignaling,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("subgopLength" Lude..=) Lude.<$> subgopLength,
            ("qualityLevel" Lude..=) Lude.<$> qualityLevel,
            ("slices" Lude..=) Lude.<$> slices,
            ("profile" Lude..=) Lude.<$> profile,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("minIInterval" Lude..=) Lude.<$> minIInterval,
            ("qvbrQualityLevel" Lude..=) Lude.<$> qvbrQualityLevel,
            ("colorSpaceSettings" Lude..=) Lude.<$> colorSpaceSettings,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("flickerAq" Lude..=) Lude.<$> flickerAq,
            ("bufSize" Lude..=) Lude.<$> bufSize,
            ("spatialAq" Lude..=) Lude.<$> spatialAq,
            ("gopNumBFrames" Lude..=) Lude.<$> gopNumBFrames,
            ("fixedAfd" Lude..=) Lude.<$> fixedAfd,
            ("softness" Lude..=) Lude.<$> softness,
            ("filterSettings" Lude..=) Lude.<$> filterSettings,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("forceFieldPictures" Lude..=) Lude.<$> forceFieldPictures,
            ("entropyEncoding" Lude..=) Lude.<$> entropyEncoding,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("colorMetadata" Lude..=) Lude.<$> colorMetadata,
            ("lookAheadRateControl" Lude..=) Lude.<$> lookAheadRateControl,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("level" Lude..=) Lude.<$> level,
            ("gopBReference" Lude..=) Lude.<$> gopBReference,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("syntax" Lude..=) Lude.<$> syntax,
            ("bufFillPct" Lude..=) Lude.<$> bufFillPct,
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("numRefFrames" Lude..=) Lude.<$> numRefFrames,
            ("parDenominator" Lude..=) Lude.<$> parDenominator
          ]
      )
