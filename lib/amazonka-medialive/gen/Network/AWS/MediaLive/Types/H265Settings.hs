{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Settings
  ( H265Settings (..),

    -- * Smart constructor
    mkH265Settings,

    -- * Lenses
    hFramerateNumerator,
    hFramerateDenominator,
    hAdaptiveQuantization,
    hAfdSignaling,
    hAlternativeTransferFunction,
    hBitrate,
    hBufSize,
    hColorMetadata,
    hColorSpaceSettings,
    hFilterSettings,
    hFixedAfd,
    hFlickerAq,
    hGopClosedCadence,
    hGopSize,
    hGopSizeUnits,
    hLevel,
    hLookAheadRateControl,
    hMaxBitrate,
    hMinIInterval,
    hParDenominator,
    hParNumerator,
    hProfile,
    hQvbrQualityLevel,
    hRateControlMode,
    hScanType,
    hSceneChangeDetect,
    hSlices,
    hTier,
    hTimecodeInsertion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AfdSignaling as Types
import qualified Network.AWS.MediaLive.Types.FixedAfd as Types
import qualified Network.AWS.MediaLive.Types.H265AdaptiveQuantization as Types
import qualified Network.AWS.MediaLive.Types.H265AlternativeTransferFunction as Types
import qualified Network.AWS.MediaLive.Types.H265ColorMetadata as Types
import qualified Network.AWS.MediaLive.Types.H265ColorSpaceSettings as Types
import qualified Network.AWS.MediaLive.Types.H265FilterSettings as Types
import qualified Network.AWS.MediaLive.Types.H265FlickerAq as Types
import qualified Network.AWS.MediaLive.Types.H265GopSizeUnits as Types
import qualified Network.AWS.MediaLive.Types.H265Level as Types
import qualified Network.AWS.MediaLive.Types.H265LookAheadRateControl as Types
import qualified Network.AWS.MediaLive.Types.H265Profile as Types
import qualified Network.AWS.MediaLive.Types.H265RateControlMode as Types
import qualified Network.AWS.MediaLive.Types.H265ScanType as Types
import qualified Network.AWS.MediaLive.Types.H265SceneChangeDetect as Types
import qualified Network.AWS.MediaLive.Types.H265Tier as Types
import qualified Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | H265 Settings
--
-- /See:/ 'mkH265Settings' smart constructor.
data H265Settings = H265Settings'
  { -- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
    framerateNumerator :: Core.Natural,
    -- | Framerate denominator.
    framerateDenominator :: Core.Natural,
    -- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
    adaptiveQuantization :: Core.Maybe Types.H265AdaptiveQuantization,
    -- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
    afdSignaling :: Core.Maybe Types.AfdSignaling,
    -- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
    alternativeTransferFunction :: Core.Maybe Types.H265AlternativeTransferFunction,
    -- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
    bitrate :: Core.Maybe Core.Natural,
    -- | Size of buffer (HRD buffer model) in bits.
    bufSize :: Core.Maybe Core.Natural,
    -- | Includes colorspace metadata in the output.
    colorMetadata :: Core.Maybe Types.H265ColorMetadata,
    -- | Color Space settings
    colorSpaceSettings :: Core.Maybe Types.H265ColorSpaceSettings,
    -- | Optional filters that you can apply to an encode.
    filterSettings :: Core.Maybe Types.H265FilterSettings,
    -- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
    fixedAfd :: Core.Maybe Types.FixedAfd,
    -- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
    flickerAq :: Core.Maybe Types.H265FlickerAq,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
    gopClosedCadence :: Core.Maybe Core.Natural,
    -- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
    --
    -- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
    -- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
    gopSize :: Core.Maybe Core.Double,
    -- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
    gopSizeUnits :: Core.Maybe Types.H265GopSizeUnits,
    -- | H.265 Level.
    level :: Core.Maybe Types.H265Level,
    -- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
    lookAheadRateControl :: Core.Maybe Types.H265LookAheadRateControl,
    -- | For QVBR: See the tooltip for Quality level
    maxBitrate :: Core.Maybe Core.Natural,
    -- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Core.Maybe Core.Natural,
    -- | Pixel Aspect Ratio denominator.
    parDenominator :: Core.Maybe Core.Natural,
    -- | Pixel Aspect Ratio numerator.
    parNumerator :: Core.Maybe Core.Natural,
    -- | H.265 Profile.
    profile :: Core.Maybe Types.H265Profile,
    -- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
    --
    -- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
    -- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
    -- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
    qvbrQualityLevel :: Core.Maybe Core.Natural,
    -- | Rate control mode.
    --
    --
    -- QVBR: Quality will match the specified quality level except when it is constrained by the
    -- maximum bitrate.  Recommended if you or your viewers pay for bandwidth.
    --
    -- CBR: Quality varies, depending on the video complexity. Recommended only if you distribute
    -- your assets to devices that cannot handle variable bitrates.
    --
    -- Multiplex: This rate control mode is only supported (and is required) when the video is being
    -- delivered to a MediaLive Multiplex in which case the rate control configuration is controlled
    -- by the properties within the Multiplex Program.
    rateControlMode :: Core.Maybe Types.H265RateControlMode,
    -- | Sets the scan type of the output to progressive or top-field-first interlaced.
    scanType :: Core.Maybe Types.H265ScanType,
    -- | Scene change detection.
    sceneChangeDetect :: Core.Maybe Types.H265SceneChangeDetect,
    -- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
    --
    -- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
    slices :: Core.Maybe Core.Natural,
    -- | H.265 Tier.
    tier :: Core.Maybe Types.H265Tier,
    -- | Determines how timecodes should be inserted into the video elementary stream.
    --
    -- - 'disabled': Do not include timecodes
    -- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
    timecodeInsertion :: Core.Maybe Types.H265TimecodeInsertionBehavior
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H265Settings' value with any optional fields omitted.
mkH265Settings ::
  -- | 'framerateNumerator'
  Core.Natural ->
  -- | 'framerateDenominator'
  Core.Natural ->
  H265Settings
mkH265Settings framerateNumerator framerateDenominator =
  H265Settings'
    { framerateNumerator,
      framerateDenominator,
      adaptiveQuantization = Core.Nothing,
      afdSignaling = Core.Nothing,
      alternativeTransferFunction = Core.Nothing,
      bitrate = Core.Nothing,
      bufSize = Core.Nothing,
      colorMetadata = Core.Nothing,
      colorSpaceSettings = Core.Nothing,
      filterSettings = Core.Nothing,
      fixedAfd = Core.Nothing,
      flickerAq = Core.Nothing,
      gopClosedCadence = Core.Nothing,
      gopSize = Core.Nothing,
      gopSizeUnits = Core.Nothing,
      level = Core.Nothing,
      lookAheadRateControl = Core.Nothing,
      maxBitrate = Core.Nothing,
      minIInterval = Core.Nothing,
      parDenominator = Core.Nothing,
      parNumerator = Core.Nothing,
      profile = Core.Nothing,
      qvbrQualityLevel = Core.Nothing,
      rateControlMode = Core.Nothing,
      scanType = Core.Nothing,
      sceneChangeDetect = Core.Nothing,
      slices = Core.Nothing,
      tier = Core.Nothing,
      timecodeInsertion = Core.Nothing
    }

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateNumerator :: Lens.Lens' H265Settings Core.Natural
hFramerateNumerator = Lens.field @"framerateNumerator"
{-# DEPRECATED hFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Framerate denominator.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFramerateDenominator :: Lens.Lens' H265Settings Core.Natural
hFramerateDenominator = Lens.field @"framerateDenominator"
{-# DEPRECATED hFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAdaptiveQuantization :: Lens.Lens' H265Settings (Core.Maybe Types.H265AdaptiveQuantization)
hAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# DEPRECATED hAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAfdSignaling :: Lens.Lens' H265Settings (Core.Maybe Types.AfdSignaling)
hAfdSignaling = Lens.field @"afdSignaling"
{-# DEPRECATED hAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
--
-- /Note:/ Consider using 'alternativeTransferFunction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAlternativeTransferFunction :: Lens.Lens' H265Settings (Core.Maybe Types.H265AlternativeTransferFunction)
hAlternativeTransferFunction = Lens.field @"alternativeTransferFunction"
{-# DEPRECATED hAlternativeTransferFunction "Use generic-lens or generic-optics with 'alternativeTransferFunction' instead." #-}

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBitrate :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hBitrate = Lens.field @"bitrate"
{-# DEPRECATED hBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Size of buffer (HRD buffer model) in bits.
--
-- /Note:/ Consider using 'bufSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hBufSize :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hBufSize = Lens.field @"bufSize"
{-# DEPRECATED hBufSize "Use generic-lens or generic-optics with 'bufSize' instead." #-}

-- | Includes colorspace metadata in the output.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorMetadata :: Lens.Lens' H265Settings (Core.Maybe Types.H265ColorMetadata)
hColorMetadata = Lens.field @"colorMetadata"
{-# DEPRECATED hColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Color Space settings
--
-- /Note:/ Consider using 'colorSpaceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorSpaceSettings :: Lens.Lens' H265Settings (Core.Maybe Types.H265ColorSpaceSettings)
hColorSpaceSettings = Lens.field @"colorSpaceSettings"
{-# DEPRECATED hColorSpaceSettings "Use generic-lens or generic-optics with 'colorSpaceSettings' instead." #-}

-- | Optional filters that you can apply to an encode.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFilterSettings :: Lens.Lens' H265Settings (Core.Maybe Types.H265FilterSettings)
hFilterSettings = Lens.field @"filterSettings"
{-# DEPRECATED hFilterSettings "Use generic-lens or generic-optics with 'filterSettings' instead." #-}

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFixedAfd :: Lens.Lens' H265Settings (Core.Maybe Types.FixedAfd)
hFixedAfd = Lens.field @"fixedAfd"
{-# DEPRECATED hFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- /Note:/ Consider using 'flickerAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFlickerAq :: Lens.Lens' H265Settings (Core.Maybe Types.H265FlickerAq)
hFlickerAq = Lens.field @"flickerAq"
{-# DEPRECATED hFlickerAq "Use generic-lens or generic-optics with 'flickerAq' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopClosedCadence :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hGopClosedCadence = Lens.field @"gopClosedCadence"
{-# DEPRECATED hGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSize :: Lens.Lens' H265Settings (Core.Maybe Core.Double)
hGopSize = Lens.field @"gopSize"
{-# DEPRECATED hGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hGopSizeUnits :: Lens.Lens' H265Settings (Core.Maybe Types.H265GopSizeUnits)
hGopSizeUnits = Lens.field @"gopSizeUnits"
{-# DEPRECATED hGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | H.265 Level.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLevel :: Lens.Lens' H265Settings (Core.Maybe Types.H265Level)
hLevel = Lens.field @"level"
{-# DEPRECATED hLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- /Note:/ Consider using 'lookAheadRateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hLookAheadRateControl :: Lens.Lens' H265Settings (Core.Maybe Types.H265LookAheadRateControl)
hLookAheadRateControl = Lens.field @"lookAheadRateControl"
{-# DEPRECATED hLookAheadRateControl "Use generic-lens or generic-optics with 'lookAheadRateControl' instead." #-}

-- | For QVBR: See the tooltip for Quality level
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMaxBitrate :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hMaxBitrate = Lens.field @"maxBitrate"
{-# DEPRECATED hMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMinIInterval :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hMinIInterval = Lens.field @"minIInterval"
{-# DEPRECATED hMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Pixel Aspect Ratio denominator.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParDenominator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hParDenominator = Lens.field @"parDenominator"
{-# DEPRECATED hParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

-- | Pixel Aspect Ratio numerator.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParNumerator :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hParNumerator = Lens.field @"parNumerator"
{-# DEPRECATED hParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | H.265 Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hProfile :: Lens.Lens' H265Settings (Core.Maybe Types.H265Profile)
hProfile = Lens.field @"profile"
{-# DEPRECATED hProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrQualityLevel :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hQvbrQualityLevel = Lens.field @"qvbrQualityLevel"
{-# DEPRECATED hQvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead." #-}

-- | Rate control mode.
--
--
-- QVBR: Quality will match the specified quality level except when it is constrained by the
-- maximum bitrate.  Recommended if you or your viewers pay for bandwidth.
--
-- CBR: Quality varies, depending on the video complexity. Recommended only if you distribute
-- your assets to devices that cannot handle variable bitrates.
--
-- Multiplex: This rate control mode is only supported (and is required) when the video is being
-- delivered to a MediaLive Multiplex in which case the rate control configuration is controlled
-- by the properties within the Multiplex Program.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRateControlMode :: Lens.Lens' H265Settings (Core.Maybe Types.H265RateControlMode)
hRateControlMode = Lens.field @"rateControlMode"
{-# DEPRECATED hRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hScanType :: Lens.Lens' H265Settings (Core.Maybe Types.H265ScanType)
hScanType = Lens.field @"scanType"
{-# DEPRECATED hScanType "Use generic-lens or generic-optics with 'scanType' instead." #-}

-- | Scene change detection.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSceneChangeDetect :: Lens.Lens' H265Settings (Core.Maybe Types.H265SceneChangeDetect)
hSceneChangeDetect = Lens.field @"sceneChangeDetect"
{-# DEPRECATED hSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSlices :: Lens.Lens' H265Settings (Core.Maybe Core.Natural)
hSlices = Lens.field @"slices"
{-# DEPRECATED hSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | H.265 Tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTier :: Lens.Lens' H265Settings (Core.Maybe Types.H265Tier)
hTier = Lens.field @"tier"
{-# DEPRECATED hTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTimecodeInsertion :: Lens.Lens' H265Settings (Core.Maybe Types.H265TimecodeInsertionBehavior)
hTimecodeInsertion = Lens.field @"timecodeInsertion"
{-# DEPRECATED hTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

instance Core.FromJSON H265Settings where
  toJSON H265Settings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("framerateNumerator" Core..= framerateNumerator),
            Core.Just ("framerateDenominator" Core..= framerateDenominator),
            ("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
            ("afdSignaling" Core..=) Core.<$> afdSignaling,
            ("alternativeTransferFunction" Core..=)
              Core.<$> alternativeTransferFunction,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("bufSize" Core..=) Core.<$> bufSize,
            ("colorMetadata" Core..=) Core.<$> colorMetadata,
            ("colorSpaceSettings" Core..=) Core.<$> colorSpaceSettings,
            ("filterSettings" Core..=) Core.<$> filterSettings,
            ("fixedAfd" Core..=) Core.<$> fixedAfd,
            ("flickerAq" Core..=) Core.<$> flickerAq,
            ("gopClosedCadence" Core..=) Core.<$> gopClosedCadence,
            ("gopSize" Core..=) Core.<$> gopSize,
            ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
            ("level" Core..=) Core.<$> level,
            ("lookAheadRateControl" Core..=) Core.<$> lookAheadRateControl,
            ("maxBitrate" Core..=) Core.<$> maxBitrate,
            ("minIInterval" Core..=) Core.<$> minIInterval,
            ("parDenominator" Core..=) Core.<$> parDenominator,
            ("parNumerator" Core..=) Core.<$> parNumerator,
            ("profile" Core..=) Core.<$> profile,
            ("qvbrQualityLevel" Core..=) Core.<$> qvbrQualityLevel,
            ("rateControlMode" Core..=) Core.<$> rateControlMode,
            ("scanType" Core..=) Core.<$> scanType,
            ("sceneChangeDetect" Core..=) Core.<$> sceneChangeDetect,
            ("slices" Core..=) Core.<$> slices,
            ("tier" Core..=) Core.<$> tier,
            ("timecodeInsertion" Core..=) Core.<$> timecodeInsertion
          ]
      )

instance Core.FromJSON H265Settings where
  parseJSON =
    Core.withObject "H265Settings" Core.$
      \x ->
        H265Settings'
          Core.<$> (x Core..: "framerateNumerator")
          Core.<*> (x Core..: "framerateDenominator")
          Core.<*> (x Core..:? "adaptiveQuantization")
          Core.<*> (x Core..:? "afdSignaling")
          Core.<*> (x Core..:? "alternativeTransferFunction")
          Core.<*> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "bufSize")
          Core.<*> (x Core..:? "colorMetadata")
          Core.<*> (x Core..:? "colorSpaceSettings")
          Core.<*> (x Core..:? "filterSettings")
          Core.<*> (x Core..:? "fixedAfd")
          Core.<*> (x Core..:? "flickerAq")
          Core.<*> (x Core..:? "gopClosedCadence")
          Core.<*> (x Core..:? "gopSize")
          Core.<*> (x Core..:? "gopSizeUnits")
          Core.<*> (x Core..:? "level")
          Core.<*> (x Core..:? "lookAheadRateControl")
          Core.<*> (x Core..:? "maxBitrate")
          Core.<*> (x Core..:? "minIInterval")
          Core.<*> (x Core..:? "parDenominator")
          Core.<*> (x Core..:? "parNumerator")
          Core.<*> (x Core..:? "profile")
          Core.<*> (x Core..:? "qvbrQualityLevel")
          Core.<*> (x Core..:? "rateControlMode")
          Core.<*> (x Core..:? "scanType")
          Core.<*> (x Core..:? "sceneChangeDetect")
          Core.<*> (x Core..:? "slices")
          Core.<*> (x Core..:? "tier")
          Core.<*> (x Core..:? "timecodeInsertion")
