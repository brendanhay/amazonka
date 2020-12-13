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
    hsSceneChangeDetect,
    hsScanType,
    hsTimecodeInsertion,
    hsParNumerator,
    hsAfdSignaling,
    hsGopSize,
    hsGopSizeUnits,
    hsSlices,
    hsProfile,
    hsAlternativeTransferFunction,
    hsRateControlMode,
    hsMinIInterval,
    hsQvbrQualityLevel,
    hsColorSpaceSettings,
    hsFlickerAq,
    hsBufSize,
    hsTier,
    hsFixedAfd,
    hsFilterSettings,
    hsBitrate,
    hsFramerateDenominator,
    hsColorMetadata,
    hsLookAheadRateControl,
    hsAdaptiveQuantization,
    hsFramerateNumerator,
    hsLevel,
    hsMaxBitrate,
    hsGopClosedCadence,
    hsParDenominator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.H265AdaptiveQuantization
import Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
import Network.AWS.MediaLive.Types.H265ColorMetadata
import Network.AWS.MediaLive.Types.H265ColorSpaceSettings
import Network.AWS.MediaLive.Types.H265FilterSettings
import Network.AWS.MediaLive.Types.H265FlickerAq
import Network.AWS.MediaLive.Types.H265GopSizeUnits
import Network.AWS.MediaLive.Types.H265Level
import Network.AWS.MediaLive.Types.H265LookAheadRateControl
import Network.AWS.MediaLive.Types.H265Profile
import Network.AWS.MediaLive.Types.H265RateControlMode
import Network.AWS.MediaLive.Types.H265ScanType
import Network.AWS.MediaLive.Types.H265SceneChangeDetect
import Network.AWS.MediaLive.Types.H265Tier
import Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
import qualified Network.AWS.Prelude as Lude

-- | H265 Settings
--
-- /See:/ 'mkH265Settings' smart constructor.
data H265Settings = H265Settings'
  { -- | Scene change detection.
    sceneChangeDetect :: Lude.Maybe H265SceneChangeDetect,
    -- | Sets the scan type of the output to progressive or top-field-first interlaced.
    scanType :: Lude.Maybe H265ScanType,
    -- | Determines how timecodes should be inserted into the video elementary stream.
    --
    -- - 'disabled': Do not include timecodes
    -- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
    timecodeInsertion :: Lude.Maybe H265TimecodeInsertionBehavior,
    -- | Pixel Aspect Ratio numerator.
    parNumerator :: Lude.Maybe Lude.Natural,
    -- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
    afdSignaling :: Lude.Maybe AfdSignaling,
    -- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
    --
    -- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
    -- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
    gopSize :: Lude.Maybe Lude.Double,
    -- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
    gopSizeUnits :: Lude.Maybe H265GopSizeUnits,
    -- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
    --
    -- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
    slices :: Lude.Maybe Lude.Natural,
    -- | H.265 Profile.
    profile :: Lude.Maybe H265Profile,
    -- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
    alternativeTransferFunction :: Lude.Maybe H265AlternativeTransferFunction,
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
    rateControlMode :: Lude.Maybe H265RateControlMode,
    -- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
    minIInterval :: Lude.Maybe Lude.Natural,
    -- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
    --
    -- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
    -- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
    -- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
    qvbrQualityLevel :: Lude.Maybe Lude.Natural,
    -- | Color Space settings
    colorSpaceSettings :: Lude.Maybe H265ColorSpaceSettings,
    -- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
    flickerAq :: Lude.Maybe H265FlickerAq,
    -- | Size of buffer (HRD buffer model) in bits.
    bufSize :: Lude.Maybe Lude.Natural,
    -- | H.265 Tier.
    tier :: Lude.Maybe H265Tier,
    -- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
    fixedAfd :: Lude.Maybe FixedAfd,
    -- | Optional filters that you can apply to an encode.
    filterSettings :: Lude.Maybe H265FilterSettings,
    -- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | Framerate denominator.
    framerateDenominator :: Lude.Natural,
    -- | Includes colorspace metadata in the output.
    colorMetadata :: Lude.Maybe H265ColorMetadata,
    -- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
    lookAheadRateControl :: Lude.Maybe H265LookAheadRateControl,
    -- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
    adaptiveQuantization :: Lude.Maybe H265AdaptiveQuantization,
    -- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
    framerateNumerator :: Lude.Natural,
    -- | H.265 Level.
    level :: Lude.Maybe H265Level,
    -- | For QVBR: See the tooltip for Quality level
    maxBitrate :: Lude.Maybe Lude.Natural,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    -- | Pixel Aspect Ratio denominator.
    parDenominator :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- * 'sceneChangeDetect' - Scene change detection.
-- * 'scanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
-- * 'timecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
-- * 'parNumerator' - Pixel Aspect Ratio numerator.
-- * 'afdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
-- * 'gopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
-- * 'gopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
-- * 'slices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
-- * 'profile' - H.265 Profile.
-- * 'alternativeTransferFunction' - Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
-- * 'rateControlMode' - Rate control mode.
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
-- * 'minIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
-- * 'qvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
-- * 'colorSpaceSettings' - Color Space settings
-- * 'flickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
-- * 'bufSize' - Size of buffer (HRD buffer model) in bits.
-- * 'tier' - H.265 Tier.
-- * 'fixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
-- * 'filterSettings' - Optional filters that you can apply to an encode.
-- * 'bitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
-- * 'framerateDenominator' - Framerate denominator.
-- * 'colorMetadata' - Includes colorspace metadata in the output.
-- * 'lookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
-- * 'adaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
-- * 'framerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
-- * 'level' - H.265 Level.
-- * 'maxBitrate' - For QVBR: See the tooltip for Quality level
-- * 'gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
-- * 'parDenominator' - Pixel Aspect Ratio denominator.
mkH265Settings ::
  -- | 'framerateDenominator'
  Lude.Natural ->
  -- | 'framerateNumerator'
  Lude.Natural ->
  H265Settings
mkH265Settings pFramerateDenominator_ pFramerateNumerator_ =
  H265Settings'
    { sceneChangeDetect = Lude.Nothing,
      scanType = Lude.Nothing,
      timecodeInsertion = Lude.Nothing,
      parNumerator = Lude.Nothing,
      afdSignaling = Lude.Nothing,
      gopSize = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      slices = Lude.Nothing,
      profile = Lude.Nothing,
      alternativeTransferFunction = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      minIInterval = Lude.Nothing,
      qvbrQualityLevel = Lude.Nothing,
      colorSpaceSettings = Lude.Nothing,
      flickerAq = Lude.Nothing,
      bufSize = Lude.Nothing,
      tier = Lude.Nothing,
      fixedAfd = Lude.Nothing,
      filterSettings = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = pFramerateDenominator_,
      colorMetadata = Lude.Nothing,
      lookAheadRateControl = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = pFramerateNumerator_,
      level = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      gopClosedCadence = Lude.Nothing,
      parDenominator = Lude.Nothing
    }

-- | Scene change detection.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSceneChangeDetect :: Lens.Lens' H265Settings (Lude.Maybe H265SceneChangeDetect)
hsSceneChangeDetect = Lens.lens (sceneChangeDetect :: H265Settings -> Lude.Maybe H265SceneChangeDetect) (\s a -> s {sceneChangeDetect = a} :: H265Settings)
{-# DEPRECATED hsSceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead." #-}

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsScanType :: Lens.Lens' H265Settings (Lude.Maybe H265ScanType)
hsScanType = Lens.lens (scanType :: H265Settings -> Lude.Maybe H265ScanType) (\s a -> s {scanType = a} :: H265Settings)
{-# DEPRECATED hsScanType "Use generic-lens or generic-optics with 'scanType' instead." #-}

-- | Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTimecodeInsertion :: Lens.Lens' H265Settings (Lude.Maybe H265TimecodeInsertionBehavior)
hsTimecodeInsertion = Lens.lens (timecodeInsertion :: H265Settings -> Lude.Maybe H265TimecodeInsertionBehavior) (\s a -> s {timecodeInsertion = a} :: H265Settings)
{-# DEPRECATED hsTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

-- | Pixel Aspect Ratio numerator.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParNumerator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsParNumerator = Lens.lens (parNumerator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: H265Settings)
{-# DEPRECATED hsParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAfdSignaling :: Lens.Lens' H265Settings (Lude.Maybe AfdSignaling)
hsAfdSignaling = Lens.lens (afdSignaling :: H265Settings -> Lude.Maybe AfdSignaling) (\s a -> s {afdSignaling = a} :: H265Settings)
{-# DEPRECATED hsAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSize :: Lens.Lens' H265Settings (Lude.Maybe Lude.Double)
hsGopSize = Lens.lens (gopSize :: H265Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: H265Settings)
{-# DEPRECATED hsGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSizeUnits :: Lens.Lens' H265Settings (Lude.Maybe H265GopSizeUnits)
hsGopSizeUnits = Lens.lens (gopSizeUnits :: H265Settings -> Lude.Maybe H265GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: H265Settings)
{-# DEPRECATED hsGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlices :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsSlices = Lens.lens (slices :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {slices = a} :: H265Settings)
{-# DEPRECATED hsSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | H.265 Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsProfile :: Lens.Lens' H265Settings (Lude.Maybe H265Profile)
hsProfile = Lens.lens (profile :: H265Settings -> Lude.Maybe H265Profile) (\s a -> s {profile = a} :: H265Settings)
{-# DEPRECATED hsProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
--
-- /Note:/ Consider using 'alternativeTransferFunction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAlternativeTransferFunction :: Lens.Lens' H265Settings (Lude.Maybe H265AlternativeTransferFunction)
hsAlternativeTransferFunction = Lens.lens (alternativeTransferFunction :: H265Settings -> Lude.Maybe H265AlternativeTransferFunction) (\s a -> s {alternativeTransferFunction = a} :: H265Settings)
{-# DEPRECATED hsAlternativeTransferFunction "Use generic-lens or generic-optics with 'alternativeTransferFunction' instead." #-}

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
hsRateControlMode :: Lens.Lens' H265Settings (Lude.Maybe H265RateControlMode)
hsRateControlMode = Lens.lens (rateControlMode :: H265Settings -> Lude.Maybe H265RateControlMode) (\s a -> s {rateControlMode = a} :: H265Settings)
{-# DEPRECATED hsRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMinIInterval :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsMinIInterval = Lens.lens (minIInterval :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {minIInterval = a} :: H265Settings)
{-# DEPRECATED hsMinIInterval "Use generic-lens or generic-optics with 'minIInterval' instead." #-}

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQvbrQualityLevel :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsQvbrQualityLevel = Lens.lens (qvbrQualityLevel :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {qvbrQualityLevel = a} :: H265Settings)
{-# DEPRECATED hsQvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead." #-}

-- | Color Space settings
--
-- /Note:/ Consider using 'colorSpaceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsColorSpaceSettings :: Lens.Lens' H265Settings (Lude.Maybe H265ColorSpaceSettings)
hsColorSpaceSettings = Lens.lens (colorSpaceSettings :: H265Settings -> Lude.Maybe H265ColorSpaceSettings) (\s a -> s {colorSpaceSettings = a} :: H265Settings)
{-# DEPRECATED hsColorSpaceSettings "Use generic-lens or generic-optics with 'colorSpaceSettings' instead." #-}

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- /Note:/ Consider using 'flickerAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFlickerAq :: Lens.Lens' H265Settings (Lude.Maybe H265FlickerAq)
hsFlickerAq = Lens.lens (flickerAq :: H265Settings -> Lude.Maybe H265FlickerAq) (\s a -> s {flickerAq = a} :: H265Settings)
{-# DEPRECATED hsFlickerAq "Use generic-lens or generic-optics with 'flickerAq' instead." #-}

-- | Size of buffer (HRD buffer model) in bits.
--
-- /Note:/ Consider using 'bufSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBufSize :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsBufSize = Lens.lens (bufSize :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bufSize = a} :: H265Settings)
{-# DEPRECATED hsBufSize "Use generic-lens or generic-optics with 'bufSize' instead." #-}

-- | H.265 Tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTier :: Lens.Lens' H265Settings (Lude.Maybe H265Tier)
hsTier = Lens.lens (tier :: H265Settings -> Lude.Maybe H265Tier) (\s a -> s {tier = a} :: H265Settings)
{-# DEPRECATED hsTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFixedAfd :: Lens.Lens' H265Settings (Lude.Maybe FixedAfd)
hsFixedAfd = Lens.lens (fixedAfd :: H265Settings -> Lude.Maybe FixedAfd) (\s a -> s {fixedAfd = a} :: H265Settings)
{-# DEPRECATED hsFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | Optional filters that you can apply to an encode.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFilterSettings :: Lens.Lens' H265Settings (Lude.Maybe H265FilterSettings)
hsFilterSettings = Lens.lens (filterSettings :: H265Settings -> Lude.Maybe H265FilterSettings) (\s a -> s {filterSettings = a} :: H265Settings)
{-# DEPRECATED hsFilterSettings "Use generic-lens or generic-optics with 'filterSettings' instead." #-}

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBitrate :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsBitrate = Lens.lens (bitrate :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: H265Settings)
{-# DEPRECATED hsBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Framerate denominator.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateDenominator :: Lens.Lens' H265Settings Lude.Natural
hsFramerateDenominator = Lens.lens (framerateDenominator :: H265Settings -> Lude.Natural) (\s a -> s {framerateDenominator = a} :: H265Settings)
{-# DEPRECATED hsFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Includes colorspace metadata in the output.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsColorMetadata :: Lens.Lens' H265Settings (Lude.Maybe H265ColorMetadata)
hsColorMetadata = Lens.lens (colorMetadata :: H265Settings -> Lude.Maybe H265ColorMetadata) (\s a -> s {colorMetadata = a} :: H265Settings)
{-# DEPRECATED hsColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- /Note:/ Consider using 'lookAheadRateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLookAheadRateControl :: Lens.Lens' H265Settings (Lude.Maybe H265LookAheadRateControl)
hsLookAheadRateControl = Lens.lens (lookAheadRateControl :: H265Settings -> Lude.Maybe H265LookAheadRateControl) (\s a -> s {lookAheadRateControl = a} :: H265Settings)
{-# DEPRECATED hsLookAheadRateControl "Use generic-lens or generic-optics with 'lookAheadRateControl' instead." #-}

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAdaptiveQuantization :: Lens.Lens' H265Settings (Lude.Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = Lens.lens (adaptiveQuantization :: H265Settings -> Lude.Maybe H265AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: H265Settings)
{-# DEPRECATED hsAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateNumerator :: Lens.Lens' H265Settings Lude.Natural
hsFramerateNumerator = Lens.lens (framerateNumerator :: H265Settings -> Lude.Natural) (\s a -> s {framerateNumerator = a} :: H265Settings)
{-# DEPRECATED hsFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | H.265 Level.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevel :: Lens.Lens' H265Settings (Lude.Maybe H265Level)
hsLevel = Lens.lens (level :: H265Settings -> Lude.Maybe H265Level) (\s a -> s {level = a} :: H265Settings)
{-# DEPRECATED hsLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | For QVBR: See the tooltip for Quality level
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxBitrate :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsMaxBitrate = Lens.lens (maxBitrate :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: H265Settings)
{-# DEPRECATED hsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopClosedCadence :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsGopClosedCadence = Lens.lens (gopClosedCadence :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: H265Settings)
{-# DEPRECATED hsGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Pixel Aspect Ratio denominator.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParDenominator :: Lens.Lens' H265Settings (Lude.Maybe Lude.Natural)
hsParDenominator = Lens.lens (parDenominator :: H265Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: H265Settings)
{-# DEPRECATED hsParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

instance Lude.FromJSON H265Settings where
  parseJSON =
    Lude.withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            Lude.<$> (x Lude..:? "sceneChangeDetect")
            Lude.<*> (x Lude..:? "scanType")
            Lude.<*> (x Lude..:? "timecodeInsertion")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "afdSignaling")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "slices")
            Lude.<*> (x Lude..:? "profile")
            Lude.<*> (x Lude..:? "alternativeTransferFunction")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "minIInterval")
            Lude.<*> (x Lude..:? "qvbrQualityLevel")
            Lude.<*> (x Lude..:? "colorSpaceSettings")
            Lude.<*> (x Lude..:? "flickerAq")
            Lude.<*> (x Lude..:? "bufSize")
            Lude.<*> (x Lude..:? "tier")
            Lude.<*> (x Lude..:? "fixedAfd")
            Lude.<*> (x Lude..:? "filterSettings")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..: "framerateDenominator")
            Lude.<*> (x Lude..:? "colorMetadata")
            Lude.<*> (x Lude..:? "lookAheadRateControl")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..: "framerateNumerator")
            Lude.<*> (x Lude..:? "level")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "parDenominator")
      )

instance Lude.ToJSON H265Settings where
  toJSON H265Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sceneChangeDetect" Lude..=) Lude.<$> sceneChangeDetect,
            ("scanType" Lude..=) Lude.<$> scanType,
            ("timecodeInsertion" Lude..=) Lude.<$> timecodeInsertion,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("afdSignaling" Lude..=) Lude.<$> afdSignaling,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("slices" Lude..=) Lude.<$> slices,
            ("profile" Lude..=) Lude.<$> profile,
            ("alternativeTransferFunction" Lude..=)
              Lude.<$> alternativeTransferFunction,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("minIInterval" Lude..=) Lude.<$> minIInterval,
            ("qvbrQualityLevel" Lude..=) Lude.<$> qvbrQualityLevel,
            ("colorSpaceSettings" Lude..=) Lude.<$> colorSpaceSettings,
            ("flickerAq" Lude..=) Lude.<$> flickerAq,
            ("bufSize" Lude..=) Lude.<$> bufSize,
            ("tier" Lude..=) Lude.<$> tier,
            ("fixedAfd" Lude..=) Lude.<$> fixedAfd,
            ("filterSettings" Lude..=) Lude.<$> filterSettings,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            Lude.Just ("framerateDenominator" Lude..= framerateDenominator),
            ("colorMetadata" Lude..=) Lude.<$> colorMetadata,
            ("lookAheadRateControl" Lude..=) Lude.<$> lookAheadRateControl,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            Lude.Just ("framerateNumerator" Lude..= framerateNumerator),
            ("level" Lude..=) Lude.<$> level,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("parDenominator" Lude..=) Lude.<$> parDenominator
          ]
      )
