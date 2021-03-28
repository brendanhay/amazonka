{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264Settings
  ( H264Settings (..)
  -- * Smart constructor
  , mkH264Settings
  -- * Lenses
  , hsAdaptiveQuantization
  , hsAfdSignaling
  , hsBitrate
  , hsBufFillPct
  , hsBufSize
  , hsColorMetadata
  , hsColorSpaceSettings
  , hsEntropyEncoding
  , hsFilterSettings
  , hsFixedAfd
  , hsFlickerAq
  , hsForceFieldPictures
  , hsFramerateControl
  , hsFramerateDenominator
  , hsFramerateNumerator
  , hsGopBReference
  , hsGopClosedCadence
  , hsGopNumBFrames
  , hsGopSize
  , hsGopSizeUnits
  , hsLevel
  , hsLookAheadRateControl
  , hsMaxBitrate
  , hsMinIInterval
  , hsNumRefFrames
  , hsParControl
  , hsParDenominator
  , hsParNumerator
  , hsProfile
  , hsQualityLevel
  , hsQvbrQualityLevel
  , hsRateControlMode
  , hsScanType
  , hsSceneChangeDetect
  , hsSlices
  , hsSoftness
  , hsSpatialAq
  , hsSubgopLength
  , hsSyntax
  , hsTemporalAq
  , hsTimecodeInsertion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AfdSignaling as Types
import qualified Network.AWS.MediaLive.Types.FixedAfd as Types
import qualified Network.AWS.MediaLive.Types.H264AdaptiveQuantization as Types
import qualified Network.AWS.MediaLive.Types.H264ColorMetadata as Types
import qualified Network.AWS.MediaLive.Types.H264ColorSpaceSettings as Types
import qualified Network.AWS.MediaLive.Types.H264EntropyEncoding as Types
import qualified Network.AWS.MediaLive.Types.H264FilterSettings as Types
import qualified Network.AWS.MediaLive.Types.H264FlickerAq as Types
import qualified Network.AWS.MediaLive.Types.H264ForceFieldPictures as Types
import qualified Network.AWS.MediaLive.Types.H264FramerateControl as Types
import qualified Network.AWS.MediaLive.Types.H264GopBReference as Types
import qualified Network.AWS.MediaLive.Types.H264GopSizeUnits as Types
import qualified Network.AWS.MediaLive.Types.H264Level as Types
import qualified Network.AWS.MediaLive.Types.H264LookAheadRateControl as Types
import qualified Network.AWS.MediaLive.Types.H264ParControl as Types
import qualified Network.AWS.MediaLive.Types.H264Profile as Types
import qualified Network.AWS.MediaLive.Types.H264QualityLevel as Types
import qualified Network.AWS.MediaLive.Types.H264RateControlMode as Types
import qualified Network.AWS.MediaLive.Types.H264ScanType as Types
import qualified Network.AWS.MediaLive.Types.H264SceneChangeDetect as Types
import qualified Network.AWS.MediaLive.Types.H264SpatialAq as Types
import qualified Network.AWS.MediaLive.Types.H264SubGopLength as Types
import qualified Network.AWS.MediaLive.Types.H264Syntax as Types
import qualified Network.AWS.MediaLive.Types.H264TemporalAq as Types
import qualified Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | H264 Settings
--
-- /See:/ 'mkH264Settings' smart constructor.
data H264Settings = H264Settings'
  { adaptiveQuantization :: Core.Maybe Types.H264AdaptiveQuantization
    -- ^ Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
  , afdSignaling :: Core.Maybe Types.AfdSignaling
    -- ^ Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
  , bufFillPct :: Core.Maybe Core.Natural
    -- ^ Percentage of the buffer that should initially be filled (HRD buffer model).
  , bufSize :: Core.Maybe Core.Natural
    -- ^ Size of buffer (HRD buffer model) in bits.
  , colorMetadata :: Core.Maybe Types.H264ColorMetadata
    -- ^ Includes colorspace metadata in the output.
  , colorSpaceSettings :: Core.Maybe Types.H264ColorSpaceSettings
    -- ^ Color Space settings
  , entropyEncoding :: Core.Maybe Types.H264EntropyEncoding
    -- ^ Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
  , filterSettings :: Core.Maybe Types.H264FilterSettings
    -- ^ Optional filters that you can apply to an encode.
  , fixedAfd :: Core.Maybe Types.FixedAfd
    -- ^ Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
  , flickerAq :: Core.Maybe Types.H264FlickerAq
    -- ^ If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
  , forceFieldPictures :: Core.Maybe Types.H264ForceFieldPictures
    -- ^ This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.)
--
-- enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately.
-- disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
  , framerateControl :: Core.Maybe Types.H264FramerateControl
    -- ^ This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ Framerate denominator.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
  , gopBReference :: Core.Maybe Types.H264GopBReference
    -- ^ Documentation update needed
  , gopClosedCadence :: Core.Maybe Core.Natural
    -- ^ Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
  , gopNumBFrames :: Core.Maybe Core.Natural
    -- ^ Number of B-frames between reference frames.
  , gopSize :: Core.Maybe Core.Double
    -- ^ GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
  , gopSizeUnits :: Core.Maybe Types.H264GopSizeUnits
    -- ^ Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
  , level :: Core.Maybe Types.H264Level
    -- ^ H.264 Level.
  , lookAheadRateControl :: Core.Maybe Types.H264LookAheadRateControl
    -- ^ Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
  , maxBitrate :: Core.Maybe Core.Natural
    -- ^ For QVBR: See the tooltip for Quality level
--
--
-- For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
  , minIInterval :: Core.Maybe Core.Natural
    -- ^ Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
  , numRefFrames :: Core.Maybe Core.Natural
    -- ^ Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
  , parControl :: Core.Maybe Types.H264ParControl
    -- ^ This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
  , parDenominator :: Core.Maybe Core.Natural
    -- ^ Pixel Aspect Ratio denominator.
  , parNumerator :: Core.Maybe Core.Natural
    -- ^ Pixel Aspect Ratio numerator.
  , profile :: Core.Maybe Types.H264Profile
    -- ^ H.264 Profile.
  , qualityLevel :: Core.Maybe Types.H264QualityLevel
    -- ^ Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel).
--
-- - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY.
-- - STANDARD_QUALITY: Valid for any Rate control mode.
  , qvbrQualityLevel :: Core.Maybe Core.Natural
    -- ^ Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
  , rateControlMode :: Core.Maybe Types.H264RateControlMode
    -- ^ Rate control mode.
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
  , scanType :: Core.Maybe Types.H264ScanType
    -- ^ Sets the scan type of the output to progressive or top-field-first interlaced.
  , sceneChangeDetect :: Core.Maybe Types.H264SceneChangeDetect
    -- ^ Scene change detection.
--
--
-- - On: inserts I-frames when scene change is detected.
-- - Off: does not force an I-frame when scene change is detected.
  , slices :: Core.Maybe Core.Natural
    -- ^ Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
  , softness :: Core.Maybe Core.Natural
    -- ^ Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
  , spatialAq :: Core.Maybe Types.H264SpatialAq
    -- ^ If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
  , subgopLength :: Core.Maybe Types.H264SubGopLength
    -- ^ If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
  , syntax :: Core.Maybe Types.H264Syntax
    -- ^ Produces a bitstream compliant with SMPTE RP-2027.
  , temporalAq :: Core.Maybe Types.H264TemporalAq
    -- ^ If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
  , timecodeInsertion :: Core.Maybe Types.H264TimecodeInsertionBehavior
    -- ^ Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H264Settings' value with any optional fields omitted.
mkH264Settings
    :: H264Settings
mkH264Settings
  = H264Settings'{adaptiveQuantization = Core.Nothing,
                  afdSignaling = Core.Nothing, bitrate = Core.Nothing,
                  bufFillPct = Core.Nothing, bufSize = Core.Nothing,
                  colorMetadata = Core.Nothing, colorSpaceSettings = Core.Nothing,
                  entropyEncoding = Core.Nothing, filterSettings = Core.Nothing,
                  fixedAfd = Core.Nothing, flickerAq = Core.Nothing,
                  forceFieldPictures = Core.Nothing, framerateControl = Core.Nothing,
                  framerateDenominator = Core.Nothing,
                  framerateNumerator = Core.Nothing, gopBReference = Core.Nothing,
                  gopClosedCadence = Core.Nothing, gopNumBFrames = Core.Nothing,
                  gopSize = Core.Nothing, gopSizeUnits = Core.Nothing,
                  level = Core.Nothing, lookAheadRateControl = Core.Nothing,
                  maxBitrate = Core.Nothing, minIInterval = Core.Nothing,
                  numRefFrames = Core.Nothing, parControl = Core.Nothing,
                  parDenominator = Core.Nothing, parNumerator = Core.Nothing,
                  profile = Core.Nothing, qualityLevel = Core.Nothing,
                  qvbrQualityLevel = Core.Nothing, rateControlMode = Core.Nothing,
                  scanType = Core.Nothing, sceneChangeDetect = Core.Nothing,
                  slices = Core.Nothing, softness = Core.Nothing,
                  spatialAq = Core.Nothing, subgopLength = Core.Nothing,
                  syntax = Core.Nothing, temporalAq = Core.Nothing,
                  timecodeInsertion = Core.Nothing}

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAdaptiveQuantization :: Lens.Lens' H264Settings (Core.Maybe Types.H264AdaptiveQuantization)
hsAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE hsAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAfdSignaling :: Lens.Lens' H264Settings (Core.Maybe Types.AfdSignaling)
hsAfdSignaling = Lens.field @"afdSignaling"
{-# INLINEABLE hsAfdSignaling #-}
{-# DEPRECATED afdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead"  #-}

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsBitrate = Lens.field @"bitrate"
{-# INLINEABLE hsBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- /Note:/ Consider using 'bufFillPct' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBufFillPct :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsBufFillPct = Lens.field @"bufFillPct"
{-# INLINEABLE hsBufFillPct #-}
{-# DEPRECATED bufFillPct "Use generic-lens or generic-optics with 'bufFillPct' instead"  #-}

-- | Size of buffer (HRD buffer model) in bits.
--
-- /Note:/ Consider using 'bufSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsBufSize :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsBufSize = Lens.field @"bufSize"
{-# INLINEABLE hsBufSize #-}
{-# DEPRECATED bufSize "Use generic-lens or generic-optics with 'bufSize' instead"  #-}

-- | Includes colorspace metadata in the output.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsColorMetadata :: Lens.Lens' H264Settings (Core.Maybe Types.H264ColorMetadata)
hsColorMetadata = Lens.field @"colorMetadata"
{-# INLINEABLE hsColorMetadata #-}
{-# DEPRECATED colorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead"  #-}

-- | Color Space settings
--
-- /Note:/ Consider using 'colorSpaceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsColorSpaceSettings :: Lens.Lens' H264Settings (Core.Maybe Types.H264ColorSpaceSettings)
hsColorSpaceSettings = Lens.field @"colorSpaceSettings"
{-# INLINEABLE hsColorSpaceSettings #-}
{-# DEPRECATED colorSpaceSettings "Use generic-lens or generic-optics with 'colorSpaceSettings' instead"  #-}

-- | Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
--
-- /Note:/ Consider using 'entropyEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsEntropyEncoding :: Lens.Lens' H264Settings (Core.Maybe Types.H264EntropyEncoding)
hsEntropyEncoding = Lens.field @"entropyEncoding"
{-# INLINEABLE hsEntropyEncoding #-}
{-# DEPRECATED entropyEncoding "Use generic-lens or generic-optics with 'entropyEncoding' instead"  #-}

-- | Optional filters that you can apply to an encode.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFilterSettings :: Lens.Lens' H264Settings (Core.Maybe Types.H264FilterSettings)
hsFilterSettings = Lens.field @"filterSettings"
{-# INLINEABLE hsFilterSettings #-}
{-# DEPRECATED filterSettings "Use generic-lens or generic-optics with 'filterSettings' instead"  #-}

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFixedAfd :: Lens.Lens' H264Settings (Core.Maybe Types.FixedAfd)
hsFixedAfd = Lens.field @"fixedAfd"
{-# INLINEABLE hsFixedAfd #-}
{-# DEPRECATED fixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead"  #-}

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- /Note:/ Consider using 'flickerAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFlickerAq :: Lens.Lens' H264Settings (Core.Maybe Types.H264FlickerAq)
hsFlickerAq = Lens.field @"flickerAq"
{-# INLINEABLE hsFlickerAq #-}
{-# DEPRECATED flickerAq "Use generic-lens or generic-optics with 'flickerAq' instead"  #-}

-- | This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.)
--
-- enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately.
-- disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
--
-- /Note:/ Consider using 'forceFieldPictures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsForceFieldPictures :: Lens.Lens' H264Settings (Core.Maybe Types.H264ForceFieldPictures)
hsForceFieldPictures = Lens.field @"forceFieldPictures"
{-# INLINEABLE hsForceFieldPictures #-}
{-# DEPRECATED forceFieldPictures "Use generic-lens or generic-optics with 'forceFieldPictures' instead"  #-}

-- | This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateControl :: Lens.Lens' H264Settings (Core.Maybe Types.H264FramerateControl)
hsFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE hsFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Framerate denominator.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE hsFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFramerateNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE hsFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'gopBReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopBReference :: Lens.Lens' H264Settings (Core.Maybe Types.H264GopBReference)
hsGopBReference = Lens.field @"gopBReference"
{-# INLINEABLE hsGopBReference #-}
{-# DEPRECATED gopBReference "Use generic-lens or generic-optics with 'gopBReference' instead"  #-}

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopClosedCadence :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsGopClosedCadence = Lens.field @"gopClosedCadence"
{-# INLINEABLE hsGopClosedCadence #-}
{-# DEPRECATED gopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead"  #-}

-- | Number of B-frames between reference frames.
--
-- /Note:/ Consider using 'gopNumBFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopNumBFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsGopNumBFrames = Lens.field @"gopNumBFrames"
{-# INLINEABLE hsGopNumBFrames #-}
{-# DEPRECATED gopNumBFrames "Use generic-lens or generic-optics with 'gopNumBFrames' instead"  #-}

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits.
--
-- If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSize :: Lens.Lens' H264Settings (Core.Maybe Core.Double)
hsGopSize = Lens.field @"gopSize"
{-# INLINEABLE hsGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsGopSizeUnits :: Lens.Lens' H264Settings (Core.Maybe Types.H264GopSizeUnits)
hsGopSizeUnits = Lens.field @"gopSizeUnits"
{-# INLINEABLE hsGopSizeUnits #-}
{-# DEPRECATED gopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead"  #-}

-- | H.264 Level.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevel :: Lens.Lens' H264Settings (Core.Maybe Types.H264Level)
hsLevel = Lens.field @"level"
{-# INLINEABLE hsLevel #-}
{-# DEPRECATED level "Use generic-lens or generic-optics with 'level' instead"  #-}

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- /Note:/ Consider using 'lookAheadRateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLookAheadRateControl :: Lens.Lens' H264Settings (Core.Maybe Types.H264LookAheadRateControl)
hsLookAheadRateControl = Lens.field @"lookAheadRateControl"
{-# INLINEABLE hsLookAheadRateControl #-}
{-# DEPRECATED lookAheadRateControl "Use generic-lens or generic-optics with 'lookAheadRateControl' instead"  #-}

-- | For QVBR: See the tooltip for Quality level
--
--
-- For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxBitrate :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE hsMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- /Note:/ Consider using 'minIInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMinIInterval :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsMinIInterval = Lens.field @"minIInterval"
{-# INLINEABLE hsMinIInterval #-}
{-# DEPRECATED minIInterval "Use generic-lens or generic-optics with 'minIInterval' instead"  #-}

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- /Note:/ Consider using 'numRefFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsNumRefFrames :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsNumRefFrames = Lens.field @"numRefFrames"
{-# INLINEABLE hsNumRefFrames #-}
{-# DEPRECATED numRefFrames "Use generic-lens or generic-optics with 'numRefFrames' instead"  #-}

-- | This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParControl :: Lens.Lens' H264Settings (Core.Maybe Types.H264ParControl)
hsParControl = Lens.field @"parControl"
{-# INLINEABLE hsParControl #-}
{-# DEPRECATED parControl "Use generic-lens or generic-optics with 'parControl' instead"  #-}

-- | Pixel Aspect Ratio denominator.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParDenominator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsParDenominator = Lens.field @"parDenominator"
{-# INLINEABLE hsParDenominator #-}
{-# DEPRECATED parDenominator "Use generic-lens or generic-optics with 'parDenominator' instead"  #-}

-- | Pixel Aspect Ratio numerator.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsParNumerator :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsParNumerator = Lens.field @"parNumerator"
{-# INLINEABLE hsParNumerator #-}
{-# DEPRECATED parNumerator "Use generic-lens or generic-optics with 'parNumerator' instead"  #-}

-- | H.264 Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsProfile :: Lens.Lens' H264Settings (Core.Maybe Types.H264Profile)
hsProfile = Lens.field @"profile"
{-# INLINEABLE hsProfile #-}
{-# DEPRECATED profile "Use generic-lens or generic-optics with 'profile' instead"  #-}

-- | Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel).
--
-- - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY.
-- - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- /Note:/ Consider using 'qualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQualityLevel :: Lens.Lens' H264Settings (Core.Maybe Types.H264QualityLevel)
hsQualityLevel = Lens.field @"qualityLevel"
{-# INLINEABLE hsQualityLevel #-}
{-# DEPRECATED qualityLevel "Use generic-lens or generic-optics with 'qualityLevel' instead"  #-}

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are:
--
-- - Primary screen: Quality level: 8 to 10. Max bitrate: 4M
-- - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M
-- - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsQvbrQualityLevel :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsQvbrQualityLevel = Lens.field @"qvbrQualityLevel"
{-# INLINEABLE hsQvbrQualityLevel #-}
{-# DEPRECATED qvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead"  #-}

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
hsRateControlMode :: Lens.Lens' H264Settings (Core.Maybe Types.H264RateControlMode)
hsRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE hsRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsScanType :: Lens.Lens' H264Settings (Core.Maybe Types.H264ScanType)
hsScanType = Lens.field @"scanType"
{-# INLINEABLE hsScanType #-}
{-# DEPRECATED scanType "Use generic-lens or generic-optics with 'scanType' instead"  #-}

-- | Scene change detection.
--
--
-- - On: inserts I-frames when scene change is detected.
-- - Off: does not force an I-frame when scene change is detected.
--
-- /Note:/ Consider using 'sceneChangeDetect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSceneChangeDetect :: Lens.Lens' H264Settings (Core.Maybe Types.H264SceneChangeDetect)
hsSceneChangeDetect = Lens.field @"sceneChangeDetect"
{-# INLINEABLE hsSceneChangeDetect #-}
{-# DEPRECATED sceneChangeDetect "Use generic-lens or generic-optics with 'sceneChangeDetect' instead"  #-}

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSlices :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsSlices = Lens.field @"slices"
{-# INLINEABLE hsSlices #-}
{-# DEPRECATED slices "Use generic-lens or generic-optics with 'slices' instead"  #-}

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- /Note:/ Consider using 'softness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSoftness :: Lens.Lens' H264Settings (Core.Maybe Core.Natural)
hsSoftness = Lens.field @"softness"
{-# INLINEABLE hsSoftness #-}
{-# DEPRECATED softness "Use generic-lens or generic-optics with 'softness' instead"  #-}

-- | If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
--
-- /Note:/ Consider using 'spatialAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSpatialAq :: Lens.Lens' H264Settings (Core.Maybe Types.H264SpatialAq)
hsSpatialAq = Lens.field @"spatialAq"
{-# INLINEABLE hsSpatialAq #-}
{-# DEPRECATED spatialAq "Use generic-lens or generic-optics with 'spatialAq' instead"  #-}

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
--
-- /Note:/ Consider using 'subgopLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSubgopLength :: Lens.Lens' H264Settings (Core.Maybe Types.H264SubGopLength)
hsSubgopLength = Lens.field @"subgopLength"
{-# INLINEABLE hsSubgopLength #-}
{-# DEPRECATED subgopLength "Use generic-lens or generic-optics with 'subgopLength' instead"  #-}

-- | Produces a bitstream compliant with SMPTE RP-2027.
--
-- /Note:/ Consider using 'syntax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSyntax :: Lens.Lens' H264Settings (Core.Maybe Types.H264Syntax)
hsSyntax = Lens.field @"syntax"
{-# INLINEABLE hsSyntax #-}
{-# DEPRECATED syntax "Use generic-lens or generic-optics with 'syntax' instead"  #-}

-- | If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
--
-- /Note:/ Consider using 'temporalAq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTemporalAq :: Lens.Lens' H264Settings (Core.Maybe Types.H264TemporalAq)
hsTemporalAq = Lens.field @"temporalAq"
{-# INLINEABLE hsTemporalAq #-}
{-# DEPRECATED temporalAq "Use generic-lens or generic-optics with 'temporalAq' instead"  #-}

-- | Determines how timecodes should be inserted into the video elementary stream.
--
-- - 'disabled': Do not include timecodes
-- - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsTimecodeInsertion :: Lens.Lens' H264Settings (Core.Maybe Types.H264TimecodeInsertionBehavior)
hsTimecodeInsertion = Lens.field @"timecodeInsertion"
{-# INLINEABLE hsTimecodeInsertion #-}
{-# DEPRECATED timecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead"  #-}

instance Core.FromJSON H264Settings where
        toJSON H264Settings{..}
          = Core.object
              (Core.catMaybes
                 [("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("afdSignaling" Core..=) Core.<$> afdSignaling,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("bufFillPct" Core..=) Core.<$> bufFillPct,
                  ("bufSize" Core..=) Core.<$> bufSize,
                  ("colorMetadata" Core..=) Core.<$> colorMetadata,
                  ("colorSpaceSettings" Core..=) Core.<$> colorSpaceSettings,
                  ("entropyEncoding" Core..=) Core.<$> entropyEncoding,
                  ("filterSettings" Core..=) Core.<$> filterSettings,
                  ("fixedAfd" Core..=) Core.<$> fixedAfd,
                  ("flickerAq" Core..=) Core.<$> flickerAq,
                  ("forceFieldPictures" Core..=) Core.<$> forceFieldPictures,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("gopBReference" Core..=) Core.<$> gopBReference,
                  ("gopClosedCadence" Core..=) Core.<$> gopClosedCadence,
                  ("gopNumBFrames" Core..=) Core.<$> gopNumBFrames,
                  ("gopSize" Core..=) Core.<$> gopSize,
                  ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
                  ("level" Core..=) Core.<$> level,
                  ("lookAheadRateControl" Core..=) Core.<$> lookAheadRateControl,
                  ("maxBitrate" Core..=) Core.<$> maxBitrate,
                  ("minIInterval" Core..=) Core.<$> minIInterval,
                  ("numRefFrames" Core..=) Core.<$> numRefFrames,
                  ("parControl" Core..=) Core.<$> parControl,
                  ("parDenominator" Core..=) Core.<$> parDenominator,
                  ("parNumerator" Core..=) Core.<$> parNumerator,
                  ("profile" Core..=) Core.<$> profile,
                  ("qualityLevel" Core..=) Core.<$> qualityLevel,
                  ("qvbrQualityLevel" Core..=) Core.<$> qvbrQualityLevel,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("scanType" Core..=) Core.<$> scanType,
                  ("sceneChangeDetect" Core..=) Core.<$> sceneChangeDetect,
                  ("slices" Core..=) Core.<$> slices,
                  ("softness" Core..=) Core.<$> softness,
                  ("spatialAq" Core..=) Core.<$> spatialAq,
                  ("subgopLength" Core..=) Core.<$> subgopLength,
                  ("syntax" Core..=) Core.<$> syntax,
                  ("temporalAq" Core..=) Core.<$> temporalAq,
                  ("timecodeInsertion" Core..=) Core.<$> timecodeInsertion])

instance Core.FromJSON H264Settings where
        parseJSON
          = Core.withObject "H264Settings" Core.$
              \ x ->
                H264Settings' Core.<$>
                  (x Core..:? "adaptiveQuantization") Core.<*>
                    x Core..:? "afdSignaling"
                    Core.<*> x Core..:? "bitrate"
                    Core.<*> x Core..:? "bufFillPct"
                    Core.<*> x Core..:? "bufSize"
                    Core.<*> x Core..:? "colorMetadata"
                    Core.<*> x Core..:? "colorSpaceSettings"
                    Core.<*> x Core..:? "entropyEncoding"
                    Core.<*> x Core..:? "filterSettings"
                    Core.<*> x Core..:? "fixedAfd"
                    Core.<*> x Core..:? "flickerAq"
                    Core.<*> x Core..:? "forceFieldPictures"
                    Core.<*> x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "gopBReference"
                    Core.<*> x Core..:? "gopClosedCadence"
                    Core.<*> x Core..:? "gopNumBFrames"
                    Core.<*> x Core..:? "gopSize"
                    Core.<*> x Core..:? "gopSizeUnits"
                    Core.<*> x Core..:? "level"
                    Core.<*> x Core..:? "lookAheadRateControl"
                    Core.<*> x Core..:? "maxBitrate"
                    Core.<*> x Core..:? "minIInterval"
                    Core.<*> x Core..:? "numRefFrames"
                    Core.<*> x Core..:? "parControl"
                    Core.<*> x Core..:? "parDenominator"
                    Core.<*> x Core..:? "parNumerator"
                    Core.<*> x Core..:? "profile"
                    Core.<*> x Core..:? "qualityLevel"
                    Core.<*> x Core..:? "qvbrQualityLevel"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "scanType"
                    Core.<*> x Core..:? "sceneChangeDetect"
                    Core.<*> x Core..:? "slices"
                    Core.<*> x Core..:? "softness"
                    Core.<*> x Core..:? "spatialAq"
                    Core.<*> x Core..:? "subgopLength"
                    Core.<*> x Core..:? "syntax"
                    Core.<*> x Core..:? "temporalAq"
                    Core.<*> x Core..:? "timecodeInsertion"
