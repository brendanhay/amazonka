{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Mpeg2Settings
  ( Mpeg2Settings (..)
  -- * Smart constructor
  , mkMpeg2Settings
  -- * Lenses
  , msFramerateNumerator
  , msFramerateDenominator
  , msAdaptiveQuantization
  , msAfdSignaling
  , msColorMetadata
  , msColorSpace
  , msDisplayAspectRatio
  , msFilterSettings
  , msFixedAfd
  , msGopClosedCadence
  , msGopNumBFrames
  , msGopSize
  , msGopSizeUnits
  , msScanType
  , msSubgopLength
  , msTimecodeInsertion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AfdSignaling as Types
import qualified Network.AWS.MediaLive.Types.FixedAfd as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2ColorMetadata as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2ColorSpace as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2DisplayRatio as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2FilterSettings as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2ScanType as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2SubGopLength as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Mpeg2 Settings
--
-- /See:/ 'mkMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { framerateNumerator :: Core.Natural
    -- ^ The framerate numerator. For example, 24000. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
  , framerateDenominator :: Core.Natural
    -- ^ description": "The framerate denominator. For example, 1001. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
  , adaptiveQuantization :: Core.Maybe Types.Mpeg2AdaptiveQuantization
    -- ^ Choose Off to disable adaptive quantization. Or choose another value to enable the quantizer and set its strength. The strengths are: Auto, Off, Low, Medium, High. When you enable this field, MediaLive allows intra-frame quantizers to vary, which might improve visual quality.
  , afdSignaling :: Core.Maybe Types.AfdSignaling
    -- ^ Indicates the AFD values that MediaLive will write into the video encode. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose AUTO.
--
-- AUTO: MediaLive will try to preserve the input AFD value (in cases where multiple AFD values are valid).
-- FIXED: MediaLive will use the value you specify in fixedAFD.
  , colorMetadata :: Core.Maybe Types.Mpeg2ColorMetadata
    -- ^ Specifies whether to include the color space metadata. The metadata describes the color space that applies to the video (the colorSpace field). We recommend that you insert the metadata.
  , colorSpace :: Core.Maybe Types.Mpeg2ColorSpace
    -- ^ Choose the type of color space conversion to apply to the output. For detailed information on setting up both the input and the output to obtain the desired color space in the output, see the section on \"MediaLive Features - Video - color space\" in the MediaLive User Guide.
--
-- PASSTHROUGH: Keep the color space of the input content - do not convert it.
-- AUTO:Convert all content that is SD to rec 601, and convert all content that is HD to rec 709.
  , displayAspectRatio :: Core.Maybe Types.Mpeg2DisplayRatio
    -- ^ Sets the pixel aspect ratio for the encode.
  , filterSettings :: Core.Maybe Types.Mpeg2FilterSettings
    -- ^ Optionally specify a noise reduction filter, which can improve quality of compressed content. If you do not choose a filter, no filter will be applied.
--
-- TEMPORAL: This filter is useful for both source content that is noisy (when it has excessive digital artifacts) and source content that is clean.
-- When the content is noisy, the filter cleans up the source content before the encoding phase, with these two effects: First, it improves the output video quality because the content has been cleaned up. Secondly, it decreases the bandwidth because MediaLive does not waste bits on encoding noise.
-- When the content is reasonably clean, the filter tends to decrease the bitrate.
  , fixedAfd :: Core.Maybe Types.FixedAfd
    -- ^ Complete this field only when afdSignaling is set to FIXED. Enter the AFD value (4 bits) to write on all frames of the video encode.
  , gopClosedCadence :: Core.Maybe Core.Natural
    -- ^ MPEG2: default is open GOP.
  , gopNumBFrames :: Core.Maybe Core.Natural
    -- ^ Relates to the GOP structure. The number of B-frames between reference frames. If you do not know what a B-frame is, use the default.
  , gopSize :: Core.Maybe Core.Double
    -- ^ Relates to the GOP structure. The GOP size (keyframe interval) in the units specified in gopSizeUnits. If you do not know what GOP is, use the default.
--
-- If gopSizeUnits is frames, then the gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, the gopSize must be greater than 0, but does not need to be an integer.
  , gopSizeUnits :: Core.Maybe Types.Mpeg2GopSizeUnits
    -- ^ Relates to the GOP structure. Specifies whether the gopSize is specified in frames or seconds. If you do not plan to change the default gopSize, leave the default. If you specify SECONDS, MediaLive will internally convert the gop size to a frame count.
  , scanType :: Core.Maybe Types.Mpeg2ScanType
    -- ^ Set the scan type of the output to PROGRESSIVE or INTERLACED (top field first).
  , subgopLength :: Core.Maybe Types.Mpeg2SubGopLength
    -- ^ Relates to the GOP structure. If you do not know what GOP is, use the default.
--
-- FIXED: Set the number of B-frames in each sub-GOP to the value in gopNumBFrames.
-- DYNAMIC: Let MediaLive optimize the number of B-frames in each sub-GOP, to improve visual quality.
  , timecodeInsertion :: Core.Maybe Types.Mpeg2TimecodeInsertionBehavior
    -- ^ Determines how MediaLive inserts timecodes in the output video. For detailed information about setting up the input and the output for a timecode, see the section on \"MediaLive Features - Timecode configuration\" in the MediaLive User Guide.
--
-- DISABLED: do not include timecodes.
-- GOP_TIMECODE: Include timecode metadata in the GOP header.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mpeg2Settings' value with any optional fields omitted.
mkMpeg2Settings
    :: Core.Natural -- ^ 'framerateNumerator'
    -> Core.Natural -- ^ 'framerateDenominator'
    -> Mpeg2Settings
mkMpeg2Settings framerateNumerator framerateDenominator
  = Mpeg2Settings'{framerateNumerator, framerateDenominator,
                   adaptiveQuantization = Core.Nothing, afdSignaling = Core.Nothing,
                   colorMetadata = Core.Nothing, colorSpace = Core.Nothing,
                   displayAspectRatio = Core.Nothing, filterSettings = Core.Nothing,
                   fixedAfd = Core.Nothing, gopClosedCadence = Core.Nothing,
                   gopNumBFrames = Core.Nothing, gopSize = Core.Nothing,
                   gopSizeUnits = Core.Nothing, scanType = Core.Nothing,
                   subgopLength = Core.Nothing, timecodeInsertion = Core.Nothing}

-- | The framerate numerator. For example, 24000. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateNumerator :: Lens.Lens' Mpeg2Settings Core.Natural
msFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE msFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | description": "The framerate denominator. For example, 1001. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateDenominator :: Lens.Lens' Mpeg2Settings Core.Natural
msFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE msFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | Choose Off to disable adaptive quantization. Or choose another value to enable the quantizer and set its strength. The strengths are: Auto, Off, Low, Medium, High. When you enable this field, MediaLive allows intra-frame quantizers to vary, which might improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE msAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | Indicates the AFD values that MediaLive will write into the video encode. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose AUTO.
--
-- AUTO: MediaLive will try to preserve the input AFD value (in cases where multiple AFD values are valid).
-- FIXED: MediaLive will use the value you specify in fixedAFD.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAfdSignaling :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.AfdSignaling)
msAfdSignaling = Lens.field @"afdSignaling"
{-# INLINEABLE msAfdSignaling #-}
{-# DEPRECATED afdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead"  #-}

-- | Specifies whether to include the color space metadata. The metadata describes the color space that applies to the video (the colorSpace field). We recommend that you insert the metadata.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msColorMetadata :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2ColorMetadata)
msColorMetadata = Lens.field @"colorMetadata"
{-# INLINEABLE msColorMetadata #-}
{-# DEPRECATED colorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead"  #-}

-- | Choose the type of color space conversion to apply to the output. For detailed information on setting up both the input and the output to obtain the desired color space in the output, see the section on \"MediaLive Features - Video - color space\" in the MediaLive User Guide.
--
-- PASSTHROUGH: Keep the color space of the input content - do not convert it.
-- AUTO:Convert all content that is SD to rec 601, and convert all content that is HD to rec 709.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msColorSpace :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2ColorSpace)
msColorSpace = Lens.field @"colorSpace"
{-# INLINEABLE msColorSpace #-}
{-# DEPRECATED colorSpace "Use generic-lens or generic-optics with 'colorSpace' instead"  #-}

-- | Sets the pixel aspect ratio for the encode.
--
-- /Note:/ Consider using 'displayAspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msDisplayAspectRatio :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2DisplayRatio)
msDisplayAspectRatio = Lens.field @"displayAspectRatio"
{-# INLINEABLE msDisplayAspectRatio #-}
{-# DEPRECATED displayAspectRatio "Use generic-lens or generic-optics with 'displayAspectRatio' instead"  #-}

-- | Optionally specify a noise reduction filter, which can improve quality of compressed content. If you do not choose a filter, no filter will be applied.
--
-- TEMPORAL: This filter is useful for both source content that is noisy (when it has excessive digital artifacts) and source content that is clean.
-- When the content is noisy, the filter cleans up the source content before the encoding phase, with these two effects: First, it improves the output video quality because the content has been cleaned up. Secondly, it decreases the bandwidth because MediaLive does not waste bits on encoding noise.
-- When the content is reasonably clean, the filter tends to decrease the bitrate.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFilterSettings :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2FilterSettings)
msFilterSettings = Lens.field @"filterSettings"
{-# INLINEABLE msFilterSettings #-}
{-# DEPRECATED filterSettings "Use generic-lens or generic-optics with 'filterSettings' instead"  #-}

-- | Complete this field only when afdSignaling is set to FIXED. Enter the AFD value (4 bits) to write on all frames of the video encode.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFixedAfd :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.FixedAfd)
msFixedAfd = Lens.field @"fixedAfd"
{-# INLINEABLE msFixedAfd #-}
{-# DEPRECATED fixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead"  #-}

-- | MPEG2: default is open GOP.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopClosedCadence :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msGopClosedCadence = Lens.field @"gopClosedCadence"
{-# INLINEABLE msGopClosedCadence #-}
{-# DEPRECATED gopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead"  #-}

-- | Relates to the GOP structure. The number of B-frames between reference frames. If you do not know what a B-frame is, use the default.
--
-- /Note:/ Consider using 'gopNumBFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopNumBFrames :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Natural)
msGopNumBFrames = Lens.field @"gopNumBFrames"
{-# INLINEABLE msGopNumBFrames #-}
{-# DEPRECATED gopNumBFrames "Use generic-lens or generic-optics with 'gopNumBFrames' instead"  #-}

-- | Relates to the GOP structure. The GOP size (keyframe interval) in the units specified in gopSizeUnits. If you do not know what GOP is, use the default.
--
-- If gopSizeUnits is frames, then the gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, the gopSize must be greater than 0, but does not need to be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSize :: Lens.Lens' Mpeg2Settings (Core.Maybe Core.Double)
msGopSize = Lens.field @"gopSize"
{-# INLINEABLE msGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Relates to the GOP structure. Specifies whether the gopSize is specified in frames or seconds. If you do not plan to change the default gopSize, leave the default. If you specify SECONDS, MediaLive will internally convert the gop size to a frame count.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSizeUnits :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2GopSizeUnits)
msGopSizeUnits = Lens.field @"gopSizeUnits"
{-# INLINEABLE msGopSizeUnits #-}
{-# DEPRECATED gopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead"  #-}

-- | Set the scan type of the output to PROGRESSIVE or INTERLACED (top field first).
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScanType :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2ScanType)
msScanType = Lens.field @"scanType"
{-# INLINEABLE msScanType #-}
{-# DEPRECATED scanType "Use generic-lens or generic-optics with 'scanType' instead"  #-}

-- | Relates to the GOP structure. If you do not know what GOP is, use the default.
--
-- FIXED: Set the number of B-frames in each sub-GOP to the value in gopNumBFrames.
-- DYNAMIC: Let MediaLive optimize the number of B-frames in each sub-GOP, to improve visual quality.
--
-- /Note:/ Consider using 'subgopLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSubgopLength :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2SubGopLength)
msSubgopLength = Lens.field @"subgopLength"
{-# INLINEABLE msSubgopLength #-}
{-# DEPRECATED subgopLength "Use generic-lens or generic-optics with 'subgopLength' instead"  #-}

-- | Determines how MediaLive inserts timecodes in the output video. For detailed information about setting up the input and the output for a timecode, see the section on \"MediaLive Features - Timecode configuration\" in the MediaLive User Guide.
--
-- DISABLED: do not include timecodes.
-- GOP_TIMECODE: Include timecode metadata in the GOP header.
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimecodeInsertion :: Lens.Lens' Mpeg2Settings (Core.Maybe Types.Mpeg2TimecodeInsertionBehavior)
msTimecodeInsertion = Lens.field @"timecodeInsertion"
{-# INLINEABLE msTimecodeInsertion #-}
{-# DEPRECATED timecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead"  #-}

instance Core.FromJSON Mpeg2Settings where
        toJSON Mpeg2Settings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("framerateNumerator" Core..= framerateNumerator),
                  Core.Just ("framerateDenominator" Core..= framerateDenominator),
                  ("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("afdSignaling" Core..=) Core.<$> afdSignaling,
                  ("colorMetadata" Core..=) Core.<$> colorMetadata,
                  ("colorSpace" Core..=) Core.<$> colorSpace,
                  ("displayAspectRatio" Core..=) Core.<$> displayAspectRatio,
                  ("filterSettings" Core..=) Core.<$> filterSettings,
                  ("fixedAfd" Core..=) Core.<$> fixedAfd,
                  ("gopClosedCadence" Core..=) Core.<$> gopClosedCadence,
                  ("gopNumBFrames" Core..=) Core.<$> gopNumBFrames,
                  ("gopSize" Core..=) Core.<$> gopSize,
                  ("gopSizeUnits" Core..=) Core.<$> gopSizeUnits,
                  ("scanType" Core..=) Core.<$> scanType,
                  ("subgopLength" Core..=) Core.<$> subgopLength,
                  ("timecodeInsertion" Core..=) Core.<$> timecodeInsertion])

instance Core.FromJSON Mpeg2Settings where
        parseJSON
          = Core.withObject "Mpeg2Settings" Core.$
              \ x ->
                Mpeg2Settings' Core.<$>
                  (x Core..: "framerateNumerator") Core.<*>
                    x Core..: "framerateDenominator"
                    Core.<*> x Core..:? "adaptiveQuantization"
                    Core.<*> x Core..:? "afdSignaling"
                    Core.<*> x Core..:? "colorMetadata"
                    Core.<*> x Core..:? "colorSpace"
                    Core.<*> x Core..:? "displayAspectRatio"
                    Core.<*> x Core..:? "filterSettings"
                    Core.<*> x Core..:? "fixedAfd"
                    Core.<*> x Core..:? "gopClosedCadence"
                    Core.<*> x Core..:? "gopNumBFrames"
                    Core.<*> x Core..:? "gopSize"
                    Core.<*> x Core..:? "gopSizeUnits"
                    Core.<*> x Core..:? "scanType"
                    Core.<*> x Core..:? "subgopLength"
                    Core.<*> x Core..:? "timecodeInsertion"
