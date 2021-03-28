{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ScheduleActionSettings
  ( ScheduleActionSettings (..)
  -- * Smart constructor
  , mkScheduleActionSettings
  -- * Lenses
  , sasHlsId3SegmentTaggingSettings
  , sasHlsTimedMetadataSettings
  , sasInputPrepareSettings
  , sasInputSwitchSettings
  , sasPauseStateSettings
  , sasScte35ReturnToNetworkSettings
  , sasScte35SpliceInsertSettings
  , sasScte35TimeSignalSettings
  , sasStaticImageActivateSettings
  , sasStaticImageDeactivateSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'mkScheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { hlsId3SegmentTaggingSettings :: Core.Maybe Types.HlsId3SegmentTaggingScheduleActionSettings
    -- ^ Action to insert HLS ID3 segment tagging
  , hlsTimedMetadataSettings :: Core.Maybe Types.HlsTimedMetadataScheduleActionSettings
    -- ^ Action to insert HLS metadata
  , inputPrepareSettings :: Core.Maybe Types.InputPrepareScheduleActionSettings
    -- ^ Action to prepare an input for a future immediate input switch
  , inputSwitchSettings :: Core.Maybe Types.InputSwitchScheduleActionSettings
    -- ^ Action to switch the input
  , pauseStateSettings :: Core.Maybe Types.PauseStateScheduleActionSettings
    -- ^ Action to pause or unpause one or both channel pipelines
  , scte35ReturnToNetworkSettings :: Core.Maybe Types.Scte35ReturnToNetworkScheduleActionSettings
    -- ^ Action to insert SCTE-35 return_to_network message
  , scte35SpliceInsertSettings :: Core.Maybe Types.Scte35SpliceInsertScheduleActionSettings
    -- ^ Action to insert SCTE-35 splice_insert message
  , scte35TimeSignalSettings :: Core.Maybe Types.Scte35TimeSignalScheduleActionSettings
    -- ^ Action to insert SCTE-35 time_signal message
  , staticImageActivateSettings :: Core.Maybe Types.StaticImageActivateScheduleActionSettings
    -- ^ Action to activate a static image overlay
  , staticImageDeactivateSettings :: Core.Maybe Types.StaticImageDeactivateScheduleActionSettings
    -- ^ Action to deactivate a static image overlay
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleActionSettings' value with any optional fields omitted.
mkScheduleActionSettings
    :: ScheduleActionSettings
mkScheduleActionSettings
  = ScheduleActionSettings'{hlsId3SegmentTaggingSettings =
                              Core.Nothing,
                            hlsTimedMetadataSettings = Core.Nothing,
                            inputPrepareSettings = Core.Nothing,
                            inputSwitchSettings = Core.Nothing,
                            pauseStateSettings = Core.Nothing,
                            scte35ReturnToNetworkSettings = Core.Nothing,
                            scte35SpliceInsertSettings = Core.Nothing,
                            scte35TimeSignalSettings = Core.Nothing,
                            staticImageActivateSettings = Core.Nothing,
                            staticImageDeactivateSettings = Core.Nothing}

-- | Action to insert HLS ID3 segment tagging
--
-- /Note:/ Consider using 'hlsId3SegmentTaggingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasHlsId3SegmentTaggingSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.HlsId3SegmentTaggingScheduleActionSettings)
sasHlsId3SegmentTaggingSettings = Lens.field @"hlsId3SegmentTaggingSettings"
{-# INLINEABLE sasHlsId3SegmentTaggingSettings #-}
{-# DEPRECATED hlsId3SegmentTaggingSettings "Use generic-lens or generic-optics with 'hlsId3SegmentTaggingSettings' instead"  #-}

-- | Action to insert HLS metadata
--
-- /Note:/ Consider using 'hlsTimedMetadataSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasHlsTimedMetadataSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.HlsTimedMetadataScheduleActionSettings)
sasHlsTimedMetadataSettings = Lens.field @"hlsTimedMetadataSettings"
{-# INLINEABLE sasHlsTimedMetadataSettings #-}
{-# DEPRECATED hlsTimedMetadataSettings "Use generic-lens or generic-optics with 'hlsTimedMetadataSettings' instead"  #-}

-- | Action to prepare an input for a future immediate input switch
--
-- /Note:/ Consider using 'inputPrepareSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasInputPrepareSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.InputPrepareScheduleActionSettings)
sasInputPrepareSettings = Lens.field @"inputPrepareSettings"
{-# INLINEABLE sasInputPrepareSettings #-}
{-# DEPRECATED inputPrepareSettings "Use generic-lens or generic-optics with 'inputPrepareSettings' instead"  #-}

-- | Action to switch the input
--
-- /Note:/ Consider using 'inputSwitchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasInputSwitchSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.InputSwitchScheduleActionSettings)
sasInputSwitchSettings = Lens.field @"inputSwitchSettings"
{-# INLINEABLE sasInputSwitchSettings #-}
{-# DEPRECATED inputSwitchSettings "Use generic-lens or generic-optics with 'inputSwitchSettings' instead"  #-}

-- | Action to pause or unpause one or both channel pipelines
--
-- /Note:/ Consider using 'pauseStateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasPauseStateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.PauseStateScheduleActionSettings)
sasPauseStateSettings = Lens.field @"pauseStateSettings"
{-# INLINEABLE sasPauseStateSettings #-}
{-# DEPRECATED pauseStateSettings "Use generic-lens or generic-optics with 'pauseStateSettings' instead"  #-}

-- | Action to insert SCTE-35 return_to_network message
--
-- /Note:/ Consider using 'scte35ReturnToNetworkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35ReturnToNetworkSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.Scte35ReturnToNetworkScheduleActionSettings)
sasScte35ReturnToNetworkSettings = Lens.field @"scte35ReturnToNetworkSettings"
{-# INLINEABLE sasScte35ReturnToNetworkSettings #-}
{-# DEPRECATED scte35ReturnToNetworkSettings "Use generic-lens or generic-optics with 'scte35ReturnToNetworkSettings' instead"  #-}

-- | Action to insert SCTE-35 splice_insert message
--
-- /Note:/ Consider using 'scte35SpliceInsertSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35SpliceInsertSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.Scte35SpliceInsertScheduleActionSettings)
sasScte35SpliceInsertSettings = Lens.field @"scte35SpliceInsertSettings"
{-# INLINEABLE sasScte35SpliceInsertSettings #-}
{-# DEPRECATED scte35SpliceInsertSettings "Use generic-lens or generic-optics with 'scte35SpliceInsertSettings' instead"  #-}

-- | Action to insert SCTE-35 time_signal message
--
-- /Note:/ Consider using 'scte35TimeSignalSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35TimeSignalSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.Scte35TimeSignalScheduleActionSettings)
sasScte35TimeSignalSettings = Lens.field @"scte35TimeSignalSettings"
{-# INLINEABLE sasScte35TimeSignalSettings #-}
{-# DEPRECATED scte35TimeSignalSettings "Use generic-lens or generic-optics with 'scte35TimeSignalSettings' instead"  #-}

-- | Action to activate a static image overlay
--
-- /Note:/ Consider using 'staticImageActivateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStaticImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.StaticImageActivateScheduleActionSettings)
sasStaticImageActivateSettings = Lens.field @"staticImageActivateSettings"
{-# INLINEABLE sasStaticImageActivateSettings #-}
{-# DEPRECATED staticImageActivateSettings "Use generic-lens or generic-optics with 'staticImageActivateSettings' instead"  #-}

-- | Action to deactivate a static image overlay
--
-- /Note:/ Consider using 'staticImageDeactivateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStaticImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Types.StaticImageDeactivateScheduleActionSettings)
sasStaticImageDeactivateSettings = Lens.field @"staticImageDeactivateSettings"
{-# INLINEABLE sasStaticImageDeactivateSettings #-}
{-# DEPRECATED staticImageDeactivateSettings "Use generic-lens or generic-optics with 'staticImageDeactivateSettings' instead"  #-}

instance Core.FromJSON ScheduleActionSettings where
        toJSON ScheduleActionSettings{..}
          = Core.object
              (Core.catMaybes
                 [("hlsId3SegmentTaggingSettings" Core..=) Core.<$>
                    hlsId3SegmentTaggingSettings,
                  ("hlsTimedMetadataSettings" Core..=) Core.<$>
                    hlsTimedMetadataSettings,
                  ("inputPrepareSettings" Core..=) Core.<$> inputPrepareSettings,
                  ("inputSwitchSettings" Core..=) Core.<$> inputSwitchSettings,
                  ("pauseStateSettings" Core..=) Core.<$> pauseStateSettings,
                  ("scte35ReturnToNetworkSettings" Core..=) Core.<$>
                    scte35ReturnToNetworkSettings,
                  ("scte35SpliceInsertSettings" Core..=) Core.<$>
                    scte35SpliceInsertSettings,
                  ("scte35TimeSignalSettings" Core..=) Core.<$>
                    scte35TimeSignalSettings,
                  ("staticImageActivateSettings" Core..=) Core.<$>
                    staticImageActivateSettings,
                  ("staticImageDeactivateSettings" Core..=) Core.<$>
                    staticImageDeactivateSettings])

instance Core.FromJSON ScheduleActionSettings where
        parseJSON
          = Core.withObject "ScheduleActionSettings" Core.$
              \ x ->
                ScheduleActionSettings' Core.<$>
                  (x Core..:? "hlsId3SegmentTaggingSettings") Core.<*>
                    x Core..:? "hlsTimedMetadataSettings"
                    Core.<*> x Core..:? "inputPrepareSettings"
                    Core.<*> x Core..:? "inputSwitchSettings"
                    Core.<*> x Core..:? "pauseStateSettings"
                    Core.<*> x Core..:? "scte35ReturnToNetworkSettings"
                    Core.<*> x Core..:? "scte35SpliceInsertSettings"
                    Core.<*> x Core..:? "scte35TimeSignalSettings"
                    Core.<*> x Core..:? "staticImageActivateSettings"
                    Core.<*> x Core..:? "staticImageDeactivateSettings"
