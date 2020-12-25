{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfig
  ( TimecodeConfig (..),

    -- * Smart constructor
    mkTimecodeConfig,

    -- * Lenses
    tcSource,
    tcSyncThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TimecodeConfigSource as Types
import qualified Network.AWS.Prelude as Core

-- | Timecode Config
--
-- /See:/ 'mkTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { -- | Identifies the source for the timecode that will be associated with the events outputs.
    --
    -- -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased).
    -- -System Clock (systemclock): Use the UTC time.
    -- -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
    source :: Types.TimecodeConfigSource,
    -- | Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
    syncThreshold :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimecodeConfig' value with any optional fields omitted.
mkTimecodeConfig ::
  -- | 'source'
  Types.TimecodeConfigSource ->
  TimecodeConfig
mkTimecodeConfig source =
  TimecodeConfig' {source, syncThreshold = Core.Nothing}

-- | Identifies the source for the timecode that will be associated with the events outputs.
--
-- -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased).
-- -System Clock (systemclock): Use the UTC time.
-- -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TimecodeConfig Types.TimecodeConfigSource
tcSource = Lens.field @"source"
{-# DEPRECATED tcSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
--
-- /Note:/ Consider using 'syncThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSyncThreshold :: Lens.Lens' TimecodeConfig (Core.Maybe Core.Natural)
tcSyncThreshold = Lens.field @"syncThreshold"
{-# DEPRECATED tcSyncThreshold "Use generic-lens or generic-optics with 'syncThreshold' instead." #-}

instance Core.FromJSON TimecodeConfig where
  toJSON TimecodeConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("source" Core..= source),
            ("syncThreshold" Core..=) Core.<$> syncThreshold
          ]
      )

instance Core.FromJSON TimecodeConfig where
  parseJSON =
    Core.withObject "TimecodeConfig" Core.$
      \x ->
        TimecodeConfig'
          Core.<$> (x Core..: "source") Core.<*> (x Core..:? "syncThreshold")
