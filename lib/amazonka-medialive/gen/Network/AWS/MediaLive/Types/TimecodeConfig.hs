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
    tcSyncThreshold,
    tcSource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TimecodeConfigSource
import qualified Network.AWS.Prelude as Lude

-- | Timecode Config
--
-- /See:/ 'mkTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { syncThreshold ::
      Lude.Maybe Lude.Natural,
    source :: TimecodeConfigSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimecodeConfig' with the minimum fields required to make a request.
--
-- * 'source' - Identifies the source for the timecode that will be associated with the events outputs.
--
-- -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased).
-- -System Clock (systemclock): Use the UTC time.
-- -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
-- * 'syncThreshold' - Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
mkTimecodeConfig ::
  -- | 'source'
  TimecodeConfigSource ->
  TimecodeConfig
mkTimecodeConfig pSource_ =
  TimecodeConfig' {syncThreshold = Lude.Nothing, source = pSource_}

-- | Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
--
-- /Note:/ Consider using 'syncThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSyncThreshold :: Lens.Lens' TimecodeConfig (Lude.Maybe Lude.Natural)
tcSyncThreshold = Lens.lens (syncThreshold :: TimecodeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {syncThreshold = a} :: TimecodeConfig)
{-# DEPRECATED tcSyncThreshold "Use generic-lens or generic-optics with 'syncThreshold' instead." #-}

-- | Identifies the source for the timecode that will be associated with the events outputs.
--
-- -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased).
-- -System Clock (systemclock): Use the UTC time.
-- -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TimecodeConfig TimecodeConfigSource
tcSource = Lens.lens (source :: TimecodeConfig -> TimecodeConfigSource) (\s a -> s {source = a} :: TimecodeConfig)
{-# DEPRECATED tcSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON TimecodeConfig where
  parseJSON =
    Lude.withObject
      "TimecodeConfig"
      ( \x ->
          TimecodeConfig'
            Lude.<$> (x Lude..:? "syncThreshold") Lude.<*> (x Lude..: "source")
      )

instance Lude.ToJSON TimecodeConfig where
  toJSON TimecodeConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("syncThreshold" Lude..=) Lude.<$> syncThreshold,
            Lude.Just ("source" Lude..= source)
          ]
      )
