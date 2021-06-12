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
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TimecodeConfigSource

-- | Timecode Config
--
-- /See:/ 'newTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { -- | Threshold in frames beyond which output timecode is resynchronized to
    -- the input timecode. Discrepancies below this threshold are permitted to
    -- avoid unnecessary discontinuities in the output timecode. No timecode
    -- sync when this is not specified.
    syncThreshold :: Core.Maybe Core.Natural,
    -- | Identifies the source for the timecode that will be associated with the
    -- events outputs. -Embedded (embedded): Initialize the output timecode
    -- with timecode from the the source. If no embedded timecode is detected
    -- in the source, the system falls back to using \"Start at 0\"
    -- (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0
    -- (zerobased): The time of the first frame of the event will be
    -- 00:00:00:00.
    source :: TimecodeConfigSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimecodeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncThreshold', 'timecodeConfig_syncThreshold' - Threshold in frames beyond which output timecode is resynchronized to
-- the input timecode. Discrepancies below this threshold are permitted to
-- avoid unnecessary discontinuities in the output timecode. No timecode
-- sync when this is not specified.
--
-- 'source', 'timecodeConfig_source' - Identifies the source for the timecode that will be associated with the
-- events outputs. -Embedded (embedded): Initialize the output timecode
-- with timecode from the the source. If no embedded timecode is detected
-- in the source, the system falls back to using \"Start at 0\"
-- (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0
-- (zerobased): The time of the first frame of the event will be
-- 00:00:00:00.
newTimecodeConfig ::
  -- | 'source'
  TimecodeConfigSource ->
  TimecodeConfig
newTimecodeConfig pSource_ =
  TimecodeConfig'
    { syncThreshold = Core.Nothing,
      source = pSource_
    }

-- | Threshold in frames beyond which output timecode is resynchronized to
-- the input timecode. Discrepancies below this threshold are permitted to
-- avoid unnecessary discontinuities in the output timecode. No timecode
-- sync when this is not specified.
timecodeConfig_syncThreshold :: Lens.Lens' TimecodeConfig (Core.Maybe Core.Natural)
timecodeConfig_syncThreshold = Lens.lens (\TimecodeConfig' {syncThreshold} -> syncThreshold) (\s@TimecodeConfig' {} a -> s {syncThreshold = a} :: TimecodeConfig)

-- | Identifies the source for the timecode that will be associated with the
-- events outputs. -Embedded (embedded): Initialize the output timecode
-- with timecode from the the source. If no embedded timecode is detected
-- in the source, the system falls back to using \"Start at 0\"
-- (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0
-- (zerobased): The time of the first frame of the event will be
-- 00:00:00:00.
timecodeConfig_source :: Lens.Lens' TimecodeConfig TimecodeConfigSource
timecodeConfig_source = Lens.lens (\TimecodeConfig' {source} -> source) (\s@TimecodeConfig' {} a -> s {source = a} :: TimecodeConfig)

instance Core.FromJSON TimecodeConfig where
  parseJSON =
    Core.withObject
      "TimecodeConfig"
      ( \x ->
          TimecodeConfig'
            Core.<$> (x Core..:? "syncThreshold")
            Core.<*> (x Core..: "source")
      )

instance Core.Hashable TimecodeConfig

instance Core.NFData TimecodeConfig

instance Core.ToJSON TimecodeConfig where
  toJSON TimecodeConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("syncThreshold" Core..=) Core.<$> syncThreshold,
            Core.Just ("source" Core..= source)
          ]
      )
