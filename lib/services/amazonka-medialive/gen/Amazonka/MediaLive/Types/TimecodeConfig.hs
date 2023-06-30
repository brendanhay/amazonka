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
-- Module      : Amazonka.MediaLive.Types.TimecodeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TimecodeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.TimecodeConfigSource
import qualified Amazonka.Prelude as Prelude

-- | Timecode Config
--
-- /See:/ 'newTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { -- | Threshold in frames beyond which output timecode is resynchronized to
    -- the input timecode. Discrepancies below this threshold are permitted to
    -- avoid unnecessary discontinuities in the output timecode. No timecode
    -- sync when this is not specified.
    syncThreshold :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the source for the timecode that will be associated with the
    -- events outputs. -Embedded (embedded): Initialize the output timecode
    -- with timecode from the the source. If no embedded timecode is detected
    -- in the source, the system falls back to using \"Start at 0\"
    -- (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0
    -- (zerobased): The time of the first frame of the event will be
    -- 00:00:00:00.
    source :: TimecodeConfigSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { syncThreshold = Prelude.Nothing,
      source = pSource_
    }

-- | Threshold in frames beyond which output timecode is resynchronized to
-- the input timecode. Discrepancies below this threshold are permitted to
-- avoid unnecessary discontinuities in the output timecode. No timecode
-- sync when this is not specified.
timecodeConfig_syncThreshold :: Lens.Lens' TimecodeConfig (Prelude.Maybe Prelude.Natural)
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

instance Data.FromJSON TimecodeConfig where
  parseJSON =
    Data.withObject
      "TimecodeConfig"
      ( \x ->
          TimecodeConfig'
            Prelude.<$> (x Data..:? "syncThreshold")
            Prelude.<*> (x Data..: "source")
      )

instance Prelude.Hashable TimecodeConfig where
  hashWithSalt _salt TimecodeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` syncThreshold
      `Prelude.hashWithSalt` source

instance Prelude.NFData TimecodeConfig where
  rnf TimecodeConfig' {..} =
    Prelude.rnf syncThreshold
      `Prelude.seq` Prelude.rnf source

instance Data.ToJSON TimecodeConfig where
  toJSON TimecodeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("syncThreshold" Data..=) Prelude.<$> syncThreshold,
            Prelude.Just ("source" Data..= source)
          ]
      )
