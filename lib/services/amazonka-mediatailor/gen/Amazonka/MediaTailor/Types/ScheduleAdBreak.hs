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
-- Module      : Amazonka.MediaTailor.Types.ScheduleAdBreak
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ScheduleAdBreak where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The schedule\'s ad break properties.
--
-- /See:/ 'newScheduleAdBreak' smart constructor.
data ScheduleAdBreak = ScheduleAdBreak'
  { -- | The approximate duration of the ad break, in seconds.
    approximateDurationSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The approximate time that the ad will start playing.
    approximateStartTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the source location containing the VOD source used for the
    -- ad break.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The name of the VOD source used for the ad break.
    vodSourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleAdBreak' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateDurationSeconds', 'scheduleAdBreak_approximateDurationSeconds' - The approximate duration of the ad break, in seconds.
--
-- 'approximateStartTime', 'scheduleAdBreak_approximateStartTime' - The approximate time that the ad will start playing.
--
-- 'sourceLocationName', 'scheduleAdBreak_sourceLocationName' - The name of the source location containing the VOD source used for the
-- ad break.
--
-- 'vodSourceName', 'scheduleAdBreak_vodSourceName' - The name of the VOD source used for the ad break.
newScheduleAdBreak ::
  ScheduleAdBreak
newScheduleAdBreak =
  ScheduleAdBreak'
    { approximateDurationSeconds =
        Prelude.Nothing,
      approximateStartTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      vodSourceName = Prelude.Nothing
    }

-- | The approximate duration of the ad break, in seconds.
scheduleAdBreak_approximateDurationSeconds :: Lens.Lens' ScheduleAdBreak (Prelude.Maybe Prelude.Integer)
scheduleAdBreak_approximateDurationSeconds = Lens.lens (\ScheduleAdBreak' {approximateDurationSeconds} -> approximateDurationSeconds) (\s@ScheduleAdBreak' {} a -> s {approximateDurationSeconds = a} :: ScheduleAdBreak)

-- | The approximate time that the ad will start playing.
scheduleAdBreak_approximateStartTime :: Lens.Lens' ScheduleAdBreak (Prelude.Maybe Prelude.UTCTime)
scheduleAdBreak_approximateStartTime = Lens.lens (\ScheduleAdBreak' {approximateStartTime} -> approximateStartTime) (\s@ScheduleAdBreak' {} a -> s {approximateStartTime = a} :: ScheduleAdBreak) Prelude.. Lens.mapping Data._Time

-- | The name of the source location containing the VOD source used for the
-- ad break.
scheduleAdBreak_sourceLocationName :: Lens.Lens' ScheduleAdBreak (Prelude.Maybe Prelude.Text)
scheduleAdBreak_sourceLocationName = Lens.lens (\ScheduleAdBreak' {sourceLocationName} -> sourceLocationName) (\s@ScheduleAdBreak' {} a -> s {sourceLocationName = a} :: ScheduleAdBreak)

-- | The name of the VOD source used for the ad break.
scheduleAdBreak_vodSourceName :: Lens.Lens' ScheduleAdBreak (Prelude.Maybe Prelude.Text)
scheduleAdBreak_vodSourceName = Lens.lens (\ScheduleAdBreak' {vodSourceName} -> vodSourceName) (\s@ScheduleAdBreak' {} a -> s {vodSourceName = a} :: ScheduleAdBreak)

instance Data.FromJSON ScheduleAdBreak where
  parseJSON =
    Data.withObject
      "ScheduleAdBreak"
      ( \x ->
          ScheduleAdBreak'
            Prelude.<$> (x Data..:? "ApproximateDurationSeconds")
            Prelude.<*> (x Data..:? "ApproximateStartTime")
            Prelude.<*> (x Data..:? "SourceLocationName")
            Prelude.<*> (x Data..:? "VodSourceName")
      )

instance Prelude.Hashable ScheduleAdBreak where
  hashWithSalt _salt ScheduleAdBreak' {..} =
    _salt
      `Prelude.hashWithSalt` approximateDurationSeconds
      `Prelude.hashWithSalt` approximateStartTime
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` vodSourceName

instance Prelude.NFData ScheduleAdBreak where
  rnf ScheduleAdBreak' {..} =
    Prelude.rnf approximateDurationSeconds
      `Prelude.seq` Prelude.rnf approximateStartTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName
