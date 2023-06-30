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
-- Module      : Amazonka.M2.Types.MaintenanceSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.MaintenanceSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information about the maintenance schedule.
--
-- /See:/ 'newMaintenanceSchedule' smart constructor.
data MaintenanceSchedule = MaintenanceSchedule'
  { -- | The time the scheduled maintenance is to end.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time the scheduled maintenance is to start.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'maintenanceSchedule_endTime' - The time the scheduled maintenance is to end.
--
-- 'startTime', 'maintenanceSchedule_startTime' - The time the scheduled maintenance is to start.
newMaintenanceSchedule ::
  MaintenanceSchedule
newMaintenanceSchedule =
  MaintenanceSchedule'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The time the scheduled maintenance is to end.
maintenanceSchedule_endTime :: Lens.Lens' MaintenanceSchedule (Prelude.Maybe Prelude.UTCTime)
maintenanceSchedule_endTime = Lens.lens (\MaintenanceSchedule' {endTime} -> endTime) (\s@MaintenanceSchedule' {} a -> s {endTime = a} :: MaintenanceSchedule) Prelude.. Lens.mapping Data._Time

-- | The time the scheduled maintenance is to start.
maintenanceSchedule_startTime :: Lens.Lens' MaintenanceSchedule (Prelude.Maybe Prelude.UTCTime)
maintenanceSchedule_startTime = Lens.lens (\MaintenanceSchedule' {startTime} -> startTime) (\s@MaintenanceSchedule' {} a -> s {startTime = a} :: MaintenanceSchedule) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MaintenanceSchedule where
  parseJSON =
    Data.withObject
      "MaintenanceSchedule"
      ( \x ->
          MaintenanceSchedule'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable MaintenanceSchedule where
  hashWithSalt _salt MaintenanceSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData MaintenanceSchedule where
  rnf MaintenanceSchedule' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
