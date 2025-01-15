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
-- Module      : Amazonka.M2.Types.PendingMaintenance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PendingMaintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.MaintenanceSchedule
import qualified Amazonka.Prelude as Prelude

-- | The scheduled maintenance for a runtime engine.
--
-- /See:/ 'newPendingMaintenance' smart constructor.
data PendingMaintenance = PendingMaintenance'
  { -- | The specific runtime engine that the maintenance schedule applies to.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The maintenance schedule for the runtime engine version.
    schedule :: Prelude.Maybe MaintenanceSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'pendingMaintenance_engineVersion' - The specific runtime engine that the maintenance schedule applies to.
--
-- 'schedule', 'pendingMaintenance_schedule' - The maintenance schedule for the runtime engine version.
newPendingMaintenance ::
  PendingMaintenance
newPendingMaintenance =
  PendingMaintenance'
    { engineVersion =
        Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The specific runtime engine that the maintenance schedule applies to.
pendingMaintenance_engineVersion :: Lens.Lens' PendingMaintenance (Prelude.Maybe Prelude.Text)
pendingMaintenance_engineVersion = Lens.lens (\PendingMaintenance' {engineVersion} -> engineVersion) (\s@PendingMaintenance' {} a -> s {engineVersion = a} :: PendingMaintenance)

-- | The maintenance schedule for the runtime engine version.
pendingMaintenance_schedule :: Lens.Lens' PendingMaintenance (Prelude.Maybe MaintenanceSchedule)
pendingMaintenance_schedule = Lens.lens (\PendingMaintenance' {schedule} -> schedule) (\s@PendingMaintenance' {} a -> s {schedule = a} :: PendingMaintenance)

instance Data.FromJSON PendingMaintenance where
  parseJSON =
    Data.withObject
      "PendingMaintenance"
      ( \x ->
          PendingMaintenance'
            Prelude.<$> (x Data..:? "engineVersion")
            Prelude.<*> (x Data..:? "schedule")
      )

instance Prelude.Hashable PendingMaintenance where
  hashWithSalt _salt PendingMaintenance' {..} =
    _salt
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` schedule

instance Prelude.NFData PendingMaintenance where
  rnf PendingMaintenance' {..} =
    Prelude.rnf engineVersion `Prelude.seq`
      Prelude.rnf schedule
