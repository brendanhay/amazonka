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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PendingMaintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.MaintenanceSchedule
import qualified Amazonka.Prelude as Prelude

-- | The scheduled maintenance for a runtime engine.
--
-- /See:/ 'newPendingMaintenance' smart constructor.
data PendingMaintenance = PendingMaintenance'
  { -- | The maintenance schedule for the engine version.
    schedule :: Prelude.Maybe MaintenanceSchedule,
    -- | The specific runtime engine that the maintenance schedule applies to.
    engineVersion :: Prelude.Maybe Prelude.Text
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
-- 'schedule', 'pendingMaintenance_schedule' - The maintenance schedule for the engine version.
--
-- 'engineVersion', 'pendingMaintenance_engineVersion' - The specific runtime engine that the maintenance schedule applies to.
newPendingMaintenance ::
  PendingMaintenance
newPendingMaintenance =
  PendingMaintenance'
    { schedule = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The maintenance schedule for the engine version.
pendingMaintenance_schedule :: Lens.Lens' PendingMaintenance (Prelude.Maybe MaintenanceSchedule)
pendingMaintenance_schedule = Lens.lens (\PendingMaintenance' {schedule} -> schedule) (\s@PendingMaintenance' {} a -> s {schedule = a} :: PendingMaintenance)

-- | The specific runtime engine that the maintenance schedule applies to.
pendingMaintenance_engineVersion :: Lens.Lens' PendingMaintenance (Prelude.Maybe Prelude.Text)
pendingMaintenance_engineVersion = Lens.lens (\PendingMaintenance' {engineVersion} -> engineVersion) (\s@PendingMaintenance' {} a -> s {engineVersion = a} :: PendingMaintenance)

instance Core.FromJSON PendingMaintenance where
  parseJSON =
    Core.withObject
      "PendingMaintenance"
      ( \x ->
          PendingMaintenance'
            Prelude.<$> (x Core..:? "schedule")
            Prelude.<*> (x Core..:? "engineVersion")
      )

instance Prelude.Hashable PendingMaintenance where
  hashWithSalt _salt PendingMaintenance' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData PendingMaintenance where
  rnf PendingMaintenance' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf engineVersion
