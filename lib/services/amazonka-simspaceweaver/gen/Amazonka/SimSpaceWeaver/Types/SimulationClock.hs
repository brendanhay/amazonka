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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationClock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationClock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.ClockStatus
import Amazonka.SimSpaceWeaver.Types.ClockTargetStatus

-- | Status information about the simulation clock.
--
-- /See:/ 'newSimulationClock' smart constructor.
data SimulationClock = SimulationClock'
  { -- | The current status of the simulation clock.
    status :: Prelude.Maybe ClockStatus,
    -- | The desired status of the simulation clock.
    targetStatus :: Prelude.Maybe ClockTargetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationClock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'simulationClock_status' - The current status of the simulation clock.
--
-- 'targetStatus', 'simulationClock_targetStatus' - The desired status of the simulation clock.
newSimulationClock ::
  SimulationClock
newSimulationClock =
  SimulationClock'
    { status = Prelude.Nothing,
      targetStatus = Prelude.Nothing
    }

-- | The current status of the simulation clock.
simulationClock_status :: Lens.Lens' SimulationClock (Prelude.Maybe ClockStatus)
simulationClock_status = Lens.lens (\SimulationClock' {status} -> status) (\s@SimulationClock' {} a -> s {status = a} :: SimulationClock)

-- | The desired status of the simulation clock.
simulationClock_targetStatus :: Lens.Lens' SimulationClock (Prelude.Maybe ClockTargetStatus)
simulationClock_targetStatus = Lens.lens (\SimulationClock' {targetStatus} -> targetStatus) (\s@SimulationClock' {} a -> s {targetStatus = a} :: SimulationClock)

instance Data.FromJSON SimulationClock where
  parseJSON =
    Data.withObject
      "SimulationClock"
      ( \x ->
          SimulationClock'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TargetStatus")
      )

instance Prelude.Hashable SimulationClock where
  hashWithSalt _salt SimulationClock' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData SimulationClock where
  rnf SimulationClock' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf targetStatus
