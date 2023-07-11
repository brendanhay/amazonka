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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationAppMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationAppMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.SimulationAppStatus
import Amazonka.SimSpaceWeaver.Types.SimulationAppTargetStatus

-- | A collection of metadata about an app.
--
-- /See:/ 'newSimulationAppMetadata' smart constructor.
data SimulationAppMetadata = SimulationAppMetadata'
  { -- | The domain of the app. For more information about domains, see
    -- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
    -- in the /Amazon Web Services SimSpace Weaver User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the app.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation of the app.
    simulation :: Prelude.Maybe Prelude.Text,
    -- | The current status of the app.
    status :: Prelude.Maybe SimulationAppStatus,
    -- | The desired status of the app.
    targetStatus :: Prelude.Maybe SimulationAppTargetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationAppMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'simulationAppMetadata_domain' - The domain of the app. For more information about domains, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
-- in the /Amazon Web Services SimSpace Weaver User Guide/.
--
-- 'name', 'simulationAppMetadata_name' - The name of the app.
--
-- 'simulation', 'simulationAppMetadata_simulation' - The name of the simulation of the app.
--
-- 'status', 'simulationAppMetadata_status' - The current status of the app.
--
-- 'targetStatus', 'simulationAppMetadata_targetStatus' - The desired status of the app.
newSimulationAppMetadata ::
  SimulationAppMetadata
newSimulationAppMetadata =
  SimulationAppMetadata'
    { domain = Prelude.Nothing,
      name = Prelude.Nothing,
      simulation = Prelude.Nothing,
      status = Prelude.Nothing,
      targetStatus = Prelude.Nothing
    }

-- | The domain of the app. For more information about domains, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
-- in the /Amazon Web Services SimSpace Weaver User Guide/.
simulationAppMetadata_domain :: Lens.Lens' SimulationAppMetadata (Prelude.Maybe Prelude.Text)
simulationAppMetadata_domain = Lens.lens (\SimulationAppMetadata' {domain} -> domain) (\s@SimulationAppMetadata' {} a -> s {domain = a} :: SimulationAppMetadata)

-- | The name of the app.
simulationAppMetadata_name :: Lens.Lens' SimulationAppMetadata (Prelude.Maybe Prelude.Text)
simulationAppMetadata_name = Lens.lens (\SimulationAppMetadata' {name} -> name) (\s@SimulationAppMetadata' {} a -> s {name = a} :: SimulationAppMetadata)

-- | The name of the simulation of the app.
simulationAppMetadata_simulation :: Lens.Lens' SimulationAppMetadata (Prelude.Maybe Prelude.Text)
simulationAppMetadata_simulation = Lens.lens (\SimulationAppMetadata' {simulation} -> simulation) (\s@SimulationAppMetadata' {} a -> s {simulation = a} :: SimulationAppMetadata)

-- | The current status of the app.
simulationAppMetadata_status :: Lens.Lens' SimulationAppMetadata (Prelude.Maybe SimulationAppStatus)
simulationAppMetadata_status = Lens.lens (\SimulationAppMetadata' {status} -> status) (\s@SimulationAppMetadata' {} a -> s {status = a} :: SimulationAppMetadata)

-- | The desired status of the app.
simulationAppMetadata_targetStatus :: Lens.Lens' SimulationAppMetadata (Prelude.Maybe SimulationAppTargetStatus)
simulationAppMetadata_targetStatus = Lens.lens (\SimulationAppMetadata' {targetStatus} -> targetStatus) (\s@SimulationAppMetadata' {} a -> s {targetStatus = a} :: SimulationAppMetadata)

instance Data.FromJSON SimulationAppMetadata where
  parseJSON =
    Data.withObject
      "SimulationAppMetadata"
      ( \x ->
          SimulationAppMetadata'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Simulation")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TargetStatus")
      )

instance Prelude.Hashable SimulationAppMetadata where
  hashWithSalt _salt SimulationAppMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` simulation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData SimulationAppMetadata where
  rnf SimulationAppMetadata' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf simulation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetStatus
