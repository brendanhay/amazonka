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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.SimulationStatus
import Amazonka.SimSpaceWeaver.Types.SimulationTargetStatus

-- | A collection of data about the simulation.
--
-- /See:/ 'newSimulationMetadata' smart constructor.
data SimulationMetadata = SimulationMetadata'
  { -- | The Amazon Resource Name (ARN) of the simulation. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the simulation was created, expressed as the number of
    -- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
    -- 1, 1970).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the simulation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the simulation.
    status :: Prelude.Maybe SimulationStatus,
    -- | The desired status of the simulation.
    targetStatus :: Prelude.Maybe SimulationTargetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'simulationMetadata_arn' - The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationTime', 'simulationMetadata_creationTime' - The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
--
-- 'name', 'simulationMetadata_name' - The name of the simulation.
--
-- 'status', 'simulationMetadata_status' - The current status of the simulation.
--
-- 'targetStatus', 'simulationMetadata_targetStatus' - The desired status of the simulation.
newSimulationMetadata ::
  SimulationMetadata
newSimulationMetadata =
  SimulationMetadata'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      targetStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
simulationMetadata_arn :: Lens.Lens' SimulationMetadata (Prelude.Maybe Prelude.Text)
simulationMetadata_arn = Lens.lens (\SimulationMetadata' {arn} -> arn) (\s@SimulationMetadata' {} a -> s {arn = a} :: SimulationMetadata)

-- | The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
simulationMetadata_creationTime :: Lens.Lens' SimulationMetadata (Prelude.Maybe Prelude.UTCTime)
simulationMetadata_creationTime = Lens.lens (\SimulationMetadata' {creationTime} -> creationTime) (\s@SimulationMetadata' {} a -> s {creationTime = a} :: SimulationMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the simulation.
simulationMetadata_name :: Lens.Lens' SimulationMetadata (Prelude.Maybe Prelude.Text)
simulationMetadata_name = Lens.lens (\SimulationMetadata' {name} -> name) (\s@SimulationMetadata' {} a -> s {name = a} :: SimulationMetadata)

-- | The current status of the simulation.
simulationMetadata_status :: Lens.Lens' SimulationMetadata (Prelude.Maybe SimulationStatus)
simulationMetadata_status = Lens.lens (\SimulationMetadata' {status} -> status) (\s@SimulationMetadata' {} a -> s {status = a} :: SimulationMetadata)

-- | The desired status of the simulation.
simulationMetadata_targetStatus :: Lens.Lens' SimulationMetadata (Prelude.Maybe SimulationTargetStatus)
simulationMetadata_targetStatus = Lens.lens (\SimulationMetadata' {targetStatus} -> targetStatus) (\s@SimulationMetadata' {} a -> s {targetStatus = a} :: SimulationMetadata)

instance Data.FromJSON SimulationMetadata where
  parseJSON =
    Data.withObject
      "SimulationMetadata"
      ( \x ->
          SimulationMetadata'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TargetStatus")
      )

instance Prelude.Hashable SimulationMetadata where
  hashWithSalt _salt SimulationMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData SimulationMetadata where
  rnf SimulationMetadata' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf targetStatus
