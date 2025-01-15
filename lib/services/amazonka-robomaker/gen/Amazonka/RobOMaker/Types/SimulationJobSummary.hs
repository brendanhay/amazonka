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
-- Module      : Amazonka.RobOMaker.Types.SimulationJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.ComputeType
import Amazonka.RobOMaker.Types.SimulationJobStatus

-- | Summary information for a simulation job.
--
-- /See:/ 'newSimulationJobSummary' smart constructor.
data SimulationJobSummary = SimulationJobSummary'
  { -- | The Amazon Resource Name (ARN) of the simulation job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The compute type for the simulation job summary.
    computeType :: Prelude.Maybe ComputeType,
    -- | The names of the data sources.
    dataSourceNames :: Prelude.Maybe [Prelude.Text],
    -- | The time, in milliseconds since the epoch, when the simulation job was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the simulation job.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of simulation job robot application names.
    robotApplicationNames :: Prelude.Maybe [Prelude.Text],
    -- | A list of simulation job simulation application names.
    simulationApplicationNames :: Prelude.Maybe [Prelude.Text],
    -- | The status of the simulation job.
    status :: Prelude.Maybe SimulationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'simulationJobSummary_arn' - The Amazon Resource Name (ARN) of the simulation job.
--
-- 'computeType', 'simulationJobSummary_computeType' - The compute type for the simulation job summary.
--
-- 'dataSourceNames', 'simulationJobSummary_dataSourceNames' - The names of the data sources.
--
-- 'lastUpdatedAt', 'simulationJobSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
--
-- 'name', 'simulationJobSummary_name' - The name of the simulation job.
--
-- 'robotApplicationNames', 'simulationJobSummary_robotApplicationNames' - A list of simulation job robot application names.
--
-- 'simulationApplicationNames', 'simulationJobSummary_simulationApplicationNames' - A list of simulation job simulation application names.
--
-- 'status', 'simulationJobSummary_status' - The status of the simulation job.
newSimulationJobSummary ::
  SimulationJobSummary
newSimulationJobSummary =
  SimulationJobSummary'
    { arn = Prelude.Nothing,
      computeType = Prelude.Nothing,
      dataSourceNames = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      robotApplicationNames = Prelude.Nothing,
      simulationApplicationNames = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the simulation job.
simulationJobSummary_arn :: Lens.Lens' SimulationJobSummary (Prelude.Maybe Prelude.Text)
simulationJobSummary_arn = Lens.lens (\SimulationJobSummary' {arn} -> arn) (\s@SimulationJobSummary' {} a -> s {arn = a} :: SimulationJobSummary)

-- | The compute type for the simulation job summary.
simulationJobSummary_computeType :: Lens.Lens' SimulationJobSummary (Prelude.Maybe ComputeType)
simulationJobSummary_computeType = Lens.lens (\SimulationJobSummary' {computeType} -> computeType) (\s@SimulationJobSummary' {} a -> s {computeType = a} :: SimulationJobSummary)

-- | The names of the data sources.
simulationJobSummary_dataSourceNames :: Lens.Lens' SimulationJobSummary (Prelude.Maybe [Prelude.Text])
simulationJobSummary_dataSourceNames = Lens.lens (\SimulationJobSummary' {dataSourceNames} -> dataSourceNames) (\s@SimulationJobSummary' {} a -> s {dataSourceNames = a} :: SimulationJobSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
simulationJobSummary_lastUpdatedAt :: Lens.Lens' SimulationJobSummary (Prelude.Maybe Prelude.UTCTime)
simulationJobSummary_lastUpdatedAt = Lens.lens (\SimulationJobSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@SimulationJobSummary' {} a -> s {lastUpdatedAt = a} :: SimulationJobSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the simulation job.
simulationJobSummary_name :: Lens.Lens' SimulationJobSummary (Prelude.Maybe Prelude.Text)
simulationJobSummary_name = Lens.lens (\SimulationJobSummary' {name} -> name) (\s@SimulationJobSummary' {} a -> s {name = a} :: SimulationJobSummary)

-- | A list of simulation job robot application names.
simulationJobSummary_robotApplicationNames :: Lens.Lens' SimulationJobSummary (Prelude.Maybe [Prelude.Text])
simulationJobSummary_robotApplicationNames = Lens.lens (\SimulationJobSummary' {robotApplicationNames} -> robotApplicationNames) (\s@SimulationJobSummary' {} a -> s {robotApplicationNames = a} :: SimulationJobSummary) Prelude.. Lens.mapping Lens.coerced

-- | A list of simulation job simulation application names.
simulationJobSummary_simulationApplicationNames :: Lens.Lens' SimulationJobSummary (Prelude.Maybe [Prelude.Text])
simulationJobSummary_simulationApplicationNames = Lens.lens (\SimulationJobSummary' {simulationApplicationNames} -> simulationApplicationNames) (\s@SimulationJobSummary' {} a -> s {simulationApplicationNames = a} :: SimulationJobSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status of the simulation job.
simulationJobSummary_status :: Lens.Lens' SimulationJobSummary (Prelude.Maybe SimulationJobStatus)
simulationJobSummary_status = Lens.lens (\SimulationJobSummary' {status} -> status) (\s@SimulationJobSummary' {} a -> s {status = a} :: SimulationJobSummary)

instance Data.FromJSON SimulationJobSummary where
  parseJSON =
    Data.withObject
      "SimulationJobSummary"
      ( \x ->
          SimulationJobSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "computeType")
            Prelude.<*> ( x
                            Data..:? "dataSourceNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x
                            Data..:? "robotApplicationNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "simulationApplicationNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable SimulationJobSummary where
  hashWithSalt _salt SimulationJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` dataSourceNames
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` robotApplicationNames
      `Prelude.hashWithSalt` simulationApplicationNames
      `Prelude.hashWithSalt` status

instance Prelude.NFData SimulationJobSummary where
  rnf SimulationJobSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf computeType `Prelude.seq`
        Prelude.rnf dataSourceNames `Prelude.seq`
          Prelude.rnf lastUpdatedAt `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf robotApplicationNames `Prelude.seq`
                Prelude.rnf simulationApplicationNames `Prelude.seq`
                  Prelude.rnf status
