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
-- Module      : Amazonka.RobOMaker.Types.Compute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.Compute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.ComputeType

-- | Compute information for the simulation job.
--
-- /See:/ 'newCompute' smart constructor.
data Compute = Compute'
  { -- | Compute type information for the simulation job.
    computeType :: Prelude.Maybe ComputeType,
    -- | Compute GPU unit limit for the simulation job. It is the same as the
    -- number of GPUs allocated to the SimulationJob.
    gpuUnitLimit :: Prelude.Maybe Prelude.Natural,
    -- | The simulation unit limit. Your simulation is allocated CPU and memory
    -- proportional to the supplied simulation unit limit. A simulation unit is
    -- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
    -- consume up to the maximum value provided. The default is 15.
    simulationUnitLimit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Compute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeType', 'compute_computeType' - Compute type information for the simulation job.
--
-- 'gpuUnitLimit', 'compute_gpuUnitLimit' - Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
--
-- 'simulationUnitLimit', 'compute_simulationUnitLimit' - The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
newCompute ::
  Compute
newCompute =
  Compute'
    { computeType = Prelude.Nothing,
      gpuUnitLimit = Prelude.Nothing,
      simulationUnitLimit = Prelude.Nothing
    }

-- | Compute type information for the simulation job.
compute_computeType :: Lens.Lens' Compute (Prelude.Maybe ComputeType)
compute_computeType = Lens.lens (\Compute' {computeType} -> computeType) (\s@Compute' {} a -> s {computeType = a} :: Compute)

-- | Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
compute_gpuUnitLimit :: Lens.Lens' Compute (Prelude.Maybe Prelude.Natural)
compute_gpuUnitLimit = Lens.lens (\Compute' {gpuUnitLimit} -> gpuUnitLimit) (\s@Compute' {} a -> s {gpuUnitLimit = a} :: Compute)

-- | The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
compute_simulationUnitLimit :: Lens.Lens' Compute (Prelude.Maybe Prelude.Natural)
compute_simulationUnitLimit = Lens.lens (\Compute' {simulationUnitLimit} -> simulationUnitLimit) (\s@Compute' {} a -> s {simulationUnitLimit = a} :: Compute)

instance Data.FromJSON Compute where
  parseJSON =
    Data.withObject
      "Compute"
      ( \x ->
          Compute'
            Prelude.<$> (x Data..:? "computeType")
            Prelude.<*> (x Data..:? "gpuUnitLimit")
            Prelude.<*> (x Data..:? "simulationUnitLimit")
      )

instance Prelude.Hashable Compute where
  hashWithSalt _salt Compute' {..} =
    _salt `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` gpuUnitLimit
      `Prelude.hashWithSalt` simulationUnitLimit

instance Prelude.NFData Compute where
  rnf Compute' {..} =
    Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf gpuUnitLimit
      `Prelude.seq` Prelude.rnf simulationUnitLimit

instance Data.ToJSON Compute where
  toJSON Compute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computeType" Data..=) Prelude.<$> computeType,
            ("gpuUnitLimit" Data..=) Prelude.<$> gpuUnitLimit,
            ("simulationUnitLimit" Data..=)
              Prelude.<$> simulationUnitLimit
          ]
      )
