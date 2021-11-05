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
-- Module      : Network.AWS.RobOMaker.Types.Compute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.Compute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.ComputeType

-- | Compute information for the simulation job.
--
-- /See:/ 'newCompute' smart constructor.
data Compute = Compute'
  { -- | The simulation unit limit. Your simulation is allocated CPU and memory
    -- proportional to the supplied simulation unit limit. A simulation unit is
    -- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
    -- consume up to the maximum value provided. The default is 15.
    simulationUnitLimit :: Prelude.Maybe Prelude.Natural,
    -- | Compute GPU unit limit for the simulation job. It is the same as the
    -- number of GPUs allocated to the SimulationJob.
    gpuUnitLimit :: Prelude.Maybe Prelude.Natural,
    -- | Compute type information for the simulation job.
    computeType :: Prelude.Maybe ComputeType
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
-- 'simulationUnitLimit', 'compute_simulationUnitLimit' - The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
--
-- 'gpuUnitLimit', 'compute_gpuUnitLimit' - Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
--
-- 'computeType', 'compute_computeType' - Compute type information for the simulation job.
newCompute ::
  Compute
newCompute =
  Compute'
    { simulationUnitLimit = Prelude.Nothing,
      gpuUnitLimit = Prelude.Nothing,
      computeType = Prelude.Nothing
    }

-- | The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
compute_simulationUnitLimit :: Lens.Lens' Compute (Prelude.Maybe Prelude.Natural)
compute_simulationUnitLimit = Lens.lens (\Compute' {simulationUnitLimit} -> simulationUnitLimit) (\s@Compute' {} a -> s {simulationUnitLimit = a} :: Compute)

-- | Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
compute_gpuUnitLimit :: Lens.Lens' Compute (Prelude.Maybe Prelude.Natural)
compute_gpuUnitLimit = Lens.lens (\Compute' {gpuUnitLimit} -> gpuUnitLimit) (\s@Compute' {} a -> s {gpuUnitLimit = a} :: Compute)

-- | Compute type information for the simulation job.
compute_computeType :: Lens.Lens' Compute (Prelude.Maybe ComputeType)
compute_computeType = Lens.lens (\Compute' {computeType} -> computeType) (\s@Compute' {} a -> s {computeType = a} :: Compute)

instance Core.FromJSON Compute where
  parseJSON =
    Core.withObject
      "Compute"
      ( \x ->
          Compute'
            Prelude.<$> (x Core..:? "simulationUnitLimit")
            Prelude.<*> (x Core..:? "gpuUnitLimit")
            Prelude.<*> (x Core..:? "computeType")
      )

instance Prelude.Hashable Compute

instance Prelude.NFData Compute

instance Core.ToJSON Compute where
  toJSON Compute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("simulationUnitLimit" Core..=)
              Prelude.<$> simulationUnitLimit,
            ("gpuUnitLimit" Core..=) Prelude.<$> gpuUnitLimit,
            ("computeType" Core..=) Prelude.<$> computeType
          ]
      )
