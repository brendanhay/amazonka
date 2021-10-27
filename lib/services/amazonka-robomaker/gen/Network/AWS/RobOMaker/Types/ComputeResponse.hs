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
-- Module      : Network.AWS.RobOMaker.Types.ComputeResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.ComputeResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.ComputeType

-- | Compute information for the simulation job
--
-- /See:/ 'newComputeResponse' smart constructor.
data ComputeResponse = ComputeResponse'
  { -- | The simulation unit limit. Your simulation is allocated CPU and memory
    -- proportional to the supplied simulation unit limit. A simulation unit is
    -- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
    -- consume up to the maximum value provided. The default is 15.
    simulationUnitLimit :: Prelude.Maybe Prelude.Natural,
    -- | Compute GPU unit limit for the simulation job. It is the same as the
    -- number of GPUs allocated to the SimulationJob.
    gpuUnitLimit :: Prelude.Maybe Prelude.Natural,
    -- | Compute type response information for the simulation job.
    computeType :: Prelude.Maybe ComputeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulationUnitLimit', 'computeResponse_simulationUnitLimit' - The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
--
-- 'gpuUnitLimit', 'computeResponse_gpuUnitLimit' - Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
--
-- 'computeType', 'computeResponse_computeType' - Compute type response information for the simulation job.
newComputeResponse ::
  ComputeResponse
newComputeResponse =
  ComputeResponse'
    { simulationUnitLimit =
        Prelude.Nothing,
      gpuUnitLimit = Prelude.Nothing,
      computeType = Prelude.Nothing
    }

-- | The simulation unit limit. Your simulation is allocated CPU and memory
-- proportional to the supplied simulation unit limit. A simulation unit is
-- 1 vcpu and 2GB of memory. You are only billed for the SU utilization you
-- consume up to the maximum value provided. The default is 15.
computeResponse_simulationUnitLimit :: Lens.Lens' ComputeResponse (Prelude.Maybe Prelude.Natural)
computeResponse_simulationUnitLimit = Lens.lens (\ComputeResponse' {simulationUnitLimit} -> simulationUnitLimit) (\s@ComputeResponse' {} a -> s {simulationUnitLimit = a} :: ComputeResponse)

-- | Compute GPU unit limit for the simulation job. It is the same as the
-- number of GPUs allocated to the SimulationJob.
computeResponse_gpuUnitLimit :: Lens.Lens' ComputeResponse (Prelude.Maybe Prelude.Natural)
computeResponse_gpuUnitLimit = Lens.lens (\ComputeResponse' {gpuUnitLimit} -> gpuUnitLimit) (\s@ComputeResponse' {} a -> s {gpuUnitLimit = a} :: ComputeResponse)

-- | Compute type response information for the simulation job.
computeResponse_computeType :: Lens.Lens' ComputeResponse (Prelude.Maybe ComputeType)
computeResponse_computeType = Lens.lens (\ComputeResponse' {computeType} -> computeType) (\s@ComputeResponse' {} a -> s {computeType = a} :: ComputeResponse)

instance Core.FromJSON ComputeResponse where
  parseJSON =
    Core.withObject
      "ComputeResponse"
      ( \x ->
          ComputeResponse'
            Prelude.<$> (x Core..:? "simulationUnitLimit")
            Prelude.<*> (x Core..:? "gpuUnitLimit")
            Prelude.<*> (x Core..:? "computeType")
      )

instance Prelude.Hashable ComputeResponse

instance Prelude.NFData ComputeResponse
