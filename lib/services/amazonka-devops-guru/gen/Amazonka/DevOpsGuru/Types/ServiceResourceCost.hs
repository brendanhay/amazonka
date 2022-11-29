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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceResourceCost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceResourceCost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.CostEstimationServiceResourceState
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the estimated monthly cost to
-- analyze an Amazon Web Services resource. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
--
-- /See:/ 'newServiceResourceCost' smart constructor.
data ServiceResourceCost = ServiceResourceCost'
  { -- | The type of the Amazon Web Services resource.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The state of the resource. The resource is @ACTIVE@ if it produces
    -- metrics, events, or logs within an hour, otherwise it is @INACTIVE@. You
    -- pay for the number of active Amazon Web Services resource hours analyzed
    -- for each resource. Inactive resources are not charged.
    state :: Prelude.Maybe CostEstimationServiceResourceState,
    -- | The number of active resources analyzed for this service to create a
    -- monthly cost estimate.
    count :: Prelude.Maybe Prelude.Int,
    -- | The price per hour to analyze the resources in the service. For more
    -- information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
    -- and
    -- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
    unitCost :: Prelude.Maybe Prelude.Double,
    -- | The total estimated monthly cost to analyze the active resources for
    -- this resource.
    cost :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceResourceCost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'serviceResourceCost_type' - The type of the Amazon Web Services resource.
--
-- 'state', 'serviceResourceCost_state' - The state of the resource. The resource is @ACTIVE@ if it produces
-- metrics, events, or logs within an hour, otherwise it is @INACTIVE@. You
-- pay for the number of active Amazon Web Services resource hours analyzed
-- for each resource. Inactive resources are not charged.
--
-- 'count', 'serviceResourceCost_count' - The number of active resources analyzed for this service to create a
-- monthly cost estimate.
--
-- 'unitCost', 'serviceResourceCost_unitCost' - The price per hour to analyze the resources in the service. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
--
-- 'cost', 'serviceResourceCost_cost' - The total estimated monthly cost to analyze the active resources for
-- this resource.
newServiceResourceCost ::
  ServiceResourceCost
newServiceResourceCost =
  ServiceResourceCost'
    { type' = Prelude.Nothing,
      state = Prelude.Nothing,
      count = Prelude.Nothing,
      unitCost = Prelude.Nothing,
      cost = Prelude.Nothing
    }

-- | The type of the Amazon Web Services resource.
serviceResourceCost_type :: Lens.Lens' ServiceResourceCost (Prelude.Maybe Prelude.Text)
serviceResourceCost_type = Lens.lens (\ServiceResourceCost' {type'} -> type') (\s@ServiceResourceCost' {} a -> s {type' = a} :: ServiceResourceCost)

-- | The state of the resource. The resource is @ACTIVE@ if it produces
-- metrics, events, or logs within an hour, otherwise it is @INACTIVE@. You
-- pay for the number of active Amazon Web Services resource hours analyzed
-- for each resource. Inactive resources are not charged.
serviceResourceCost_state :: Lens.Lens' ServiceResourceCost (Prelude.Maybe CostEstimationServiceResourceState)
serviceResourceCost_state = Lens.lens (\ServiceResourceCost' {state} -> state) (\s@ServiceResourceCost' {} a -> s {state = a} :: ServiceResourceCost)

-- | The number of active resources analyzed for this service to create a
-- monthly cost estimate.
serviceResourceCost_count :: Lens.Lens' ServiceResourceCost (Prelude.Maybe Prelude.Int)
serviceResourceCost_count = Lens.lens (\ServiceResourceCost' {count} -> count) (\s@ServiceResourceCost' {} a -> s {count = a} :: ServiceResourceCost)

-- | The price per hour to analyze the resources in the service. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
serviceResourceCost_unitCost :: Lens.Lens' ServiceResourceCost (Prelude.Maybe Prelude.Double)
serviceResourceCost_unitCost = Lens.lens (\ServiceResourceCost' {unitCost} -> unitCost) (\s@ServiceResourceCost' {} a -> s {unitCost = a} :: ServiceResourceCost)

-- | The total estimated monthly cost to analyze the active resources for
-- this resource.
serviceResourceCost_cost :: Lens.Lens' ServiceResourceCost (Prelude.Maybe Prelude.Double)
serviceResourceCost_cost = Lens.lens (\ServiceResourceCost' {cost} -> cost) (\s@ServiceResourceCost' {} a -> s {cost = a} :: ServiceResourceCost)

instance Core.FromJSON ServiceResourceCost where
  parseJSON =
    Core.withObject
      "ServiceResourceCost"
      ( \x ->
          ServiceResourceCost'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "UnitCost")
            Prelude.<*> (x Core..:? "Cost")
      )

instance Prelude.Hashable ServiceResourceCost where
  hashWithSalt _salt ServiceResourceCost' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` unitCost
      `Prelude.hashWithSalt` cost

instance Prelude.NFData ServiceResourceCost where
  rnf ServiceResourceCost' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf unitCost
      `Prelude.seq` Prelude.rnf cost
