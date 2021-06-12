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
-- Module      : Network.AWS.EMR.Types.OnDemandProvisioningSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.OnDemandProvisioningSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
import qualified Network.AWS.Lens as Lens

-- | The launch specification for On-Demand Instances in the instance fleet,
-- which determines the allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand Instances
-- allocation strategy is available in Amazon EMR version 5.12.1 and later.
--
-- /See:/ 'newOnDemandProvisioningSpecification' smart constructor.
data OnDemandProvisioningSpecification = OnDemandProvisioningSpecification'
  { -- | Specifies the strategy to use in launching On-Demand Instance fleets.
    -- Currently, the only option is lowest-price (the default), which launches
    -- the lowest price first.
    allocationStrategy :: OnDemandProvisioningAllocationStrategy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OnDemandProvisioningSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'onDemandProvisioningSpecification_allocationStrategy' - Specifies the strategy to use in launching On-Demand Instance fleets.
-- Currently, the only option is lowest-price (the default), which launches
-- the lowest price first.
newOnDemandProvisioningSpecification ::
  -- | 'allocationStrategy'
  OnDemandProvisioningAllocationStrategy ->
  OnDemandProvisioningSpecification
newOnDemandProvisioningSpecification
  pAllocationStrategy_ =
    OnDemandProvisioningSpecification'
      { allocationStrategy =
          pAllocationStrategy_
      }

-- | Specifies the strategy to use in launching On-Demand Instance fleets.
-- Currently, the only option is lowest-price (the default), which launches
-- the lowest price first.
onDemandProvisioningSpecification_allocationStrategy :: Lens.Lens' OnDemandProvisioningSpecification OnDemandProvisioningAllocationStrategy
onDemandProvisioningSpecification_allocationStrategy = Lens.lens (\OnDemandProvisioningSpecification' {allocationStrategy} -> allocationStrategy) (\s@OnDemandProvisioningSpecification' {} a -> s {allocationStrategy = a} :: OnDemandProvisioningSpecification)

instance
  Core.FromJSON
    OnDemandProvisioningSpecification
  where
  parseJSON =
    Core.withObject
      "OnDemandProvisioningSpecification"
      ( \x ->
          OnDemandProvisioningSpecification'
            Core.<$> (x Core..: "AllocationStrategy")
      )

instance
  Core.Hashable
    OnDemandProvisioningSpecification

instance
  Core.NFData
    OnDemandProvisioningSpecification

instance
  Core.ToJSON
    OnDemandProvisioningSpecification
  where
  toJSON OnDemandProvisioningSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AllocationStrategy" Core..= allocationStrategy)
          ]
      )
