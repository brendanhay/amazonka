{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.OnDemandProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.OnDemandProvisioningSpecification
  ( OnDemandProvisioningSpecification (..)
  -- * Smart constructor
  , mkOnDemandProvisioningSpecification
  -- * Lenses
  , odpsAllocationStrategy
  ) where

import qualified Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy. 
--
-- /See:/ 'mkOnDemandProvisioningSpecification' smart constructor.
newtype OnDemandProvisioningSpecification = OnDemandProvisioningSpecification'
  { allocationStrategy :: Types.OnDemandProvisioningAllocationStrategy
    -- ^ Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OnDemandProvisioningSpecification' value with any optional fields omitted.
mkOnDemandProvisioningSpecification
    :: Types.OnDemandProvisioningAllocationStrategy -- ^ 'allocationStrategy'
    -> OnDemandProvisioningSpecification
mkOnDemandProvisioningSpecification allocationStrategy
  = OnDemandProvisioningSpecification'{allocationStrategy}

-- | Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first. 
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odpsAllocationStrategy :: Lens.Lens' OnDemandProvisioningSpecification Types.OnDemandProvisioningAllocationStrategy
odpsAllocationStrategy = Lens.field @"allocationStrategy"
{-# INLINEABLE odpsAllocationStrategy #-}
{-# DEPRECATED allocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead"  #-}

instance Core.FromJSON OnDemandProvisioningSpecification where
        toJSON OnDemandProvisioningSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AllocationStrategy" Core..= allocationStrategy)])

instance Core.FromJSON OnDemandProvisioningSpecification where
        parseJSON
          = Core.withObject "OnDemandProvisioningSpecification" Core.$
              \ x ->
                OnDemandProvisioningSpecification' Core.<$>
                  (x Core..: "AllocationStrategy")
