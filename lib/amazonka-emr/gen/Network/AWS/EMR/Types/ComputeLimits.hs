{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ComputeLimits
  ( ComputeLimits (..)
  -- * Smart constructor
  , mkComputeLimits
  -- * Lenses
  , clUnitType
  , clMinimumCapacityUnits
  , clMaximumCapacityUnits
  , clMaximumCoreCapacityUnits
  , clMaximumOnDemandCapacityUnits
  ) where

import qualified Network.AWS.EMR.Types.ComputeLimitsUnitType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster can not be above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- /See:/ 'mkComputeLimits' smart constructor.
data ComputeLimits = ComputeLimits'
  { unitType :: Types.ComputeLimitsUnitType
    -- ^ The unit type used for specifying a managed scaling policy. 
  , minimumCapacityUnits :: Core.Int
    -- ^ The lower boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
  , maximumCapacityUnits :: Core.Int
    -- ^ The upper boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
  , maximumCoreCapacityUnits :: Core.Maybe Core.Int
    -- ^ The upper boundary of EC2 units for core node type in a cluster. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The core units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between core and task nodes. 
  , maximumOnDemandCapacityUnits :: Core.Maybe Core.Int
    -- ^ The upper boundary of On-Demand EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The On-Demand units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between On-Demand and Spot Instances. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeLimits' value with any optional fields omitted.
mkComputeLimits
    :: Types.ComputeLimitsUnitType -- ^ 'unitType'
    -> Core.Int -- ^ 'minimumCapacityUnits'
    -> Core.Int -- ^ 'maximumCapacityUnits'
    -> ComputeLimits
mkComputeLimits unitType minimumCapacityUnits maximumCapacityUnits
  = ComputeLimits'{unitType, minimumCapacityUnits,
                   maximumCapacityUnits, maximumCoreCapacityUnits = Core.Nothing,
                   maximumOnDemandCapacityUnits = Core.Nothing}

-- | The unit type used for specifying a managed scaling policy. 
--
-- /Note:/ Consider using 'unitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clUnitType :: Lens.Lens' ComputeLimits Types.ComputeLimitsUnitType
clUnitType = Lens.field @"unitType"
{-# INLINEABLE clUnitType #-}
{-# DEPRECATED unitType "Use generic-lens or generic-optics with 'unitType' instead"  #-}

-- | The lower boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- /Note:/ Consider using 'minimumCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMinimumCapacityUnits :: Lens.Lens' ComputeLimits Core.Int
clMinimumCapacityUnits = Lens.field @"minimumCapacityUnits"
{-# INLINEABLE clMinimumCapacityUnits #-}
{-# DEPRECATED minimumCapacityUnits "Use generic-lens or generic-optics with 'minimumCapacityUnits' instead"  #-}

-- | The upper boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- /Note:/ Consider using 'maximumCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumCapacityUnits :: Lens.Lens' ComputeLimits Core.Int
clMaximumCapacityUnits = Lens.field @"maximumCapacityUnits"
{-# INLINEABLE clMaximumCapacityUnits #-}
{-# DEPRECATED maximumCapacityUnits "Use generic-lens or generic-optics with 'maximumCapacityUnits' instead"  #-}

-- | The upper boundary of EC2 units for core node type in a cluster. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The core units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between core and task nodes. 
--
-- /Note:/ Consider using 'maximumCoreCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumCoreCapacityUnits :: Lens.Lens' ComputeLimits (Core.Maybe Core.Int)
clMaximumCoreCapacityUnits = Lens.field @"maximumCoreCapacityUnits"
{-# INLINEABLE clMaximumCoreCapacityUnits #-}
{-# DEPRECATED maximumCoreCapacityUnits "Use generic-lens or generic-optics with 'maximumCoreCapacityUnits' instead"  #-}

-- | The upper boundary of On-Demand EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The On-Demand units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between On-Demand and Spot Instances. 
--
-- /Note:/ Consider using 'maximumOnDemandCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumOnDemandCapacityUnits :: Lens.Lens' ComputeLimits (Core.Maybe Core.Int)
clMaximumOnDemandCapacityUnits = Lens.field @"maximumOnDemandCapacityUnits"
{-# INLINEABLE clMaximumOnDemandCapacityUnits #-}
{-# DEPRECATED maximumOnDemandCapacityUnits "Use generic-lens or generic-optics with 'maximumOnDemandCapacityUnits' instead"  #-}

instance Core.FromJSON ComputeLimits where
        toJSON ComputeLimits{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UnitType" Core..= unitType),
                  Core.Just ("MinimumCapacityUnits" Core..= minimumCapacityUnits),
                  Core.Just ("MaximumCapacityUnits" Core..= maximumCapacityUnits),
                  ("MaximumCoreCapacityUnits" Core..=) Core.<$>
                    maximumCoreCapacityUnits,
                  ("MaximumOnDemandCapacityUnits" Core..=) Core.<$>
                    maximumOnDemandCapacityUnits])

instance Core.FromJSON ComputeLimits where
        parseJSON
          = Core.withObject "ComputeLimits" Core.$
              \ x ->
                ComputeLimits' Core.<$>
                  (x Core..: "UnitType") Core.<*> x Core..: "MinimumCapacityUnits"
                    Core.<*> x Core..: "MaximumCapacityUnits"
                    Core.<*> x Core..:? "MaximumCoreCapacityUnits"
                    Core.<*> x Core..:? "MaximumOnDemandCapacityUnits"
