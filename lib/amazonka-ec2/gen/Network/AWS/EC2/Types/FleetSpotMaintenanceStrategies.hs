{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
  ( FleetSpotMaintenanceStrategies (..)
  -- * Smart constructor
  , mkFleetSpotMaintenanceStrategies
  -- * Lenses
  , fsmsCapacityRebalance
  ) where

import qualified Network.AWS.EC2.Types.FleetSpotCapacityRebalance as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotMaintenanceStrategies' smart constructor.
newtype FleetSpotMaintenanceStrategies = FleetSpotMaintenanceStrategies'
  { capacityRebalance :: Core.Maybe Types.FleetSpotCapacityRebalance
    -- ^ The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FleetSpotMaintenanceStrategies' value with any optional fields omitted.
mkFleetSpotMaintenanceStrategies
    :: FleetSpotMaintenanceStrategies
mkFleetSpotMaintenanceStrategies
  = FleetSpotMaintenanceStrategies'{capacityRebalance = Core.Nothing}

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsmsCapacityRebalance :: Lens.Lens' FleetSpotMaintenanceStrategies (Core.Maybe Types.FleetSpotCapacityRebalance)
fsmsCapacityRebalance = Lens.field @"capacityRebalance"
{-# INLINEABLE fsmsCapacityRebalance #-}
{-# DEPRECATED capacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead"  #-}

instance Core.FromXML FleetSpotMaintenanceStrategies where
        parseXML x
          = FleetSpotMaintenanceStrategies' Core.<$>
              (x Core..@? "capacityRebalance")
