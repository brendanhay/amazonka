{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
  ( FleetSpotMaintenanceStrategiesRequest (..),

    -- * Smart constructor
    mkFleetSpotMaintenanceStrategiesRequest,

    -- * Lenses
    fsmsrCapacityRebalance,
  )
where

import qualified Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotMaintenanceStrategiesRequest' smart constructor.
newtype FleetSpotMaintenanceStrategiesRequest = FleetSpotMaintenanceStrategiesRequest'
  { -- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
    capacityRebalance :: Core.Maybe Types.FleetSpotCapacityRebalanceRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FleetSpotMaintenanceStrategiesRequest' value with any optional fields omitted.
mkFleetSpotMaintenanceStrategiesRequest ::
  FleetSpotMaintenanceStrategiesRequest
mkFleetSpotMaintenanceStrategiesRequest =
  FleetSpotMaintenanceStrategiesRequest'
    { capacityRebalance =
        Core.Nothing
    }

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsmsrCapacityRebalance :: Lens.Lens' FleetSpotMaintenanceStrategiesRequest (Core.Maybe Types.FleetSpotCapacityRebalanceRequest)
fsmsrCapacityRebalance = Lens.field @"capacityRebalance"
{-# DEPRECATED fsmsrCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}
