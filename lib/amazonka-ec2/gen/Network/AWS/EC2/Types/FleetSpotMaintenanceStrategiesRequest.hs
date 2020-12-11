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

import Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotMaintenanceStrategiesRequest' smart constructor.
newtype FleetSpotMaintenanceStrategiesRequest = FleetSpotMaintenanceStrategiesRequest'
  { capacityRebalance ::
      Lude.Maybe
        FleetSpotCapacityRebalanceRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetSpotMaintenanceStrategiesRequest' with the minimum fields required to make a request.
--
-- * 'capacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
mkFleetSpotMaintenanceStrategiesRequest ::
  FleetSpotMaintenanceStrategiesRequest
mkFleetSpotMaintenanceStrategiesRequest =
  FleetSpotMaintenanceStrategiesRequest'
    { capacityRebalance =
        Lude.Nothing
    }

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsmsrCapacityRebalance :: Lens.Lens' FleetSpotMaintenanceStrategiesRequest (Lude.Maybe FleetSpotCapacityRebalanceRequest)
fsmsrCapacityRebalance = Lens.lens (capacityRebalance :: FleetSpotMaintenanceStrategiesRequest -> Lude.Maybe FleetSpotCapacityRebalanceRequest) (\s a -> s {capacityRebalance = a} :: FleetSpotMaintenanceStrategiesRequest)
{-# DEPRECATED fsmsrCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

instance Lude.ToQuery FleetSpotMaintenanceStrategiesRequest where
  toQuery FleetSpotMaintenanceStrategiesRequest' {..} =
    Lude.mconcat ["CapacityRebalance" Lude.=: capacityRebalance]
