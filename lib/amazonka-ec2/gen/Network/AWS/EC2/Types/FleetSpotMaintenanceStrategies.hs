-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
  ( FleetSpotMaintenanceStrategies (..),

    -- * Smart constructor
    mkFleetSpotMaintenanceStrategies,

    -- * Lenses
    fsmsCapacityRebalance,
  )
where

import Network.AWS.EC2.Types.FleetSpotCapacityRebalance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotMaintenanceStrategies' smart constructor.
newtype FleetSpotMaintenanceStrategies = FleetSpotMaintenanceStrategies'
  { capacityRebalance ::
      Lude.Maybe
        FleetSpotCapacityRebalance
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetSpotMaintenanceStrategies' with the minimum fields required to make a request.
--
-- * 'capacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
mkFleetSpotMaintenanceStrategies ::
  FleetSpotMaintenanceStrategies
mkFleetSpotMaintenanceStrategies =
  FleetSpotMaintenanceStrategies' {capacityRebalance = Lude.Nothing}

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsmsCapacityRebalance :: Lens.Lens' FleetSpotMaintenanceStrategies (Lude.Maybe FleetSpotCapacityRebalance)
fsmsCapacityRebalance = Lens.lens (capacityRebalance :: FleetSpotMaintenanceStrategies -> Lude.Maybe FleetSpotCapacityRebalance) (\s a -> s {capacityRebalance = a} :: FleetSpotMaintenanceStrategies)
{-# DEPRECATED fsmsCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

instance Lude.FromXML FleetSpotMaintenanceStrategies where
  parseXML x =
    FleetSpotMaintenanceStrategies'
      Lude.<$> (x Lude..@? "capacityRebalance")
