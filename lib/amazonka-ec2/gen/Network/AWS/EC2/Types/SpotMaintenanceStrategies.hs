{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotMaintenanceStrategies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotMaintenanceStrategies
  ( SpotMaintenanceStrategies (..),

    -- * Smart constructor
    mkSpotMaintenanceStrategies,

    -- * Lenses
    smsCapacityRebalance,
  )
where

import Network.AWS.EC2.Types.SpotCapacityRebalance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /See:/ 'mkSpotMaintenanceStrategies' smart constructor.
newtype SpotMaintenanceStrategies = SpotMaintenanceStrategies'
  { capacityRebalance ::
      Lude.Maybe SpotCapacityRebalance
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotMaintenanceStrategies' with the minimum fields required to make a request.
--
-- * 'capacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
mkSpotMaintenanceStrategies ::
  SpotMaintenanceStrategies
mkSpotMaintenanceStrategies =
  SpotMaintenanceStrategies' {capacityRebalance = Lude.Nothing}

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsCapacityRebalance :: Lens.Lens' SpotMaintenanceStrategies (Lude.Maybe SpotCapacityRebalance)
smsCapacityRebalance = Lens.lens (capacityRebalance :: SpotMaintenanceStrategies -> Lude.Maybe SpotCapacityRebalance) (\s a -> s {capacityRebalance = a} :: SpotMaintenanceStrategies)
{-# DEPRECATED smsCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

instance Lude.FromXML SpotMaintenanceStrategies where
  parseXML x =
    SpotMaintenanceStrategies'
      Lude.<$> (x Lude..@? "capacityRebalance")

instance Lude.ToQuery SpotMaintenanceStrategies where
  toQuery SpotMaintenanceStrategies' {..} =
    Lude.mconcat ["CapacityRebalance" Lude.=: capacityRebalance]
