{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotCapacityRebalance
  ( FleetSpotCapacityRebalance (..),

    -- * Smart constructor
    mkFleetSpotCapacityRebalance,

    -- * Lenses
    fscrReplacementStrategy,
  )
where

import Network.AWS.EC2.Types.FleetReplacementStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotCapacityRebalance' smart constructor.
newtype FleetSpotCapacityRebalance = FleetSpotCapacityRebalance'
  { replacementStrategy ::
      Lude.Maybe
        FleetReplacementStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetSpotCapacityRebalance' with the minimum fields required to make a request.
--
-- * 'replacementStrategy' - To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
mkFleetSpotCapacityRebalance ::
  FleetSpotCapacityRebalance
mkFleetSpotCapacityRebalance =
  FleetSpotCapacityRebalance' {replacementStrategy = Lude.Nothing}

-- | To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscrReplacementStrategy :: Lens.Lens' FleetSpotCapacityRebalance (Lude.Maybe FleetReplacementStrategy)
fscrReplacementStrategy = Lens.lens (replacementStrategy :: FleetSpotCapacityRebalance -> Lude.Maybe FleetReplacementStrategy) (\s a -> s {replacementStrategy = a} :: FleetSpotCapacityRebalance)
{-# DEPRECATED fscrReplacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead." #-}

instance Lude.FromXML FleetSpotCapacityRebalance where
  parseXML x =
    FleetSpotCapacityRebalance'
      Lude.<$> (x Lude..@? "replacementStrategy")
