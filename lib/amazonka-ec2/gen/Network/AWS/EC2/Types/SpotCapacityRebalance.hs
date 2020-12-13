{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotCapacityRebalance
  ( SpotCapacityRebalance (..),

    -- * Smart constructor
    mkSpotCapacityRebalance,

    -- * Lenses
    scrReplacementStrategy,
  )
where

import Network.AWS.EC2.Types.ReplacementStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#spot-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /See:/ 'mkSpotCapacityRebalance' smart constructor.
newtype SpotCapacityRebalance = SpotCapacityRebalance'
  { -- | The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error.
    --
    -- To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
    replacementStrategy :: Lude.Maybe ReplacementStrategy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotCapacityRebalance' with the minimum fields required to make a request.
--
-- * 'replacementStrategy' - The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
mkSpotCapacityRebalance ::
  SpotCapacityRebalance
mkSpotCapacityRebalance =
  SpotCapacityRebalance' {replacementStrategy = Lude.Nothing}

-- | The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrReplacementStrategy :: Lens.Lens' SpotCapacityRebalance (Lude.Maybe ReplacementStrategy)
scrReplacementStrategy = Lens.lens (replacementStrategy :: SpotCapacityRebalance -> Lude.Maybe ReplacementStrategy) (\s a -> s {replacementStrategy = a} :: SpotCapacityRebalance)
{-# DEPRECATED scrReplacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead." #-}

instance Lude.FromXML SpotCapacityRebalance where
  parseXML x =
    SpotCapacityRebalance'
      Lude.<$> (x Lude..@? "replacementStrategy")

instance Lude.ToQuery SpotCapacityRebalance where
  toQuery SpotCapacityRebalance' {..} =
    Lude.mconcat ["ReplacementStrategy" Lude.=: replacementStrategy]
