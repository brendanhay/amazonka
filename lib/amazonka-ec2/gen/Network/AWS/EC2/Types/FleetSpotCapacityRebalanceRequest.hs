-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
  ( FleetSpotCapacityRebalanceRequest (..),

    -- * Smart constructor
    mkFleetSpotCapacityRebalanceRequest,

    -- * Lenses
    fscrrReplacementStrategy,
  )
where

import Network.AWS.EC2.Types.FleetReplacementStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkFleetSpotCapacityRebalanceRequest' smart constructor.
newtype FleetSpotCapacityRebalanceRequest = FleetSpotCapacityRebalanceRequest'
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

-- | Creates a value of 'FleetSpotCapacityRebalanceRequest' with the minimum fields required to make a request.
--
-- * 'replacementStrategy' - The replacement strategy to use. Only available for fleets of type @maintain@ .
--
-- To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
mkFleetSpotCapacityRebalanceRequest ::
  FleetSpotCapacityRebalanceRequest
mkFleetSpotCapacityRebalanceRequest =
  FleetSpotCapacityRebalanceRequest'
    { replacementStrategy =
        Lude.Nothing
    }

-- | The replacement strategy to use. Only available for fleets of type @maintain@ .
--
-- To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscrrReplacementStrategy :: Lens.Lens' FleetSpotCapacityRebalanceRequest (Lude.Maybe FleetReplacementStrategy)
fscrrReplacementStrategy = Lens.lens (replacementStrategy :: FleetSpotCapacityRebalanceRequest -> Lude.Maybe FleetReplacementStrategy) (\s a -> s {replacementStrategy = a} :: FleetSpotCapacityRebalanceRequest)
{-# DEPRECATED fscrrReplacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead." #-}

instance Lude.ToQuery FleetSpotCapacityRebalanceRequest where
  toQuery FleetSpotCapacityRebalanceRequest' {..} =
    Lude.mconcat ["ReplacementStrategy" Lude.=: replacementStrategy]
