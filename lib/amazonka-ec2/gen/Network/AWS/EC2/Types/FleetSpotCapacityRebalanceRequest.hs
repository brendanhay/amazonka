{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.EC2.Types.FleetReplacementStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkFleetSpotCapacityRebalanceRequest' smart constructor.
newtype FleetSpotCapacityRebalanceRequest = FleetSpotCapacityRebalanceRequest'
  { -- | The replacement strategy to use. Only available for fleets of type @maintain@ .
    --
    -- To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
    replacementStrategy :: Core.Maybe Types.FleetReplacementStrategy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FleetSpotCapacityRebalanceRequest' value with any optional fields omitted.
mkFleetSpotCapacityRebalanceRequest ::
  FleetSpotCapacityRebalanceRequest
mkFleetSpotCapacityRebalanceRequest =
  FleetSpotCapacityRebalanceRequest'
    { replacementStrategy =
        Core.Nothing
    }

-- | The replacement strategy to use. Only available for fleets of type @maintain@ .
--
-- To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscrrReplacementStrategy :: Lens.Lens' FleetSpotCapacityRebalanceRequest (Core.Maybe Types.FleetReplacementStrategy)
fscrrReplacementStrategy = Lens.field @"replacementStrategy"
{-# DEPRECATED fscrrReplacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead." #-}
