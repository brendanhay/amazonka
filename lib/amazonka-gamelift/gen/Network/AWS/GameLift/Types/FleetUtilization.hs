{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.FleetUtilization
  ( FleetUtilization (..)
  -- * Smart constructor
  , mkFleetUtilization
  -- * Lenses
  , fuActiveGameSessionCount
  , fuActiveServerProcessCount
  , fuCurrentPlayerSessionCount
  , fuFleetId
  , fuMaximumPlayerSessionCount
  ) where

import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Current status of fleet utilization, including the number of game and player sessions being hosted.
--
--
--     * 'CreateFleet' 
--
--
--     * 'ListFleets' 
--
--
--     * 'DeleteFleet' 
--
--
--     * 'DescribeFleetAttributes' 
--
--
--     * 'UpdateFleetAttributes' 
--
--
--     * 'StartFleetActions' or 'StopFleetActions' 
--
--
--
-- /See:/ 'mkFleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
  { activeGameSessionCount :: Core.Maybe Core.Natural
    -- ^ Number of active game sessions currently being hosted on all instances in the fleet.
  , activeServerProcessCount :: Core.Maybe Core.Natural
    -- ^ Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
  , currentPlayerSessionCount :: Core.Maybe Core.Natural
    -- ^ Number of active player sessions currently being hosted on all instances in the fleet.
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet.
  , maximumPlayerSessionCount :: Core.Maybe Core.Natural
    -- ^ The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetUtilization' value with any optional fields omitted.
mkFleetUtilization
    :: FleetUtilization
mkFleetUtilization
  = FleetUtilization'{activeGameSessionCount = Core.Nothing,
                      activeServerProcessCount = Core.Nothing,
                      currentPlayerSessionCount = Core.Nothing, fleetId = Core.Nothing,
                      maximumPlayerSessionCount = Core.Nothing}

-- | Number of active game sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'activeGameSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuActiveGameSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fuActiveGameSessionCount = Lens.field @"activeGameSessionCount"
{-# INLINEABLE fuActiveGameSessionCount #-}
{-# DEPRECATED activeGameSessionCount "Use generic-lens or generic-optics with 'activeGameSessionCount' instead"  #-}

-- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
--
-- /Note:/ Consider using 'activeServerProcessCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuActiveServerProcessCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fuActiveServerProcessCount = Lens.field @"activeServerProcessCount"
{-# INLINEABLE fuActiveServerProcessCount #-}
{-# DEPRECATED activeServerProcessCount "Use generic-lens or generic-optics with 'activeServerProcessCount' instead"  #-}

-- | Number of active player sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'currentPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuCurrentPlayerSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fuCurrentPlayerSessionCount = Lens.field @"currentPlayerSessionCount"
{-# INLINEABLE fuCurrentPlayerSessionCount #-}
{-# DEPRECATED currentPlayerSessionCount "Use generic-lens or generic-optics with 'currentPlayerSessionCount' instead"  #-}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuFleetId :: Lens.Lens' FleetUtilization (Core.Maybe Types.FleetId)
fuFleetId = Lens.field @"fleetId"
{-# INLINEABLE fuFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuMaximumPlayerSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fuMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# INLINEABLE fuMaximumPlayerSessionCount #-}
{-# DEPRECATED maximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead"  #-}

instance Core.FromJSON FleetUtilization where
        parseJSON
          = Core.withObject "FleetUtilization" Core.$
              \ x ->
                FleetUtilization' Core.<$>
                  (x Core..:? "ActiveGameSessionCount") Core.<*>
                    x Core..:? "ActiveServerProcessCount"
                    Core.<*> x Core..:? "CurrentPlayerSessionCount"
                    Core.<*> x Core..:? "FleetId"
                    Core.<*> x Core..:? "MaximumPlayerSessionCount"
