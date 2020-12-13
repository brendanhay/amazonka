{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetUtilization
  ( FleetUtilization (..),

    -- * Smart constructor
    mkFleetUtilization,

    -- * Lenses
    fuActiveGameSessionCount,
    fuMaximumPlayerSessionCount,
    fuCurrentPlayerSessionCount,
    fuFleetId,
    fuActiveServerProcessCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | Number of active game sessions currently being hosted on all instances in the fleet.
    activeGameSessionCount :: Lude.Maybe Lude.Natural,
    -- | The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
    maximumPlayerSessionCount :: Lude.Maybe Lude.Natural,
    -- | Number of active player sessions currently being hosted on all instances in the fleet.
    currentPlayerSessionCount :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for a fleet.
    fleetId :: Lude.Maybe Lude.Text,
    -- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
    activeServerProcessCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetUtilization' with the minimum fields required to make a request.
--
-- * 'activeGameSessionCount' - Number of active game sessions currently being hosted on all instances in the fleet.
-- * 'maximumPlayerSessionCount' - The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
-- * 'currentPlayerSessionCount' - Number of active player sessions currently being hosted on all instances in the fleet.
-- * 'fleetId' - A unique identifier for a fleet.
-- * 'activeServerProcessCount' - Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
mkFleetUtilization ::
  FleetUtilization
mkFleetUtilization =
  FleetUtilization'
    { activeGameSessionCount = Lude.Nothing,
      maximumPlayerSessionCount = Lude.Nothing,
      currentPlayerSessionCount = Lude.Nothing,
      fleetId = Lude.Nothing,
      activeServerProcessCount = Lude.Nothing
    }

-- | Number of active game sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'activeGameSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuActiveGameSessionCount :: Lens.Lens' FleetUtilization (Lude.Maybe Lude.Natural)
fuActiveGameSessionCount = Lens.lens (activeGameSessionCount :: FleetUtilization -> Lude.Maybe Lude.Natural) (\s a -> s {activeGameSessionCount = a} :: FleetUtilization)
{-# DEPRECATED fuActiveGameSessionCount "Use generic-lens or generic-optics with 'activeGameSessionCount' instead." #-}

-- | The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuMaximumPlayerSessionCount :: Lens.Lens' FleetUtilization (Lude.Maybe Lude.Natural)
fuMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: FleetUtilization -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: FleetUtilization)
{-# DEPRECATED fuMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Number of active player sessions currently being hosted on all instances in the fleet.
--
-- /Note:/ Consider using 'currentPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuCurrentPlayerSessionCount :: Lens.Lens' FleetUtilization (Lude.Maybe Lude.Natural)
fuCurrentPlayerSessionCount = Lens.lens (currentPlayerSessionCount :: FleetUtilization -> Lude.Maybe Lude.Natural) (\s a -> s {currentPlayerSessionCount = a} :: FleetUtilization)
{-# DEPRECATED fuCurrentPlayerSessionCount "Use generic-lens or generic-optics with 'currentPlayerSessionCount' instead." #-}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuFleetId :: Lens.Lens' FleetUtilization (Lude.Maybe Lude.Text)
fuFleetId = Lens.lens (fleetId :: FleetUtilization -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: FleetUtilization)
{-# DEPRECATED fuFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
--
-- /Note:/ Consider using 'activeServerProcessCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuActiveServerProcessCount :: Lens.Lens' FleetUtilization (Lude.Maybe Lude.Natural)
fuActiveServerProcessCount = Lens.lens (activeServerProcessCount :: FleetUtilization -> Lude.Maybe Lude.Natural) (\s a -> s {activeServerProcessCount = a} :: FleetUtilization)
{-# DEPRECATED fuActiveServerProcessCount "Use generic-lens or generic-optics with 'activeServerProcessCount' instead." #-}

instance Lude.FromJSON FleetUtilization where
  parseJSON =
    Lude.withObject
      "FleetUtilization"
      ( \x ->
          FleetUtilization'
            Lude.<$> (x Lude..:? "ActiveGameSessionCount")
            Lude.<*> (x Lude..:? "MaximumPlayerSessionCount")
            Lude.<*> (x Lude..:? "CurrentPlayerSessionCount")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "ActiveServerProcessCount")
      )
