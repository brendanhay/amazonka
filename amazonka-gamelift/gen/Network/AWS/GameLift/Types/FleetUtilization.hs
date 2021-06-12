{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetUtilization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Current status of fleet utilization, including the number of game and
-- player sessions being hosted.
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- /See:/ 'newFleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
  { -- | Number of active game sessions currently being hosted on all instances
    -- in the fleet.
    activeGameSessionCount :: Core.Maybe Core.Natural,
    -- | Number of active player sessions currently being hosted on all instances
    -- in the fleet.
    currentPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | The maximum number of players allowed across all game sessions currently
    -- being hosted on all instances in the fleet.
    maximumPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | A unique identifier for a fleet.
    fleetId :: Core.Maybe Core.Text,
    -- | Number of server processes in an @ACTIVE@ status currently running
    -- across all instances in the fleet
    activeServerProcessCount :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FleetUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeGameSessionCount', 'fleetUtilization_activeGameSessionCount' - Number of active game sessions currently being hosted on all instances
-- in the fleet.
--
-- 'currentPlayerSessionCount', 'fleetUtilization_currentPlayerSessionCount' - Number of active player sessions currently being hosted on all instances
-- in the fleet.
--
-- 'maximumPlayerSessionCount', 'fleetUtilization_maximumPlayerSessionCount' - The maximum number of players allowed across all game sessions currently
-- being hosted on all instances in the fleet.
--
-- 'fleetId', 'fleetUtilization_fleetId' - A unique identifier for a fleet.
--
-- 'activeServerProcessCount', 'fleetUtilization_activeServerProcessCount' - Number of server processes in an @ACTIVE@ status currently running
-- across all instances in the fleet
newFleetUtilization ::
  FleetUtilization
newFleetUtilization =
  FleetUtilization'
    { activeGameSessionCount =
        Core.Nothing,
      currentPlayerSessionCount = Core.Nothing,
      maximumPlayerSessionCount = Core.Nothing,
      fleetId = Core.Nothing,
      activeServerProcessCount = Core.Nothing
    }

-- | Number of active game sessions currently being hosted on all instances
-- in the fleet.
fleetUtilization_activeGameSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fleetUtilization_activeGameSessionCount = Lens.lens (\FleetUtilization' {activeGameSessionCount} -> activeGameSessionCount) (\s@FleetUtilization' {} a -> s {activeGameSessionCount = a} :: FleetUtilization)

-- | Number of active player sessions currently being hosted on all instances
-- in the fleet.
fleetUtilization_currentPlayerSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fleetUtilization_currentPlayerSessionCount = Lens.lens (\FleetUtilization' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@FleetUtilization' {} a -> s {currentPlayerSessionCount = a} :: FleetUtilization)

-- | The maximum number of players allowed across all game sessions currently
-- being hosted on all instances in the fleet.
fleetUtilization_maximumPlayerSessionCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fleetUtilization_maximumPlayerSessionCount = Lens.lens (\FleetUtilization' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@FleetUtilization' {} a -> s {maximumPlayerSessionCount = a} :: FleetUtilization)

-- | A unique identifier for a fleet.
fleetUtilization_fleetId :: Lens.Lens' FleetUtilization (Core.Maybe Core.Text)
fleetUtilization_fleetId = Lens.lens (\FleetUtilization' {fleetId} -> fleetId) (\s@FleetUtilization' {} a -> s {fleetId = a} :: FleetUtilization)

-- | Number of server processes in an @ACTIVE@ status currently running
-- across all instances in the fleet
fleetUtilization_activeServerProcessCount :: Lens.Lens' FleetUtilization (Core.Maybe Core.Natural)
fleetUtilization_activeServerProcessCount = Lens.lens (\FleetUtilization' {activeServerProcessCount} -> activeServerProcessCount) (\s@FleetUtilization' {} a -> s {activeServerProcessCount = a} :: FleetUtilization)

instance Core.FromJSON FleetUtilization where
  parseJSON =
    Core.withObject
      "FleetUtilization"
      ( \x ->
          FleetUtilization'
            Core.<$> (x Core..:? "ActiveGameSessionCount")
            Core.<*> (x Core..:? "CurrentPlayerSessionCount")
            Core.<*> (x Core..:? "MaximumPlayerSessionCount")
            Core.<*> (x Core..:? "FleetId")
            Core.<*> (x Core..:? "ActiveServerProcessCount")
      )

instance Core.Hashable FleetUtilization

instance Core.NFData FleetUtilization
