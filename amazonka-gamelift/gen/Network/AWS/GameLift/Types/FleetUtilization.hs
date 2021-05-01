{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    activeGameSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | Number of active player sessions currently being hosted on all instances
    -- in the fleet.
    currentPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of players allowed across all game sessions currently
    -- being hosted on all instances in the fleet.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Number of server processes in an @ACTIVE@ status currently running
    -- across all instances in the fleet
    activeServerProcessCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      currentPlayerSessionCount = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      activeServerProcessCount = Prelude.Nothing
    }

-- | Number of active game sessions currently being hosted on all instances
-- in the fleet.
fleetUtilization_activeGameSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_activeGameSessionCount = Lens.lens (\FleetUtilization' {activeGameSessionCount} -> activeGameSessionCount) (\s@FleetUtilization' {} a -> s {activeGameSessionCount = a} :: FleetUtilization)

-- | Number of active player sessions currently being hosted on all instances
-- in the fleet.
fleetUtilization_currentPlayerSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_currentPlayerSessionCount = Lens.lens (\FleetUtilization' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@FleetUtilization' {} a -> s {currentPlayerSessionCount = a} :: FleetUtilization)

-- | The maximum number of players allowed across all game sessions currently
-- being hosted on all instances in the fleet.
fleetUtilization_maximumPlayerSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_maximumPlayerSessionCount = Lens.lens (\FleetUtilization' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@FleetUtilization' {} a -> s {maximumPlayerSessionCount = a} :: FleetUtilization)

-- | A unique identifier for a fleet.
fleetUtilization_fleetId :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Text)
fleetUtilization_fleetId = Lens.lens (\FleetUtilization' {fleetId} -> fleetId) (\s@FleetUtilization' {} a -> s {fleetId = a} :: FleetUtilization)

-- | Number of server processes in an @ACTIVE@ status currently running
-- across all instances in the fleet
fleetUtilization_activeServerProcessCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_activeServerProcessCount = Lens.lens (\FleetUtilization' {activeServerProcessCount} -> activeServerProcessCount) (\s@FleetUtilization' {} a -> s {activeServerProcessCount = a} :: FleetUtilization)

instance Prelude.FromJSON FleetUtilization where
  parseJSON =
    Prelude.withObject
      "FleetUtilization"
      ( \x ->
          FleetUtilization'
            Prelude.<$> (x Prelude..:? "ActiveGameSessionCount")
            Prelude.<*> (x Prelude..:? "CurrentPlayerSessionCount")
            Prelude.<*> (x Prelude..:? "MaximumPlayerSessionCount")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "ActiveServerProcessCount")
      )

instance Prelude.Hashable FleetUtilization

instance Prelude.NFData FleetUtilization
