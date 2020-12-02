{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetUtilization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Current status of fleet utilization, including the number of game and player sessions being hosted.
--
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- /See:/ 'fleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
  { _fuActiveGameSessionCount ::
      !(Maybe Nat),
    _fuMaximumPlayerSessionCount :: !(Maybe Nat),
    _fuCurrentPlayerSessionCount :: !(Maybe Nat),
    _fuFleetId :: !(Maybe Text),
    _fuActiveServerProcessCount :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fuActiveGameSessionCount' - Number of active game sessions currently being hosted on all instances in the fleet.
--
-- * 'fuMaximumPlayerSessionCount' - The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
--
-- * 'fuCurrentPlayerSessionCount' - Number of active player sessions currently being hosted on all instances in the fleet.
--
-- * 'fuFleetId' - A unique identifier for a fleet.
--
-- * 'fuActiveServerProcessCount' - Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
fleetUtilization ::
  FleetUtilization
fleetUtilization =
  FleetUtilization'
    { _fuActiveGameSessionCount = Nothing,
      _fuMaximumPlayerSessionCount = Nothing,
      _fuCurrentPlayerSessionCount = Nothing,
      _fuFleetId = Nothing,
      _fuActiveServerProcessCount = Nothing
    }

-- | Number of active game sessions currently being hosted on all instances in the fleet.
fuActiveGameSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveGameSessionCount = lens _fuActiveGameSessionCount (\s a -> s {_fuActiveGameSessionCount = a}) . mapping _Nat

-- | The maximum number of players allowed across all game sessions currently being hosted on all instances in the fleet.
fuMaximumPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuMaximumPlayerSessionCount = lens _fuMaximumPlayerSessionCount (\s a -> s {_fuMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Number of active player sessions currently being hosted on all instances in the fleet.
fuCurrentPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuCurrentPlayerSessionCount = lens _fuCurrentPlayerSessionCount (\s a -> s {_fuCurrentPlayerSessionCount = a}) . mapping _Nat

-- | A unique identifier for a fleet.
fuFleetId :: Lens' FleetUtilization (Maybe Text)
fuFleetId = lens _fuFleetId (\s a -> s {_fuFleetId = a})

-- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
fuActiveServerProcessCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveServerProcessCount = lens _fuActiveServerProcessCount (\s a -> s {_fuActiveServerProcessCount = a}) . mapping _Nat

instance FromJSON FleetUtilization where
  parseJSON =
    withObject
      "FleetUtilization"
      ( \x ->
          FleetUtilization'
            <$> (x .:? "ActiveGameSessionCount")
            <*> (x .:? "MaximumPlayerSessionCount")
            <*> (x .:? "CurrentPlayerSessionCount")
            <*> (x .:? "FleetId")
            <*> (x .:? "ActiveServerProcessCount")
      )

instance Hashable FleetUtilization

instance NFData FleetUtilization
