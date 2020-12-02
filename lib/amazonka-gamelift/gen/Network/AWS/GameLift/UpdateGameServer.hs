{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Updates information about a registered game server to help GameLift FleetIQ to track game server availability. This operation is called by a game server process that is running on an instance in a game server group.
--
-- Use this operation to update the following types of game server information. You can make all three types of updates in the same request:
--
--     * To update the game server's utilization status, identify the game server and game server group and specify the current utilization status. Use this status to identify when game servers are currently hosting games and when they are available to be claimed.
--
--     * To report health status, identify the game server and game server group and set health check to @HEALTHY@ . If a game server does not report health status for a certain length of time, the game server is no longer considered healthy. As a result, it will be eventually deregistered from the game server group to avoid affecting utilization metrics. The best practice is to report health every 60 seconds.
--
--     * To change game server metadata, provide updated game server data.
--
--
--
-- Once a game server is successfully updated, the relevant statuses and timestamps are updated.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
--     * 'RegisterGameServer'
--
--     * 'ListGameServers'
--
--     * 'ClaimGameServer'
--
--     * 'DescribeGameServer'
--
--     * 'UpdateGameServer'
--
--     * 'DeregisterGameServer'
module Network.AWS.GameLift.UpdateGameServer
  ( -- * Creating a Request
    updateGameServer,
    UpdateGameServer,

    -- * Request Lenses
    ugsHealthCheck,
    ugsGameServerData,
    ugsUtilizationStatus,
    ugsGameServerGroupName,
    ugsGameServerId,

    -- * Destructuring the Response
    updateGameServerResponse,
    UpdateGameServerResponse,

    -- * Response Lenses
    ursGameServer,
    ursResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGameServer' smart constructor.
data UpdateGameServer = UpdateGameServer'
  { _ugsHealthCheck ::
      !(Maybe GameServerHealthCheck),
    _ugsGameServerData :: !(Maybe Text),
    _ugsUtilizationStatus ::
      !(Maybe GameServerUtilizationStatus),
    _ugsGameServerGroupName :: !Text,
    _ugsGameServerId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsHealthCheck' - Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
--
-- * 'ugsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- * 'ugsUtilizationStatus' - Indicates whether the game server is available or is currently hosting gameplay.
--
-- * 'ugsGameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'ugsGameServerId' - A custom string that uniquely identifies the game server to update.
updateGameServer ::
  -- | 'ugsGameServerGroupName'
  Text ->
  -- | 'ugsGameServerId'
  Text ->
  UpdateGameServer
updateGameServer pGameServerGroupName_ pGameServerId_ =
  UpdateGameServer'
    { _ugsHealthCheck = Nothing,
      _ugsGameServerData = Nothing,
      _ugsUtilizationStatus = Nothing,
      _ugsGameServerGroupName = pGameServerGroupName_,
      _ugsGameServerId = pGameServerId_
    }

-- | Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
ugsHealthCheck :: Lens' UpdateGameServer (Maybe GameServerHealthCheck)
ugsHealthCheck = lens _ugsHealthCheck (\s a -> s {_ugsHealthCheck = a})

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
ugsGameServerData :: Lens' UpdateGameServer (Maybe Text)
ugsGameServerData = lens _ugsGameServerData (\s a -> s {_ugsGameServerData = a})

-- | Indicates whether the game server is available or is currently hosting gameplay.
ugsUtilizationStatus :: Lens' UpdateGameServer (Maybe GameServerUtilizationStatus)
ugsUtilizationStatus = lens _ugsUtilizationStatus (\s a -> s {_ugsUtilizationStatus = a})

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
ugsGameServerGroupName :: Lens' UpdateGameServer Text
ugsGameServerGroupName = lens _ugsGameServerGroupName (\s a -> s {_ugsGameServerGroupName = a})

-- | A custom string that uniquely identifies the game server to update.
ugsGameServerId :: Lens' UpdateGameServer Text
ugsGameServerId = lens _ugsGameServerId (\s a -> s {_ugsGameServerId = a})

instance AWSRequest UpdateGameServer where
  type Rs UpdateGameServer = UpdateGameServerResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          UpdateGameServerResponse'
            <$> (x .?> "GameServer") <*> (pure (fromEnum s))
      )

instance Hashable UpdateGameServer

instance NFData UpdateGameServer

instance ToHeaders UpdateGameServer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.UpdateGameServer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateGameServer where
  toJSON UpdateGameServer' {..} =
    object
      ( catMaybes
          [ ("HealthCheck" .=) <$> _ugsHealthCheck,
            ("GameServerData" .=) <$> _ugsGameServerData,
            ("UtilizationStatus" .=) <$> _ugsUtilizationStatus,
            Just ("GameServerGroupName" .= _ugsGameServerGroupName),
            Just ("GameServerId" .= _ugsGameServerId)
          ]
      )

instance ToPath UpdateGameServer where
  toPath = const "/"

instance ToQuery UpdateGameServer where
  toQuery = const mempty

-- | /See:/ 'updateGameServerResponse' smart constructor.
data UpdateGameServerResponse = UpdateGameServerResponse'
  { _ursGameServer ::
      !(Maybe GameServer),
    _ursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursGameServer' - Object that describes the newly updated game server.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateGameServerResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UpdateGameServerResponse
updateGameServerResponse pResponseStatus_ =
  UpdateGameServerResponse'
    { _ursGameServer = Nothing,
      _ursResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly updated game server.
ursGameServer :: Lens' UpdateGameServerResponse (Maybe GameServer)
ursGameServer = lens _ursGameServer (\s a -> s {_ursGameServer = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateGameServerResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UpdateGameServerResponse
