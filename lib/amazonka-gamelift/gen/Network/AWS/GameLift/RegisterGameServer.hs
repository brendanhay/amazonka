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
-- Module      : Network.AWS.GameLift.RegisterGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Creates a new game server resource and notifies GameLift FleetIQ that the game server is ready to host gameplay and players. This operation is called by a game server process that is running on an instance in a game server group. Registering game servers enables GameLift FleetIQ to track available game servers and enables game clients and services to claim a game server for a new game session.
--
-- To register a game server, identify the game server group and instance where the game server is running, and provide a unique identifier for the game server. You can also include connection and game server data. When a game client or service requests a game server by calling 'ClaimGameServer' , this information is returned in the response.
--
-- Once a game server is successfully registered, it is put in status @AVAILABLE@ . A request to register a game server may fail if the instance it is running on is in the process of shutting down as part of instance balancing or scale-down activity.
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
module Network.AWS.GameLift.RegisterGameServer
  ( -- * Creating a Request
    registerGameServer,
    RegisterGameServer,

    -- * Request Lenses
    rgsGameServerData,
    rgsConnectionInfo,
    rgsGameServerGroupName,
    rgsGameServerId,
    rgsInstanceId,

    -- * Destructuring the Response
    registerGameServerResponse,
    RegisterGameServerResponse,

    -- * Response Lenses
    rgsrsGameServer,
    rgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'
  { _rgsGameServerData ::
      !(Maybe Text),
    _rgsConnectionInfo :: !(Maybe Text),
    _rgsGameServerGroupName :: !Text,
    _rgsGameServerId :: !Text,
    _rgsInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- * 'rgsConnectionInfo' - Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
--
-- * 'rgsGameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'rgsGameServerId' - A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
--
-- * 'rgsInstanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
registerGameServer ::
  -- | 'rgsGameServerGroupName'
  Text ->
  -- | 'rgsGameServerId'
  Text ->
  -- | 'rgsInstanceId'
  Text ->
  RegisterGameServer
registerGameServer
  pGameServerGroupName_
  pGameServerId_
  pInstanceId_ =
    RegisterGameServer'
      { _rgsGameServerData = Nothing,
        _rgsConnectionInfo = Nothing,
        _rgsGameServerGroupName = pGameServerGroupName_,
        _rgsGameServerId = pGameServerId_,
        _rgsInstanceId = pInstanceId_
      }

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
rgsGameServerData :: Lens' RegisterGameServer (Maybe Text)
rgsGameServerData = lens _rgsGameServerData (\s a -> s {_rgsGameServerData = a})

-- | Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
rgsConnectionInfo :: Lens' RegisterGameServer (Maybe Text)
rgsConnectionInfo = lens _rgsConnectionInfo (\s a -> s {_rgsConnectionInfo = a})

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
rgsGameServerGroupName :: Lens' RegisterGameServer Text
rgsGameServerGroupName = lens _rgsGameServerGroupName (\s a -> s {_rgsGameServerGroupName = a})

-- | A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
rgsGameServerId :: Lens' RegisterGameServer Text
rgsGameServerId = lens _rgsGameServerId (\s a -> s {_rgsGameServerId = a})

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
rgsInstanceId :: Lens' RegisterGameServer Text
rgsInstanceId = lens _rgsInstanceId (\s a -> s {_rgsInstanceId = a})

instance AWSRequest RegisterGameServer where
  type Rs RegisterGameServer = RegisterGameServerResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          RegisterGameServerResponse'
            <$> (x .?> "GameServer") <*> (pure (fromEnum s))
      )

instance Hashable RegisterGameServer

instance NFData RegisterGameServer

instance ToHeaders RegisterGameServer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.RegisterGameServer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterGameServer where
  toJSON RegisterGameServer' {..} =
    object
      ( catMaybes
          [ ("GameServerData" .=) <$> _rgsGameServerData,
            ("ConnectionInfo" .=) <$> _rgsConnectionInfo,
            Just ("GameServerGroupName" .= _rgsGameServerGroupName),
            Just ("GameServerId" .= _rgsGameServerId),
            Just ("InstanceId" .= _rgsInstanceId)
          ]
      )

instance ToPath RegisterGameServer where
  toPath = const "/"

instance ToQuery RegisterGameServer where
  toQuery = const mempty

-- | /See:/ 'registerGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'
  { _rgsrsGameServer ::
      !(Maybe GameServer),
    _rgsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsrsGameServer' - Object that describes the newly registered game server.
--
-- * 'rgsrsResponseStatus' - -- | The response status code.
registerGameServerResponse ::
  -- | 'rgsrsResponseStatus'
  Int ->
  RegisterGameServerResponse
registerGameServerResponse pResponseStatus_ =
  RegisterGameServerResponse'
    { _rgsrsGameServer = Nothing,
      _rgsrsResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly registered game server.
rgsrsGameServer :: Lens' RegisterGameServerResponse (Maybe GameServer)
rgsrsGameServer = lens _rgsrsGameServer (\s a -> s {_rgsrsGameServer = a})

-- | -- | The response status code.
rgsrsResponseStatus :: Lens' RegisterGameServerResponse Int
rgsrsResponseStatus = lens _rgsrsResponseStatus (\s a -> s {_rgsrsResponseStatus = a})

instance NFData RegisterGameServerResponse
