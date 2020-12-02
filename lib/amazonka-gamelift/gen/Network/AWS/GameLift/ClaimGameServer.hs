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
-- Module      : Network.AWS.GameLift.ClaimGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Locates an available game server and temporarily reserves it to host gameplay and players. This operation is called from a game client or client service (such as a matchmaker) to request hosting resources for a new game session. In response, GameLift FleetIQ locates an available game server, places it in @CLAIMED@ status for 60 seconds, and returns connection information that players can use to connect to the game server.
--
-- To claim a game server, identify a game server group. You can also specify a game server ID, although this approach bypasses GameLift FleetIQ placement optimization. Optionally, include game data to pass to the game server at the start of a game session, such as a game map or player information.
--
-- When a game server is successfully claimed, connection information is returned. A claimed game server's utilization status remains @AVAILABLE@ while the claim status is set to @CLAIMED@ for up to 60 seconds. This time period gives the game server time to update its status to @UTILIZED@ (using 'UpdateGameServer' ) once players join. If the game server's status is not updated within 60 seconds, the game server reverts to unclaimed status and is available to be claimed by another request. The claim time period is a fixed value and is not configurable.
--
-- If you try to claim a specific game server, this request will fail in the following cases:
--
--     * If the game server utilization status is @UTILIZED@ .
--
--     * If the game server claim status is @CLAIMED@ .
--
--
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
module Network.AWS.GameLift.ClaimGameServer
  ( -- * Creating a Request
    claimGameServer,
    ClaimGameServer,

    -- * Request Lenses
    cgsGameServerData,
    cgsGameServerId,
    cgsGameServerGroupName,

    -- * Destructuring the Response
    claimGameServerResponse,
    ClaimGameServerResponse,

    -- * Response Lenses
    crsGameServer,
    crsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'claimGameServer' smart constructor.
data ClaimGameServer = ClaimGameServer'
  { _cgsGameServerData ::
      !(Maybe Text),
    _cgsGameServerId :: !(Maybe Text),
    _cgsGameServerGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClaimGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- * 'cgsGameServerId' - A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
--
-- * 'cgsGameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim.
claimGameServer ::
  -- | 'cgsGameServerGroupName'
  Text ->
  ClaimGameServer
claimGameServer pGameServerGroupName_ =
  ClaimGameServer'
    { _cgsGameServerData = Nothing,
      _cgsGameServerId = Nothing,
      _cgsGameServerGroupName = pGameServerGroupName_
    }

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
cgsGameServerData :: Lens' ClaimGameServer (Maybe Text)
cgsGameServerData = lens _cgsGameServerData (\s a -> s {_cgsGameServerData = a})

-- | A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
cgsGameServerId :: Lens' ClaimGameServer (Maybe Text)
cgsGameServerId = lens _cgsGameServerId (\s a -> s {_cgsGameServerId = a})

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim.
cgsGameServerGroupName :: Lens' ClaimGameServer Text
cgsGameServerGroupName = lens _cgsGameServerGroupName (\s a -> s {_cgsGameServerGroupName = a})

instance AWSRequest ClaimGameServer where
  type Rs ClaimGameServer = ClaimGameServerResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ClaimGameServerResponse'
            <$> (x .?> "GameServer") <*> (pure (fromEnum s))
      )

instance Hashable ClaimGameServer

instance NFData ClaimGameServer

instance ToHeaders ClaimGameServer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ClaimGameServer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ClaimGameServer where
  toJSON ClaimGameServer' {..} =
    object
      ( catMaybes
          [ ("GameServerData" .=) <$> _cgsGameServerData,
            ("GameServerId" .=) <$> _cgsGameServerId,
            Just ("GameServerGroupName" .= _cgsGameServerGroupName)
          ]
      )

instance ToPath ClaimGameServer where
  toPath = const "/"

instance ToQuery ClaimGameServer where
  toQuery = const mempty

-- | /See:/ 'claimGameServerResponse' smart constructor.
data ClaimGameServerResponse = ClaimGameServerResponse'
  { _crsGameServer ::
      !(Maybe GameServer),
    _crsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClaimGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsGameServer' - Object that describes the newly claimed game server.
--
-- * 'crsResponseStatus' - -- | The response status code.
claimGameServerResponse ::
  -- | 'crsResponseStatus'
  Int ->
  ClaimGameServerResponse
claimGameServerResponse pResponseStatus_ =
  ClaimGameServerResponse'
    { _crsGameServer = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly claimed game server.
crsGameServer :: Lens' ClaimGameServerResponse (Maybe GameServer)
crsGameServer = lens _crsGameServer (\s a -> s {_crsGameServer = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' ClaimGameServerResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData ClaimGameServerResponse
