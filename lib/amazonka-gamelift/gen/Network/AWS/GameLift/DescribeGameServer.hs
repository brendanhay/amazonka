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
-- Module      : Network.AWS.GameLift.DescribeGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Retrieves information for a registered game server. Information includes game server status, health check info, and the instance that the game server is running on.
--
-- To retrieve game server information, specify the game server ID. If successful, the requested game server object is returned.
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
module Network.AWS.GameLift.DescribeGameServer
  ( -- * Creating a Request
    describeGameServer,
    DescribeGameServer,

    -- * Request Lenses
    dGameServerGroupName,
    dGameServerId,

    -- * Destructuring the Response
    describeGameServerResponse,
    DescribeGameServerResponse,

    -- * Response Lenses
    ddrsGameServer,
    ddrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGameServer' smart constructor.
data DescribeGameServer = DescribeGameServer'
  { _dGameServerGroupName ::
      !Text,
    _dGameServerId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'dGameServerId' - A custom string that uniquely identifies the game server information to be retrieved.
describeGameServer ::
  -- | 'dGameServerGroupName'
  Text ->
  -- | 'dGameServerId'
  Text ->
  DescribeGameServer
describeGameServer pGameServerGroupName_ pGameServerId_ =
  DescribeGameServer'
    { _dGameServerGroupName =
        pGameServerGroupName_,
      _dGameServerId = pGameServerId_
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
dGameServerGroupName :: Lens' DescribeGameServer Text
dGameServerGroupName = lens _dGameServerGroupName (\s a -> s {_dGameServerGroupName = a})

-- | A custom string that uniquely identifies the game server information to be retrieved.
dGameServerId :: Lens' DescribeGameServer Text
dGameServerId = lens _dGameServerId (\s a -> s {_dGameServerId = a})

instance AWSRequest DescribeGameServer where
  type Rs DescribeGameServer = DescribeGameServerResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeGameServerResponse'
            <$> (x .?> "GameServer") <*> (pure (fromEnum s))
      )

instance Hashable DescribeGameServer

instance NFData DescribeGameServer

instance ToHeaders DescribeGameServer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DescribeGameServer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeGameServer where
  toJSON DescribeGameServer' {..} =
    object
      ( catMaybes
          [ Just ("GameServerGroupName" .= _dGameServerGroupName),
            Just ("GameServerId" .= _dGameServerId)
          ]
      )

instance ToPath DescribeGameServer where
  toPath = const "/"

instance ToQuery DescribeGameServer where
  toQuery = const mempty

-- | /See:/ 'describeGameServerResponse' smart constructor.
data DescribeGameServerResponse = DescribeGameServerResponse'
  { _ddrsGameServer ::
      !(Maybe GameServer),
    _ddrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsGameServer' - Object that describes the requested game server.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeGameServerResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DescribeGameServerResponse
describeGameServerResponse pResponseStatus_ =
  DescribeGameServerResponse'
    { _ddrsGameServer = Nothing,
      _ddrsResponseStatus = pResponseStatus_
    }

-- | Object that describes the requested game server.
ddrsGameServer :: Lens' DescribeGameServerResponse (Maybe GameServer)
ddrsGameServer = lens _ddrsGameServer (\s a -> s {_ddrsGameServer = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeGameServerResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DescribeGameServerResponse
