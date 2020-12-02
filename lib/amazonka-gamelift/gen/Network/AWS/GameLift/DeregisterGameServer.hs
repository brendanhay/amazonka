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
-- Module      : Network.AWS.GameLift.DeregisterGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Removes the game server from a game server group. As a result of this operation, the deregistered game server can no longer be claimed and will not be returned in a list of active game servers.
--
-- To deregister a game server, specify the game server group and game server ID. If successful, this operation emits a CloudWatch event with termination timestamp and reason.
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
module Network.AWS.GameLift.DeregisterGameServer
  ( -- * Creating a Request
    deregisterGameServer,
    DeregisterGameServer,

    -- * Request Lenses
    dgsGameServerGroupName,
    dgsGameServerId,

    -- * Destructuring the Response
    deregisterGameServerResponse,
    DeregisterGameServerResponse,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
  { _dgsGameServerGroupName ::
      !Text,
    _dgsGameServerId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsGameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'dgsGameServerId' - A custom string that uniquely identifies the game server to deregister.
deregisterGameServer ::
  -- | 'dgsGameServerGroupName'
  Text ->
  -- | 'dgsGameServerId'
  Text ->
  DeregisterGameServer
deregisterGameServer pGameServerGroupName_ pGameServerId_ =
  DeregisterGameServer'
    { _dgsGameServerGroupName =
        pGameServerGroupName_,
      _dgsGameServerId = pGameServerId_
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
dgsGameServerGroupName :: Lens' DeregisterGameServer Text
dgsGameServerGroupName = lens _dgsGameServerGroupName (\s a -> s {_dgsGameServerGroupName = a})

-- | A custom string that uniquely identifies the game server to deregister.
dgsGameServerId :: Lens' DeregisterGameServer Text
dgsGameServerId = lens _dgsGameServerId (\s a -> s {_dgsGameServerId = a})

instance AWSRequest DeregisterGameServer where
  type Rs DeregisterGameServer = DeregisterGameServerResponse
  request = postJSON gameLift
  response = receiveNull DeregisterGameServerResponse'

instance Hashable DeregisterGameServer

instance NFData DeregisterGameServer

instance ToHeaders DeregisterGameServer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DeregisterGameServer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeregisterGameServer where
  toJSON DeregisterGameServer' {..} =
    object
      ( catMaybes
          [ Just ("GameServerGroupName" .= _dgsGameServerGroupName),
            Just ("GameServerId" .= _dgsGameServerId)
          ]
      )

instance ToPath DeregisterGameServer where
  toPath = const "/"

instance ToQuery DeregisterGameServer where
  toQuery = const mempty

-- | /See:/ 'deregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterGameServerResponse' with the minimum fields required to make a request.
deregisterGameServerResponse ::
  DeregisterGameServerResponse
deregisterGameServerResponse = DeregisterGameServerResponse'

instance NFData DeregisterGameServerResponse
