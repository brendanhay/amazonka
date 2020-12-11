{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Creates a new game server resource and notifies GameLift FleetIQ that the game server is ready to host gameplay and players. This operation is called by a game server process that is running on an instance in a game server group. Registering game servers enables GameLift FleetIQ to track available game servers and enables game clients and services to claim a game server for a new game session.
-- To register a game server, identify the game server group and instance where the game server is running, and provide a unique identifier for the game server. You can also include connection and game server data. When a game client or service requests a game server by calling 'ClaimGameServer' , this information is returned in the response.
-- Once a game server is successfully registered, it is put in status @AVAILABLE@ . A request to register a game server may fail if the instance it is running on is in the process of shutting down as part of instance balancing or scale-down activity.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
-- __Related operations__
--
--     * 'RegisterGameServer'
--
--
--     * 'ListGameServers'
--
--
--     * 'ClaimGameServer'
--
--
--     * 'DescribeGameServer'
--
--
--     * 'UpdateGameServer'
--
--
--     * 'DeregisterGameServer'
module Network.AWS.GameLift.RegisterGameServer
  ( -- * Creating a request
    RegisterGameServer (..),
    mkRegisterGameServer,

    -- ** Request lenses
    rgsGameServerData,
    rgsConnectionInfo,
    rgsGameServerGroupName,
    rgsGameServerId,
    rgsInstanceId,

    -- * Destructuring the response
    RegisterGameServerResponse (..),
    mkRegisterGameServerResponse,

    -- ** Response lenses
    rgsrsGameServer,
    rgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'
  { gameServerData ::
      Lude.Maybe Lude.Text,
    connectionInfo :: Lude.Maybe Lude.Text,
    gameServerGroupName :: Lude.Text,
    gameServerId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterGameServer' with the minimum fields required to make a request.
--
-- * 'connectionInfo' - Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
-- * 'gameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerId' - A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
-- * 'instanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
mkRegisterGameServer ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'gameServerId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  RegisterGameServer
mkRegisterGameServer
  pGameServerGroupName_
  pGameServerId_
  pInstanceId_ =
    RegisterGameServer'
      { gameServerData = Lude.Nothing,
        connectionInfo = Lude.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        gameServerId = pGameServerId_,
        instanceId = pInstanceId_
      }

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerData :: Lens.Lens' RegisterGameServer (Lude.Maybe Lude.Text)
rgsGameServerData = Lens.lens (gameServerData :: RegisterGameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerData = a} :: RegisterGameServer)
{-# DEPRECATED rgsGameServerData "Use generic-lens or generic-optics with 'gameServerData' instead." #-}

-- | Information that is needed to make inbound client connections to the game server. This might include the IP address and port, DNS name, and other information.
--
-- /Note:/ Consider using 'connectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsConnectionInfo :: Lens.Lens' RegisterGameServer (Lude.Maybe Lude.Text)
rgsConnectionInfo = Lens.lens (connectionInfo :: RegisterGameServer -> Lude.Maybe Lude.Text) (\s a -> s {connectionInfo = a} :: RegisterGameServer)
{-# DEPRECATED rgsConnectionInfo "Use generic-lens or generic-optics with 'connectionInfo' instead." #-}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerGroupName :: Lens.Lens' RegisterGameServer Lude.Text
rgsGameServerGroupName = Lens.lens (gameServerGroupName :: RegisterGameServer -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: RegisterGameServer)
{-# DEPRECATED rgsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server to register. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsGameServerId :: Lens.Lens' RegisterGameServer Lude.Text
rgsGameServerId = Lens.lens (gameServerId :: RegisterGameServer -> Lude.Text) (\s a -> s {gameServerId = a} :: RegisterGameServer)
{-# DEPRECATED rgsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsInstanceId :: Lens.Lens' RegisterGameServer Lude.Text
rgsInstanceId = Lens.lens (instanceId :: RegisterGameServer -> Lude.Text) (\s a -> s {instanceId = a} :: RegisterGameServer)
{-# DEPRECATED rgsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest RegisterGameServer where
  type Rs RegisterGameServer = RegisterGameServerResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterGameServerResponse'
            Lude.<$> (x Lude..?> "GameServer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterGameServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.RegisterGameServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterGameServer where
  toJSON RegisterGameServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GameServerData" Lude..=) Lude.<$> gameServerData,
            ("ConnectionInfo" Lude..=) Lude.<$> connectionInfo,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("GameServerId" Lude..= gameServerId),
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath RegisterGameServer where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterGameServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'
  { gameServer ::
      Lude.Maybe GameServer,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterGameServerResponse' with the minimum fields required to make a request.
--
-- * 'gameServer' - Object that describes the newly registered game server.
-- * 'responseStatus' - The response status code.
mkRegisterGameServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterGameServerResponse
mkRegisterGameServerResponse pResponseStatus_ =
  RegisterGameServerResponse'
    { gameServer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly registered game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsrsGameServer :: Lens.Lens' RegisterGameServerResponse (Lude.Maybe GameServer)
rgsrsGameServer = Lens.lens (gameServer :: RegisterGameServerResponse -> Lude.Maybe GameServer) (\s a -> s {gameServer = a} :: RegisterGameServerResponse)
{-# DEPRECATED rgsrsGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsrsResponseStatus :: Lens.Lens' RegisterGameServerResponse Lude.Int
rgsrsResponseStatus = Lens.lens (responseStatus :: RegisterGameServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterGameServerResponse)
{-# DEPRECATED rgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
