{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Updates information about a registered game server to help GameLift FleetIQ to track game server availability. This operation is called by a game server process that is running on an instance in a game server group.
-- Use this operation to update the following types of game server information. You can make all three types of updates in the same request:
--
--     * To update the game server's utilization status, identify the game server and game server group and specify the current utilization status. Use this status to identify when game servers are currently hosting games and when they are available to be claimed.
--
--
--     * To report health status, identify the game server and game server group and set health check to @HEALTHY@ . If a game server does not report health status for a certain length of time, the game server is no longer considered healthy. As a result, it will be eventually deregistered from the game server group to avoid affecting utilization metrics. The best practice is to report health every 60 seconds.
--
--
--     * To change game server metadata, provide updated game server data.
--
--
-- Once a game server is successfully updated, the relevant statuses and timestamps are updated.
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
module Network.AWS.GameLift.UpdateGameServer
  ( -- * Creating a request
    UpdateGameServer (..),
    mkUpdateGameServer,

    -- ** Request lenses
    ugsHealthCheck,
    ugsGameServerData,
    ugsUtilizationStatus,
    ugsGameServerGroupName,
    ugsGameServerId,

    -- * Destructuring the response
    UpdateGameServerResponse (..),
    mkUpdateGameServerResponse,

    -- ** Response lenses
    ursGameServer,
    ursResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGameServer' smart constructor.
data UpdateGameServer = UpdateGameServer'
  { healthCheck ::
      Lude.Maybe GameServerHealthCheck,
    gameServerData :: Lude.Maybe Lude.Text,
    utilizationStatus ::
      Lude.Maybe GameServerUtilizationStatus,
    gameServerGroupName :: Lude.Text,
    gameServerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameServer' with the minimum fields required to make a request.
--
-- * 'gameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerId' - A custom string that uniquely identifies the game server to update.
-- * 'healthCheck' - Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
-- * 'utilizationStatus' - Indicates whether the game server is available or is currently hosting gameplay.
mkUpdateGameServer ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'gameServerId'
  Lude.Text ->
  UpdateGameServer
mkUpdateGameServer pGameServerGroupName_ pGameServerId_ =
  UpdateGameServer'
    { healthCheck = Lude.Nothing,
      gameServerData = Lude.Nothing,
      utilizationStatus = Lude.Nothing,
      gameServerGroupName = pGameServerGroupName_,
      gameServerId = pGameServerId_
    }

-- | Indicates health status of the game server. A request that includes this parameter updates the game server's /LastHealthCheckTime/ timestamp.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsHealthCheck :: Lens.Lens' UpdateGameServer (Lude.Maybe GameServerHealthCheck)
ugsHealthCheck = Lens.lens (healthCheck :: UpdateGameServer -> Lude.Maybe GameServerHealthCheck) (\s a -> s {healthCheck = a} :: UpdateGameServer)
{-# DEPRECATED ugsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerData :: Lens.Lens' UpdateGameServer (Lude.Maybe Lude.Text)
ugsGameServerData = Lens.lens (gameServerData :: UpdateGameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerData = a} :: UpdateGameServer)
{-# DEPRECATED ugsGameServerData "Use generic-lens or generic-optics with 'gameServerData' instead." #-}

-- | Indicates whether the game server is available or is currently hosting gameplay.
--
-- /Note:/ Consider using 'utilizationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsUtilizationStatus :: Lens.Lens' UpdateGameServer (Lude.Maybe GameServerUtilizationStatus)
ugsUtilizationStatus = Lens.lens (utilizationStatus :: UpdateGameServer -> Lude.Maybe GameServerUtilizationStatus) (\s a -> s {utilizationStatus = a} :: UpdateGameServer)
{-# DEPRECATED ugsUtilizationStatus "Use generic-lens or generic-optics with 'utilizationStatus' instead." #-}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerGroupName :: Lens.Lens' UpdateGameServer Lude.Text
ugsGameServerGroupName = Lens.lens (gameServerGroupName :: UpdateGameServer -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: UpdateGameServer)
{-# DEPRECATED ugsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server to update.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameServerId :: Lens.Lens' UpdateGameServer Lude.Text
ugsGameServerId = Lens.lens (gameServerId :: UpdateGameServer -> Lude.Text) (\s a -> s {gameServerId = a} :: UpdateGameServer)
{-# DEPRECATED ugsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

instance Lude.AWSRequest UpdateGameServer where
  type Rs UpdateGameServer = UpdateGameServerResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGameServerResponse'
            Lude.<$> (x Lude..?> "GameServer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGameServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateGameServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGameServer where
  toJSON UpdateGameServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HealthCheck" Lude..=) Lude.<$> healthCheck,
            ("GameServerData" Lude..=) Lude.<$> gameServerData,
            ("UtilizationStatus" Lude..=) Lude.<$> utilizationStatus,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("GameServerId" Lude..= gameServerId)
          ]
      )

instance Lude.ToPath UpdateGameServer where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGameServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGameServerResponse' smart constructor.
data UpdateGameServerResponse = UpdateGameServerResponse'
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

-- | Creates a value of 'UpdateGameServerResponse' with the minimum fields required to make a request.
--
-- * 'gameServer' - Object that describes the newly updated game server.
-- * 'responseStatus' - The response status code.
mkUpdateGameServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGameServerResponse
mkUpdateGameServerResponse pResponseStatus_ =
  UpdateGameServerResponse'
    { gameServer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly updated game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursGameServer :: Lens.Lens' UpdateGameServerResponse (Lude.Maybe GameServer)
ursGameServer = Lens.lens (gameServer :: UpdateGameServerResponse -> Lude.Maybe GameServer) (\s a -> s {gameServer = a} :: UpdateGameServerResponse)
{-# DEPRECATED ursGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateGameServerResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateGameServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGameServerResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
