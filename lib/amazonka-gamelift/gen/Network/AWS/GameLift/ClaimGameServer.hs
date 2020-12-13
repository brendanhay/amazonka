{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Locates an available game server and temporarily reserves it to host gameplay and players. This operation is called from a game client or client service (such as a matchmaker) to request hosting resources for a new game session. In response, GameLift FleetIQ locates an available game server, places it in @CLAIMED@ status for 60 seconds, and returns connection information that players can use to connect to the game server.
-- To claim a game server, identify a game server group. You can also specify a game server ID, although this approach bypasses GameLift FleetIQ placement optimization. Optionally, include game data to pass to the game server at the start of a game session, such as a game map or player information.
-- When a game server is successfully claimed, connection information is returned. A claimed game server's utilization status remains @AVAILABLE@ while the claim status is set to @CLAIMED@ for up to 60 seconds. This time period gives the game server time to update its status to @UTILIZED@ (using 'UpdateGameServer' ) once players join. If the game server's status is not updated within 60 seconds, the game server reverts to unclaimed status and is available to be claimed by another request. The claim time period is a fixed value and is not configurable.
-- If you try to claim a specific game server, this request will fail in the following cases:
--
--     * If the game server utilization status is @UTILIZED@ .
--
--
--     * If the game server claim status is @CLAIMED@ .
--
--
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
module Network.AWS.GameLift.ClaimGameServer
  ( -- * Creating a request
    ClaimGameServer (..),
    mkClaimGameServer,

    -- ** Request lenses
    cgsGameServerGroupName,
    cgsGameServerData,
    cgsGameServerId,

    -- * Destructuring the response
    ClaimGameServerResponse (..),
    mkClaimGameServerResponse,

    -- ** Response lenses
    crsGameServer,
    crsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkClaimGameServer' smart constructor.
data ClaimGameServer = ClaimGameServer'
  { -- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim.
    gameServerGroupName :: Lude.Text,
    -- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
    gameServerData :: Lude.Maybe Lude.Text,
    -- | A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
    gameServerId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClaimGameServer' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim.
-- * 'gameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
-- * 'gameServerId' - A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
mkClaimGameServer ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  ClaimGameServer
mkClaimGameServer pGameServerGroupName_ =
  ClaimGameServer'
    { gameServerGroupName = pGameServerGroupName_,
      gameServerData = Lude.Nothing,
      gameServerId = Lude.Nothing
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.. If you are not specifying a game server to claim, this value identifies where you want GameLift FleetIQ to look for an available game server to claim.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerGroupName :: Lens.Lens' ClaimGameServer Lude.Text
cgsGameServerGroupName = Lens.lens (gameServerGroupName :: ClaimGameServer -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: ClaimGameServer)
{-# DEPRECATED cgsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on game servers using 'ListGameServers' or 'ClaimGameServer' .
--
-- /Note:/ Consider using 'gameServerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerData :: Lens.Lens' ClaimGameServer (Lude.Maybe Lude.Text)
cgsGameServerData = Lens.lens (gameServerData :: ClaimGameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerData = a} :: ClaimGameServer)
{-# DEPRECATED cgsGameServerData "Use generic-lens or generic-optics with 'gameServerData' instead." #-}

-- | A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameServerId :: Lens.Lens' ClaimGameServer (Lude.Maybe Lude.Text)
cgsGameServerId = Lens.lens (gameServerId :: ClaimGameServer -> Lude.Maybe Lude.Text) (\s a -> s {gameServerId = a} :: ClaimGameServer)
{-# DEPRECATED cgsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

instance Lude.AWSRequest ClaimGameServer where
  type Rs ClaimGameServer = ClaimGameServerResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ClaimGameServerResponse'
            Lude.<$> (x Lude..?> "GameServer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ClaimGameServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ClaimGameServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ClaimGameServer where
  toJSON ClaimGameServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            ("GameServerData" Lude..=) Lude.<$> gameServerData,
            ("GameServerId" Lude..=) Lude.<$> gameServerId
          ]
      )

instance Lude.ToPath ClaimGameServer where
  toPath = Lude.const "/"

instance Lude.ToQuery ClaimGameServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkClaimGameServerResponse' smart constructor.
data ClaimGameServerResponse = ClaimGameServerResponse'
  { -- | Object that describes the newly claimed game server.
    gameServer :: Lude.Maybe GameServer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClaimGameServerResponse' with the minimum fields required to make a request.
--
-- * 'gameServer' - Object that describes the newly claimed game server.
-- * 'responseStatus' - The response status code.
mkClaimGameServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ClaimGameServerResponse
mkClaimGameServerResponse pResponseStatus_ =
  ClaimGameServerResponse'
    { gameServer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly claimed game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsGameServer :: Lens.Lens' ClaimGameServerResponse (Lude.Maybe GameServer)
crsGameServer = Lens.lens (gameServer :: ClaimGameServerResponse -> Lude.Maybe GameServer) (\s a -> s {gameServer = a} :: ClaimGameServerResponse)
{-# DEPRECATED crsGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ClaimGameServerResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: ClaimGameServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ClaimGameServerResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
