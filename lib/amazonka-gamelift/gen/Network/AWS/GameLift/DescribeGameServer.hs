{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Retrieves information for a registered game server. Information includes game server status, health check info, and the instance that the game server is running on.
-- To retrieve game server information, specify the game server ID. If successful, the requested game server object is returned.
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
module Network.AWS.GameLift.DescribeGameServer
  ( -- * Creating a request
    DescribeGameServer (..),
    mkDescribeGameServer,

    -- ** Request lenses
    dGameServerGroupName,
    dGameServerId,

    -- * Destructuring the response
    DescribeGameServerResponse (..),
    mkDescribeGameServerResponse,

    -- ** Response lenses
    ddrsGameServer,
    ddrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGameServer' smart constructor.
data DescribeGameServer = DescribeGameServer'
  { gameServerGroupName ::
      Lude.Text,
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

-- | Creates a value of 'DescribeGameServer' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerId' - A custom string that uniquely identifies the game server information to be retrieved.
mkDescribeGameServer ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'gameServerId'
  Lude.Text ->
  DescribeGameServer
mkDescribeGameServer pGameServerGroupName_ pGameServerId_ =
  DescribeGameServer'
    { gameServerGroupName = pGameServerGroupName_,
      gameServerId = pGameServerId_
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGameServerGroupName :: Lens.Lens' DescribeGameServer Lude.Text
dGameServerGroupName = Lens.lens (gameServerGroupName :: DescribeGameServer -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: DescribeGameServer)
{-# DEPRECATED dGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server information to be retrieved.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGameServerId :: Lens.Lens' DescribeGameServer Lude.Text
dGameServerId = Lens.lens (gameServerId :: DescribeGameServer -> Lude.Text) (\s a -> s {gameServerId = a} :: DescribeGameServer)
{-# DEPRECATED dGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

instance Lude.AWSRequest DescribeGameServer where
  type Rs DescribeGameServer = DescribeGameServerResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameServerResponse'
            Lude.<$> (x Lude..?> "GameServer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameServer where
  toJSON DescribeGameServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("GameServerId" Lude..= gameServerId)
          ]
      )

instance Lude.ToPath DescribeGameServer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGameServerResponse' smart constructor.
data DescribeGameServerResponse = DescribeGameServerResponse'
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

-- | Creates a value of 'DescribeGameServerResponse' with the minimum fields required to make a request.
--
-- * 'gameServer' - Object that describes the requested game server.
-- * 'responseStatus' - The response status code.
mkDescribeGameServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameServerResponse
mkDescribeGameServerResponse pResponseStatus_ =
  DescribeGameServerResponse'
    { gameServer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the requested game server.
--
-- /Note:/ Consider using 'gameServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsGameServer :: Lens.Lens' DescribeGameServerResponse (Lude.Maybe GameServer)
ddrsGameServer = Lens.lens (gameServer :: DescribeGameServerResponse -> Lude.Maybe GameServer) (\s a -> s {gameServer = a} :: DescribeGameServerResponse)
{-# DEPRECATED ddrsGameServer "Use generic-lens or generic-optics with 'gameServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeGameServerResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeGameServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameServerResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
