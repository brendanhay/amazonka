{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Removes the game server from a game server group. As a result of this operation, the deregistered game server can no longer be claimed and will not be returned in a list of active game servers.
-- To deregister a game server, specify the game server group and game server ID. If successful, this operation emits a CloudWatch event with termination timestamp and reason.
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
module Network.AWS.GameLift.DeregisterGameServer
  ( -- * Creating a request
    DeregisterGameServer (..),
    mkDeregisterGameServer,

    -- ** Request lenses
    dgsGameServerGroupName,
    dgsGameServerId,

    -- * Destructuring the response
    DeregisterGameServerResponse (..),
    mkDeregisterGameServerResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
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

-- | Creates a value of 'DeregisterGameServer' with the minimum fields required to make a request.
--
-- * 'gameServerGroupName' - A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
-- * 'gameServerId' - A custom string that uniquely identifies the game server to deregister.
mkDeregisterGameServer ::
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'gameServerId'
  Lude.Text ->
  DeregisterGameServer
mkDeregisterGameServer pGameServerGroupName_ pGameServerId_ =
  DeregisterGameServer'
    { gameServerGroupName =
        pGameServerGroupName_,
      gameServerId = pGameServerId_
    }

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameServerGroupName :: Lens.Lens' DeregisterGameServer Lude.Text
dgsGameServerGroupName = Lens.lens (gameServerGroupName :: DeregisterGameServer -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: DeregisterGameServer)
{-# DEPRECATED dgsGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | A custom string that uniquely identifies the game server to deregister.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameServerId :: Lens.Lens' DeregisterGameServer Lude.Text
dgsGameServerId = Lens.lens (gameServerId :: DeregisterGameServer -> Lude.Text) (\s a -> s {gameServerId = a} :: DeregisterGameServer)
{-# DEPRECATED dgsGameServerId "Use generic-lens or generic-optics with 'gameServerId' instead." #-}

instance Lude.AWSRequest DeregisterGameServer where
  type Rs DeregisterGameServer = DeregisterGameServerResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeregisterGameServerResponse'

instance Lude.ToHeaders DeregisterGameServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeregisterGameServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterGameServer where
  toJSON DeregisterGameServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("GameServerId" Lude..= gameServerId)
          ]
      )

instance Lude.ToPath DeregisterGameServer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterGameServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterGameServerResponse' with the minimum fields required to make a request.
mkDeregisterGameServerResponse ::
  DeregisterGameServerResponse
mkDeregisterGameServerResponse = DeregisterGameServerResponse'
