{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeregisterGameServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Removes the game server from a game server group. As a result of this
-- operation, the deregistered game server can no longer be claimed and
-- will not be returned in a list of active game servers.
--
-- To deregister a game server, specify the game server group and game
-- server ID. If successful, this operation emits a CloudWatch event with
-- termination timestamp and reason.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
-- -   RegisterGameServer
--
-- -   ListGameServers
--
-- -   ClaimGameServer
--
-- -   DescribeGameServer
--
-- -   UpdateGameServer
--
-- -   DeregisterGameServer
module Network.AWS.GameLift.DeregisterGameServer
  ( -- * Creating a Request
    DeregisterGameServer (..),
    newDeregisterGameServer,

    -- * Request Lenses
    deregisterGameServer_gameServerGroupName,
    deregisterGameServer_gameServerId,

    -- * Destructuring the Response
    DeregisterGameServerResponse (..),
    newDeregisterGameServerResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
  { -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Core.Text,
    -- | A custom string that uniquely identifies the game server to deregister.
    gameServerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'deregisterGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'gameServerId', 'deregisterGameServer_gameServerId' - A custom string that uniquely identifies the game server to deregister.
newDeregisterGameServer ::
  -- | 'gameServerGroupName'
  Core.Text ->
  -- | 'gameServerId'
  Core.Text ->
  DeregisterGameServer
newDeregisterGameServer
  pGameServerGroupName_
  pGameServerId_ =
    DeregisterGameServer'
      { gameServerGroupName =
          pGameServerGroupName_,
        gameServerId = pGameServerId_
      }

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
deregisterGameServer_gameServerGroupName :: Lens.Lens' DeregisterGameServer Core.Text
deregisterGameServer_gameServerGroupName = Lens.lens (\DeregisterGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@DeregisterGameServer' {} a -> s {gameServerGroupName = a} :: DeregisterGameServer)

-- | A custom string that uniquely identifies the game server to deregister.
deregisterGameServer_gameServerId :: Lens.Lens' DeregisterGameServer Core.Text
deregisterGameServer_gameServerId = Lens.lens (\DeregisterGameServer' {gameServerId} -> gameServerId) (\s@DeregisterGameServer' {} a -> s {gameServerId = a} :: DeregisterGameServer)

instance Core.AWSRequest DeregisterGameServer where
  type
    AWSResponse DeregisterGameServer =
      DeregisterGameServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeregisterGameServerResponse'

instance Core.Hashable DeregisterGameServer

instance Core.NFData DeregisterGameServer

instance Core.ToHeaders DeregisterGameServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DeregisterGameServer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterGameServer where
  toJSON DeregisterGameServer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("GameServerId" Core..= gameServerId)
          ]
      )

instance Core.ToPath DeregisterGameServer where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterGameServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterGameServerResponse ::
  DeregisterGameServerResponse
newDeregisterGameServerResponse =
  DeregisterGameServerResponse'

instance Core.NFData DeregisterGameServerResponse
