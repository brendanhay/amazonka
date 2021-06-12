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
-- Module      : Network.AWS.GameLift.RegisterGameServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Creates a new game server resource and notifies GameLift FleetIQ that
-- the game server is ready to host gameplay and players. This operation is
-- called by a game server process that is running on an instance in a game
-- server group. Registering game servers enables GameLift FleetIQ to track
-- available game servers and enables game clients and services to claim a
-- game server for a new game session.
--
-- To register a game server, identify the game server group and instance
-- where the game server is running, and provide a unique identifier for
-- the game server. You can also include connection and game server data.
-- When a game client or service requests a game server by calling
-- ClaimGameServer, this information is returned in the response.
--
-- Once a game server is successfully registered, it is put in status
-- @AVAILABLE@. A request to register a game server may fail if the
-- instance it is running on is in the process of shutting down as part of
-- instance balancing or scale-down activity.
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
module Network.AWS.GameLift.RegisterGameServer
  ( -- * Creating a Request
    RegisterGameServer (..),
    newRegisterGameServer,

    -- * Request Lenses
    registerGameServer_gameServerData,
    registerGameServer_connectionInfo,
    registerGameServer_gameServerGroupName,
    registerGameServer_gameServerId,
    registerGameServer_instanceId,

    -- * Destructuring the Response
    RegisterGameServerResponse (..),
    newRegisterGameServerResponse,

    -- * Response Lenses
    registerGameServerResponse_gameServer,
    registerGameServerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'
  { -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers using ListGameServers or ClaimGameServer.
    gameServerData :: Core.Maybe Core.Text,
    -- | Information that is needed to make inbound client connections to the
    -- game server. This might include the IP address and port, DNS name, and
    -- other information.
    connectionInfo :: Core.Maybe Core.Text,
    -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Core.Text,
    -- | A custom string that uniquely identifies the game server to register.
    -- Game server IDs are developer-defined and must be unique across all game
    -- server groups in your AWS account.
    gameServerId :: Core.Text,
    -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerData', 'registerGameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
--
-- 'connectionInfo', 'registerGameServer_connectionInfo' - Information that is needed to make inbound client connections to the
-- game server. This might include the IP address and port, DNS name, and
-- other information.
--
-- 'gameServerGroupName', 'registerGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'gameServerId', 'registerGameServer_gameServerId' - A custom string that uniquely identifies the game server to register.
-- Game server IDs are developer-defined and must be unique across all game
-- server groups in your AWS account.
--
-- 'instanceId', 'registerGameServer_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
newRegisterGameServer ::
  -- | 'gameServerGroupName'
  Core.Text ->
  -- | 'gameServerId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  RegisterGameServer
newRegisterGameServer
  pGameServerGroupName_
  pGameServerId_
  pInstanceId_ =
    RegisterGameServer'
      { gameServerData = Core.Nothing,
        connectionInfo = Core.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        gameServerId = pGameServerId_,
        instanceId = pInstanceId_
      }

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
registerGameServer_gameServerData :: Lens.Lens' RegisterGameServer (Core.Maybe Core.Text)
registerGameServer_gameServerData = Lens.lens (\RegisterGameServer' {gameServerData} -> gameServerData) (\s@RegisterGameServer' {} a -> s {gameServerData = a} :: RegisterGameServer)

-- | Information that is needed to make inbound client connections to the
-- game server. This might include the IP address and port, DNS name, and
-- other information.
registerGameServer_connectionInfo :: Lens.Lens' RegisterGameServer (Core.Maybe Core.Text)
registerGameServer_connectionInfo = Lens.lens (\RegisterGameServer' {connectionInfo} -> connectionInfo) (\s@RegisterGameServer' {} a -> s {connectionInfo = a} :: RegisterGameServer)

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
registerGameServer_gameServerGroupName :: Lens.Lens' RegisterGameServer Core.Text
registerGameServer_gameServerGroupName = Lens.lens (\RegisterGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@RegisterGameServer' {} a -> s {gameServerGroupName = a} :: RegisterGameServer)

-- | A custom string that uniquely identifies the game server to register.
-- Game server IDs are developer-defined and must be unique across all game
-- server groups in your AWS account.
registerGameServer_gameServerId :: Lens.Lens' RegisterGameServer Core.Text
registerGameServer_gameServerId = Lens.lens (\RegisterGameServer' {gameServerId} -> gameServerId) (\s@RegisterGameServer' {} a -> s {gameServerId = a} :: RegisterGameServer)

-- | The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
registerGameServer_instanceId :: Lens.Lens' RegisterGameServer Core.Text
registerGameServer_instanceId = Lens.lens (\RegisterGameServer' {instanceId} -> instanceId) (\s@RegisterGameServer' {} a -> s {instanceId = a} :: RegisterGameServer)

instance Core.AWSRequest RegisterGameServer where
  type
    AWSResponse RegisterGameServer =
      RegisterGameServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterGameServerResponse'
            Core.<$> (x Core..?> "GameServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterGameServer

instance Core.NFData RegisterGameServer

instance Core.ToHeaders RegisterGameServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.RegisterGameServer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterGameServer where
  toJSON RegisterGameServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GameServerData" Core..=) Core.<$> gameServerData,
            ("ConnectionInfo" Core..=) Core.<$> connectionInfo,
            Core.Just
              ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("GameServerId" Core..= gameServerId),
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath RegisterGameServer where
  toPath = Core.const "/"

instance Core.ToQuery RegisterGameServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'
  { -- | Object that describes the newly registered game server.
    gameServer :: Core.Maybe GameServer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServer', 'registerGameServerResponse_gameServer' - Object that describes the newly registered game server.
--
-- 'httpStatus', 'registerGameServerResponse_httpStatus' - The response's http status code.
newRegisterGameServerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterGameServerResponse
newRegisterGameServerResponse pHttpStatus_ =
  RegisterGameServerResponse'
    { gameServer =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly registered game server.
registerGameServerResponse_gameServer :: Lens.Lens' RegisterGameServerResponse (Core.Maybe GameServer)
registerGameServerResponse_gameServer = Lens.lens (\RegisterGameServerResponse' {gameServer} -> gameServer) (\s@RegisterGameServerResponse' {} a -> s {gameServer = a} :: RegisterGameServerResponse)

-- | The response's http status code.
registerGameServerResponse_httpStatus :: Lens.Lens' RegisterGameServerResponse Core.Int
registerGameServerResponse_httpStatus = Lens.lens (\RegisterGameServerResponse' {httpStatus} -> httpStatus) (\s@RegisterGameServerResponse' {} a -> s {httpStatus = a} :: RegisterGameServerResponse)

instance Core.NFData RegisterGameServerResponse
