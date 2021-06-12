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
-- Module      : Network.AWS.GameLift.UpdateGameServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Updates information about a registered game server to help GameLift
-- FleetIQ to track game server availability. This operation is called by a
-- game server process that is running on an instance in a game server
-- group.
--
-- Use this operation to update the following types of game server
-- information. You can make all three types of updates in the same
-- request:
--
-- -   To update the game server\'s utilization status, identify the game
--     server and game server group and specify the current utilization
--     status. Use this status to identify when game servers are currently
--     hosting games and when they are available to be claimed.
--
-- -   To report health status, identify the game server and game server
--     group and set health check to @HEALTHY@. If a game server does not
--     report health status for a certain length of time, the game server
--     is no longer considered healthy. As a result, it will be eventually
--     deregistered from the game server group to avoid affecting
--     utilization metrics. The best practice is to report health every 60
--     seconds.
--
-- -   To change game server metadata, provide updated game server data.
--
-- Once a game server is successfully updated, the relevant statuses and
-- timestamps are updated.
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
module Network.AWS.GameLift.UpdateGameServer
  ( -- * Creating a Request
    UpdateGameServer (..),
    newUpdateGameServer,

    -- * Request Lenses
    updateGameServer_utilizationStatus,
    updateGameServer_gameServerData,
    updateGameServer_healthCheck,
    updateGameServer_gameServerGroupName,
    updateGameServer_gameServerId,

    -- * Destructuring the Response
    UpdateGameServerResponse (..),
    newUpdateGameServerResponse,

    -- * Response Lenses
    updateGameServerResponse_gameServer,
    updateGameServerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGameServer' smart constructor.
data UpdateGameServer = UpdateGameServer'
  { -- | Indicates whether the game server is available or is currently hosting
    -- gameplay.
    utilizationStatus :: Core.Maybe GameServerUtilizationStatus,
    -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers using ListGameServers or ClaimGameServer.
    gameServerData :: Core.Maybe Core.Text,
    -- | Indicates health status of the game server. A request that includes this
    -- parameter updates the game server\'s /LastHealthCheckTime/ timestamp.
    healthCheck :: Core.Maybe GameServerHealthCheck,
    -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Core.Text,
    -- | A custom string that uniquely identifies the game server to update.
    gameServerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'utilizationStatus', 'updateGameServer_utilizationStatus' - Indicates whether the game server is available or is currently hosting
-- gameplay.
--
-- 'gameServerData', 'updateGameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
--
-- 'healthCheck', 'updateGameServer_healthCheck' - Indicates health status of the game server. A request that includes this
-- parameter updates the game server\'s /LastHealthCheckTime/ timestamp.
--
-- 'gameServerGroupName', 'updateGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'gameServerId', 'updateGameServer_gameServerId' - A custom string that uniquely identifies the game server to update.
newUpdateGameServer ::
  -- | 'gameServerGroupName'
  Core.Text ->
  -- | 'gameServerId'
  Core.Text ->
  UpdateGameServer
newUpdateGameServer
  pGameServerGroupName_
  pGameServerId_ =
    UpdateGameServer'
      { utilizationStatus = Core.Nothing,
        gameServerData = Core.Nothing,
        healthCheck = Core.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        gameServerId = pGameServerId_
      }

-- | Indicates whether the game server is available or is currently hosting
-- gameplay.
updateGameServer_utilizationStatus :: Lens.Lens' UpdateGameServer (Core.Maybe GameServerUtilizationStatus)
updateGameServer_utilizationStatus = Lens.lens (\UpdateGameServer' {utilizationStatus} -> utilizationStatus) (\s@UpdateGameServer' {} a -> s {utilizationStatus = a} :: UpdateGameServer)

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
updateGameServer_gameServerData :: Lens.Lens' UpdateGameServer (Core.Maybe Core.Text)
updateGameServer_gameServerData = Lens.lens (\UpdateGameServer' {gameServerData} -> gameServerData) (\s@UpdateGameServer' {} a -> s {gameServerData = a} :: UpdateGameServer)

-- | Indicates health status of the game server. A request that includes this
-- parameter updates the game server\'s /LastHealthCheckTime/ timestamp.
updateGameServer_healthCheck :: Lens.Lens' UpdateGameServer (Core.Maybe GameServerHealthCheck)
updateGameServer_healthCheck = Lens.lens (\UpdateGameServer' {healthCheck} -> healthCheck) (\s@UpdateGameServer' {} a -> s {healthCheck = a} :: UpdateGameServer)

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
updateGameServer_gameServerGroupName :: Lens.Lens' UpdateGameServer Core.Text
updateGameServer_gameServerGroupName = Lens.lens (\UpdateGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@UpdateGameServer' {} a -> s {gameServerGroupName = a} :: UpdateGameServer)

-- | A custom string that uniquely identifies the game server to update.
updateGameServer_gameServerId :: Lens.Lens' UpdateGameServer Core.Text
updateGameServer_gameServerId = Lens.lens (\UpdateGameServer' {gameServerId} -> gameServerId) (\s@UpdateGameServer' {} a -> s {gameServerId = a} :: UpdateGameServer)

instance Core.AWSRequest UpdateGameServer where
  type
    AWSResponse UpdateGameServer =
      UpdateGameServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameServerResponse'
            Core.<$> (x Core..?> "GameServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGameServer

instance Core.NFData UpdateGameServer

instance Core.ToHeaders UpdateGameServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.UpdateGameServer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGameServer where
  toJSON UpdateGameServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UtilizationStatus" Core..=)
              Core.<$> utilizationStatus,
            ("GameServerData" Core..=) Core.<$> gameServerData,
            ("HealthCheck" Core..=) Core.<$> healthCheck,
            Core.Just
              ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("GameServerId" Core..= gameServerId)
          ]
      )

instance Core.ToPath UpdateGameServer where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGameServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGameServerResponse' smart constructor.
data UpdateGameServerResponse = UpdateGameServerResponse'
  { -- | Object that describes the newly updated game server.
    gameServer :: Core.Maybe GameServer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServer', 'updateGameServerResponse_gameServer' - Object that describes the newly updated game server.
--
-- 'httpStatus', 'updateGameServerResponse_httpStatus' - The response's http status code.
newUpdateGameServerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGameServerResponse
newUpdateGameServerResponse pHttpStatus_ =
  UpdateGameServerResponse'
    { gameServer =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly updated game server.
updateGameServerResponse_gameServer :: Lens.Lens' UpdateGameServerResponse (Core.Maybe GameServer)
updateGameServerResponse_gameServer = Lens.lens (\UpdateGameServerResponse' {gameServer} -> gameServer) (\s@UpdateGameServerResponse' {} a -> s {gameServer = a} :: UpdateGameServerResponse)

-- | The response's http status code.
updateGameServerResponse_httpStatus :: Lens.Lens' UpdateGameServerResponse Core.Int
updateGameServerResponse_httpStatus = Lens.lens (\UpdateGameServerResponse' {httpStatus} -> httpStatus) (\s@UpdateGameServerResponse' {} a -> s {httpStatus = a} :: UpdateGameServerResponse)

instance Core.NFData UpdateGameServerResponse
