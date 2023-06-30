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
-- Module      : Amazonka.GameLift.RegisterGameServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
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
--
-- Once a game server is successfully registered, it is put in status
-- @AVAILABLE@. A request to register a game server may fail if the
-- instance it is running on is in the process of shutting down as part of
-- instance balancing or scale-down activity.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
module Amazonka.GameLift.RegisterGameServer
  ( -- * Creating a Request
    RegisterGameServer (..),
    newRegisterGameServer,

    -- * Request Lenses
    registerGameServer_connectionInfo,
    registerGameServer_gameServerData,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'
  { -- | Information that is needed to make inbound client connections to the
    -- game server. This might include the IP address and port, DNS name, and
    -- other information.
    connectionInfo :: Prelude.Maybe Prelude.Text,
    -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers.
    gameServerData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game server group where the game server is
    -- running.
    gameServerGroupName :: Prelude.Text,
    -- | A custom string that uniquely identifies the game server to register.
    -- Game server IDs are developer-defined and must be unique across all game
    -- server groups in your Amazon Web Services account.
    gameServerId :: Prelude.Text,
    -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionInfo', 'registerGameServer_connectionInfo' - Information that is needed to make inbound client connections to the
-- game server. This might include the IP address and port, DNS name, and
-- other information.
--
-- 'gameServerData', 'registerGameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers.
--
-- 'gameServerGroupName', 'registerGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running.
--
-- 'gameServerId', 'registerGameServer_gameServerId' - A custom string that uniquely identifies the game server to register.
-- Game server IDs are developer-defined and must be unique across all game
-- server groups in your Amazon Web Services account.
--
-- 'instanceId', 'registerGameServer_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
newRegisterGameServer ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  -- | 'gameServerId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  RegisterGameServer
newRegisterGameServer
  pGameServerGroupName_
  pGameServerId_
  pInstanceId_ =
    RegisterGameServer'
      { connectionInfo =
          Prelude.Nothing,
        gameServerData = Prelude.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        gameServerId = pGameServerId_,
        instanceId = pInstanceId_
      }

-- | Information that is needed to make inbound client connections to the
-- game server. This might include the IP address and port, DNS name, and
-- other information.
registerGameServer_connectionInfo :: Lens.Lens' RegisterGameServer (Prelude.Maybe Prelude.Text)
registerGameServer_connectionInfo = Lens.lens (\RegisterGameServer' {connectionInfo} -> connectionInfo) (\s@RegisterGameServer' {} a -> s {connectionInfo = a} :: RegisterGameServer)

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers.
registerGameServer_gameServerData :: Lens.Lens' RegisterGameServer (Prelude.Maybe Prelude.Text)
registerGameServer_gameServerData = Lens.lens (\RegisterGameServer' {gameServerData} -> gameServerData) (\s@RegisterGameServer' {} a -> s {gameServerData = a} :: RegisterGameServer)

-- | A unique identifier for the game server group where the game server is
-- running.
registerGameServer_gameServerGroupName :: Lens.Lens' RegisterGameServer Prelude.Text
registerGameServer_gameServerGroupName = Lens.lens (\RegisterGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@RegisterGameServer' {} a -> s {gameServerGroupName = a} :: RegisterGameServer)

-- | A custom string that uniquely identifies the game server to register.
-- Game server IDs are developer-defined and must be unique across all game
-- server groups in your Amazon Web Services account.
registerGameServer_gameServerId :: Lens.Lens' RegisterGameServer Prelude.Text
registerGameServer_gameServerId = Lens.lens (\RegisterGameServer' {gameServerId} -> gameServerId) (\s@RegisterGameServer' {} a -> s {gameServerId = a} :: RegisterGameServer)

-- | The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
registerGameServer_instanceId :: Lens.Lens' RegisterGameServer Prelude.Text
registerGameServer_instanceId = Lens.lens (\RegisterGameServer' {instanceId} -> instanceId) (\s@RegisterGameServer' {} a -> s {instanceId = a} :: RegisterGameServer)

instance Core.AWSRequest RegisterGameServer where
  type
    AWSResponse RegisterGameServer =
      RegisterGameServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterGameServerResponse'
            Prelude.<$> (x Data..?> "GameServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterGameServer where
  hashWithSalt _salt RegisterGameServer' {..} =
    _salt
      `Prelude.hashWithSalt` connectionInfo
      `Prelude.hashWithSalt` gameServerData
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` gameServerId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData RegisterGameServer where
  rnf RegisterGameServer' {..} =
    Prelude.rnf connectionInfo
      `Prelude.seq` Prelude.rnf gameServerData
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf gameServerId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders RegisterGameServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.RegisterGameServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterGameServer where
  toJSON RegisterGameServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionInfo" Data..=)
              Prelude.<$> connectionInfo,
            ("GameServerData" Data..=)
              Prelude.<$> gameServerData,
            Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName),
            Prelude.Just ("GameServerId" Data..= gameServerId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath RegisterGameServer where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterGameServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'
  { -- | Object that describes the newly registered game server.
    gameServer :: Prelude.Maybe GameServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RegisterGameServerResponse
newRegisterGameServerResponse pHttpStatus_ =
  RegisterGameServerResponse'
    { gameServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly registered game server.
registerGameServerResponse_gameServer :: Lens.Lens' RegisterGameServerResponse (Prelude.Maybe GameServer)
registerGameServerResponse_gameServer = Lens.lens (\RegisterGameServerResponse' {gameServer} -> gameServer) (\s@RegisterGameServerResponse' {} a -> s {gameServer = a} :: RegisterGameServerResponse)

-- | The response's http status code.
registerGameServerResponse_httpStatus :: Lens.Lens' RegisterGameServerResponse Prelude.Int
registerGameServerResponse_httpStatus = Lens.lens (\RegisterGameServerResponse' {httpStatus} -> httpStatus) (\s@RegisterGameServerResponse' {} a -> s {httpStatus = a} :: RegisterGameServerResponse)

instance Prelude.NFData RegisterGameServerResponse where
  rnf RegisterGameServerResponse' {..} =
    Prelude.rnf gameServer
      `Prelude.seq` Prelude.rnf httpStatus
