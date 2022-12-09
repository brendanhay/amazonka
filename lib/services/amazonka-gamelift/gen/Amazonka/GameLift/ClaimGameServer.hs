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
-- Module      : Amazonka.GameLift.ClaimGameServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Locates an available game server and temporarily reserves it to host
-- gameplay and players. This operation is called from a game client or
-- client service (such as a matchmaker) to request hosting resources for a
-- new game session. In response, GameLift FleetIQ locates an available
-- game server, places it in @CLAIMED@ status for 60 seconds, and returns
-- connection information that players can use to connect to the game
-- server.
--
-- To claim a game server, identify a game server group. You can also
-- specify a game server ID, although this approach bypasses GameLift
-- FleetIQ placement optimization. Optionally, include game data to pass to
-- the game server at the start of a game session, such as a game map or
-- player information.
--
-- When a game server is successfully claimed, connection information is
-- returned. A claimed game server\'s utilization status remains
-- @AVAILABLE@ while the claim status is set to @CLAIMED@ for up to 60
-- seconds. This time period gives the game server time to update its
-- status to @UTILIZED@ after players join. If the game server\'s status is
-- not updated within 60 seconds, the game server reverts to unclaimed
-- status and is available to be claimed by another request. The claim time
-- period is a fixed value and is not configurable.
--
-- If you try to claim a specific game server, this request will fail in
-- the following cases:
--
-- -   If the game server utilization status is @UTILIZED@.
--
-- -   If the game server claim status is @CLAIMED@.
--
-- When claiming a specific game server, this request will succeed even if
-- the game server is running on an instance in @DRAINING@ status. To avoid
-- this, first check the instance status by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameServerInstances.html DescribeGameServerInstances>
-- .
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
module Amazonka.GameLift.ClaimGameServer
  ( -- * Creating a Request
    ClaimGameServer (..),
    newClaimGameServer,

    -- * Request Lenses
    claimGameServer_gameServerData,
    claimGameServer_gameServerId,
    claimGameServer_gameServerGroupName,

    -- * Destructuring the Response
    ClaimGameServerResponse (..),
    newClaimGameServerResponse,

    -- * Response Lenses
    claimGameServerResponse_gameServer,
    claimGameServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newClaimGameServer' smart constructor.
data ClaimGameServer = ClaimGameServer'
  { -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers.
    gameServerData :: Prelude.Maybe Prelude.Text,
    -- | A custom string that uniquely identifies the game server to claim. If
    -- this parameter is left empty, GameLift FleetIQ searches for an available
    -- game server in the specified game server group.
    gameServerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game server group where the game server is
    -- running. If you are not specifying a game server to claim, this value
    -- identifies where you want GameLift FleetIQ to look for an available game
    -- server to claim.
    gameServerGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerData', 'claimGameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers.
--
-- 'gameServerId', 'claimGameServer_gameServerId' - A custom string that uniquely identifies the game server to claim. If
-- this parameter is left empty, GameLift FleetIQ searches for an available
-- game server in the specified game server group.
--
-- 'gameServerGroupName', 'claimGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. If you are not specifying a game server to claim, this value
-- identifies where you want GameLift FleetIQ to look for an available game
-- server to claim.
newClaimGameServer ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  ClaimGameServer
newClaimGameServer pGameServerGroupName_ =
  ClaimGameServer'
    { gameServerData = Prelude.Nothing,
      gameServerId = Prelude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers.
claimGameServer_gameServerData :: Lens.Lens' ClaimGameServer (Prelude.Maybe Prelude.Text)
claimGameServer_gameServerData = Lens.lens (\ClaimGameServer' {gameServerData} -> gameServerData) (\s@ClaimGameServer' {} a -> s {gameServerData = a} :: ClaimGameServer)

-- | A custom string that uniquely identifies the game server to claim. If
-- this parameter is left empty, GameLift FleetIQ searches for an available
-- game server in the specified game server group.
claimGameServer_gameServerId :: Lens.Lens' ClaimGameServer (Prelude.Maybe Prelude.Text)
claimGameServer_gameServerId = Lens.lens (\ClaimGameServer' {gameServerId} -> gameServerId) (\s@ClaimGameServer' {} a -> s {gameServerId = a} :: ClaimGameServer)

-- | A unique identifier for the game server group where the game server is
-- running. If you are not specifying a game server to claim, this value
-- identifies where you want GameLift FleetIQ to look for an available game
-- server to claim.
claimGameServer_gameServerGroupName :: Lens.Lens' ClaimGameServer Prelude.Text
claimGameServer_gameServerGroupName = Lens.lens (\ClaimGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@ClaimGameServer' {} a -> s {gameServerGroupName = a} :: ClaimGameServer)

instance Core.AWSRequest ClaimGameServer where
  type
    AWSResponse ClaimGameServer =
      ClaimGameServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ClaimGameServerResponse'
            Prelude.<$> (x Data..?> "GameServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClaimGameServer where
  hashWithSalt _salt ClaimGameServer' {..} =
    _salt `Prelude.hashWithSalt` gameServerData
      `Prelude.hashWithSalt` gameServerId
      `Prelude.hashWithSalt` gameServerGroupName

instance Prelude.NFData ClaimGameServer where
  rnf ClaimGameServer' {..} =
    Prelude.rnf gameServerData
      `Prelude.seq` Prelude.rnf gameServerId
      `Prelude.seq` Prelude.rnf gameServerGroupName

instance Data.ToHeaders ClaimGameServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ClaimGameServer" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ClaimGameServer where
  toJSON ClaimGameServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GameServerData" Data..=)
              Prelude.<$> gameServerData,
            ("GameServerId" Data..=) Prelude.<$> gameServerId,
            Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName)
          ]
      )

instance Data.ToPath ClaimGameServer where
  toPath = Prelude.const "/"

instance Data.ToQuery ClaimGameServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClaimGameServerResponse' smart constructor.
data ClaimGameServerResponse = ClaimGameServerResponse'
  { -- | Object that describes the newly claimed game server.
    gameServer :: Prelude.Maybe GameServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServer', 'claimGameServerResponse_gameServer' - Object that describes the newly claimed game server.
--
-- 'httpStatus', 'claimGameServerResponse_httpStatus' - The response's http status code.
newClaimGameServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ClaimGameServerResponse
newClaimGameServerResponse pHttpStatus_ =
  ClaimGameServerResponse'
    { gameServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly claimed game server.
claimGameServerResponse_gameServer :: Lens.Lens' ClaimGameServerResponse (Prelude.Maybe GameServer)
claimGameServerResponse_gameServer = Lens.lens (\ClaimGameServerResponse' {gameServer} -> gameServer) (\s@ClaimGameServerResponse' {} a -> s {gameServer = a} :: ClaimGameServerResponse)

-- | The response's http status code.
claimGameServerResponse_httpStatus :: Lens.Lens' ClaimGameServerResponse Prelude.Int
claimGameServerResponse_httpStatus = Lens.lens (\ClaimGameServerResponse' {httpStatus} -> httpStatus) (\s@ClaimGameServerResponse' {} a -> s {httpStatus = a} :: ClaimGameServerResponse)

instance Prelude.NFData ClaimGameServerResponse where
  rnf ClaimGameServerResponse' {..} =
    Prelude.rnf gameServer
      `Prelude.seq` Prelude.rnf httpStatus
