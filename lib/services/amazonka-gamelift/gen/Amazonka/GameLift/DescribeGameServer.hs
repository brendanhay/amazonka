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
-- Module      : Amazonka.GameLift.DescribeGameServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
--
-- Retrieves information for a registered game server. Information includes
-- game server status, health check info, and the instance that the game
-- server is running on.
--
-- To retrieve game server information, specify the game server ID. If
-- successful, the requested game server object is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related actions__
--
-- RegisterGameServer | ListGameServers | ClaimGameServer |
-- DescribeGameServer | UpdateGameServer | DeregisterGameServer |
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/reference-awssdk-fleetiq.html All APIs by task>
module Amazonka.GameLift.DescribeGameServer
  ( -- * Creating a Request
    DescribeGameServer (..),
    newDescribeGameServer,

    -- * Request Lenses
    describeGameServer_gameServerGroupName,
    describeGameServer_gameServerId,

    -- * Destructuring the Response
    DescribeGameServerResponse (..),
    newDescribeGameServerResponse,

    -- * Response Lenses
    describeGameServerResponse_gameServer,
    describeGameServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGameServer' smart constructor.
data DescribeGameServer = DescribeGameServer'
  { -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text,
    -- | A custom string that uniquely identifies the game server information to
    -- be retrieved.
    gameServerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'describeGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'gameServerId', 'describeGameServer_gameServerId' - A custom string that uniquely identifies the game server information to
-- be retrieved.
newDescribeGameServer ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  -- | 'gameServerId'
  Prelude.Text ->
  DescribeGameServer
newDescribeGameServer
  pGameServerGroupName_
  pGameServerId_ =
    DescribeGameServer'
      { gameServerGroupName =
          pGameServerGroupName_,
        gameServerId = pGameServerId_
      }

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
describeGameServer_gameServerGroupName :: Lens.Lens' DescribeGameServer Prelude.Text
describeGameServer_gameServerGroupName = Lens.lens (\DescribeGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@DescribeGameServer' {} a -> s {gameServerGroupName = a} :: DescribeGameServer)

-- | A custom string that uniquely identifies the game server information to
-- be retrieved.
describeGameServer_gameServerId :: Lens.Lens' DescribeGameServer Prelude.Text
describeGameServer_gameServerId = Lens.lens (\DescribeGameServer' {gameServerId} -> gameServerId) (\s@DescribeGameServer' {} a -> s {gameServerId = a} :: DescribeGameServer)

instance Core.AWSRequest DescribeGameServer where
  type
    AWSResponse DescribeGameServer =
      DescribeGameServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerResponse'
            Prelude.<$> (x Data..?> "GameServer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameServer where
  hashWithSalt _salt DescribeGameServer' {..} =
    _salt `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` gameServerId

instance Prelude.NFData DescribeGameServer where
  rnf DescribeGameServer' {..} =
    Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf gameServerId

instance Data.ToHeaders DescribeGameServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeGameServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGameServer where
  toJSON DescribeGameServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName),
            Prelude.Just ("GameServerId" Data..= gameServerId)
          ]
      )

instance Data.ToPath DescribeGameServer where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGameServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGameServerResponse' smart constructor.
data DescribeGameServerResponse = DescribeGameServerResponse'
  { -- | Object that describes the requested game server.
    gameServer :: Prelude.Maybe GameServer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServer', 'describeGameServerResponse_gameServer' - Object that describes the requested game server.
--
-- 'httpStatus', 'describeGameServerResponse_httpStatus' - The response's http status code.
newDescribeGameServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGameServerResponse
newDescribeGameServerResponse pHttpStatus_ =
  DescribeGameServerResponse'
    { gameServer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the requested game server.
describeGameServerResponse_gameServer :: Lens.Lens' DescribeGameServerResponse (Prelude.Maybe GameServer)
describeGameServerResponse_gameServer = Lens.lens (\DescribeGameServerResponse' {gameServer} -> gameServer) (\s@DescribeGameServerResponse' {} a -> s {gameServer = a} :: DescribeGameServerResponse)

-- | The response's http status code.
describeGameServerResponse_httpStatus :: Lens.Lens' DescribeGameServerResponse Prelude.Int
describeGameServerResponse_httpStatus = Lens.lens (\DescribeGameServerResponse' {httpStatus} -> httpStatus) (\s@DescribeGameServerResponse' {} a -> s {httpStatus = a} :: DescribeGameServerResponse)

instance Prelude.NFData DescribeGameServerResponse where
  rnf DescribeGameServerResponse' {..} =
    Prelude.rnf gameServer
      `Prelude.seq` Prelude.rnf httpStatus
