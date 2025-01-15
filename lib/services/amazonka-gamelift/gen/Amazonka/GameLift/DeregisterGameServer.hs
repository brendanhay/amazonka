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
-- Module      : Amazonka.GameLift.DeregisterGameServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the GameLift FleetIQ solution and game
-- server groups.__
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
module Amazonka.GameLift.DeregisterGameServer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
  { -- | A unique identifier for the game server group where the game server is
    -- running.
    gameServerGroupName :: Prelude.Text,
    -- | A custom string that uniquely identifies the game server to deregister.
    gameServerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterGameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroupName', 'deregisterGameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running.
--
-- 'gameServerId', 'deregisterGameServer_gameServerId' - A custom string that uniquely identifies the game server to deregister.
newDeregisterGameServer ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  -- | 'gameServerId'
  Prelude.Text ->
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
-- running.
deregisterGameServer_gameServerGroupName :: Lens.Lens' DeregisterGameServer Prelude.Text
deregisterGameServer_gameServerGroupName = Lens.lens (\DeregisterGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@DeregisterGameServer' {} a -> s {gameServerGroupName = a} :: DeregisterGameServer)

-- | A custom string that uniquely identifies the game server to deregister.
deregisterGameServer_gameServerId :: Lens.Lens' DeregisterGameServer Prelude.Text
deregisterGameServer_gameServerId = Lens.lens (\DeregisterGameServer' {gameServerId} -> gameServerId) (\s@DeregisterGameServer' {} a -> s {gameServerId = a} :: DeregisterGameServer)

instance Core.AWSRequest DeregisterGameServer where
  type
    AWSResponse DeregisterGameServer =
      DeregisterGameServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeregisterGameServerResponse'

instance Prelude.Hashable DeregisterGameServer where
  hashWithSalt _salt DeregisterGameServer' {..} =
    _salt
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` gameServerId

instance Prelude.NFData DeregisterGameServer where
  rnf DeregisterGameServer' {..} =
    Prelude.rnf gameServerGroupName `Prelude.seq`
      Prelude.rnf gameServerId

instance Data.ToHeaders DeregisterGameServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeregisterGameServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterGameServer where
  toJSON DeregisterGameServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GameServerGroupName" Data..= gameServerGroupName),
            Prelude.Just ("GameServerId" Data..= gameServerId)
          ]
      )

instance Data.ToPath DeregisterGameServer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterGameServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterGameServerResponse ::
  DeregisterGameServerResponse
newDeregisterGameServerResponse =
  DeregisterGameServerResponse'

instance Prelude.NFData DeregisterGameServerResponse where
  rnf _ = ()
