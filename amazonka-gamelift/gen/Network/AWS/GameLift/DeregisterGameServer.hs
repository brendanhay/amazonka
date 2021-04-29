{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
  { -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text,
    -- | A custom string that uniquely identifies the game server to deregister.
    gameServerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- running. Use either the GameServerGroup name or ARN value.
deregisterGameServer_gameServerGroupName :: Lens.Lens' DeregisterGameServer Prelude.Text
deregisterGameServer_gameServerGroupName = Lens.lens (\DeregisterGameServer' {gameServerGroupName} -> gameServerGroupName) (\s@DeregisterGameServer' {} a -> s {gameServerGroupName = a} :: DeregisterGameServer)

-- | A custom string that uniquely identifies the game server to deregister.
deregisterGameServer_gameServerId :: Lens.Lens' DeregisterGameServer Prelude.Text
deregisterGameServer_gameServerId = Lens.lens (\DeregisterGameServer' {gameServerId} -> gameServerId) (\s@DeregisterGameServer' {} a -> s {gameServerId = a} :: DeregisterGameServer)

instance Prelude.AWSRequest DeregisterGameServer where
  type
    Rs DeregisterGameServer =
      DeregisterGameServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeregisterGameServerResponse'

instance Prelude.Hashable DeregisterGameServer

instance Prelude.NFData DeregisterGameServer

instance Prelude.ToHeaders DeregisterGameServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DeregisterGameServer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterGameServer where
  toJSON DeregisterGameServer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "GameServerGroupName"
                  Prelude..= gameServerGroupName
              ),
            Prelude.Just
              ("GameServerId" Prelude..= gameServerId)
          ]
      )

instance Prelude.ToPath DeregisterGameServer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterGameServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterGameServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterGameServerResponse ::
  DeregisterGameServerResponse
newDeregisterGameServerResponse =
  DeregisterGameServerResponse'

instance Prelude.NFData DeregisterGameServerResponse
