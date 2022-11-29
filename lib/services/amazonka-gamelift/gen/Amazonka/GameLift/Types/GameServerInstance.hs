{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types.GameServerInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.GameServerInstanceStatus
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the GameLift FleetIQ and game server
-- groups.__
--
-- Additional properties, including status, that describe an EC2 instance
-- in a game server group. Instance configurations are set with game server
-- group properties (see @DescribeGameServerGroup@ and with the EC2 launch
-- template that was used when creating the game server group.
--
-- Retrieve game server instances for a game server group by calling
-- @DescribeGameServerInstances@.
--
-- __Related actions__
--
-- CreateGameServerGroup | ListGameServerGroups | DescribeGameServerGroup |
-- UpdateGameServerGroup | DeleteGameServerGroup | ResumeGameServerGroup |
-- SuspendGameServerGroup | DescribeGameServerInstances |
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/reference-awssdk-fleetiq.html All APIs by task>
--
-- /See:/ 'newGameServerInstance' smart constructor.
data GameServerInstance = GameServerInstance'
  { -- | Current status of the game server instance.
    --
    -- -   __ACTIVE__ -- The instance is viable for hosting game servers.
    --
    -- -   __DRAINING__ -- The instance is not viable for hosting game servers.
    --     Existing game servers are in the process of ending, and new game
    --     servers are not started on this instance unless no other resources
    --     are available. When the instance is put in DRAINING, a new instance
    --     is started up to replace it. Once the instance has no UTILIZED game
    --     servers, it will be terminated in favor of the new instance.
    --
    -- -   __SPOT_TERMINATING__ -- The instance is in the process of shutting
    --     down due to a Spot instance interruption. No new game servers are
    --     started on this instance.
    instanceStatus :: Prelude.Maybe GameServerInstanceStatus,
    -- | A developer-defined identifier for the game server group that includes
    -- the game server instance. The name is unique for each Region in each
    -- Amazon Web Services account.
    gameServerGroupName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A generated unique identifier for the game server group that includes
    -- the game server instance.
    gameServerGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameServerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceStatus', 'gameServerInstance_instanceStatus' - Current status of the game server instance.
--
-- -   __ACTIVE__ -- The instance is viable for hosting game servers.
--
-- -   __DRAINING__ -- The instance is not viable for hosting game servers.
--     Existing game servers are in the process of ending, and new game
--     servers are not started on this instance unless no other resources
--     are available. When the instance is put in DRAINING, a new instance
--     is started up to replace it. Once the instance has no UTILIZED game
--     servers, it will be terminated in favor of the new instance.
--
-- -   __SPOT_TERMINATING__ -- The instance is in the process of shutting
--     down due to a Spot instance interruption. No new game servers are
--     started on this instance.
--
-- 'gameServerGroupName', 'gameServerInstance_gameServerGroupName' - A developer-defined identifier for the game server group that includes
-- the game server instance. The name is unique for each Region in each
-- Amazon Web Services account.
--
-- 'instanceId', 'gameServerInstance_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
--
-- 'gameServerGroupArn', 'gameServerInstance_gameServerGroupArn' - A generated unique identifier for the game server group that includes
-- the game server instance.
newGameServerInstance ::
  GameServerInstance
newGameServerInstance =
  GameServerInstance'
    { instanceStatus =
        Prelude.Nothing,
      gameServerGroupName = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      gameServerGroupArn = Prelude.Nothing
    }

-- | Current status of the game server instance.
--
-- -   __ACTIVE__ -- The instance is viable for hosting game servers.
--
-- -   __DRAINING__ -- The instance is not viable for hosting game servers.
--     Existing game servers are in the process of ending, and new game
--     servers are not started on this instance unless no other resources
--     are available. When the instance is put in DRAINING, a new instance
--     is started up to replace it. Once the instance has no UTILIZED game
--     servers, it will be terminated in favor of the new instance.
--
-- -   __SPOT_TERMINATING__ -- The instance is in the process of shutting
--     down due to a Spot instance interruption. No new game servers are
--     started on this instance.
gameServerInstance_instanceStatus :: Lens.Lens' GameServerInstance (Prelude.Maybe GameServerInstanceStatus)
gameServerInstance_instanceStatus = Lens.lens (\GameServerInstance' {instanceStatus} -> instanceStatus) (\s@GameServerInstance' {} a -> s {instanceStatus = a} :: GameServerInstance)

-- | A developer-defined identifier for the game server group that includes
-- the game server instance. The name is unique for each Region in each
-- Amazon Web Services account.
gameServerInstance_gameServerGroupName :: Lens.Lens' GameServerInstance (Prelude.Maybe Prelude.Text)
gameServerInstance_gameServerGroupName = Lens.lens (\GameServerInstance' {gameServerGroupName} -> gameServerGroupName) (\s@GameServerInstance' {} a -> s {gameServerGroupName = a} :: GameServerInstance)

-- | The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
gameServerInstance_instanceId :: Lens.Lens' GameServerInstance (Prelude.Maybe Prelude.Text)
gameServerInstance_instanceId = Lens.lens (\GameServerInstance' {instanceId} -> instanceId) (\s@GameServerInstance' {} a -> s {instanceId = a} :: GameServerInstance)

-- | A generated unique identifier for the game server group that includes
-- the game server instance.
gameServerInstance_gameServerGroupArn :: Lens.Lens' GameServerInstance (Prelude.Maybe Prelude.Text)
gameServerInstance_gameServerGroupArn = Lens.lens (\GameServerInstance' {gameServerGroupArn} -> gameServerGroupArn) (\s@GameServerInstance' {} a -> s {gameServerGroupArn = a} :: GameServerInstance)

instance Core.FromJSON GameServerInstance where
  parseJSON =
    Core.withObject
      "GameServerInstance"
      ( \x ->
          GameServerInstance'
            Prelude.<$> (x Core..:? "InstanceStatus")
            Prelude.<*> (x Core..:? "GameServerGroupName")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "GameServerGroupArn")
      )

instance Prelude.Hashable GameServerInstance where
  hashWithSalt _salt GameServerInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceStatus
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` gameServerGroupArn

instance Prelude.NFData GameServerInstance where
  rnf GameServerInstance' {..} =
    Prelude.rnf instanceStatus
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf gameServerGroupArn
