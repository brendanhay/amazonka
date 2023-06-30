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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
-- /See:/ 'newGameServerInstance' smart constructor.
data GameServerInstance = GameServerInstance'
  { -- | A generated unique identifier for the game server group that includes
    -- the game server instance.
    gameServerGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A developer-defined identifier for the game server group that includes
    -- the game server instance. The name is unique for each Region in each
    -- Amazon Web Services account.
    gameServerGroupName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Current status of the game server instance
    instanceStatus :: Prelude.Maybe GameServerInstanceStatus
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
-- 'gameServerGroupArn', 'gameServerInstance_gameServerGroupArn' - A generated unique identifier for the game server group that includes
-- the game server instance.
--
-- 'gameServerGroupName', 'gameServerInstance_gameServerGroupName' - A developer-defined identifier for the game server group that includes
-- the game server instance. The name is unique for each Region in each
-- Amazon Web Services account.
--
-- 'instanceId', 'gameServerInstance_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
--
-- 'instanceStatus', 'gameServerInstance_instanceStatus' - Current status of the game server instance
newGameServerInstance ::
  GameServerInstance
newGameServerInstance =
  GameServerInstance'
    { gameServerGroupArn =
        Prelude.Nothing,
      gameServerGroupName = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceStatus = Prelude.Nothing
    }

-- | A generated unique identifier for the game server group that includes
-- the game server instance.
gameServerInstance_gameServerGroupArn :: Lens.Lens' GameServerInstance (Prelude.Maybe Prelude.Text)
gameServerInstance_gameServerGroupArn = Lens.lens (\GameServerInstance' {gameServerGroupArn} -> gameServerGroupArn) (\s@GameServerInstance' {} a -> s {gameServerGroupArn = a} :: GameServerInstance)

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

-- | Current status of the game server instance
gameServerInstance_instanceStatus :: Lens.Lens' GameServerInstance (Prelude.Maybe GameServerInstanceStatus)
gameServerInstance_instanceStatus = Lens.lens (\GameServerInstance' {instanceStatus} -> instanceStatus) (\s@GameServerInstance' {} a -> s {instanceStatus = a} :: GameServerInstance)

instance Data.FromJSON GameServerInstance where
  parseJSON =
    Data.withObject
      "GameServerInstance"
      ( \x ->
          GameServerInstance'
            Prelude.<$> (x Data..:? "GameServerGroupArn")
            Prelude.<*> (x Data..:? "GameServerGroupName")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "InstanceStatus")
      )

instance Prelude.Hashable GameServerInstance where
  hashWithSalt _salt GameServerInstance' {..} =
    _salt
      `Prelude.hashWithSalt` gameServerGroupArn
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceStatus

instance Prelude.NFData GameServerInstance where
  rnf GameServerInstance' {..} =
    Prelude.rnf gameServerGroupArn
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceStatus
