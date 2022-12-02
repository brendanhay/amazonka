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
-- Module      : Amazonka.GameLift.Types.GameServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.GameServerClaimStatus
import Amazonka.GameLift.Types.GameServerUtilizationStatus
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the GameLift FleetIQ and game server
-- groups.__
--
-- Properties describing a game server that is running on an instance in a
-- GameServerGroup.
--
-- A game server is created by a successful call to @RegisterGameServer@
-- and deleted by calling @DeregisterGameServer@. A game server is claimed
-- to host a game session by calling @ClaimGameServer@.
--
-- __Related actions__
--
-- RegisterGameServer | ListGameServers | ClaimGameServer |
-- DescribeGameServer | UpdateGameServer | DeregisterGameServer |
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/reference-awssdk-fleetiq.html All APIs by task>
--
-- /See:/ 'newGameServer' smart constructor.
data GameServer = GameServer'
  { -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers using ListGameServers or ClaimGameServer.
    gameServerData :: Prelude.Maybe Prelude.Text,
    -- | Indicates when an available game server has been reserved for gameplay
    -- but has not yet started hosting a game. Once it is claimed, the game
    -- server remains in @CLAIMED@ status for a maximum of one minute. During
    -- this time, game clients connect to the game server to start the game and
    -- trigger the game server to update its utilization status. After one
    -- minute, the game server claim status reverts to null.
    claimStatus :: Prelude.Maybe GameServerClaimStatus,
    -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp that indicates the last time the game server was claimed with
    -- a ClaimGameServer request. The format is a number expressed in Unix time
    -- as milliseconds (for example @\"1469498468.057\"@). This value is used
    -- to calculate when a claimed game server\'s status should revert to null.
    lastClaimTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN identifier for the game server group where the game server is
    -- located.
    gameServerGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp that indicates when the game server was created with a
    -- RegisterGameServer request. The format is a number expressed in Unix
    -- time as milliseconds (for example @\"1469498468.057\"@).
    registrationTime :: Prelude.Maybe Data.POSIX,
    -- | Timestamp that indicates the last time the game server was updated with
    -- health status using an UpdateGameServer request. The format is a number
    -- expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@). After game server registration, this property is
    -- only changed when a game server update specifies a health check value.
    lastHealthCheckTime :: Prelude.Maybe Data.POSIX,
    -- | The port and IP address that must be used to establish a client
    -- connection to the game server.
    connectionInfo :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the game server is currently available for new games
    -- or is busy. Possible statuses include:
    --
    -- -   @AVAILABLE@ - The game server is available to be claimed. A game
    --     server that has been claimed remains in this status until it reports
    --     game hosting activity.
    --
    -- -   @UTILIZED@ - The game server is currently hosting a game session
    --     with players.
    utilizationStatus :: Prelude.Maybe GameServerUtilizationStatus,
    -- | A custom string that uniquely identifies the game server. Game server
    -- IDs are developer-defined and are unique across all game server groups
    -- in an Amazon Web Services account.
    gameServerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerData', 'gameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
--
-- 'claimStatus', 'gameServer_claimStatus' - Indicates when an available game server has been reserved for gameplay
-- but has not yet started hosting a game. Once it is claimed, the game
-- server remains in @CLAIMED@ status for a maximum of one minute. During
-- this time, game clients connect to the game server to start the game and
-- trigger the game server to update its utilization status. After one
-- minute, the game server claim status reverts to null.
--
-- 'gameServerGroupName', 'gameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'lastClaimTime', 'gameServer_lastClaimTime' - Timestamp that indicates the last time the game server was claimed with
-- a ClaimGameServer request. The format is a number expressed in Unix time
-- as milliseconds (for example @\"1469498468.057\"@). This value is used
-- to calculate when a claimed game server\'s status should revert to null.
--
-- 'instanceId', 'gameServer_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
--
-- 'gameServerGroupArn', 'gameServer_gameServerGroupArn' - The ARN identifier for the game server group where the game server is
-- located.
--
-- 'registrationTime', 'gameServer_registrationTime' - Timestamp that indicates when the game server was created with a
-- RegisterGameServer request. The format is a number expressed in Unix
-- time as milliseconds (for example @\"1469498468.057\"@).
--
-- 'lastHealthCheckTime', 'gameServer_lastHealthCheckTime' - Timestamp that indicates the last time the game server was updated with
-- health status using an UpdateGameServer request. The format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@). After game server registration, this property is
-- only changed when a game server update specifies a health check value.
--
-- 'connectionInfo', 'gameServer_connectionInfo' - The port and IP address that must be used to establish a client
-- connection to the game server.
--
-- 'utilizationStatus', 'gameServer_utilizationStatus' - Indicates whether the game server is currently available for new games
-- or is busy. Possible statuses include:
--
-- -   @AVAILABLE@ - The game server is available to be claimed. A game
--     server that has been claimed remains in this status until it reports
--     game hosting activity.
--
-- -   @UTILIZED@ - The game server is currently hosting a game session
--     with players.
--
-- 'gameServerId', 'gameServer_gameServerId' - A custom string that uniquely identifies the game server. Game server
-- IDs are developer-defined and are unique across all game server groups
-- in an Amazon Web Services account.
newGameServer ::
  GameServer
newGameServer =
  GameServer'
    { gameServerData = Prelude.Nothing,
      claimStatus = Prelude.Nothing,
      gameServerGroupName = Prelude.Nothing,
      lastClaimTime = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      gameServerGroupArn = Prelude.Nothing,
      registrationTime = Prelude.Nothing,
      lastHealthCheckTime = Prelude.Nothing,
      connectionInfo = Prelude.Nothing,
      utilizationStatus = Prelude.Nothing,
      gameServerId = Prelude.Nothing
    }

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
gameServer_gameServerData :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerData = Lens.lens (\GameServer' {gameServerData} -> gameServerData) (\s@GameServer' {} a -> s {gameServerData = a} :: GameServer)

-- | Indicates when an available game server has been reserved for gameplay
-- but has not yet started hosting a game. Once it is claimed, the game
-- server remains in @CLAIMED@ status for a maximum of one minute. During
-- this time, game clients connect to the game server to start the game and
-- trigger the game server to update its utilization status. After one
-- minute, the game server claim status reverts to null.
gameServer_claimStatus :: Lens.Lens' GameServer (Prelude.Maybe GameServerClaimStatus)
gameServer_claimStatus = Lens.lens (\GameServer' {claimStatus} -> claimStatus) (\s@GameServer' {} a -> s {claimStatus = a} :: GameServer)

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
gameServer_gameServerGroupName :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerGroupName = Lens.lens (\GameServer' {gameServerGroupName} -> gameServerGroupName) (\s@GameServer' {} a -> s {gameServerGroupName = a} :: GameServer)

-- | Timestamp that indicates the last time the game server was claimed with
-- a ClaimGameServer request. The format is a number expressed in Unix time
-- as milliseconds (for example @\"1469498468.057\"@). This value is used
-- to calculate when a claimed game server\'s status should revert to null.
gameServer_lastClaimTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_lastClaimTime = Lens.lens (\GameServer' {lastClaimTime} -> lastClaimTime) (\s@GameServer' {} a -> s {lastClaimTime = a} :: GameServer) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
gameServer_instanceId :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_instanceId = Lens.lens (\GameServer' {instanceId} -> instanceId) (\s@GameServer' {} a -> s {instanceId = a} :: GameServer)

-- | The ARN identifier for the game server group where the game server is
-- located.
gameServer_gameServerGroupArn :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerGroupArn = Lens.lens (\GameServer' {gameServerGroupArn} -> gameServerGroupArn) (\s@GameServer' {} a -> s {gameServerGroupArn = a} :: GameServer)

-- | Timestamp that indicates when the game server was created with a
-- RegisterGameServer request. The format is a number expressed in Unix
-- time as milliseconds (for example @\"1469498468.057\"@).
gameServer_registrationTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_registrationTime = Lens.lens (\GameServer' {registrationTime} -> registrationTime) (\s@GameServer' {} a -> s {registrationTime = a} :: GameServer) Prelude.. Lens.mapping Data._Time

-- | Timestamp that indicates the last time the game server was updated with
-- health status using an UpdateGameServer request. The format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@). After game server registration, this property is
-- only changed when a game server update specifies a health check value.
gameServer_lastHealthCheckTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_lastHealthCheckTime = Lens.lens (\GameServer' {lastHealthCheckTime} -> lastHealthCheckTime) (\s@GameServer' {} a -> s {lastHealthCheckTime = a} :: GameServer) Prelude.. Lens.mapping Data._Time

-- | The port and IP address that must be used to establish a client
-- connection to the game server.
gameServer_connectionInfo :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_connectionInfo = Lens.lens (\GameServer' {connectionInfo} -> connectionInfo) (\s@GameServer' {} a -> s {connectionInfo = a} :: GameServer)

-- | Indicates whether the game server is currently available for new games
-- or is busy. Possible statuses include:
--
-- -   @AVAILABLE@ - The game server is available to be claimed. A game
--     server that has been claimed remains in this status until it reports
--     game hosting activity.
--
-- -   @UTILIZED@ - The game server is currently hosting a game session
--     with players.
gameServer_utilizationStatus :: Lens.Lens' GameServer (Prelude.Maybe GameServerUtilizationStatus)
gameServer_utilizationStatus = Lens.lens (\GameServer' {utilizationStatus} -> utilizationStatus) (\s@GameServer' {} a -> s {utilizationStatus = a} :: GameServer)

-- | A custom string that uniquely identifies the game server. Game server
-- IDs are developer-defined and are unique across all game server groups
-- in an Amazon Web Services account.
gameServer_gameServerId :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerId = Lens.lens (\GameServer' {gameServerId} -> gameServerId) (\s@GameServer' {} a -> s {gameServerId = a} :: GameServer)

instance Data.FromJSON GameServer where
  parseJSON =
    Data.withObject
      "GameServer"
      ( \x ->
          GameServer'
            Prelude.<$> (x Data..:? "GameServerData")
            Prelude.<*> (x Data..:? "ClaimStatus")
            Prelude.<*> (x Data..:? "GameServerGroupName")
            Prelude.<*> (x Data..:? "LastClaimTime")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "GameServerGroupArn")
            Prelude.<*> (x Data..:? "RegistrationTime")
            Prelude.<*> (x Data..:? "LastHealthCheckTime")
            Prelude.<*> (x Data..:? "ConnectionInfo")
            Prelude.<*> (x Data..:? "UtilizationStatus")
            Prelude.<*> (x Data..:? "GameServerId")
      )

instance Prelude.Hashable GameServer where
  hashWithSalt _salt GameServer' {..} =
    _salt `Prelude.hashWithSalt` gameServerData
      `Prelude.hashWithSalt` claimStatus
      `Prelude.hashWithSalt` gameServerGroupName
      `Prelude.hashWithSalt` lastClaimTime
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` gameServerGroupArn
      `Prelude.hashWithSalt` registrationTime
      `Prelude.hashWithSalt` lastHealthCheckTime
      `Prelude.hashWithSalt` connectionInfo
      `Prelude.hashWithSalt` utilizationStatus
      `Prelude.hashWithSalt` gameServerId

instance Prelude.NFData GameServer where
  rnf GameServer' {..} =
    Prelude.rnf gameServerData
      `Prelude.seq` Prelude.rnf claimStatus
      `Prelude.seq` Prelude.rnf gameServerGroupName
      `Prelude.seq` Prelude.rnf lastClaimTime
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf gameServerGroupArn
      `Prelude.seq` Prelude.rnf registrationTime
      `Prelude.seq` Prelude.rnf lastHealthCheckTime
      `Prelude.seq` Prelude.rnf connectionInfo
      `Prelude.seq` Prelude.rnf utilizationStatus
      `Prelude.seq` Prelude.rnf gameServerId
