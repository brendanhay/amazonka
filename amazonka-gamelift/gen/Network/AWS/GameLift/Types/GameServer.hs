{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.Types.GameServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServer where

import Network.AWS.GameLift.Types.GameServerClaimStatus
import Network.AWS.GameLift.Types.GameServerUtilizationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game
-- server groups.__
--
-- Properties describing a game server that is running on an instance in a
-- GameServerGroup.
--
-- A game server is created by a successful call to @RegisterGameServer@
-- and deleted by calling @DeregisterGameServer@. A game server is claimed
-- to host a game session by calling @ClaimGameServer@.
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
--
-- /See:/ 'newGameServer' smart constructor.
data GameServer = GameServer'
  { -- | The unique identifier for the instance where the game server is running.
    -- This ID is available in the instance metadata. EC2 instance IDs use a
    -- 17-character format, for example: @i-1234567890abcdef0@.
    instanceId :: Prelude.Maybe Prelude.Text,
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
    -- | Indicates when an available game server has been reserved for gameplay
    -- but has not yet started hosting a game. Once it is claimed, the game
    -- server remains in @CLAIMED@ status for a maximum of one minute. During
    -- this time, game clients connect to the game server to start the game and
    -- trigger the game server to update its utilization status. After one
    -- minute, the game server claim status reverts to null.
    claimStatus :: Prelude.Maybe GameServerClaimStatus,
    -- | A set of custom game server properties, formatted as a single string
    -- value. This data is passed to a game client or service when it requests
    -- information on game servers using ListGameServers or ClaimGameServer.
    gameServerData :: Prelude.Maybe Prelude.Text,
    -- | Timestamp that indicates the last time the game server was claimed with
    -- a ClaimGameServer request. The format is a number expressed in Unix time
    -- as milliseconds (for example @\"1469498468.057\"@). This value is used
    -- to calculate when a claimed game server\'s status should revert to null.
    lastClaimTime :: Prelude.Maybe Prelude.POSIX,
    -- | Timestamp that indicates when the game server was created with a
    -- RegisterGameServer request. The format is a number expressed in Unix
    -- time as milliseconds (for example @\"1469498468.057\"@).
    registrationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN identifier for the game server group where the game server is
    -- located.
    gameServerGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A custom string that uniquely identifies the game server. Game server
    -- IDs are developer-defined and are unique across all game server groups
    -- in an AWS account.
    gameServerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game server group where the game server is
    -- running. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port and IP address that must be used to establish a client
    -- connection to the game server.
    connectionInfo :: Prelude.Maybe Prelude.Text,
    -- | Timestamp that indicates the last time the game server was updated with
    -- health status using an UpdateGameServer request. The format is a number
    -- expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@). After game server registration, this property is
    -- only changed when a game server update specifies a health check value.
    lastHealthCheckTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GameServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'gameServer_instanceId' - The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
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
-- 'claimStatus', 'gameServer_claimStatus' - Indicates when an available game server has been reserved for gameplay
-- but has not yet started hosting a game. Once it is claimed, the game
-- server remains in @CLAIMED@ status for a maximum of one minute. During
-- this time, game clients connect to the game server to start the game and
-- trigger the game server to update its utilization status. After one
-- minute, the game server claim status reverts to null.
--
-- 'gameServerData', 'gameServer_gameServerData' - A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
--
-- 'lastClaimTime', 'gameServer_lastClaimTime' - Timestamp that indicates the last time the game server was claimed with
-- a ClaimGameServer request. The format is a number expressed in Unix time
-- as milliseconds (for example @\"1469498468.057\"@). This value is used
-- to calculate when a claimed game server\'s status should revert to null.
--
-- 'registrationTime', 'gameServer_registrationTime' - Timestamp that indicates when the game server was created with a
-- RegisterGameServer request. The format is a number expressed in Unix
-- time as milliseconds (for example @\"1469498468.057\"@).
--
-- 'gameServerGroupArn', 'gameServer_gameServerGroupArn' - The ARN identifier for the game server group where the game server is
-- located.
--
-- 'gameServerId', 'gameServer_gameServerId' - A custom string that uniquely identifies the game server. Game server
-- IDs are developer-defined and are unique across all game server groups
-- in an AWS account.
--
-- 'gameServerGroupName', 'gameServer_gameServerGroupName' - A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
--
-- 'connectionInfo', 'gameServer_connectionInfo' - The port and IP address that must be used to establish a client
-- connection to the game server.
--
-- 'lastHealthCheckTime', 'gameServer_lastHealthCheckTime' - Timestamp that indicates the last time the game server was updated with
-- health status using an UpdateGameServer request. The format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@). After game server registration, this property is
-- only changed when a game server update specifies a health check value.
newGameServer ::
  GameServer
newGameServer =
  GameServer'
    { instanceId = Prelude.Nothing,
      utilizationStatus = Prelude.Nothing,
      claimStatus = Prelude.Nothing,
      gameServerData = Prelude.Nothing,
      lastClaimTime = Prelude.Nothing,
      registrationTime = Prelude.Nothing,
      gameServerGroupArn = Prelude.Nothing,
      gameServerId = Prelude.Nothing,
      gameServerGroupName = Prelude.Nothing,
      connectionInfo = Prelude.Nothing,
      lastHealthCheckTime = Prelude.Nothing
    }

-- | The unique identifier for the instance where the game server is running.
-- This ID is available in the instance metadata. EC2 instance IDs use a
-- 17-character format, for example: @i-1234567890abcdef0@.
gameServer_instanceId :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_instanceId = Lens.lens (\GameServer' {instanceId} -> instanceId) (\s@GameServer' {} a -> s {instanceId = a} :: GameServer)

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

-- | Indicates when an available game server has been reserved for gameplay
-- but has not yet started hosting a game. Once it is claimed, the game
-- server remains in @CLAIMED@ status for a maximum of one minute. During
-- this time, game clients connect to the game server to start the game and
-- trigger the game server to update its utilization status. After one
-- minute, the game server claim status reverts to null.
gameServer_claimStatus :: Lens.Lens' GameServer (Prelude.Maybe GameServerClaimStatus)
gameServer_claimStatus = Lens.lens (\GameServer' {claimStatus} -> claimStatus) (\s@GameServer' {} a -> s {claimStatus = a} :: GameServer)

-- | A set of custom game server properties, formatted as a single string
-- value. This data is passed to a game client or service when it requests
-- information on game servers using ListGameServers or ClaimGameServer.
gameServer_gameServerData :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerData = Lens.lens (\GameServer' {gameServerData} -> gameServerData) (\s@GameServer' {} a -> s {gameServerData = a} :: GameServer)

-- | Timestamp that indicates the last time the game server was claimed with
-- a ClaimGameServer request. The format is a number expressed in Unix time
-- as milliseconds (for example @\"1469498468.057\"@). This value is used
-- to calculate when a claimed game server\'s status should revert to null.
gameServer_lastClaimTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_lastClaimTime = Lens.lens (\GameServer' {lastClaimTime} -> lastClaimTime) (\s@GameServer' {} a -> s {lastClaimTime = a} :: GameServer) Prelude.. Lens.mapping Prelude._Time

-- | Timestamp that indicates when the game server was created with a
-- RegisterGameServer request. The format is a number expressed in Unix
-- time as milliseconds (for example @\"1469498468.057\"@).
gameServer_registrationTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_registrationTime = Lens.lens (\GameServer' {registrationTime} -> registrationTime) (\s@GameServer' {} a -> s {registrationTime = a} :: GameServer) Prelude.. Lens.mapping Prelude._Time

-- | The ARN identifier for the game server group where the game server is
-- located.
gameServer_gameServerGroupArn :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerGroupArn = Lens.lens (\GameServer' {gameServerGroupArn} -> gameServerGroupArn) (\s@GameServer' {} a -> s {gameServerGroupArn = a} :: GameServer)

-- | A custom string that uniquely identifies the game server. Game server
-- IDs are developer-defined and are unique across all game server groups
-- in an AWS account.
gameServer_gameServerId :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerId = Lens.lens (\GameServer' {gameServerId} -> gameServerId) (\s@GameServer' {} a -> s {gameServerId = a} :: GameServer)

-- | A unique identifier for the game server group where the game server is
-- running. Use either the GameServerGroup name or ARN value.
gameServer_gameServerGroupName :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_gameServerGroupName = Lens.lens (\GameServer' {gameServerGroupName} -> gameServerGroupName) (\s@GameServer' {} a -> s {gameServerGroupName = a} :: GameServer)

-- | The port and IP address that must be used to establish a client
-- connection to the game server.
gameServer_connectionInfo :: Lens.Lens' GameServer (Prelude.Maybe Prelude.Text)
gameServer_connectionInfo = Lens.lens (\GameServer' {connectionInfo} -> connectionInfo) (\s@GameServer' {} a -> s {connectionInfo = a} :: GameServer)

-- | Timestamp that indicates the last time the game server was updated with
-- health status using an UpdateGameServer request. The format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@). After game server registration, this property is
-- only changed when a game server update specifies a health check value.
gameServer_lastHealthCheckTime :: Lens.Lens' GameServer (Prelude.Maybe Prelude.UTCTime)
gameServer_lastHealthCheckTime = Lens.lens (\GameServer' {lastHealthCheckTime} -> lastHealthCheckTime) (\s@GameServer' {} a -> s {lastHealthCheckTime = a} :: GameServer) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON GameServer where
  parseJSON =
    Prelude.withObject
      "GameServer"
      ( \x ->
          GameServer'
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "UtilizationStatus")
            Prelude.<*> (x Prelude..:? "ClaimStatus")
            Prelude.<*> (x Prelude..:? "GameServerData")
            Prelude.<*> (x Prelude..:? "LastClaimTime")
            Prelude.<*> (x Prelude..:? "RegistrationTime")
            Prelude.<*> (x Prelude..:? "GameServerGroupArn")
            Prelude.<*> (x Prelude..:? "GameServerId")
            Prelude.<*> (x Prelude..:? "GameServerGroupName")
            Prelude.<*> (x Prelude..:? "ConnectionInfo")
            Prelude.<*> (x Prelude..:? "LastHealthCheckTime")
      )

instance Prelude.Hashable GameServer

instance Prelude.NFData GameServer
