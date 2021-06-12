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
-- Module      : Network.AWS.GameLift.Types.PlayerSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSession where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.PlayerSessionStatus
import qualified Network.AWS.Lens as Lens

-- | Properties describing a player session. Player session objects are
-- created either by creating a player session for a specific game session,
-- or as part of a game session placement. A player session represents
-- either a player reservation for a game session (status @RESERVED@) or
-- actual player activity in a game session (status @ACTIVE@). A player
-- session object (including player data) is automatically passed to a game
-- session when the player connects to the game session and is validated.
--
-- When a player disconnects, the player session status changes to
-- @COMPLETED@. Once the session ends, the player session object is
-- retained for 30 days and then removed.
--
-- -   CreatePlayerSession
--
-- -   CreatePlayerSessions
--
-- -   DescribePlayerSessions
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
--
-- /See:/ 'newPlayerSession' smart constructor.
data PlayerSession = PlayerSession'
  { -- | Current status of the player session.
    --
    -- Possible player session statuses include the following:
    --
    -- -   __RESERVED__ -- The player session request has been received, but
    --     the player has not yet connected to the server process and\/or been
    --     validated.
    --
    -- -   __ACTIVE__ -- The player has been validated by the server process
    --     and is currently connected.
    --
    -- -   __COMPLETED__ -- The player connection has been dropped.
    --
    -- -   __TIMEDOUT__ -- A player session request was received, but the
    --     player did not connect and\/or was not validated within the timeout
    --     limit (60 seconds).
    status :: Core.Maybe PlayerSessionStatus,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    -- | A unique identifier for a player that is associated with this player
    -- session.
    playerId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet that the player\'s game session is
    -- running on.
    fleetArn :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet that the player\'s game session is
    -- running on.
    fleetId :: Core.Maybe Core.Text,
    -- | A unique identifier for a player session.
    playerSessionId :: Core.Maybe Core.Text,
    -- | A unique identifier for the game session that the player session is
    -- connected to.
    gameSessionId :: Core.Maybe Core.Text,
    -- | IP address of the instance that is running the game session. When
    -- connecting to a Amazon GameLift game server, a client needs to reference
    -- an IP address (or DNS name) and port number.
    ipAddress :: Core.Maybe Core.Text,
    -- | Time stamp indicating when this data object was terminated. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    terminationTime :: Core.Maybe Core.POSIX,
    -- | Port number for the game session. To connect to a Amazon GameLift server
    -- process, an app needs both the IP address and port number.
    port :: Core.Maybe Core.Natural,
    -- | DNS identifier assigned to the instance that is running the game
    -- session. Values have the following format:
    --
    -- -   TLS-enabled fleets:
    --     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
    --
    -- -   Non-TLS-enabled fleets:
    --     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
    --
    -- When connecting to a game session that is running on a TLS-enabled
    -- fleet, you must use the DNS name, not the IP address.
    dnsName :: Core.Maybe Core.Text,
    -- | Developer-defined information related to a player. Amazon GameLift does
    -- not use this data, so it can be formatted as needed for use in the game.
    playerData :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'playerSession_status' - Current status of the player session.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
--
-- 'creationTime', 'playerSession_creationTime' - Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'playerId', 'playerSession_playerId' - A unique identifier for a player that is associated with this player
-- session.
--
-- 'fleetArn', 'playerSession_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet that the player\'s game session is
-- running on.
--
-- 'fleetId', 'playerSession_fleetId' - A unique identifier for a fleet that the player\'s game session is
-- running on.
--
-- 'playerSessionId', 'playerSession_playerSessionId' - A unique identifier for a player session.
--
-- 'gameSessionId', 'playerSession_gameSessionId' - A unique identifier for the game session that the player session is
-- connected to.
--
-- 'ipAddress', 'playerSession_ipAddress' - IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
--
-- 'terminationTime', 'playerSession_terminationTime' - Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'port', 'playerSession_port' - Port number for the game session. To connect to a Amazon GameLift server
-- process, an app needs both the IP address and port number.
--
-- 'dnsName', 'playerSession_dnsName' - DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
--
-- 'playerData', 'playerSession_playerData' - Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
newPlayerSession ::
  PlayerSession
newPlayerSession =
  PlayerSession'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      playerId = Core.Nothing,
      fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      playerSessionId = Core.Nothing,
      gameSessionId = Core.Nothing,
      ipAddress = Core.Nothing,
      terminationTime = Core.Nothing,
      port = Core.Nothing,
      dnsName = Core.Nothing,
      playerData = Core.Nothing
    }

-- | Current status of the player session.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
playerSession_status :: Lens.Lens' PlayerSession (Core.Maybe PlayerSessionStatus)
playerSession_status = Lens.lens (\PlayerSession' {status} -> status) (\s@PlayerSession' {} a -> s {status = a} :: PlayerSession)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
playerSession_creationTime :: Lens.Lens' PlayerSession (Core.Maybe Core.UTCTime)
playerSession_creationTime = Lens.lens (\PlayerSession' {creationTime} -> creationTime) (\s@PlayerSession' {} a -> s {creationTime = a} :: PlayerSession) Core.. Lens.mapping Core._Time

-- | A unique identifier for a player that is associated with this player
-- session.
playerSession_playerId :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_playerId = Lens.lens (\PlayerSession' {playerId} -> playerId) (\s@PlayerSession' {} a -> s {playerId = a} :: PlayerSession)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet that the player\'s game session is
-- running on.
playerSession_fleetArn :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_fleetArn = Lens.lens (\PlayerSession' {fleetArn} -> fleetArn) (\s@PlayerSession' {} a -> s {fleetArn = a} :: PlayerSession)

-- | A unique identifier for a fleet that the player\'s game session is
-- running on.
playerSession_fleetId :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_fleetId = Lens.lens (\PlayerSession' {fleetId} -> fleetId) (\s@PlayerSession' {} a -> s {fleetId = a} :: PlayerSession)

-- | A unique identifier for a player session.
playerSession_playerSessionId :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_playerSessionId = Lens.lens (\PlayerSession' {playerSessionId} -> playerSessionId) (\s@PlayerSession' {} a -> s {playerSessionId = a} :: PlayerSession)

-- | A unique identifier for the game session that the player session is
-- connected to.
playerSession_gameSessionId :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_gameSessionId = Lens.lens (\PlayerSession' {gameSessionId} -> gameSessionId) (\s@PlayerSession' {} a -> s {gameSessionId = a} :: PlayerSession)

-- | IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
playerSession_ipAddress :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_ipAddress = Lens.lens (\PlayerSession' {ipAddress} -> ipAddress) (\s@PlayerSession' {} a -> s {ipAddress = a} :: PlayerSession)

-- | Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
playerSession_terminationTime :: Lens.Lens' PlayerSession (Core.Maybe Core.UTCTime)
playerSession_terminationTime = Lens.lens (\PlayerSession' {terminationTime} -> terminationTime) (\s@PlayerSession' {} a -> s {terminationTime = a} :: PlayerSession) Core.. Lens.mapping Core._Time

-- | Port number for the game session. To connect to a Amazon GameLift server
-- process, an app needs both the IP address and port number.
playerSession_port :: Lens.Lens' PlayerSession (Core.Maybe Core.Natural)
playerSession_port = Lens.lens (\PlayerSession' {port} -> port) (\s@PlayerSession' {} a -> s {port = a} :: PlayerSession)

-- | DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
playerSession_dnsName :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_dnsName = Lens.lens (\PlayerSession' {dnsName} -> dnsName) (\s@PlayerSession' {} a -> s {dnsName = a} :: PlayerSession)

-- | Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
playerSession_playerData :: Lens.Lens' PlayerSession (Core.Maybe Core.Text)
playerSession_playerData = Lens.lens (\PlayerSession' {playerData} -> playerData) (\s@PlayerSession' {} a -> s {playerData = a} :: PlayerSession)

instance Core.FromJSON PlayerSession where
  parseJSON =
    Core.withObject
      "PlayerSession"
      ( \x ->
          PlayerSession'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "PlayerId")
            Core.<*> (x Core..:? "FleetArn")
            Core.<*> (x Core..:? "FleetId")
            Core.<*> (x Core..:? "PlayerSessionId")
            Core.<*> (x Core..:? "GameSessionId")
            Core.<*> (x Core..:? "IpAddress")
            Core.<*> (x Core..:? "TerminationTime")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "DnsName")
            Core.<*> (x Core..:? "PlayerData")
      )

instance Core.Hashable PlayerSession

instance Core.NFData PlayerSession
