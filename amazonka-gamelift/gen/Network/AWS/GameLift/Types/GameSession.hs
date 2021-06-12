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
-- Module      : Network.AWS.GameLift.Types.GameSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSession where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionStatus
import Network.AWS.GameLift.Types.GameSessionStatusReason
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
import qualified Network.AWS.Lens as Lens

-- | Properties describing a game session.
--
-- A game session in ACTIVE status can host players. When a game session
-- ends, its status is set to @TERMINATED@.
--
-- Once the session ends, the game session object is retained for 30 days.
-- This means you can reuse idempotency token values after this time. Game
-- session logs are retained for 14 days.
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
--
-- /See:/ 'newGameSession' smart constructor.
data GameSession = GameSession'
  { -- | Set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- You can search for active game sessions based on this custom data with
    -- SearchGameSessions.
    gameProperties :: Core.Maybe [GameProperty],
    -- | Number of players currently in the game session.
    currentPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | Current status of the game session. A game session must have an @ACTIVE@
    -- status to have player sessions.
    status :: Core.Maybe GameSessionStatus,
    -- | Indicates whether or not the game session is accepting new players.
    playerSessionCreationPolicy :: Core.Maybe PlayerSessionCreationPolicy,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    -- | A unique identifier for a player. This ID is used to enforce a resource
    -- protection policy (if one exists), that limits the number of game
    -- sessions a player can create.
    creatorId :: Core.Maybe Core.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet that this game session is running on.
    fleetArn :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet that the game session is running on.
    fleetId :: Core.Maybe Core.Text,
    -- | Information about the matchmaking process that was used to create the
    -- game session. It is in JSON syntax, formatted as a string. In addition
    -- the matchmaking configuration used, it contains data on all players
    -- assigned to the match, including player attributes and team assignments.
    -- For more details on matchmaker data, see
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    -- Matchmaker data is useful when requesting match backfills, and is
    -- updated whenever new players are added during a successful backfill (see
    -- StartMatchBackfill).
    matchmakerData :: Core.Maybe Core.Text,
    -- | Set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Core.Maybe Core.Text,
    -- | A unique identifier for the game session. A game session ARN has the
    -- following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
    gameSessionId :: Core.Maybe Core.Text,
    -- | IP address of the instance that is running the game session. When
    -- connecting to a Amazon GameLift game server, a client needs to reference
    -- an IP address (or DNS name) and port number.
    ipAddress :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Core.Maybe Core.Text,
    -- | Time stamp indicating when this data object was terminated. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    terminationTime :: Core.Maybe Core.POSIX,
    -- | Port number for the game session. To connect to a Amazon GameLift game
    -- server, an app needs both the IP address and port number.
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
    -- | Provides additional information about game session status. @INTERRUPTED@
    -- indicates that the game session was hosted on a spot instance that was
    -- reclaimed, causing the active game session to be terminated.
    statusReason :: Core.Maybe GameSessionStatusReason
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GameSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameProperties', 'gameSession_gameProperties' - Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- You can search for active game sessions based on this custom data with
-- SearchGameSessions.
--
-- 'currentPlayerSessionCount', 'gameSession_currentPlayerSessionCount' - Number of players currently in the game session.
--
-- 'status', 'gameSession_status' - Current status of the game session. A game session must have an @ACTIVE@
-- status to have player sessions.
--
-- 'playerSessionCreationPolicy', 'gameSession_playerSessionCreationPolicy' - Indicates whether or not the game session is accepting new players.
--
-- 'creationTime', 'gameSession_creationTime' - Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'creatorId', 'gameSession_creatorId' - A unique identifier for a player. This ID is used to enforce a resource
-- protection policy (if one exists), that limits the number of game
-- sessions a player can create.
--
-- 'maximumPlayerSessionCount', 'gameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'fleetArn', 'gameSession_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet that this game session is running on.
--
-- 'fleetId', 'gameSession_fleetId' - A unique identifier for a fleet that the game session is running on.
--
-- 'matchmakerData', 'gameSession_matchmakerData' - Information about the matchmaking process that was used to create the
-- game session. It is in JSON syntax, formatted as a string. In addition
-- the matchmaking configuration used, it contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
-- Matchmaker data is useful when requesting match backfills, and is
-- updated whenever new players are added during a successful backfill (see
-- StartMatchBackfill).
--
-- 'gameSessionData', 'gameSession_gameSessionData' - Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionId', 'gameSession_gameSessionId' - A unique identifier for the game session. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
--
-- 'ipAddress', 'gameSession_ipAddress' - IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
--
-- 'name', 'gameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'terminationTime', 'gameSession_terminationTime' - Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'port', 'gameSession_port' - Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'dnsName', 'gameSession_dnsName' - DNS identifier assigned to the instance that is running the game
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
-- 'statusReason', 'gameSession_statusReason' - Provides additional information about game session status. @INTERRUPTED@
-- indicates that the game session was hosted on a spot instance that was
-- reclaimed, causing the active game session to be terminated.
newGameSession ::
  GameSession
newGameSession =
  GameSession'
    { gameProperties = Core.Nothing,
      currentPlayerSessionCount = Core.Nothing,
      status = Core.Nothing,
      playerSessionCreationPolicy = Core.Nothing,
      creationTime = Core.Nothing,
      creatorId = Core.Nothing,
      maximumPlayerSessionCount = Core.Nothing,
      fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      matchmakerData = Core.Nothing,
      gameSessionData = Core.Nothing,
      gameSessionId = Core.Nothing,
      ipAddress = Core.Nothing,
      name = Core.Nothing,
      terminationTime = Core.Nothing,
      port = Core.Nothing,
      dnsName = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- You can search for active game sessions based on this custom data with
-- SearchGameSessions.
gameSession_gameProperties :: Lens.Lens' GameSession (Core.Maybe [GameProperty])
gameSession_gameProperties = Lens.lens (\GameSession' {gameProperties} -> gameProperties) (\s@GameSession' {} a -> s {gameProperties = a} :: GameSession) Core.. Lens.mapping Lens._Coerce

-- | Number of players currently in the game session.
gameSession_currentPlayerSessionCount :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gameSession_currentPlayerSessionCount = Lens.lens (\GameSession' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@GameSession' {} a -> s {currentPlayerSessionCount = a} :: GameSession)

-- | Current status of the game session. A game session must have an @ACTIVE@
-- status to have player sessions.
gameSession_status :: Lens.Lens' GameSession (Core.Maybe GameSessionStatus)
gameSession_status = Lens.lens (\GameSession' {status} -> status) (\s@GameSession' {} a -> s {status = a} :: GameSession)

-- | Indicates whether or not the game session is accepting new players.
gameSession_playerSessionCreationPolicy :: Lens.Lens' GameSession (Core.Maybe PlayerSessionCreationPolicy)
gameSession_playerSessionCreationPolicy = Lens.lens (\GameSession' {playerSessionCreationPolicy} -> playerSessionCreationPolicy) (\s@GameSession' {} a -> s {playerSessionCreationPolicy = a} :: GameSession)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
gameSession_creationTime :: Lens.Lens' GameSession (Core.Maybe Core.UTCTime)
gameSession_creationTime = Lens.lens (\GameSession' {creationTime} -> creationTime) (\s@GameSession' {} a -> s {creationTime = a} :: GameSession) Core.. Lens.mapping Core._Time

-- | A unique identifier for a player. This ID is used to enforce a resource
-- protection policy (if one exists), that limits the number of game
-- sessions a player can create.
gameSession_creatorId :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_creatorId = Lens.lens (\GameSession' {creatorId} -> creatorId) (\s@GameSession' {} a -> s {creatorId = a} :: GameSession)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
gameSession_maximumPlayerSessionCount :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gameSession_maximumPlayerSessionCount = Lens.lens (\GameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@GameSession' {} a -> s {maximumPlayerSessionCount = a} :: GameSession)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet that this game session is running on.
gameSession_fleetArn :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_fleetArn = Lens.lens (\GameSession' {fleetArn} -> fleetArn) (\s@GameSession' {} a -> s {fleetArn = a} :: GameSession)

-- | A unique identifier for a fleet that the game session is running on.
gameSession_fleetId :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_fleetId = Lens.lens (\GameSession' {fleetId} -> fleetId) (\s@GameSession' {} a -> s {fleetId = a} :: GameSession)

-- | Information about the matchmaking process that was used to create the
-- game session. It is in JSON syntax, formatted as a string. In addition
-- the matchmaking configuration used, it contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
-- Matchmaker data is useful when requesting match backfills, and is
-- updated whenever new players are added during a successful backfill (see
-- StartMatchBackfill).
gameSession_matchmakerData :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_matchmakerData = Lens.lens (\GameSession' {matchmakerData} -> matchmakerData) (\s@GameSession' {} a -> s {matchmakerData = a} :: GameSession)

-- | Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSession_gameSessionData :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_gameSessionData = Lens.lens (\GameSession' {gameSessionData} -> gameSessionData) (\s@GameSession' {} a -> s {gameSessionData = a} :: GameSession)

-- | A unique identifier for the game session. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
gameSession_gameSessionId :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_gameSessionId = Lens.lens (\GameSession' {gameSessionId} -> gameSessionId) (\s@GameSession' {} a -> s {gameSessionId = a} :: GameSession)

-- | IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
gameSession_ipAddress :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_ipAddress = Lens.lens (\GameSession' {ipAddress} -> ipAddress) (\s@GameSession' {} a -> s {ipAddress = a} :: GameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
gameSession_name :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_name = Lens.lens (\GameSession' {name} -> name) (\s@GameSession' {} a -> s {name = a} :: GameSession)

-- | Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
gameSession_terminationTime :: Lens.Lens' GameSession (Core.Maybe Core.UTCTime)
gameSession_terminationTime = Lens.lens (\GameSession' {terminationTime} -> terminationTime) (\s@GameSession' {} a -> s {terminationTime = a} :: GameSession) Core.. Lens.mapping Core._Time

-- | Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number.
gameSession_port :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gameSession_port = Lens.lens (\GameSession' {port} -> port) (\s@GameSession' {} a -> s {port = a} :: GameSession)

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
gameSession_dnsName :: Lens.Lens' GameSession (Core.Maybe Core.Text)
gameSession_dnsName = Lens.lens (\GameSession' {dnsName} -> dnsName) (\s@GameSession' {} a -> s {dnsName = a} :: GameSession)

-- | Provides additional information about game session status. @INTERRUPTED@
-- indicates that the game session was hosted on a spot instance that was
-- reclaimed, causing the active game session to be terminated.
gameSession_statusReason :: Lens.Lens' GameSession (Core.Maybe GameSessionStatusReason)
gameSession_statusReason = Lens.lens (\GameSession' {statusReason} -> statusReason) (\s@GameSession' {} a -> s {statusReason = a} :: GameSession)

instance Core.FromJSON GameSession where
  parseJSON =
    Core.withObject
      "GameSession"
      ( \x ->
          GameSession'
            Core.<$> (x Core..:? "GameProperties" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CurrentPlayerSessionCount")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "PlayerSessionCreationPolicy")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CreatorId")
            Core.<*> (x Core..:? "MaximumPlayerSessionCount")
            Core.<*> (x Core..:? "FleetArn")
            Core.<*> (x Core..:? "FleetId")
            Core.<*> (x Core..:? "MatchmakerData")
            Core.<*> (x Core..:? "GameSessionData")
            Core.<*> (x Core..:? "GameSessionId")
            Core.<*> (x Core..:? "IpAddress")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "TerminationTime")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "DnsName")
            Core.<*> (x Core..:? "StatusReason")
      )

instance Core.Hashable GameSession

instance Core.NFData GameSession
