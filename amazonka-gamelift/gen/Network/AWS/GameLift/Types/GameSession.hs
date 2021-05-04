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
-- Module      : Network.AWS.GameLift.Types.GameSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSession where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionStatus
import Network.AWS.GameLift.Types.GameSessionStatusReason
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | Number of players currently in the game session.
    currentPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | Current status of the game session. A game session must have an @ACTIVE@
    -- status to have player sessions.
    status :: Prelude.Maybe GameSessionStatus,
    -- | Indicates whether or not the game session is accepting new players.
    playerSessionCreationPolicy :: Prelude.Maybe PlayerSessionCreationPolicy,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A unique identifier for a player. This ID is used to enforce a resource
    -- protection policy (if one exists), that limits the number of game
    -- sessions a player can create.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet that this game session is running on.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet that the game session is running on.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the matchmaking process that was used to create the
    -- game session. It is in JSON syntax, formatted as a string. In addition
    -- the matchmaking configuration used, it contains data on all players
    -- assigned to the match, including player attributes and team assignments.
    -- For more details on matchmaker data, see
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    -- Matchmaker data is useful when requesting match backfills, and is
    -- updated whenever new players are added during a successful backfill (see
    -- StartMatchBackfill).
    matchmakerData :: Prelude.Maybe Prelude.Text,
    -- | Set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session. A game session ARN has the
    -- following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | IP address of the instance that is running the game session. When
    -- connecting to a Amazon GameLift game server, a client needs to reference
    -- an IP address (or DNS name) and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this data object was terminated. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    terminationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Port number for the game session. To connect to a Amazon GameLift game
    -- server, an app needs both the IP address and port number.
    port :: Prelude.Maybe Prelude.Natural,
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
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | Provides additional information about game session status. @INTERRUPTED@
    -- indicates that the game session was hosted on a spot instance that was
    -- reclaimed, causing the active game session to be terminated.
    statusReason :: Prelude.Maybe GameSessionStatusReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { gameProperties = Prelude.Nothing,
      currentPlayerSessionCount = Prelude.Nothing,
      status = Prelude.Nothing,
      playerSessionCreationPolicy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      creatorId = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      matchmakerData = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      name = Prelude.Nothing,
      terminationTime = Prelude.Nothing,
      port = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- You can search for active game sessions based on this custom data with
-- SearchGameSessions.
gameSession_gameProperties :: Lens.Lens' GameSession (Prelude.Maybe [GameProperty])
gameSession_gameProperties = Lens.lens (\GameSession' {gameProperties} -> gameProperties) (\s@GameSession' {} a -> s {gameProperties = a} :: GameSession) Prelude.. Lens.mapping Prelude._Coerce

-- | Number of players currently in the game session.
gameSession_currentPlayerSessionCount :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
gameSession_currentPlayerSessionCount = Lens.lens (\GameSession' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@GameSession' {} a -> s {currentPlayerSessionCount = a} :: GameSession)

-- | Current status of the game session. A game session must have an @ACTIVE@
-- status to have player sessions.
gameSession_status :: Lens.Lens' GameSession (Prelude.Maybe GameSessionStatus)
gameSession_status = Lens.lens (\GameSession' {status} -> status) (\s@GameSession' {} a -> s {status = a} :: GameSession)

-- | Indicates whether or not the game session is accepting new players.
gameSession_playerSessionCreationPolicy :: Lens.Lens' GameSession (Prelude.Maybe PlayerSessionCreationPolicy)
gameSession_playerSessionCreationPolicy = Lens.lens (\GameSession' {playerSessionCreationPolicy} -> playerSessionCreationPolicy) (\s@GameSession' {} a -> s {playerSessionCreationPolicy = a} :: GameSession)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
gameSession_creationTime :: Lens.Lens' GameSession (Prelude.Maybe Prelude.UTCTime)
gameSession_creationTime = Lens.lens (\GameSession' {creationTime} -> creationTime) (\s@GameSession' {} a -> s {creationTime = a} :: GameSession) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier for a player. This ID is used to enforce a resource
-- protection policy (if one exists), that limits the number of game
-- sessions a player can create.
gameSession_creatorId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_creatorId = Lens.lens (\GameSession' {creatorId} -> creatorId) (\s@GameSession' {} a -> s {creatorId = a} :: GameSession)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
gameSession_maximumPlayerSessionCount :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
gameSession_maximumPlayerSessionCount = Lens.lens (\GameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@GameSession' {} a -> s {maximumPlayerSessionCount = a} :: GameSession)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet that this game session is running on.
gameSession_fleetArn :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_fleetArn = Lens.lens (\GameSession' {fleetArn} -> fleetArn) (\s@GameSession' {} a -> s {fleetArn = a} :: GameSession)

-- | A unique identifier for a fleet that the game session is running on.
gameSession_fleetId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
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
gameSession_matchmakerData :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_matchmakerData = Lens.lens (\GameSession' {matchmakerData} -> matchmakerData) (\s@GameSession' {} a -> s {matchmakerData = a} :: GameSession)

-- | Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSession_gameSessionData :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_gameSessionData = Lens.lens (\GameSession' {gameSessionData} -> gameSessionData) (\s@GameSession' {} a -> s {gameSessionData = a} :: GameSession)

-- | A unique identifier for the game session. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
gameSession_gameSessionId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_gameSessionId = Lens.lens (\GameSession' {gameSessionId} -> gameSessionId) (\s@GameSession' {} a -> s {gameSessionId = a} :: GameSession)

-- | IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
gameSession_ipAddress :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_ipAddress = Lens.lens (\GameSession' {ipAddress} -> ipAddress) (\s@GameSession' {} a -> s {ipAddress = a} :: GameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
gameSession_name :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_name = Lens.lens (\GameSession' {name} -> name) (\s@GameSession' {} a -> s {name = a} :: GameSession)

-- | Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
gameSession_terminationTime :: Lens.Lens' GameSession (Prelude.Maybe Prelude.UTCTime)
gameSession_terminationTime = Lens.lens (\GameSession' {terminationTime} -> terminationTime) (\s@GameSession' {} a -> s {terminationTime = a} :: GameSession) Prelude.. Lens.mapping Prelude._Time

-- | Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number.
gameSession_port :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
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
gameSession_dnsName :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_dnsName = Lens.lens (\GameSession' {dnsName} -> dnsName) (\s@GameSession' {} a -> s {dnsName = a} :: GameSession)

-- | Provides additional information about game session status. @INTERRUPTED@
-- indicates that the game session was hosted on a spot instance that was
-- reclaimed, causing the active game session to be terminated.
gameSession_statusReason :: Lens.Lens' GameSession (Prelude.Maybe GameSessionStatusReason)
gameSession_statusReason = Lens.lens (\GameSession' {statusReason} -> statusReason) (\s@GameSession' {} a -> s {statusReason = a} :: GameSession)

instance Prelude.FromJSON GameSession where
  parseJSON =
    Prelude.withObject
      "GameSession"
      ( \x ->
          GameSession'
            Prelude.<$> ( x Prelude..:? "GameProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CurrentPlayerSessionCount")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "PlayerSessionCreationPolicy")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "CreatorId")
            Prelude.<*> (x Prelude..:? "MaximumPlayerSessionCount")
            Prelude.<*> (x Prelude..:? "FleetArn")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "MatchmakerData")
            Prelude.<*> (x Prelude..:? "GameSessionData")
            Prelude.<*> (x Prelude..:? "GameSessionId")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "TerminationTime")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "DnsName")
            Prelude.<*> (x Prelude..:? "StatusReason")
      )

instance Prelude.Hashable GameSession

instance Prelude.NFData GameSession
