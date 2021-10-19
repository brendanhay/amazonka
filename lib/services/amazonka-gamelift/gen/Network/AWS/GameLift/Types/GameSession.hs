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
-- __Related actions__
--
-- CreateGameSession | DescribeGameSessions | DescribeGameSessionDetails |
-- SearchGameSessions | UpdateGameSession | GetGameSessionLogUrl |
-- StartGameSessionPlacement | DescribeGameSessionPlacement |
-- StopGameSessionPlacement |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newGameSession' smart constructor.
data GameSession = GameSession'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Current status of the game session. A game session must have an @ACTIVE@
    -- status to have player sessions.
    status :: Prelude.Maybe GameSessionStatus,
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session. You can
    -- search for active game sessions based on this custom data with
    -- SearchGameSessions.
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | The IP address of the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The fleet location where the game session is running. This value might
    -- specify the fleet\'s home Region or a remote location. Location is
    -- expressed as an AWS Region code such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session. A game session ARN has the
    -- following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
    gameSessionId :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift fleet that this game session is running on.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | A time stamp indicating when this data object was terminated. Format is
    -- a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    terminationTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether or not the game session is accepting new players.
    playerSessionCreationPolicy :: Prelude.Maybe PlayerSessionCreationPolicy,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Number of players currently in the game session.
    currentPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | Provides additional information about game session status. @INTERRUPTED@
    -- indicates that the game session was hosted on a spot instance that was
    -- reclaimed, causing the active game session to be terminated.
    statusReason :: Prelude.Maybe GameSessionStatusReason,
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session.
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that the game session is running on.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The DNS identifier assigned to the instance that is running the game
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
    -- | A unique identifier for a player. This ID is used to enforce a resource
    -- protection policy (if one exists), that limits the number of game
    -- sessions a player can create.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The port number for the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'gameSession_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'status', 'gameSession_status' - Current status of the game session. A game session must have an @ACTIVE@
-- status to have player sessions.
--
-- 'gameProperties', 'gameSession_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session. You can
-- search for active game sessions based on this custom data with
-- SearchGameSessions.
--
-- 'ipAddress', 'gameSession_ipAddress' - The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'location', 'gameSession_location' - The fleet location where the game session is running. This value might
-- specify the fleet\'s home Region or a remote location. Location is
-- expressed as an AWS Region code such as @us-west-2@.
--
-- 'gameSessionId', 'gameSession_gameSessionId' - A unique identifier for the game session. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
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
-- 'fleetArn', 'gameSession_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet that this game session is running on.
--
-- 'maximumPlayerSessionCount', 'gameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'terminationTime', 'gameSession_terminationTime' - A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'playerSessionCreationPolicy', 'gameSession_playerSessionCreationPolicy' - Indicates whether or not the game session is accepting new players.
--
-- 'name', 'gameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'currentPlayerSessionCount', 'gameSession_currentPlayerSessionCount' - Number of players currently in the game session.
--
-- 'statusReason', 'gameSession_statusReason' - Provides additional information about game session status. @INTERRUPTED@
-- indicates that the game session was hosted on a spot instance that was
-- reclaimed, causing the active game session to be terminated.
--
-- 'gameSessionData', 'gameSession_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session.
--
-- 'fleetId', 'gameSession_fleetId' - A unique identifier for the fleet that the game session is running on.
--
-- 'dnsName', 'gameSession_dnsName' - The DNS identifier assigned to the instance that is running the game
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
-- 'creatorId', 'gameSession_creatorId' - A unique identifier for a player. This ID is used to enforce a resource
-- protection policy (if one exists), that limits the number of game
-- sessions a player can create.
--
-- 'port', 'gameSession_port' - The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
newGameSession ::
  GameSession
newGameSession =
  GameSession'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      gameProperties = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      location = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      matchmakerData = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      terminationTime = Prelude.Nothing,
      playerSessionCreationPolicy = Prelude.Nothing,
      name = Prelude.Nothing,
      currentPlayerSessionCount = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      creatorId = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
gameSession_creationTime :: Lens.Lens' GameSession (Prelude.Maybe Prelude.UTCTime)
gameSession_creationTime = Lens.lens (\GameSession' {creationTime} -> creationTime) (\s@GameSession' {} a -> s {creationTime = a} :: GameSession) Prelude.. Lens.mapping Core._Time

-- | Current status of the game session. A game session must have an @ACTIVE@
-- status to have player sessions.
gameSession_status :: Lens.Lens' GameSession (Prelude.Maybe GameSessionStatus)
gameSession_status = Lens.lens (\GameSession' {status} -> status) (\s@GameSession' {} a -> s {status = a} :: GameSession)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session. You can
-- search for active game sessions based on this custom data with
-- SearchGameSessions.
gameSession_gameProperties :: Lens.Lens' GameSession (Prelude.Maybe [GameProperty])
gameSession_gameProperties = Lens.lens (\GameSession' {gameProperties} -> gameProperties) (\s@GameSession' {} a -> s {gameProperties = a} :: GameSession) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSession_ipAddress :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_ipAddress = Lens.lens (\GameSession' {ipAddress} -> ipAddress) (\s@GameSession' {} a -> s {ipAddress = a} :: GameSession)

-- | The fleet location where the game session is running. This value might
-- specify the fleet\'s home Region or a remote location. Location is
-- expressed as an AWS Region code such as @us-west-2@.
gameSession_location :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_location = Lens.lens (\GameSession' {location} -> location) (\s@GameSession' {} a -> s {location = a} :: GameSession)

-- | A unique identifier for the game session. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
gameSession_gameSessionId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_gameSessionId = Lens.lens (\GameSession' {gameSessionId} -> gameSessionId) (\s@GameSession' {} a -> s {gameSessionId = a} :: GameSession)

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

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet that this game session is running on.
gameSession_fleetArn :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_fleetArn = Lens.lens (\GameSession' {fleetArn} -> fleetArn) (\s@GameSession' {} a -> s {fleetArn = a} :: GameSession)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
gameSession_maximumPlayerSessionCount :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
gameSession_maximumPlayerSessionCount = Lens.lens (\GameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@GameSession' {} a -> s {maximumPlayerSessionCount = a} :: GameSession)

-- | A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
gameSession_terminationTime :: Lens.Lens' GameSession (Prelude.Maybe Prelude.UTCTime)
gameSession_terminationTime = Lens.lens (\GameSession' {terminationTime} -> terminationTime) (\s@GameSession' {} a -> s {terminationTime = a} :: GameSession) Prelude.. Lens.mapping Core._Time

-- | Indicates whether or not the game session is accepting new players.
gameSession_playerSessionCreationPolicy :: Lens.Lens' GameSession (Prelude.Maybe PlayerSessionCreationPolicy)
gameSession_playerSessionCreationPolicy = Lens.lens (\GameSession' {playerSessionCreationPolicy} -> playerSessionCreationPolicy) (\s@GameSession' {} a -> s {playerSessionCreationPolicy = a} :: GameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
gameSession_name :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_name = Lens.lens (\GameSession' {name} -> name) (\s@GameSession' {} a -> s {name = a} :: GameSession)

-- | Number of players currently in the game session.
gameSession_currentPlayerSessionCount :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
gameSession_currentPlayerSessionCount = Lens.lens (\GameSession' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@GameSession' {} a -> s {currentPlayerSessionCount = a} :: GameSession)

-- | Provides additional information about game session status. @INTERRUPTED@
-- indicates that the game session was hosted on a spot instance that was
-- reclaimed, causing the active game session to be terminated.
gameSession_statusReason :: Lens.Lens' GameSession (Prelude.Maybe GameSessionStatusReason)
gameSession_statusReason = Lens.lens (\GameSession' {statusReason} -> statusReason) (\s@GameSession' {} a -> s {statusReason = a} :: GameSession)

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session.
gameSession_gameSessionData :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_gameSessionData = Lens.lens (\GameSession' {gameSessionData} -> gameSessionData) (\s@GameSession' {} a -> s {gameSessionData = a} :: GameSession)

-- | A unique identifier for the fleet that the game session is running on.
gameSession_fleetId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_fleetId = Lens.lens (\GameSession' {fleetId} -> fleetId) (\s@GameSession' {} a -> s {fleetId = a} :: GameSession)

-- | The DNS identifier assigned to the instance that is running the game
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

-- | A unique identifier for a player. This ID is used to enforce a resource
-- protection policy (if one exists), that limits the number of game
-- sessions a player can create.
gameSession_creatorId :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Text)
gameSession_creatorId = Lens.lens (\GameSession' {creatorId} -> creatorId) (\s@GameSession' {} a -> s {creatorId = a} :: GameSession)

-- | The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSession_port :: Lens.Lens' GameSession (Prelude.Maybe Prelude.Natural)
gameSession_port = Lens.lens (\GameSession' {port} -> port) (\s@GameSession' {} a -> s {port = a} :: GameSession)

instance Core.FromJSON GameSession where
  parseJSON =
    Core.withObject
      "GameSession"
      ( \x ->
          GameSession'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "GameProperties" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "GameSessionId")
            Prelude.<*> (x Core..:? "MatchmakerData")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "MaximumPlayerSessionCount")
            Prelude.<*> (x Core..:? "TerminationTime")
            Prelude.<*> (x Core..:? "PlayerSessionCreationPolicy")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CurrentPlayerSessionCount")
            Prelude.<*> (x Core..:? "StatusReason")
            Prelude.<*> (x Core..:? "GameSessionData")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "DnsName")
            Prelude.<*> (x Core..:? "CreatorId")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable GameSession

instance Prelude.NFData GameSession
