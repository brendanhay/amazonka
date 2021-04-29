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
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacement where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionPlacementState
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.PlayerLatency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Object that describes a StartGameSessionPlacement request. This object
-- includes the full details of the original request plus the current
-- status and start\/end time stamps.
--
-- Game session placement-related operations include:
--
-- -   StartGameSessionPlacement
--
-- -   DescribeGameSessionPlacement
--
-- -   StopGameSessionPlacement
--
-- /See:/ 'newGameSessionPlacement' smart constructor.
data GameSessionPlacement = GameSessionPlacement'
  { -- | Set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | Current status of the game session placement request.
    --
    -- -   __PENDING__ -- The placement request is currently in the queue
    --     waiting to be processed.
    --
    -- -   __FULFILLED__ -- A new game session and player sessions (if
    --     requested) have been successfully created. Values for
    --     /GameSessionArn/ and /GameSessionRegion/ are available.
    --
    -- -   __CANCELLED__ -- The placement request was canceled with a call to
    --     StopGameSessionPlacement.
    --
    -- -   __TIMED_OUT__ -- A new game session was not successfully created
    --     before the time limit expired. You can resubmit the placement
    --     request as needed.
    --
    -- -   __FAILED__ -- GameLift is not able to complete the process of
    --     placing the game session. Common reasons are the game session
    --     terminated before the placement process was completed, or an
    --     unexpected internal error.
    status :: Prelude.Maybe GameSessionPlacementState,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    gameSessionQueueName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | Information on the matchmaking process for this game. Data is in JSON
    -- syntax, formatted as a string. It identifies the matchmaking
    -- configuration used to create the match, and contains data on all players
    -- assigned to the match, including player attributes and team assignments.
    -- For more details on matchmaker data, see
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    matchmakerData :: Prelude.Maybe Prelude.Text,
    -- | Set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this request was placed in the queue. Format
    -- is a number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | A unique identifier for the game session. This value is set once the new
    -- game session is placed (placement status is @FULFILLED@).
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the game session created by this placement request. This
    -- value is set once the new game session is placed (placement status is
    -- @FULFILLED@). This identifier is unique across all Regions. You can use
    -- this value as a @GameSessionId@ value as needed.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this request was completed, canceled, or
    -- timed out.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | IP address of the instance that is running the game session. When
    -- connecting to a Amazon GameLift game server, a client needs to reference
    -- an IP address (or DNS name) and port number. This value is set once the
    -- new game session is placed (placement status is @FULFILLED@).
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    gameSessionName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a game session placement.
    placementId :: Prelude.Maybe Prelude.Text,
    -- | A collection of information on player sessions created in response to
    -- the game session placement request. These player sessions are created
    -- only once a new game session is successfully placed (placement status is
    -- @FULFILLED@). This information includes the player ID (as provided in
    -- the placement request) and the corresponding player session ID. Retrieve
    -- full player sessions by calling DescribePlayerSessions with the player
    -- session ID.
    placedPlayerSessions :: Prelude.Maybe [PlacedPlayerSession],
    -- | Port number for the game session. To connect to a Amazon GameLift game
    -- server, an app needs both the IP address and port number. This value is
    -- set once the new game session is placed (placement status is
    -- @FULFILLED@).
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
    -- | Name of the Region where the game session created by this placement
    -- request is running. This value is set once the new game session is
    -- placed (placement status is @FULFILLED@).
    gameSessionRegion :: Prelude.Maybe Prelude.Text,
    -- | Set of values, expressed in milliseconds, indicating the amount of
    -- latency that a player experiences when connected to AWS Regions.
    playerLatencies :: Prelude.Maybe [PlayerLatency]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameProperties', 'gameSessionPlacement_gameProperties' - Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'status', 'gameSessionPlacement_status' - Current status of the game session placement request.
--
-- -   __PENDING__ -- The placement request is currently in the queue
--     waiting to be processed.
--
-- -   __FULFILLED__ -- A new game session and player sessions (if
--     requested) have been successfully created. Values for
--     /GameSessionArn/ and /GameSessionRegion/ are available.
--
-- -   __CANCELLED__ -- The placement request was canceled with a call to
--     StopGameSessionPlacement.
--
-- -   __TIMED_OUT__ -- A new game session was not successfully created
--     before the time limit expired. You can resubmit the placement
--     request as needed.
--
-- -   __FAILED__ -- GameLift is not able to complete the process of
--     placing the game session. Common reasons are the game session
--     terminated before the placement process was completed, or an
--     unexpected internal error.
--
-- 'gameSessionQueueName', 'gameSessionPlacement_gameSessionQueueName' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
--
-- 'maximumPlayerSessionCount', 'gameSessionPlacement_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'matchmakerData', 'gameSessionPlacement_matchmakerData' - Information on the matchmaking process for this game. Data is in JSON
-- syntax, formatted as a string. It identifies the matchmaking
-- configuration used to create the match, and contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
-- 'gameSessionData', 'gameSessionPlacement_gameSessionData' - Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'startTime', 'gameSessionPlacement_startTime' - Time stamp indicating when this request was placed in the queue. Format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'gameSessionId', 'gameSessionPlacement_gameSessionId' - A unique identifier for the game session. This value is set once the new
-- game session is placed (placement status is @FULFILLED@).
--
-- 'gameSessionArn', 'gameSessionPlacement_gameSessionArn' - Identifier for the game session created by this placement request. This
-- value is set once the new game session is placed (placement status is
-- @FULFILLED@). This identifier is unique across all Regions. You can use
-- this value as a @GameSessionId@ value as needed.
--
-- 'endTime', 'gameSessionPlacement_endTime' - Time stamp indicating when this request was completed, canceled, or
-- timed out.
--
-- 'ipAddress', 'gameSessionPlacement_ipAddress' - IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number. This value is set once the
-- new game session is placed (placement status is @FULFILLED@).
--
-- 'gameSessionName', 'gameSessionPlacement_gameSessionName' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'placementId', 'gameSessionPlacement_placementId' - A unique identifier for a game session placement.
--
-- 'placedPlayerSessions', 'gameSessionPlacement_placedPlayerSessions' - A collection of information on player sessions created in response to
-- the game session placement request. These player sessions are created
-- only once a new game session is successfully placed (placement status is
-- @FULFILLED@). This information includes the player ID (as provided in
-- the placement request) and the corresponding player session ID. Retrieve
-- full player sessions by calling DescribePlayerSessions with the player
-- session ID.
--
-- 'port', 'gameSessionPlacement_port' - Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
--
-- 'dnsName', 'gameSessionPlacement_dnsName' - DNS identifier assigned to the instance that is running the game
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
-- 'gameSessionRegion', 'gameSessionPlacement_gameSessionRegion' - Name of the Region where the game session created by this placement
-- request is running. This value is set once the new game session is
-- placed (placement status is @FULFILLED@).
--
-- 'playerLatencies', 'gameSessionPlacement_playerLatencies' - Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions.
newGameSessionPlacement ::
  GameSessionPlacement
newGameSessionPlacement =
  GameSessionPlacement'
    { gameProperties =
        Prelude.Nothing,
      status = Prelude.Nothing,
      gameSessionQueueName = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      matchmakerData = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      startTime = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      gameSessionArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      gameSessionName = Prelude.Nothing,
      placementId = Prelude.Nothing,
      placedPlayerSessions = Prelude.Nothing,
      port = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      gameSessionRegion = Prelude.Nothing,
      playerLatencies = Prelude.Nothing
    }

-- | Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSessionPlacement_gameProperties :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [GameProperty])
gameSessionPlacement_gameProperties = Lens.lens (\GameSessionPlacement' {gameProperties} -> gameProperties) (\s@GameSessionPlacement' {} a -> s {gameProperties = a} :: GameSessionPlacement) Prelude.. Lens.mapping Prelude._Coerce

-- | Current status of the game session placement request.
--
-- -   __PENDING__ -- The placement request is currently in the queue
--     waiting to be processed.
--
-- -   __FULFILLED__ -- A new game session and player sessions (if
--     requested) have been successfully created. Values for
--     /GameSessionArn/ and /GameSessionRegion/ are available.
--
-- -   __CANCELLED__ -- The placement request was canceled with a call to
--     StopGameSessionPlacement.
--
-- -   __TIMED_OUT__ -- A new game session was not successfully created
--     before the time limit expired. You can resubmit the placement
--     request as needed.
--
-- -   __FAILED__ -- GameLift is not able to complete the process of
--     placing the game session. Common reasons are the game session
--     terminated before the placement process was completed, or an
--     unexpected internal error.
gameSessionPlacement_status :: Lens.Lens' GameSessionPlacement (Prelude.Maybe GameSessionPlacementState)
gameSessionPlacement_status = Lens.lens (\GameSessionPlacement' {status} -> status) (\s@GameSessionPlacement' {} a -> s {status = a} :: GameSessionPlacement)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
gameSessionPlacement_gameSessionQueueName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionQueueName = Lens.lens (\GameSessionPlacement' {gameSessionQueueName} -> gameSessionQueueName) (\s@GameSessionPlacement' {} a -> s {gameSessionQueueName = a} :: GameSessionPlacement)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
gameSessionPlacement_maximumPlayerSessionCount :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Natural)
gameSessionPlacement_maximumPlayerSessionCount = Lens.lens (\GameSessionPlacement' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@GameSessionPlacement' {} a -> s {maximumPlayerSessionCount = a} :: GameSessionPlacement)

-- | Information on the matchmaking process for this game. Data is in JSON
-- syntax, formatted as a string. It identifies the matchmaking
-- configuration used to create the match, and contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
gameSessionPlacement_matchmakerData :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_matchmakerData = Lens.lens (\GameSessionPlacement' {matchmakerData} -> matchmakerData) (\s@GameSessionPlacement' {} a -> s {matchmakerData = a} :: GameSessionPlacement)

-- | Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSessionPlacement_gameSessionData :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionData = Lens.lens (\GameSessionPlacement' {gameSessionData} -> gameSessionData) (\s@GameSessionPlacement' {} a -> s {gameSessionData = a} :: GameSessionPlacement)

-- | Time stamp indicating when this request was placed in the queue. Format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
gameSessionPlacement_startTime :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.UTCTime)
gameSessionPlacement_startTime = Lens.lens (\GameSessionPlacement' {startTime} -> startTime) (\s@GameSessionPlacement' {} a -> s {startTime = a} :: GameSessionPlacement) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier for the game session. This value is set once the new
-- game session is placed (placement status is @FULFILLED@).
gameSessionPlacement_gameSessionId :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionId = Lens.lens (\GameSessionPlacement' {gameSessionId} -> gameSessionId) (\s@GameSessionPlacement' {} a -> s {gameSessionId = a} :: GameSessionPlacement)

-- | Identifier for the game session created by this placement request. This
-- value is set once the new game session is placed (placement status is
-- @FULFILLED@). This identifier is unique across all Regions. You can use
-- this value as a @GameSessionId@ value as needed.
gameSessionPlacement_gameSessionArn :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionArn = Lens.lens (\GameSessionPlacement' {gameSessionArn} -> gameSessionArn) (\s@GameSessionPlacement' {} a -> s {gameSessionArn = a} :: GameSessionPlacement)

-- | Time stamp indicating when this request was completed, canceled, or
-- timed out.
gameSessionPlacement_endTime :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.UTCTime)
gameSessionPlacement_endTime = Lens.lens (\GameSessionPlacement' {endTime} -> endTime) (\s@GameSessionPlacement' {} a -> s {endTime = a} :: GameSessionPlacement) Prelude.. Lens.mapping Prelude._Time

-- | IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number. This value is set once the
-- new game session is placed (placement status is @FULFILLED@).
gameSessionPlacement_ipAddress :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_ipAddress = Lens.lens (\GameSessionPlacement' {ipAddress} -> ipAddress) (\s@GameSessionPlacement' {} a -> s {ipAddress = a} :: GameSessionPlacement)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
gameSessionPlacement_gameSessionName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionName = Lens.lens (\GameSessionPlacement' {gameSessionName} -> gameSessionName) (\s@GameSessionPlacement' {} a -> s {gameSessionName = a} :: GameSessionPlacement)

-- | A unique identifier for a game session placement.
gameSessionPlacement_placementId :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_placementId = Lens.lens (\GameSessionPlacement' {placementId} -> placementId) (\s@GameSessionPlacement' {} a -> s {placementId = a} :: GameSessionPlacement)

-- | A collection of information on player sessions created in response to
-- the game session placement request. These player sessions are created
-- only once a new game session is successfully placed (placement status is
-- @FULFILLED@). This information includes the player ID (as provided in
-- the placement request) and the corresponding player session ID. Retrieve
-- full player sessions by calling DescribePlayerSessions with the player
-- session ID.
gameSessionPlacement_placedPlayerSessions :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [PlacedPlayerSession])
gameSessionPlacement_placedPlayerSessions = Lens.lens (\GameSessionPlacement' {placedPlayerSessions} -> placedPlayerSessions) (\s@GameSessionPlacement' {} a -> s {placedPlayerSessions = a} :: GameSessionPlacement) Prelude.. Lens.mapping Prelude._Coerce

-- | Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
gameSessionPlacement_port :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Natural)
gameSessionPlacement_port = Lens.lens (\GameSessionPlacement' {port} -> port) (\s@GameSessionPlacement' {} a -> s {port = a} :: GameSessionPlacement)

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
gameSessionPlacement_dnsName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_dnsName = Lens.lens (\GameSessionPlacement' {dnsName} -> dnsName) (\s@GameSessionPlacement' {} a -> s {dnsName = a} :: GameSessionPlacement)

-- | Name of the Region where the game session created by this placement
-- request is running. This value is set once the new game session is
-- placed (placement status is @FULFILLED@).
gameSessionPlacement_gameSessionRegion :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionRegion = Lens.lens (\GameSessionPlacement' {gameSessionRegion} -> gameSessionRegion) (\s@GameSessionPlacement' {} a -> s {gameSessionRegion = a} :: GameSessionPlacement)

-- | Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions.
gameSessionPlacement_playerLatencies :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [PlayerLatency])
gameSessionPlacement_playerLatencies = Lens.lens (\GameSessionPlacement' {playerLatencies} -> playerLatencies) (\s@GameSessionPlacement' {} a -> s {playerLatencies = a} :: GameSessionPlacement) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON GameSessionPlacement where
  parseJSON =
    Prelude.withObject
      "GameSessionPlacement"
      ( \x ->
          GameSessionPlacement'
            Prelude.<$> ( x Prelude..:? "GameProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "GameSessionQueueName")
            Prelude.<*> (x Prelude..:? "MaximumPlayerSessionCount")
            Prelude.<*> (x Prelude..:? "MatchmakerData")
            Prelude.<*> (x Prelude..:? "GameSessionData")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "GameSessionId")
            Prelude.<*> (x Prelude..:? "GameSessionArn")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "GameSessionName")
            Prelude.<*> (x Prelude..:? "PlacementId")
            Prelude.<*> ( x Prelude..:? "PlacedPlayerSessions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "DnsName")
            Prelude.<*> (x Prelude..:? "GameSessionRegion")
            Prelude.<*> ( x Prelude..:? "PlayerLatencies"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GameSessionPlacement

instance Prelude.NFData GameSessionPlacement
