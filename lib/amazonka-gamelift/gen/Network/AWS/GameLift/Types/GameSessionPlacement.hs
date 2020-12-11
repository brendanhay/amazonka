-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacement
  ( GameSessionPlacement (..),

    -- * Smart constructor
    mkGameSessionPlacement,

    -- * Lenses
    gspStatus,
    gspPlacementId,
    gspGameProperties,
    gspIPAddress,
    gspGameSessionName,
    gspStartTime,
    gspGameSessionId,
    gspGameSessionRegion,
    gspMatchmakerData,
    gspMaximumPlayerSessionCount,
    gspEndTime,
    gspGameSessionARN,
    gspPlayerLatencies,
    gspGameSessionData,
    gspDNSName,
    gspGameSessionQueueName,
    gspPlacedPlayerSessions,
    gspPort,
  )
where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionPlacementState
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.PlayerLatency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Object that describes a 'StartGameSessionPlacement' request. This object includes the full details of the original request plus the current status and start/end time stamps.
--
-- Game session placement-related operations include:
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
--
--
--
-- /See:/ 'mkGameSessionPlacement' smart constructor.
data GameSessionPlacement = GameSessionPlacement'
  { status ::
      Lude.Maybe GameSessionPlacementState,
    placementId :: Lude.Maybe Lude.Text,
    gameProperties :: Lude.Maybe [GameProperty],
    ipAddress :: Lude.Maybe Lude.Text,
    gameSessionName :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    gameSessionId :: Lude.Maybe Lude.Text,
    gameSessionRegion :: Lude.Maybe Lude.Text,
    matchmakerData :: Lude.Maybe Lude.Text,
    maximumPlayerSessionCount ::
      Lude.Maybe Lude.Natural,
    endTime :: Lude.Maybe Lude.Timestamp,
    gameSessionARN :: Lude.Maybe Lude.Text,
    playerLatencies :: Lude.Maybe [PlayerLatency],
    gameSessionData :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text,
    gameSessionQueueName :: Lude.Maybe Lude.Text,
    placedPlayerSessions ::
      Lude.Maybe [PlacedPlayerSession],
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameSessionPlacement' with the minimum fields required to make a request.
--
-- * 'dnsName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
-- * 'endTime' - Time stamp indicating when this request was completed, canceled, or timed out.
-- * 'gameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'gameSessionARN' - Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
-- * 'gameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'gameSessionId' - A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
-- * 'gameSessionName' - A descriptive label that is associated with a game session. Session names do not need to be unique.
-- * 'gameSessionQueueName' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
-- * 'gameSessionRegion' - Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
-- * 'ipAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
-- * 'matchmakerData' - Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
-- * 'maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
-- * 'placedPlayerSessions' - A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
-- * 'placementId' - A unique identifier for a game session placement.
-- * 'playerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
-- * 'port' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
-- * 'startTime' - Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'status' - Current status of the game session placement request.
--
--
--     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.
--
--
--     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.
--
--
--     * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .
--
--
--     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
--
--
--     * __FAILED__ -- GameLift is not able to complete the process of placing the game session. Common reasons are the game session terminated before the placement process was completed, or an unexpected internal error.
mkGameSessionPlacement ::
  GameSessionPlacement
mkGameSessionPlacement =
  GameSessionPlacement'
    { status = Lude.Nothing,
      placementId = Lude.Nothing,
      gameProperties = Lude.Nothing,
      ipAddress = Lude.Nothing,
      gameSessionName = Lude.Nothing,
      startTime = Lude.Nothing,
      gameSessionId = Lude.Nothing,
      gameSessionRegion = Lude.Nothing,
      matchmakerData = Lude.Nothing,
      maximumPlayerSessionCount = Lude.Nothing,
      endTime = Lude.Nothing,
      gameSessionARN = Lude.Nothing,
      playerLatencies = Lude.Nothing,
      gameSessionData = Lude.Nothing,
      dnsName = Lude.Nothing,
      gameSessionQueueName = Lude.Nothing,
      placedPlayerSessions = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Current status of the game session placement request.
--
--
--     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.
--
--
--     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.
--
--
--     * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .
--
--
--     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
--
--
--     * __FAILED__ -- GameLift is not able to complete the process of placing the game session. Common reasons are the game session terminated before the placement process was completed, or an unexpected internal error.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspStatus :: Lens.Lens' GameSessionPlacement (Lude.Maybe GameSessionPlacementState)
gspStatus = Lens.lens (status :: GameSessionPlacement -> Lude.Maybe GameSessionPlacementState) (\s a -> s {status = a} :: GameSessionPlacement)
{-# DEPRECATED gspStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique identifier for a game session placement.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlacementId :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspPlacementId = Lens.lens (placementId :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {placementId = a} :: GameSessionPlacement)
{-# DEPRECATED gspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameProperties :: Lens.Lens' GameSessionPlacement (Lude.Maybe [GameProperty])
gspGameProperties = Lens.lens (gameProperties :: GameSessionPlacement -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspIPAddress :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspIPAddress = Lens.lens (ipAddress :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: GameSessionPlacement)
{-# DEPRECATED gspIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'gameSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionName :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionName = Lens.lens (gameSessionName :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionName = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionName "Use generic-lens or generic-optics with 'gameSessionName' instead." #-}

-- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspStartTime :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Timestamp)
gspStartTime = Lens.lens (startTime :: GameSessionPlacement -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GameSessionPlacement)
{-# DEPRECATED gspStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionId :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionId = Lens.lens (gameSessionId :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'gameSessionRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionRegion :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionRegion = Lens.lens (gameSessionRegion :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionRegion = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionRegion "Use generic-lens or generic-optics with 'gameSessionRegion' instead." #-}

-- | Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
--
-- /Note:/ Consider using 'matchmakerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspMatchmakerData :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspMatchmakerData = Lens.lens (matchmakerData :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {matchmakerData = a} :: GameSessionPlacement)
{-# DEPRECATED gspMatchmakerData "Use generic-lens or generic-optics with 'matchmakerData' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspMaximumPlayerSessionCount :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Natural)
gspMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: GameSessionPlacement -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: GameSessionPlacement)
{-# DEPRECATED gspMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Time stamp indicating when this request was completed, canceled, or timed out.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspEndTime :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Timestamp)
gspEndTime = Lens.lens (endTime :: GameSessionPlacement -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GameSessionPlacement)
{-# DEPRECATED gspEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
--
-- /Note:/ Consider using 'gameSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionARN :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionARN = Lens.lens (gameSessionARN :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionARN = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionARN "Use generic-lens or generic-optics with 'gameSessionARN' instead." #-}

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
--
-- /Note:/ Consider using 'playerLatencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlayerLatencies :: Lens.Lens' GameSessionPlacement (Lude.Maybe [PlayerLatency])
gspPlayerLatencies = Lens.lens (playerLatencies :: GameSessionPlacement -> Lude.Maybe [PlayerLatency]) (\s a -> s {playerLatencies = a} :: GameSessionPlacement)
{-# DEPRECATED gspPlayerLatencies "Use generic-lens or generic-optics with 'playerLatencies' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionData :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionData = Lens.lens (gameSessionData :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspDNSName :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspDNSName = Lens.lens (dnsName :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: GameSessionPlacement)
{-# DEPRECATED gspDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'gameSessionQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionQueueName :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Text)
gspGameSessionQueueName = Lens.lens (gameSessionQueueName :: GameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionQueueName = a} :: GameSessionPlacement)
{-# DEPRECATED gspGameSessionQueueName "Use generic-lens or generic-optics with 'gameSessionQueueName' instead." #-}

-- | A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
--
-- /Note:/ Consider using 'placedPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlacedPlayerSessions :: Lens.Lens' GameSessionPlacement (Lude.Maybe [PlacedPlayerSession])
gspPlacedPlayerSessions = Lens.lens (placedPlayerSessions :: GameSessionPlacement -> Lude.Maybe [PlacedPlayerSession]) (\s a -> s {placedPlayerSessions = a} :: GameSessionPlacement)
{-# DEPRECATED gspPlacedPlayerSessions "Use generic-lens or generic-optics with 'placedPlayerSessions' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPort :: Lens.Lens' GameSessionPlacement (Lude.Maybe Lude.Natural)
gspPort = Lens.lens (port :: GameSessionPlacement -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: GameSessionPlacement)
{-# DEPRECATED gspPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON GameSessionPlacement where
  parseJSON =
    Lude.withObject
      "GameSessionPlacement"
      ( \x ->
          GameSessionPlacement'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PlacementId")
            Lude.<*> (x Lude..:? "GameProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "GameSessionName")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "GameSessionId")
            Lude.<*> (x Lude..:? "GameSessionRegion")
            Lude.<*> (x Lude..:? "MatchmakerData")
            Lude.<*> (x Lude..:? "MaximumPlayerSessionCount")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "GameSessionArn")
            Lude.<*> (x Lude..:? "PlayerLatencies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "GameSessionData")
            Lude.<*> (x Lude..:? "DnsName")
            Lude.<*> (x Lude..:? "GameSessionQueueName")
            Lude.<*> (x Lude..:? "PlacedPlayerSessions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Port")
      )
