-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSession
  ( GameSession (..),

    -- * Smart constructor
    mkGameSession,

    -- * Lenses
    gsCreationTime,
    gsStatus,
    gsGameProperties,
    gsIPAddress,
    gsGameSessionId,
    gsMatchmakerData,
    gsFleetARN,
    gsMaximumPlayerSessionCount,
    gsTerminationTime,
    gsPlayerSessionCreationPolicy,
    gsName,
    gsCurrentPlayerSessionCount,
    gsStatusReason,
    gsGameSessionData,
    gsFleetId,
    gsDNSName,
    gsCreatorId,
    gsPort,
  )
where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionStatus
import Network.AWS.GameLift.Types.GameSessionStatusReason
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Properties describing a game session.
--
-- A game session in ACTIVE status can host players. When a game session ends, its status is set to @TERMINATED@ .
-- Once the session ends, the game session object is retained for 30 days. This means you can reuse idempotency token values after this time. Game session logs are retained for 14 days.
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
--
--
--     * Game session placements
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
--
--
-- /See:/ 'mkGameSession' smart constructor.
data GameSession = GameSession'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe GameSessionStatus,
    gameProperties :: Lude.Maybe [GameProperty],
    ipAddress :: Lude.Maybe Lude.Text,
    gameSessionId :: Lude.Maybe Lude.Text,
    matchmakerData :: Lude.Maybe Lude.Text,
    fleetARN :: Lude.Maybe Lude.Text,
    maximumPlayerSessionCount :: Lude.Maybe Lude.Natural,
    terminationTime :: Lude.Maybe Lude.Timestamp,
    playerSessionCreationPolicy ::
      Lude.Maybe PlayerSessionCreationPolicy,
    name :: Lude.Maybe Lude.Text,
    currentPlayerSessionCount :: Lude.Maybe Lude.Natural,
    statusReason :: Lude.Maybe GameSessionStatusReason,
    gameSessionData :: Lude.Maybe Lude.Text,
    fleetId :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text,
    creatorId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GameSession' with the minimum fields required to make a request.
--
-- * 'creationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'creatorId' - A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
-- * 'currentPlayerSessionCount' - Number of players currently in the game session.
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
-- * 'fleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
-- * 'fleetId' - A unique identifier for a fleet that the game session is running on.
-- * 'gameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
-- * 'gameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'gameSessionId' - A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
-- * 'ipAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
-- * 'matchmakerData' - Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
-- * 'maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
-- * 'name' - A descriptive label that is associated with a game session. Session names do not need to be unique.
-- * 'playerSessionCreationPolicy' - Indicates whether or not the game session is accepting new players.
-- * 'port' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
-- * 'status' - Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
-- * 'statusReason' - Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
-- * 'terminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mkGameSession ::
  GameSession
mkGameSession =
  GameSession'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      gameProperties = Lude.Nothing,
      ipAddress = Lude.Nothing,
      gameSessionId = Lude.Nothing,
      matchmakerData = Lude.Nothing,
      fleetARN = Lude.Nothing,
      maximumPlayerSessionCount = Lude.Nothing,
      terminationTime = Lude.Nothing,
      playerSessionCreationPolicy = Lude.Nothing,
      name = Lude.Nothing,
      currentPlayerSessionCount = Lude.Nothing,
      statusReason = Lude.Nothing,
      gameSessionData = Lude.Nothing,
      fleetId = Lude.Nothing,
      dnsName = Lude.Nothing,
      creatorId = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCreationTime :: Lens.Lens' GameSession (Lude.Maybe Lude.Timestamp)
gsCreationTime = Lens.lens (creationTime :: GameSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: GameSession)
{-# DEPRECATED gsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsStatus :: Lens.Lens' GameSession (Lude.Maybe GameSessionStatus)
gsStatus = Lens.lens (status :: GameSession -> Lude.Maybe GameSessionStatus) (\s a -> s {status = a} :: GameSession)
{-# DEPRECATED gsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameProperties :: Lens.Lens' GameSession (Lude.Maybe [GameProperty])
gsGameProperties = Lens.lens (gameProperties :: GameSession -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: GameSession)
{-# DEPRECATED gsGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsIPAddress :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsIPAddress = Lens.lens (ipAddress :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: GameSession)
{-# DEPRECATED gsIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameSessionId :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsGameSessionId = Lens.lens (gameSessionId :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: GameSession)
{-# DEPRECATED gsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
--
-- /Note:/ Consider using 'matchmakerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMatchmakerData :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsMatchmakerData = Lens.lens (matchmakerData :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {matchmakerData = a} :: GameSession)
{-# DEPRECATED gsMatchmakerData "Use generic-lens or generic-optics with 'matchmakerData' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
--
-- /Note:/ Consider using 'fleetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsFleetARN :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsFleetARN = Lens.lens (fleetARN :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {fleetARN = a} :: GameSession)
{-# DEPRECATED gsFleetARN "Use generic-lens or generic-optics with 'fleetARN' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMaximumPlayerSessionCount :: Lens.Lens' GameSession (Lude.Maybe Lude.Natural)
gsMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: GameSession -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: GameSession)
{-# DEPRECATED gsMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsTerminationTime :: Lens.Lens' GameSession (Lude.Maybe Lude.Timestamp)
gsTerminationTime = Lens.lens (terminationTime :: GameSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {terminationTime = a} :: GameSession)
{-# DEPRECATED gsTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

-- | Indicates whether or not the game session is accepting new players.
--
-- /Note:/ Consider using 'playerSessionCreationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsPlayerSessionCreationPolicy :: Lens.Lens' GameSession (Lude.Maybe PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = Lens.lens (playerSessionCreationPolicy :: GameSession -> Lude.Maybe PlayerSessionCreationPolicy) (\s a -> s {playerSessionCreationPolicy = a} :: GameSession)
{-# DEPRECATED gsPlayerSessionCreationPolicy "Use generic-lens or generic-optics with 'playerSessionCreationPolicy' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsName :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsName = Lens.lens (name :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GameSession)
{-# DEPRECATED gsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Number of players currently in the game session.
--
-- /Note:/ Consider using 'currentPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCurrentPlayerSessionCount :: Lens.Lens' GameSession (Lude.Maybe Lude.Natural)
gsCurrentPlayerSessionCount = Lens.lens (currentPlayerSessionCount :: GameSession -> Lude.Maybe Lude.Natural) (\s a -> s {currentPlayerSessionCount = a} :: GameSession)
{-# DEPRECATED gsCurrentPlayerSessionCount "Use generic-lens or generic-optics with 'currentPlayerSessionCount' instead." #-}

-- | Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsStatusReason :: Lens.Lens' GameSession (Lude.Maybe GameSessionStatusReason)
gsStatusReason = Lens.lens (statusReason :: GameSession -> Lude.Maybe GameSessionStatusReason) (\s a -> s {statusReason = a} :: GameSession)
{-# DEPRECATED gsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameSessionData :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsGameSessionData = Lens.lens (gameSessionData :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: GameSession)
{-# DEPRECATED gsGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A unique identifier for a fleet that the game session is running on.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsFleetId :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsFleetId = Lens.lens (fleetId :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: GameSession)
{-# DEPRECATED gsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

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
gsDNSName :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsDNSName = Lens.lens (dnsName :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: GameSession)
{-# DEPRECATED gsDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCreatorId :: Lens.Lens' GameSession (Lude.Maybe Lude.Text)
gsCreatorId = Lens.lens (creatorId :: GameSession -> Lude.Maybe Lude.Text) (\s a -> s {creatorId = a} :: GameSession)
{-# DEPRECATED gsCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsPort :: Lens.Lens' GameSession (Lude.Maybe Lude.Natural)
gsPort = Lens.lens (port :: GameSession -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: GameSession)
{-# DEPRECATED gsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON GameSession where
  parseJSON =
    Lude.withObject
      "GameSession"
      ( \x ->
          GameSession'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "GameProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "GameSessionId")
            Lude.<*> (x Lude..:? "MatchmakerData")
            Lude.<*> (x Lude..:? "FleetArn")
            Lude.<*> (x Lude..:? "MaximumPlayerSessionCount")
            Lude.<*> (x Lude..:? "TerminationTime")
            Lude.<*> (x Lude..:? "PlayerSessionCreationPolicy")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CurrentPlayerSessionCount")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "GameSessionData")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "DnsName")
            Lude.<*> (x Lude..:? "CreatorId")
            Lude.<*> (x Lude..:? "Port")
      )
