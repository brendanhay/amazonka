{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    gsCreatorId,
    gsCurrentPlayerSessionCount,
    gsDnsName,
    gsFleetArn,
    gsFleetId,
    gsGameProperties,
    gsGameSessionData,
    gsGameSessionId,
    gsIpAddress,
    gsMatchmakerData,
    gsMaximumPlayerSessionCount,
    gsName,
    gsPlayerSessionCreationPolicy,
    gsPort,
    gsStatus,
    gsStatusReason,
    gsTerminationTime,
  )
where

import qualified Network.AWS.GameLift.Types.DnsName as Types
import qualified Network.AWS.GameLift.Types.FleetArn as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.GameProperty as Types
import qualified Network.AWS.GameLift.Types.GameSessionData as Types
import qualified Network.AWS.GameLift.Types.GameSessionStatus as Types
import qualified Network.AWS.GameLift.Types.GameSessionStatusReason as Types
import qualified Network.AWS.GameLift.Types.IpAddress as Types
import qualified Network.AWS.GameLift.Types.MatchmakerData as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlayerSessionCreationPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
    creatorId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Number of players currently in the game session.
    currentPlayerSessionCount :: Core.Maybe Core.Natural,
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
    dnsName :: Core.Maybe Types.DnsName,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
    fleetArn :: Core.Maybe Types.FleetArn,
    -- | A unique identifier for a fleet that the game session is running on.
    fleetId :: Core.Maybe Types.FleetId,
    -- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
    gameProperties :: Core.Maybe [Types.GameProperty],
    -- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameSessionData :: Core.Maybe Types.GameSessionData,
    -- | A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
    gameSessionId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
    ipAddress :: Core.Maybe Types.IpAddress,
    -- | Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
    matchmakerData :: Core.Maybe Types.MatchmakerData,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Indicates whether or not the game session is accepting new players.
    playerSessionCreationPolicy :: Core.Maybe Types.PlayerSessionCreationPolicy,
    -- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
    port :: Core.Maybe Core.Natural,
    -- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
    status :: Core.Maybe Types.GameSessionStatus,
    -- | Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
    statusReason :: Core.Maybe Types.GameSessionStatusReason,
    -- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    terminationTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GameSession' value with any optional fields omitted.
mkGameSession ::
  GameSession
mkGameSession =
  GameSession'
    { creationTime = Core.Nothing,
      creatorId = Core.Nothing,
      currentPlayerSessionCount = Core.Nothing,
      dnsName = Core.Nothing,
      fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      gameProperties = Core.Nothing,
      gameSessionData = Core.Nothing,
      gameSessionId = Core.Nothing,
      ipAddress = Core.Nothing,
      matchmakerData = Core.Nothing,
      maximumPlayerSessionCount = Core.Nothing,
      name = Core.Nothing,
      playerSessionCreationPolicy = Core.Nothing,
      port = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing,
      terminationTime = Core.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCreationTime :: Lens.Lens' GameSession (Core.Maybe Core.NominalDiffTime)
gsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED gsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCreatorId :: Lens.Lens' GameSession (Core.Maybe Types.NonZeroAndMaxString)
gsCreatorId = Lens.field @"creatorId"
{-# DEPRECATED gsCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

-- | Number of players currently in the game session.
--
-- /Note:/ Consider using 'currentPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCurrentPlayerSessionCount :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gsCurrentPlayerSessionCount = Lens.field @"currentPlayerSessionCount"
{-# DEPRECATED gsCurrentPlayerSessionCount "Use generic-lens or generic-optics with 'currentPlayerSessionCount' instead." #-}

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
gsDnsName :: Lens.Lens' GameSession (Core.Maybe Types.DnsName)
gsDnsName = Lens.field @"dnsName"
{-# DEPRECATED gsDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
--
-- /Note:/ Consider using 'fleetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsFleetArn :: Lens.Lens' GameSession (Core.Maybe Types.FleetArn)
gsFleetArn = Lens.field @"fleetArn"
{-# DEPRECATED gsFleetArn "Use generic-lens or generic-optics with 'fleetArn' instead." #-}

-- | A unique identifier for a fleet that the game session is running on.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsFleetId :: Lens.Lens' GameSession (Core.Maybe Types.FleetId)
gsFleetId = Lens.field @"fleetId"
{-# DEPRECATED gsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameProperties :: Lens.Lens' GameSession (Core.Maybe [Types.GameProperty])
gsGameProperties = Lens.field @"gameProperties"
{-# DEPRECATED gsGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameSessionData :: Lens.Lens' GameSession (Core.Maybe Types.GameSessionData)
gsGameSessionData = Lens.field @"gameSessionData"
{-# DEPRECATED gsGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGameSessionId :: Lens.Lens' GameSession (Core.Maybe Types.NonZeroAndMaxString)
gsGameSessionId = Lens.field @"gameSessionId"
{-# DEPRECATED gsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsIpAddress :: Lens.Lens' GameSession (Core.Maybe Types.IpAddress)
gsIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED gsIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
--
-- /Note:/ Consider using 'matchmakerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMatchmakerData :: Lens.Lens' GameSession (Core.Maybe Types.MatchmakerData)
gsMatchmakerData = Lens.field @"matchmakerData"
{-# DEPRECATED gsMatchmakerData "Use generic-lens or generic-optics with 'matchmakerData' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMaximumPlayerSessionCount :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gsMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# DEPRECATED gsMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsName :: Lens.Lens' GameSession (Core.Maybe Types.NonZeroAndMaxString)
gsName = Lens.field @"name"
{-# DEPRECATED gsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates whether or not the game session is accepting new players.
--
-- /Note:/ Consider using 'playerSessionCreationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsPlayerSessionCreationPolicy :: Lens.Lens' GameSession (Core.Maybe Types.PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = Lens.field @"playerSessionCreationPolicy"
{-# DEPRECATED gsPlayerSessionCreationPolicy "Use generic-lens or generic-optics with 'playerSessionCreationPolicy' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsPort :: Lens.Lens' GameSession (Core.Maybe Core.Natural)
gsPort = Lens.field @"port"
{-# DEPRECATED gsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsStatus :: Lens.Lens' GameSession (Core.Maybe Types.GameSessionStatus)
gsStatus = Lens.field @"status"
{-# DEPRECATED gsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsStatusReason :: Lens.Lens' GameSession (Core.Maybe Types.GameSessionStatusReason)
gsStatusReason = Lens.field @"statusReason"
{-# DEPRECATED gsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsTerminationTime :: Lens.Lens' GameSession (Core.Maybe Core.NominalDiffTime)
gsTerminationTime = Lens.field @"terminationTime"
{-# DEPRECATED gsTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

instance Core.FromJSON GameSession where
  parseJSON =
    Core.withObject "GameSession" Core.$
      \x ->
        GameSession'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "CreatorId")
          Core.<*> (x Core..:? "CurrentPlayerSessionCount")
          Core.<*> (x Core..:? "DnsName")
          Core.<*> (x Core..:? "FleetArn")
          Core.<*> (x Core..:? "FleetId")
          Core.<*> (x Core..:? "GameProperties")
          Core.<*> (x Core..:? "GameSessionData")
          Core.<*> (x Core..:? "GameSessionId")
          Core.<*> (x Core..:? "IpAddress")
          Core.<*> (x Core..:? "MatchmakerData")
          Core.<*> (x Core..:? "MaximumPlayerSessionCount")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "PlayerSessionCreationPolicy")
          Core.<*> (x Core..:? "Port")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusReason")
          Core.<*> (x Core..:? "TerminationTime")
