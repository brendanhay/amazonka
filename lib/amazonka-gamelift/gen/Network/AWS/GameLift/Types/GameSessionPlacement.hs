{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    gspDnsName,
    gspEndTime,
    gspGameProperties,
    gspGameSessionArn,
    gspGameSessionData,
    gspGameSessionId,
    gspGameSessionName,
    gspGameSessionQueueName,
    gspGameSessionRegion,
    gspIpAddress,
    gspMatchmakerData,
    gspMaximumPlayerSessionCount,
    gspPlacedPlayerSessions,
    gspPlacementId,
    gspPlayerLatencies,
    gspPort,
    gspStartTime,
    gspStatus,
  )
where

import qualified Network.AWS.GameLift.Types.DnsName as Types
import qualified Network.AWS.GameLift.Types.GameProperty as Types
import qualified Network.AWS.GameLift.Types.GameSessionData as Types
import qualified Network.AWS.GameLift.Types.GameSessionPlacementState as Types
import qualified Network.AWS.GameLift.Types.GameSessionQueueName as Types
import qualified Network.AWS.GameLift.Types.IdStringModel as Types
import qualified Network.AWS.GameLift.Types.IpAddress as Types
import qualified Network.AWS.GameLift.Types.MatchmakerData as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlacedPlayerSession as Types
import qualified Network.AWS.GameLift.Types.PlayerLatency as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
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
    -- | Time stamp indicating when this request was completed, canceled, or timed out.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameProperties :: Core.Maybe [Types.GameProperty],
    -- | Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
    gameSessionArn :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameSessionData :: Core.Maybe Types.GameSessionData,
    -- | A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
    gameSessionId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    gameSessionName :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
    gameSessionQueueName :: Core.Maybe Types.GameSessionQueueName,
    -- | Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
    gameSessionRegion :: Core.Maybe Types.NonZeroAndMaxString,
    -- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
    ipAddress :: Core.Maybe Types.IpAddress,
    -- | Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
    matchmakerData :: Core.Maybe Types.MatchmakerData,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
    placedPlayerSessions :: Core.Maybe [Types.PlacedPlayerSession],
    -- | A unique identifier for a game session placement.
    placementId :: Core.Maybe Types.IdStringModel,
    -- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
    playerLatencies :: Core.Maybe [Types.PlayerLatency],
    -- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
    port :: Core.Maybe Core.Natural,
    -- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    startTime :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.GameSessionPlacementState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GameSessionPlacement' value with any optional fields omitted.
mkGameSessionPlacement ::
  GameSessionPlacement
mkGameSessionPlacement =
  GameSessionPlacement'
    { dnsName = Core.Nothing,
      endTime = Core.Nothing,
      gameProperties = Core.Nothing,
      gameSessionArn = Core.Nothing,
      gameSessionData = Core.Nothing,
      gameSessionId = Core.Nothing,
      gameSessionName = Core.Nothing,
      gameSessionQueueName = Core.Nothing,
      gameSessionRegion = Core.Nothing,
      ipAddress = Core.Nothing,
      matchmakerData = Core.Nothing,
      maximumPlayerSessionCount = Core.Nothing,
      placedPlayerSessions = Core.Nothing,
      placementId = Core.Nothing,
      playerLatencies = Core.Nothing,
      port = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

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
gspDnsName :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.DnsName)
gspDnsName = Lens.field @"dnsName"
{-# DEPRECATED gspDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | Time stamp indicating when this request was completed, canceled, or timed out.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspEndTime :: Lens.Lens' GameSessionPlacement (Core.Maybe Core.NominalDiffTime)
gspEndTime = Lens.field @"endTime"
{-# DEPRECATED gspEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameProperties :: Lens.Lens' GameSessionPlacement (Core.Maybe [Types.GameProperty])
gspGameProperties = Lens.field @"gameProperties"
{-# DEPRECATED gspGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
--
-- /Note:/ Consider using 'gameSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionArn :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.NonZeroAndMaxString)
gspGameSessionArn = Lens.field @"gameSessionArn"
{-# DEPRECATED gspGameSessionArn "Use generic-lens or generic-optics with 'gameSessionArn' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionData :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.GameSessionData)
gspGameSessionData = Lens.field @"gameSessionData"
{-# DEPRECATED gspGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionId :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.NonZeroAndMaxString)
gspGameSessionId = Lens.field @"gameSessionId"
{-# DEPRECATED gspGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'gameSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionName :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.NonZeroAndMaxString)
gspGameSessionName = Lens.field @"gameSessionName"
{-# DEPRECATED gspGameSessionName "Use generic-lens or generic-optics with 'gameSessionName' instead." #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'gameSessionQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionQueueName :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.GameSessionQueueName)
gspGameSessionQueueName = Lens.field @"gameSessionQueueName"
{-# DEPRECATED gspGameSessionQueueName "Use generic-lens or generic-optics with 'gameSessionQueueName' instead." #-}

-- | Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'gameSessionRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspGameSessionRegion :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.NonZeroAndMaxString)
gspGameSessionRegion = Lens.field @"gameSessionRegion"
{-# DEPRECATED gspGameSessionRegion "Use generic-lens or generic-optics with 'gameSessionRegion' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspIpAddress :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.IpAddress)
gspIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED gspIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
--
-- /Note:/ Consider using 'matchmakerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspMatchmakerData :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.MatchmakerData)
gspMatchmakerData = Lens.field @"matchmakerData"
{-# DEPRECATED gspMatchmakerData "Use generic-lens or generic-optics with 'matchmakerData' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspMaximumPlayerSessionCount :: Lens.Lens' GameSessionPlacement (Core.Maybe Core.Natural)
gspMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# DEPRECATED gspMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
--
-- /Note:/ Consider using 'placedPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlacedPlayerSessions :: Lens.Lens' GameSessionPlacement (Core.Maybe [Types.PlacedPlayerSession])
gspPlacedPlayerSessions = Lens.field @"placedPlayerSessions"
{-# DEPRECATED gspPlacedPlayerSessions "Use generic-lens or generic-optics with 'placedPlayerSessions' instead." #-}

-- | A unique identifier for a game session placement.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlacementId :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.IdStringModel)
gspPlacementId = Lens.field @"placementId"
{-# DEPRECATED gspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
--
-- /Note:/ Consider using 'playerLatencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPlayerLatencies :: Lens.Lens' GameSessionPlacement (Core.Maybe [Types.PlayerLatency])
gspPlayerLatencies = Lens.field @"playerLatencies"
{-# DEPRECATED gspPlayerLatencies "Use generic-lens or generic-optics with 'playerLatencies' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspPort :: Lens.Lens' GameSessionPlacement (Core.Maybe Core.Natural)
gspPort = Lens.field @"port"
{-# DEPRECATED gspPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspStartTime :: Lens.Lens' GameSessionPlacement (Core.Maybe Core.NominalDiffTime)
gspStartTime = Lens.field @"startTime"
{-# DEPRECATED gspStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
gspStatus :: Lens.Lens' GameSessionPlacement (Core.Maybe Types.GameSessionPlacementState)
gspStatus = Lens.field @"status"
{-# DEPRECATED gspStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON GameSessionPlacement where
  parseJSON =
    Core.withObject "GameSessionPlacement" Core.$
      \x ->
        GameSessionPlacement'
          Core.<$> (x Core..:? "DnsName")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "GameProperties")
          Core.<*> (x Core..:? "GameSessionArn")
          Core.<*> (x Core..:? "GameSessionData")
          Core.<*> (x Core..:? "GameSessionId")
          Core.<*> (x Core..:? "GameSessionName")
          Core.<*> (x Core..:? "GameSessionQueueName")
          Core.<*> (x Core..:? "GameSessionRegion")
          Core.<*> (x Core..:? "IpAddress")
          Core.<*> (x Core..:? "MatchmakerData")
          Core.<*> (x Core..:? "MaximumPlayerSessionCount")
          Core.<*> (x Core..:? "PlacedPlayerSessions")
          Core.<*> (x Core..:? "PlacementId")
          Core.<*> (x Core..:? "PlayerLatencies")
          Core.<*> (x Core..:? "Port")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")
