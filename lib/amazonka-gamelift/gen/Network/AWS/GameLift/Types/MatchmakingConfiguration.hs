{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.MatchmakingConfiguration
  ( MatchmakingConfiguration (..)
  -- * Smart constructor
  , mkMatchmakingConfiguration
  -- * Lenses
  , mcAcceptanceRequired
  , mcAcceptanceTimeoutSeconds
  , mcAdditionalPlayerCount
  , mcBackfillMode
  , mcConfigurationArn
  , mcCreationTime
  , mcCustomEventData
  , mcDescription
  , mcFlexMatchMode
  , mcGameProperties
  , mcGameSessionData
  , mcGameSessionQueueArns
  , mcName
  , mcNotificationTarget
  , mcRequestTimeoutSeconds
  , mcRuleSetArn
  , mcRuleSetName
  ) where

import qualified Network.AWS.GameLift.Types.ArnStringModel as Types
import qualified Network.AWS.GameLift.Types.BackfillMode as Types
import qualified Network.AWS.GameLift.Types.ConfigurationArn as Types
import qualified Network.AWS.GameLift.Types.CustomEventData as Types
import qualified Network.AWS.GameLift.Types.Description as Types
import qualified Network.AWS.GameLift.Types.FlexMatchMode as Types
import qualified Network.AWS.GameLift.Types.GameProperty as Types
import qualified Network.AWS.GameLift.Types.GameSessionData as Types
import qualified Network.AWS.GameLift.Types.Name as Types
import qualified Network.AWS.GameLift.Types.NotificationTarget as Types
import qualified Network.AWS.GameLift.Types.RuleSetArn as Types
import qualified Network.AWS.GameLift.Types.RuleSetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.
--
-- /See:/ 'mkMatchmakingConfiguration' smart constructor.
data MatchmakingConfiguration = MatchmakingConfiguration'
  { acceptanceRequired :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
  , acceptanceTimeoutSeconds :: Core.Maybe Core.Natural
    -- ^ The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
  , additionalPlayerCount :: Core.Maybe Core.Natural
    -- ^ The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
  , backfillMode :: Core.Maybe Types.BackfillMode
    -- ^ The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
  , configurationArn :: Core.Maybe Types.ConfigurationArn
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value. 
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , customEventData :: Core.Maybe Types.CustomEventData
    -- ^ Information to attach to all events related to the matchmaking configuration. 
  , description :: Core.Maybe Types.Description
    -- ^ A descriptive label that is associated with matchmaking configuration.
  , flexMatchMode :: Core.Maybe Types.FlexMatchMode
    -- ^ Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution. 
--
--
--     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
--
--
--     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match. 
--
--
  , gameProperties :: Core.Maybe [Types.GameProperty]
    -- ^ A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
  , gameSessionData :: Core.Maybe Types.GameSessionData
    -- ^ A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
  , gameSessionQueueArns :: Core.Maybe [Types.ArnStringModel]
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
  , name :: Core.Maybe Types.Name
    -- ^ A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
  , notificationTarget :: Core.Maybe Types.NotificationTarget
    -- ^ An SNS topic ARN that is set up to receive matchmaking notifications.
  , requestTimeoutSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
  , ruleSetArn :: Core.Maybe Types.RuleSetArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
  , ruleSetName :: Core.Maybe Types.RuleSetName
    -- ^ A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MatchmakingConfiguration' value with any optional fields omitted.
mkMatchmakingConfiguration
    :: MatchmakingConfiguration
mkMatchmakingConfiguration
  = MatchmakingConfiguration'{acceptanceRequired = Core.Nothing,
                              acceptanceTimeoutSeconds = Core.Nothing,
                              additionalPlayerCount = Core.Nothing, backfillMode = Core.Nothing,
                              configurationArn = Core.Nothing, creationTime = Core.Nothing,
                              customEventData = Core.Nothing, description = Core.Nothing,
                              flexMatchMode = Core.Nothing, gameProperties = Core.Nothing,
                              gameSessionData = Core.Nothing,
                              gameSessionQueueArns = Core.Nothing, name = Core.Nothing,
                              notificationTarget = Core.Nothing,
                              requestTimeoutSeconds = Core.Nothing, ruleSetArn = Core.Nothing,
                              ruleSetName = Core.Nothing}

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAcceptanceRequired :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Core.Bool)
mcAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# INLINEABLE mcAcceptanceRequired #-}
{-# DEPRECATED acceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead"  #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAcceptanceTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Core.Natural)
mcAcceptanceTimeoutSeconds = Lens.field @"acceptanceTimeoutSeconds"
{-# INLINEABLE mcAcceptanceTimeoutSeconds #-}
{-# DEPRECATED acceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead"  #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAdditionalPlayerCount :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Core.Natural)
mcAdditionalPlayerCount = Lens.field @"additionalPlayerCount"
{-# INLINEABLE mcAdditionalPlayerCount #-}
{-# DEPRECATED additionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead"  #-}

-- | The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBackfillMode :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.BackfillMode)
mcBackfillMode = Lens.field @"backfillMode"
{-# INLINEABLE mcBackfillMode #-}
{-# DEPRECATED backfillMode "Use generic-lens or generic-optics with 'backfillMode' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value. 
--
-- /Note:/ Consider using 'configurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcConfigurationArn :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.ConfigurationArn)
mcConfigurationArn = Lens.field @"configurationArn"
{-# INLINEABLE mcConfigurationArn #-}
{-# DEPRECATED configurationArn "Use generic-lens or generic-optics with 'configurationArn' instead"  #-}

-- | The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCreationTime :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Core.NominalDiffTime)
mcCreationTime = Lens.field @"creationTime"
{-# INLINEABLE mcCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Information to attach to all events related to the matchmaking configuration. 
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCustomEventData :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.CustomEventData)
mcCustomEventData = Lens.field @"customEventData"
{-# INLINEABLE mcCustomEventData #-}
{-# DEPRECATED customEventData "Use generic-lens or generic-optics with 'customEventData' instead"  #-}

-- | A descriptive label that is associated with matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcDescription :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.Description)
mcDescription = Lens.field @"description"
{-# INLINEABLE mcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution. 
--
--
--     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
--
--
--     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match. 
--
--
--
-- /Note:/ Consider using 'flexMatchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcFlexMatchMode :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.FlexMatchMode)
mcFlexMatchMode = Lens.field @"flexMatchMode"
{-# INLINEABLE mcFlexMatchMode #-}
{-# DEPRECATED flexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead"  #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameProperties :: Lens.Lens' MatchmakingConfiguration (Core.Maybe [Types.GameProperty])
mcGameProperties = Lens.field @"gameProperties"
{-# INLINEABLE mcGameProperties #-}
{-# DEPRECATED gameProperties "Use generic-lens or generic-optics with 'gameProperties' instead"  #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameSessionData :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.GameSessionData)
mcGameSessionData = Lens.field @"gameSessionData"
{-# INLINEABLE mcGameSessionData #-}
{-# DEPRECATED gameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionQueueArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameSessionQueueArns :: Lens.Lens' MatchmakingConfiguration (Core.Maybe [Types.ArnStringModel])
mcGameSessionQueueArns = Lens.field @"gameSessionQueueArns"
{-# INLINEABLE mcGameSessionQueueArns #-}
{-# DEPRECATED gameSessionQueueArns "Use generic-lens or generic-optics with 'gameSessionQueueArns' instead"  #-}

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcName :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.Name)
mcName = Lens.field @"name"
{-# INLINEABLE mcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNotificationTarget :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.NotificationTarget)
mcNotificationTarget = Lens.field @"notificationTarget"
{-# INLINEABLE mcNotificationTarget #-}
{-# DEPRECATED notificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead"  #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRequestTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Core.Natural)
mcRequestTimeoutSeconds = Lens.field @"requestTimeoutSeconds"
{-# INLINEABLE mcRequestTimeoutSeconds #-}
{-# DEPRECATED requestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
--
-- /Note:/ Consider using 'ruleSetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRuleSetArn :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.RuleSetArn)
mcRuleSetArn = Lens.field @"ruleSetArn"
{-# INLINEABLE mcRuleSetArn #-}
{-# DEPRECATED ruleSetArn "Use generic-lens or generic-optics with 'ruleSetArn' instead"  #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRuleSetName :: Lens.Lens' MatchmakingConfiguration (Core.Maybe Types.RuleSetName)
mcRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE mcRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

instance Core.FromJSON MatchmakingConfiguration where
        parseJSON
          = Core.withObject "MatchmakingConfiguration" Core.$
              \ x ->
                MatchmakingConfiguration' Core.<$>
                  (x Core..:? "AcceptanceRequired") Core.<*>
                    x Core..:? "AcceptanceTimeoutSeconds"
                    Core.<*> x Core..:? "AdditionalPlayerCount"
                    Core.<*> x Core..:? "BackfillMode"
                    Core.<*> x Core..:? "ConfigurationArn"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "CustomEventData"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "FlexMatchMode"
                    Core.<*> x Core..:? "GameProperties"
                    Core.<*> x Core..:? "GameSessionData"
                    Core.<*> x Core..:? "GameSessionQueueArns"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "NotificationTarget"
                    Core.<*> x Core..:? "RequestTimeoutSeconds"
                    Core.<*> x Core..:? "RuleSetArn"
                    Core.<*> x Core..:? "RuleSetName"
