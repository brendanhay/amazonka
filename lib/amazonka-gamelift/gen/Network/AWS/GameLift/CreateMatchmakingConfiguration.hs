{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a new matchmaking configuration for use with FlexMatch. Whether your are using FlexMatch with GameLift hosting or as a standalone matchmaking service, the matchmaking configuration sets out rules for matching players and forming teams. If you're also using GameLift hosting, it defines how to start game sessions for each match. Your matchmaking system can use multiple configurations to handle different game scenarios. All matchmaking requests ('StartMatchmaking' or 'StartMatchBackfill' ) identify the matchmaking configuration to use and provide player attributes consistent with that configuration.
--
-- To create a matchmaking configuration, you must provide the following: configuration name and FlexMatch mode (with or without GameLift hosting); a rule set that specifies how to evaluate players and find acceptable matches; whether player acceptance is required; and the maximum time allowed for a matchmaking attempt. When using FlexMatch with GameLift hosting, you also need to identify the game session queue to use when starting a game session for the match.
-- In addition, you must set up an Amazon Simple Notification Service (SNS) to receive matchmaking notifications, and provide the topic ARN in the matchmaking configuration. An alternative method, continuously polling ticket status with 'DescribeMatchmaking' , is only suitable for games in development with low matchmaking usage.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html FlexMatch Developer Guide>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch Matchmaker>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--
--     * 'DescribeMatchmakingConfigurations'
--
--
--     * 'UpdateMatchmakingConfiguration'
--
--
--     * 'DeleteMatchmakingConfiguration'
--
--
--     * 'CreateMatchmakingRuleSet'
--
--
--     * 'DescribeMatchmakingRuleSets'
--
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.CreateMatchmakingConfiguration
  ( -- * Creating a request
    CreateMatchmakingConfiguration (..),
    mkCreateMatchmakingConfiguration,

    -- ** Request lenses
    cmcName,
    cmcRequestTimeoutSeconds,
    cmcAcceptanceRequired,
    cmcRuleSetName,
    cmcAcceptanceTimeoutSeconds,
    cmcAdditionalPlayerCount,
    cmcBackfillMode,
    cmcCustomEventData,
    cmcDescription,
    cmcFlexMatchMode,
    cmcGameProperties,
    cmcGameSessionData,
    cmcGameSessionQueueArns,
    cmcNotificationTarget,
    cmcTags,

    -- * Destructuring the response
    CreateMatchmakingConfigurationResponse (..),
    mkCreateMatchmakingConfigurationResponse,

    -- ** Response lenses
    cmcrrsConfiguration,
    cmcrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { -- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
    name :: Types.MatchmakingIdStringModel,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
    requestTimeoutSeconds :: Core.Natural,
    -- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
    acceptanceRequired :: Core.Bool,
    -- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
    ruleSetName :: Types.MatchmakingRuleSetName,
    -- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
    acceptanceTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    additionalPlayerCount :: Core.Maybe Core.Natural,
    -- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
    backfillMode :: Core.Maybe Types.BackfillMode,
    -- | Information to be added to all events related to this matchmaking configuration.
    customEventData :: Core.Maybe Types.CustomEventData,
    -- | A human-readable description of the matchmaking configuration.
    description :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.
    --
    --
    --     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
    --
    --
    --     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
    flexMatchMode :: Core.Maybe Types.FlexMatchMode,
    -- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameProperties :: Core.Maybe [Types.GameProperty],
    -- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameSessionData :: Core.Maybe Types.GameSessionData,
    -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
    gameSessionQueueArns :: Core.Maybe [Types.ArnStringModel],
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    notificationTarget :: Core.Maybe Types.SnsArnStringModel,
    -- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMatchmakingConfiguration' value with any optional fields omitted.
mkCreateMatchmakingConfiguration ::
  -- | 'name'
  Types.MatchmakingIdStringModel ->
  -- | 'requestTimeoutSeconds'
  Core.Natural ->
  -- | 'acceptanceRequired'
  Core.Bool ->
  -- | 'ruleSetName'
  Types.MatchmakingRuleSetName ->
  CreateMatchmakingConfiguration
mkCreateMatchmakingConfiguration
  name
  requestTimeoutSeconds
  acceptanceRequired
  ruleSetName =
    CreateMatchmakingConfiguration'
      { name,
        requestTimeoutSeconds,
        acceptanceRequired,
        ruleSetName,
        acceptanceTimeoutSeconds = Core.Nothing,
        additionalPlayerCount = Core.Nothing,
        backfillMode = Core.Nothing,
        customEventData = Core.Nothing,
        description = Core.Nothing,
        flexMatchMode = Core.Nothing,
        gameProperties = Core.Nothing,
        gameSessionData = Core.Nothing,
        gameSessionQueueArns = Core.Nothing,
        notificationTarget = Core.Nothing,
        tags = Core.Nothing
      }

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcName :: Lens.Lens' CreateMatchmakingConfiguration Types.MatchmakingIdStringModel
cmcName = Lens.field @"name"
{-# DEPRECATED cmcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcRequestTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration Core.Natural
cmcRequestTimeoutSeconds = Lens.field @"requestTimeoutSeconds"
{-# DEPRECATED cmcRequestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead." #-}

-- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAcceptanceRequired :: Lens.Lens' CreateMatchmakingConfiguration Core.Bool
cmcAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# DEPRECATED cmcAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcRuleSetName :: Lens.Lens' CreateMatchmakingConfiguration Types.MatchmakingRuleSetName
cmcRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED cmcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAcceptanceTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Natural)
cmcAcceptanceTimeoutSeconds = Lens.field @"acceptanceTimeoutSeconds"
{-# DEPRECATED cmcAcceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead." #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAdditionalPlayerCount :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Natural)
cmcAdditionalPlayerCount = Lens.field @"additionalPlayerCount"
{-# DEPRECATED cmcAdditionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead." #-}

-- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcBackfillMode :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.BackfillMode)
cmcBackfillMode = Lens.field @"backfillMode"
{-# DEPRECATED cmcBackfillMode "Use generic-lens or generic-optics with 'backfillMode' instead." #-}

-- | Information to be added to all events related to this matchmaking configuration.
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcCustomEventData :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.CustomEventData)
cmcCustomEventData = Lens.field @"customEventData"
{-# DEPRECATED cmcCustomEventData "Use generic-lens or generic-optics with 'customEventData' instead." #-}

-- | A human-readable description of the matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcDescription :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.NonZeroAndMaxString)
cmcDescription = Lens.field @"description"
{-# DEPRECATED cmcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
cmcFlexMatchMode :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.FlexMatchMode)
cmcFlexMatchMode = Lens.field @"flexMatchMode"
{-# DEPRECATED cmcFlexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead." #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameProperties :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [Types.GameProperty])
cmcGameProperties = Lens.field @"gameProperties"
{-# DEPRECATED cmcGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameSessionData :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.GameSessionData)
cmcGameSessionData = Lens.field @"gameSessionData"
{-# DEPRECATED cmcGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- /Note:/ Consider using 'gameSessionQueueArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameSessionQueueArns :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [Types.ArnStringModel])
cmcGameSessionQueueArns = Lens.field @"gameSessionQueueArns"
{-# DEPRECATED cmcGameSessionQueueArns "Use generic-lens or generic-optics with 'gameSessionQueueArns' instead." #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcNotificationTarget :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Types.SnsArnStringModel)
cmcNotificationTarget = Lens.field @"notificationTarget"
{-# DEPRECATED cmcNotificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead." #-}

-- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcTags :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [Types.Tag])
cmcTags = Lens.field @"tags"
{-# DEPRECATED cmcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RequestTimeoutSeconds" Core..= requestTimeoutSeconds),
            Core.Just ("AcceptanceRequired" Core..= acceptanceRequired),
            Core.Just ("RuleSetName" Core..= ruleSetName),
            ("AcceptanceTimeoutSeconds" Core..=)
              Core.<$> acceptanceTimeoutSeconds,
            ("AdditionalPlayerCount" Core..=) Core.<$> additionalPlayerCount,
            ("BackfillMode" Core..=) Core.<$> backfillMode,
            ("CustomEventData" Core..=) Core.<$> customEventData,
            ("Description" Core..=) Core.<$> description,
            ("FlexMatchMode" Core..=) Core.<$> flexMatchMode,
            ("GameProperties" Core..=) Core.<$> gameProperties,
            ("GameSessionData" Core..=) Core.<$> gameSessionData,
            ("GameSessionQueueArns" Core..=) Core.<$> gameSessionQueueArns,
            ("NotificationTarget" Core..=) Core.<$> notificationTarget,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateMatchmakingConfiguration where
  type
    Rs CreateMatchmakingConfiguration =
      CreateMatchmakingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "GameLift.CreateMatchmakingConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMatchmakingConfigurationResponse'
            Core.<$> (x Core..:? "Configuration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'
  { -- | Object that describes the newly created matchmaking configuration.
    configuration :: Core.Maybe Types.MatchmakingConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateMatchmakingConfigurationResponse' value with any optional fields omitted.
mkCreateMatchmakingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMatchmakingConfigurationResponse
mkCreateMatchmakingConfigurationResponse responseStatus =
  CreateMatchmakingConfigurationResponse'
    { configuration =
        Core.Nothing,
      responseStatus
    }

-- | Object that describes the newly created matchmaking configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcrrsConfiguration :: Lens.Lens' CreateMatchmakingConfigurationResponse (Core.Maybe Types.MatchmakingConfiguration)
cmcrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED cmcrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcrrsResponseStatus :: Lens.Lens' CreateMatchmakingConfigurationResponse Core.Int
cmcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
