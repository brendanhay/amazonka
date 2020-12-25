{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a FlexMatch matchmaking configuration. These changes affect all matches and game sessions that are created after the update. To update settings, specify the configuration name to be updated and provide the new settings.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch Matchmaker>
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
module Network.AWS.GameLift.UpdateMatchmakingConfiguration
  ( -- * Creating a request
    UpdateMatchmakingConfiguration (..),
    mkUpdateMatchmakingConfiguration,

    -- ** Request lenses
    umcName,
    umcAcceptanceRequired,
    umcAcceptanceTimeoutSeconds,
    umcAdditionalPlayerCount,
    umcBackfillMode,
    umcCustomEventData,
    umcDescription,
    umcFlexMatchMode,
    umcGameProperties,
    umcGameSessionData,
    umcGameSessionQueueArns,
    umcNotificationTarget,
    umcRequestTimeoutSeconds,
    umcRuleSetName,

    -- * Destructuring the response
    UpdateMatchmakingConfigurationResponse (..),
    mkUpdateMatchmakingConfigurationResponse,

    -- ** Response lenses
    umcrrsConfiguration,
    umcrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateMatchmakingConfiguration' smart constructor.
data UpdateMatchmakingConfiguration = UpdateMatchmakingConfiguration'
  { -- | A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
    name :: Types.Name,
    -- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
    acceptanceRequired :: Core.Maybe Core.Bool,
    -- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
    acceptanceTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    additionalPlayerCount :: Core.Maybe Core.Natural,
    -- | The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
    backfillMode :: Core.Maybe Types.BackfillMode,
    -- | Information to add to all events related to the matchmaking configuration.
    customEventData :: Core.Maybe Types.CustomEventData,
    -- | A descriptive label that is associated with matchmaking configuration.
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
    -- | An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
    notificationTarget :: Core.Maybe Types.SnsArnStringModel,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
    requestTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
    ruleSetName :: Core.Maybe Types.MatchmakingRuleSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMatchmakingConfiguration' value with any optional fields omitted.
mkUpdateMatchmakingConfiguration ::
  -- | 'name'
  Types.Name ->
  UpdateMatchmakingConfiguration
mkUpdateMatchmakingConfiguration name =
  UpdateMatchmakingConfiguration'
    { name,
      acceptanceRequired = Core.Nothing,
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
      requestTimeoutSeconds = Core.Nothing,
      ruleSetName = Core.Nothing
    }

-- | A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcName :: Lens.Lens' UpdateMatchmakingConfiguration Types.Name
umcName = Lens.field @"name"
{-# DEPRECATED umcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAcceptanceRequired :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Core.Bool)
umcAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# DEPRECATED umcAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAcceptanceTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Core.Natural)
umcAcceptanceTimeoutSeconds = Lens.field @"acceptanceTimeoutSeconds"
{-# DEPRECATED umcAcceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead." #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAdditionalPlayerCount :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Core.Natural)
umcAdditionalPlayerCount = Lens.field @"additionalPlayerCount"
{-# DEPRECATED umcAdditionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead." #-}

-- | The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcBackfillMode :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.BackfillMode)
umcBackfillMode = Lens.field @"backfillMode"
{-# DEPRECATED umcBackfillMode "Use generic-lens or generic-optics with 'backfillMode' instead." #-}

-- | Information to add to all events related to the matchmaking configuration.
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcCustomEventData :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.CustomEventData)
umcCustomEventData = Lens.field @"customEventData"
{-# DEPRECATED umcCustomEventData "Use generic-lens or generic-optics with 'customEventData' instead." #-}

-- | A descriptive label that is associated with matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcDescription :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.NonZeroAndMaxString)
umcDescription = Lens.field @"description"
{-# DEPRECATED umcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
umcFlexMatchMode :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.FlexMatchMode)
umcFlexMatchMode = Lens.field @"flexMatchMode"
{-# DEPRECATED umcFlexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead." #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameProperties :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe [Types.GameProperty])
umcGameProperties = Lens.field @"gameProperties"
{-# DEPRECATED umcGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameSessionData :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.GameSessionData)
umcGameSessionData = Lens.field @"gameSessionData"
{-# DEPRECATED umcGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- /Note:/ Consider using 'gameSessionQueueArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameSessionQueueArns :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe [Types.ArnStringModel])
umcGameSessionQueueArns = Lens.field @"gameSessionQueueArns"
{-# DEPRECATED umcGameSessionQueueArns "Use generic-lens or generic-optics with 'gameSessionQueueArns' instead." #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcNotificationTarget :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.SnsArnStringModel)
umcNotificationTarget = Lens.field @"notificationTarget"
{-# DEPRECATED umcNotificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead." #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcRequestTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Core.Natural)
umcRequestTimeoutSeconds = Lens.field @"requestTimeoutSeconds"
{-# DEPRECATED umcRequestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead." #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcRuleSetName :: Lens.Lens' UpdateMatchmakingConfiguration (Core.Maybe Types.MatchmakingRuleSetName)
umcRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED umcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Core.FromJSON UpdateMatchmakingConfiguration where
  toJSON UpdateMatchmakingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("AcceptanceRequired" Core..=) Core.<$> acceptanceRequired,
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
            ("RequestTimeoutSeconds" Core..=) Core.<$> requestTimeoutSeconds,
            ("RuleSetName" Core..=) Core.<$> ruleSetName
          ]
      )

instance Core.AWSRequest UpdateMatchmakingConfiguration where
  type
    Rs UpdateMatchmakingConfiguration =
      UpdateMatchmakingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "GameLift.UpdateMatchmakingConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMatchmakingConfigurationResponse'
            Core.<$> (x Core..:? "Configuration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateMatchmakingConfigurationResponse' smart constructor.
data UpdateMatchmakingConfigurationResponse = UpdateMatchmakingConfigurationResponse'
  { -- | The updated matchmaking configuration.
    configuration :: Core.Maybe Types.MatchmakingConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateMatchmakingConfigurationResponse' value with any optional fields omitted.
mkUpdateMatchmakingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMatchmakingConfigurationResponse
mkUpdateMatchmakingConfigurationResponse responseStatus =
  UpdateMatchmakingConfigurationResponse'
    { configuration =
        Core.Nothing,
      responseStatus
    }

-- | The updated matchmaking configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcrrsConfiguration :: Lens.Lens' UpdateMatchmakingConfigurationResponse (Core.Maybe Types.MatchmakingConfiguration)
umcrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED umcrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcrrsResponseStatus :: Lens.Lens' UpdateMatchmakingConfigurationResponse Core.Int
umcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
