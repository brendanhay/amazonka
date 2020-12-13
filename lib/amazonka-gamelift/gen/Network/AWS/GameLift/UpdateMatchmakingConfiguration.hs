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
    umcBackfillMode,
    umcGameProperties,
    umcRuleSetName,
    umcAcceptanceTimeoutSeconds,
    umcRequestTimeoutSeconds,
    umcNotificationTarget,
    umcFlexMatchMode,
    umcGameSessionQueueARNs,
    umcName,
    umcCustomEventData,
    umcAcceptanceRequired,
    umcGameSessionData,
    umcDescription,
    umcAdditionalPlayerCount,

    -- * Destructuring the response
    UpdateMatchmakingConfigurationResponse (..),
    mkUpdateMatchmakingConfigurationResponse,

    -- ** Response lenses
    umcrsConfiguration,
    umcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateMatchmakingConfiguration' smart constructor.
data UpdateMatchmakingConfiguration = UpdateMatchmakingConfiguration'
  { -- | The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
    backfillMode :: Lude.Maybe BackfillMode,
    -- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameProperties :: Lude.Maybe [GameProperty],
    -- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
    ruleSetName :: Lude.Maybe Lude.Text,
    -- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
    acceptanceTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
    requestTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
    notificationTarget :: Lude.Maybe Lude.Text,
    -- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.
    --
    --
    --     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
    --
    --
    --     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
    flexMatchMode :: Lude.Maybe FlexMatchMode,
    -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
    gameSessionQueueARNs :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
    name :: Lude.Text,
    -- | Information to add to all events related to the matchmaking configuration.
    customEventData :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
    acceptanceRequired :: Lude.Maybe Lude.Bool,
    -- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameSessionData :: Lude.Maybe Lude.Text,
    -- | A descriptive label that is associated with matchmaking configuration.
    description :: Lude.Maybe Lude.Text,
    -- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    additionalPlayerCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- * 'backfillMode' - The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'gameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'ruleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
-- * 'acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
-- * 'requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
-- * 'notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
-- * 'flexMatchMode' - Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.
--
--
--     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
--
--
--     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
--
--
-- * 'gameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
-- * 'name' - A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
-- * 'customEventData' - Information to add to all events related to the matchmaking configuration.
-- * 'acceptanceRequired' - A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
-- * 'gameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'description' - A descriptive label that is associated with matchmaking configuration.
-- * 'additionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
mkUpdateMatchmakingConfiguration ::
  -- | 'name'
  Lude.Text ->
  UpdateMatchmakingConfiguration
mkUpdateMatchmakingConfiguration pName_ =
  UpdateMatchmakingConfiguration'
    { backfillMode = Lude.Nothing,
      gameProperties = Lude.Nothing,
      ruleSetName = Lude.Nothing,
      acceptanceTimeoutSeconds = Lude.Nothing,
      requestTimeoutSeconds = Lude.Nothing,
      notificationTarget = Lude.Nothing,
      flexMatchMode = Lude.Nothing,
      gameSessionQueueARNs = Lude.Nothing,
      name = pName_,
      customEventData = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      gameSessionData = Lude.Nothing,
      description = Lude.Nothing,
      additionalPlayerCount = Lude.Nothing
    }

-- | The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcBackfillMode :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe BackfillMode)
umcBackfillMode = Lens.lens (backfillMode :: UpdateMatchmakingConfiguration -> Lude.Maybe BackfillMode) (\s a -> s {backfillMode = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcBackfillMode "Use generic-lens or generic-optics with 'backfillMode' instead." #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameProperties :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe [GameProperty])
umcGameProperties = Lens.lens (gameProperties :: UpdateMatchmakingConfiguration -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcRuleSetName :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Text)
umcRuleSetName = Lens.lens (ruleSetName :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetName = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAcceptanceTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Natural)
umcAcceptanceTimeoutSeconds = Lens.lens (acceptanceTimeoutSeconds :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {acceptanceTimeoutSeconds = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcAcceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead." #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcRequestTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Natural)
umcRequestTimeoutSeconds = Lens.lens (requestTimeoutSeconds :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {requestTimeoutSeconds = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcRequestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead." #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcNotificationTarget :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Text)
umcNotificationTarget = Lens.lens (notificationTarget :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {notificationTarget = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcNotificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead." #-}

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
umcFlexMatchMode :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe FlexMatchMode)
umcFlexMatchMode = Lens.lens (flexMatchMode :: UpdateMatchmakingConfiguration -> Lude.Maybe FlexMatchMode) (\s a -> s {flexMatchMode = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcFlexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- /Note:/ Consider using 'gameSessionQueueARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameSessionQueueARNs :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe [Lude.Text])
umcGameSessionQueueARNs = Lens.lens (gameSessionQueueARNs :: UpdateMatchmakingConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {gameSessionQueueARNs = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcGameSessionQueueARNs "Use generic-lens or generic-optics with 'gameSessionQueueARNs' instead." #-}

-- | A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcName :: Lens.Lens' UpdateMatchmakingConfiguration Lude.Text
umcName = Lens.lens (name :: UpdateMatchmakingConfiguration -> Lude.Text) (\s a -> s {name = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information to add to all events related to the matchmaking configuration.
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcCustomEventData :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Text)
umcCustomEventData = Lens.lens (customEventData :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {customEventData = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcCustomEventData "Use generic-lens or generic-optics with 'customEventData' instead." #-}

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAcceptanceRequired :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Bool)
umcAcceptanceRequired = Lens.lens (acceptanceRequired :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcGameSessionData :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Text)
umcGameSessionData = Lens.lens (gameSessionData :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A descriptive label that is associated with matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcDescription :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Text)
umcDescription = Lens.lens (description :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcAdditionalPlayerCount :: Lens.Lens' UpdateMatchmakingConfiguration (Lude.Maybe Lude.Natural)
umcAdditionalPlayerCount = Lens.lens (additionalPlayerCount :: UpdateMatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {additionalPlayerCount = a} :: UpdateMatchmakingConfiguration)
{-# DEPRECATED umcAdditionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead." #-}

instance Lude.AWSRequest UpdateMatchmakingConfiguration where
  type
    Rs UpdateMatchmakingConfiguration =
      UpdateMatchmakingConfigurationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMatchmakingConfigurationResponse'
            Lude.<$> (x Lude..?> "Configuration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMatchmakingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateMatchmakingConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMatchmakingConfiguration where
  toJSON UpdateMatchmakingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BackfillMode" Lude..=) Lude.<$> backfillMode,
            ("GameProperties" Lude..=) Lude.<$> gameProperties,
            ("RuleSetName" Lude..=) Lude.<$> ruleSetName,
            ("AcceptanceTimeoutSeconds" Lude..=)
              Lude.<$> acceptanceTimeoutSeconds,
            ("RequestTimeoutSeconds" Lude..=) Lude.<$> requestTimeoutSeconds,
            ("NotificationTarget" Lude..=) Lude.<$> notificationTarget,
            ("FlexMatchMode" Lude..=) Lude.<$> flexMatchMode,
            ("GameSessionQueueArns" Lude..=) Lude.<$> gameSessionQueueARNs,
            Lude.Just ("Name" Lude..= name),
            ("CustomEventData" Lude..=) Lude.<$> customEventData,
            ("AcceptanceRequired" Lude..=) Lude.<$> acceptanceRequired,
            ("GameSessionData" Lude..=) Lude.<$> gameSessionData,
            ("Description" Lude..=) Lude.<$> description,
            ("AdditionalPlayerCount" Lude..=) Lude.<$> additionalPlayerCount
          ]
      )

instance Lude.ToPath UpdateMatchmakingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMatchmakingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateMatchmakingConfigurationResponse' smart constructor.
data UpdateMatchmakingConfigurationResponse = UpdateMatchmakingConfigurationResponse'
  { -- | The updated matchmaking configuration.
    configuration :: Lude.Maybe MatchmakingConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'configuration' - The updated matchmaking configuration.
-- * 'responseStatus' - The response status code.
mkUpdateMatchmakingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMatchmakingConfigurationResponse
mkUpdateMatchmakingConfigurationResponse pResponseStatus_ =
  UpdateMatchmakingConfigurationResponse'
    { configuration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated matchmaking configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcrsConfiguration :: Lens.Lens' UpdateMatchmakingConfigurationResponse (Lude.Maybe MatchmakingConfiguration)
umcrsConfiguration = Lens.lens (configuration :: UpdateMatchmakingConfigurationResponse -> Lude.Maybe MatchmakingConfiguration) (\s a -> s {configuration = a} :: UpdateMatchmakingConfigurationResponse)
{-# DEPRECATED umcrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umcrsResponseStatus :: Lens.Lens' UpdateMatchmakingConfigurationResponse Lude.Int
umcrsResponseStatus = Lens.lens (responseStatus :: UpdateMatchmakingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMatchmakingConfigurationResponse)
{-# DEPRECATED umcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
