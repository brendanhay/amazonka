{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingConfiguration
  ( MatchmakingConfiguration (..),

    -- * Smart constructor
    mkMatchmakingConfiguration,

    -- * Lenses
    mcCreationTime,
    mcBackfillMode,
    mcGameProperties,
    mcRuleSetName,
    mcAcceptanceTimeoutSeconds,
    mcRequestTimeoutSeconds,
    mcNotificationTarget,
    mcFlexMatchMode,
    mcGameSessionQueueARNs,
    mcName,
    mcCustomEventData,
    mcConfigurationARN,
    mcAcceptanceRequired,
    mcGameSessionData,
    mcDescription,
    mcAdditionalPlayerCount,
    mcRuleSetARN,
  )
where

import Network.AWS.GameLift.Types.BackfillMode
import Network.AWS.GameLift.Types.FlexMatchMode
import Network.AWS.GameLift.Types.GameProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.
--
-- /See:/ 'mkMatchmakingConfiguration' smart constructor.
data MatchmakingConfiguration = MatchmakingConfiguration'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    backfillMode :: Lude.Maybe BackfillMode,
    gameProperties ::
      Lude.Maybe [GameProperty],
    ruleSetName :: Lude.Maybe Lude.Text,
    acceptanceTimeoutSeconds ::
      Lude.Maybe Lude.Natural,
    requestTimeoutSeconds ::
      Lude.Maybe Lude.Natural,
    notificationTarget ::
      Lude.Maybe Lude.Text,
    flexMatchMode :: Lude.Maybe FlexMatchMode,
    gameSessionQueueARNs ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    customEventData :: Lude.Maybe Lude.Text,
    configurationARN :: Lude.Maybe Lude.Text,
    acceptanceRequired ::
      Lude.Maybe Lude.Bool,
    gameSessionData :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    additionalPlayerCount ::
      Lude.Maybe Lude.Natural,
    ruleSetARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MatchmakingConfiguration' with the minimum fields required to make a request.
--
-- * 'acceptanceRequired' - A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
-- * 'acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
-- * 'additionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'backfillMode' - The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'configurationARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value.
-- * 'creationTime' - The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'customEventData' - Information to attach to all events related to the matchmaking configuration.
-- * 'description' - A descriptive label that is associated with matchmaking configuration.
-- * 'flexMatchMode' - Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.
--
--
--     * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.
--
--
--     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
--
--
-- * 'gameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'gameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'gameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'name' - A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
-- * 'notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
-- * 'requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
-- * 'ruleSetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
-- * 'ruleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
mkMatchmakingConfiguration ::
  MatchmakingConfiguration
mkMatchmakingConfiguration =
  MatchmakingConfiguration'
    { creationTime = Lude.Nothing,
      backfillMode = Lude.Nothing,
      gameProperties = Lude.Nothing,
      ruleSetName = Lude.Nothing,
      acceptanceTimeoutSeconds = Lude.Nothing,
      requestTimeoutSeconds = Lude.Nothing,
      notificationTarget = Lude.Nothing,
      flexMatchMode = Lude.Nothing,
      gameSessionQueueARNs = Lude.Nothing,
      name = Lude.Nothing,
      customEventData = Lude.Nothing,
      configurationARN = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      gameSessionData = Lude.Nothing,
      description = Lude.Nothing,
      additionalPlayerCount = Lude.Nothing,
      ruleSetARN = Lude.Nothing
    }

-- | The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCreationTime :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Timestamp)
mcCreationTime = Lens.lens (creationTime :: MatchmakingConfiguration -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBackfillMode :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe BackfillMode)
mcBackfillMode = Lens.lens (backfillMode :: MatchmakingConfiguration -> Lude.Maybe BackfillMode) (\s a -> s {backfillMode = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcBackfillMode "Use generic-lens or generic-optics with 'backfillMode' instead." #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameProperties :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe [GameProperty])
mcGameProperties = Lens.lens (gameProperties :: MatchmakingConfiguration -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRuleSetName :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcRuleSetName = Lens.lens (ruleSetName :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetName = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAcceptanceTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Natural)
mcAcceptanceTimeoutSeconds = Lens.lens (acceptanceTimeoutSeconds :: MatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {acceptanceTimeoutSeconds = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcAcceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead." #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRequestTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Natural)
mcRequestTimeoutSeconds = Lens.lens (requestTimeoutSeconds :: MatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {requestTimeoutSeconds = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcRequestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead." #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNotificationTarget :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcNotificationTarget = Lens.lens (notificationTarget :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {notificationTarget = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcNotificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead." #-}

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
mcFlexMatchMode :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe FlexMatchMode)
mcFlexMatchMode = Lens.lens (flexMatchMode :: MatchmakingConfiguration -> Lude.Maybe FlexMatchMode) (\s a -> s {flexMatchMode = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcFlexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionQueueARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameSessionQueueARNs :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe [Lude.Text])
mcGameSessionQueueARNs = Lens.lens (gameSessionQueueARNs :: MatchmakingConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {gameSessionQueueARNs = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcGameSessionQueueARNs "Use generic-lens or generic-optics with 'gameSessionQueueARNs' instead." #-}

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcName :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcName = Lens.lens (name :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information to attach to all events related to the matchmaking configuration.
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCustomEventData :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcCustomEventData = Lens.lens (customEventData :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {customEventData = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcCustomEventData "Use generic-lens or generic-optics with 'customEventData' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value.
--
-- /Note:/ Consider using 'configurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcConfigurationARN :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcConfigurationARN = Lens.lens (configurationARN :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {configurationARN = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcConfigurationARN "Use generic-lens or generic-optics with 'configurationARN' instead." #-}

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAcceptanceRequired :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Bool)
mcAcceptanceRequired = Lens.lens (acceptanceRequired :: MatchmakingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGameSessionData :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcGameSessionData = Lens.lens (gameSessionData :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A descriptive label that is associated with matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcDescription :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcDescription = Lens.lens (description :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAdditionalPlayerCount :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Natural)
mcAdditionalPlayerCount = Lens.lens (additionalPlayerCount :: MatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {additionalPlayerCount = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcAdditionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
--
-- /Note:/ Consider using 'ruleSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRuleSetARN :: Lens.Lens' MatchmakingConfiguration (Lude.Maybe Lude.Text)
mcRuleSetARN = Lens.lens (ruleSetARN :: MatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetARN = a} :: MatchmakingConfiguration)
{-# DEPRECATED mcRuleSetARN "Use generic-lens or generic-optics with 'ruleSetARN' instead." #-}

instance Lude.FromJSON MatchmakingConfiguration where
  parseJSON =
    Lude.withObject
      "MatchmakingConfiguration"
      ( \x ->
          MatchmakingConfiguration'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "BackfillMode")
            Lude.<*> (x Lude..:? "GameProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RuleSetName")
            Lude.<*> (x Lude..:? "AcceptanceTimeoutSeconds")
            Lude.<*> (x Lude..:? "RequestTimeoutSeconds")
            Lude.<*> (x Lude..:? "NotificationTarget")
            Lude.<*> (x Lude..:? "FlexMatchMode")
            Lude.<*> (x Lude..:? "GameSessionQueueArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CustomEventData")
            Lude.<*> (x Lude..:? "ConfigurationArn")
            Lude.<*> (x Lude..:? "AcceptanceRequired")
            Lude.<*> (x Lude..:? "GameSessionData")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "AdditionalPlayerCount")
            Lude.<*> (x Lude..:? "RuleSetArn")
      )
