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
    cmcBackfillMode,
    cmcGameProperties,
    cmcRuleSetName,
    cmcAcceptanceTimeoutSeconds,
    cmcRequestTimeoutSeconds,
    cmcNotificationTarget,
    cmcFlexMatchMode,
    cmcGameSessionQueueARNs,
    cmcName,
    cmcCustomEventData,
    cmcAcceptanceRequired,
    cmcGameSessionData,
    cmcDescription,
    cmcTags,
    cmcAdditionalPlayerCount,

    -- * Destructuring the response
    CreateMatchmakingConfigurationResponse (..),
    mkCreateMatchmakingConfigurationResponse,

    -- ** Response lenses
    cmcrsConfiguration,
    cmcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { -- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
    backfillMode :: Lude.Maybe BackfillMode,
    -- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameProperties :: Lude.Maybe [GameProperty],
    -- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
    ruleSetName :: Lude.Text,
    -- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
    acceptanceTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
    requestTimeoutSeconds :: Lude.Natural,
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
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
    -- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
    name :: Lude.Text,
    -- | Information to be added to all events related to this matchmaking configuration.
    customEventData :: Lude.Maybe Lude.Text,
    -- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
    acceptanceRequired :: Lude.Bool,
    -- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    gameSessionData :: Lude.Maybe Lude.Text,
    -- | A human-readable description of the matchmaking configuration.
    description :: Lude.Maybe Lude.Text,
    -- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Lude.Maybe [Tag],
    -- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
    additionalPlayerCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- * 'backfillMode' - The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'gameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'ruleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
-- * 'acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
-- * 'requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
-- * 'notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
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
-- * 'name' - A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
-- * 'customEventData' - Information to be added to all events related to this matchmaking configuration.
-- * 'acceptanceRequired' - A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
-- * 'gameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
-- * 'description' - A human-readable description of the matchmaking configuration.
-- * 'tags' - A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
-- * 'additionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
mkCreateMatchmakingConfiguration ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'requestTimeoutSeconds'
  Lude.Natural ->
  -- | 'name'
  Lude.Text ->
  -- | 'acceptanceRequired'
  Lude.Bool ->
  CreateMatchmakingConfiguration
mkCreateMatchmakingConfiguration
  pRuleSetName_
  pRequestTimeoutSeconds_
  pName_
  pAcceptanceRequired_ =
    CreateMatchmakingConfiguration'
      { backfillMode = Lude.Nothing,
        gameProperties = Lude.Nothing,
        ruleSetName = pRuleSetName_,
        acceptanceTimeoutSeconds = Lude.Nothing,
        requestTimeoutSeconds = pRequestTimeoutSeconds_,
        notificationTarget = Lude.Nothing,
        flexMatchMode = Lude.Nothing,
        gameSessionQueueARNs = Lude.Nothing,
        name = pName_,
        customEventData = Lude.Nothing,
        acceptanceRequired = pAcceptanceRequired_,
        gameSessionData = Lude.Nothing,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        additionalPlayerCount = Lude.Nothing
      }

-- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'backfillMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcBackfillMode :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe BackfillMode)
cmcBackfillMode = Lens.lens (backfillMode :: CreateMatchmakingConfiguration -> Lude.Maybe BackfillMode) (\s a -> s {backfillMode = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcBackfillMode "Use generic-lens or generic-optics with 'backfillMode' instead." #-}

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameProperties :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe [GameProperty])
cmcGameProperties = Lens.lens (gameProperties :: CreateMatchmakingConfiguration -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcRuleSetName :: Lens.Lens' CreateMatchmakingConfiguration Lude.Text
cmcRuleSetName = Lens.lens (ruleSetName :: CreateMatchmakingConfiguration -> Lude.Text) (\s a -> s {ruleSetName = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- /Note:/ Consider using 'acceptanceTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAcceptanceTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Natural)
cmcAcceptanceTimeoutSeconds = Lens.lens (acceptanceTimeoutSeconds :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {acceptanceTimeoutSeconds = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcAcceptanceTimeoutSeconds "Use generic-lens or generic-optics with 'acceptanceTimeoutSeconds' instead." #-}

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- /Note:/ Consider using 'requestTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcRequestTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration Lude.Natural
cmcRequestTimeoutSeconds = Lens.lens (requestTimeoutSeconds :: CreateMatchmakingConfiguration -> Lude.Natural) (\s a -> s {requestTimeoutSeconds = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcRequestTimeoutSeconds "Use generic-lens or generic-optics with 'requestTimeoutSeconds' instead." #-}

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- /Note:/ Consider using 'notificationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcNotificationTarget :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Text)
cmcNotificationTarget = Lens.lens (notificationTarget :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {notificationTarget = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcNotificationTarget "Use generic-lens or generic-optics with 'notificationTarget' instead." #-}

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
cmcFlexMatchMode :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe FlexMatchMode)
cmcFlexMatchMode = Lens.lens (flexMatchMode :: CreateMatchmakingConfiguration -> Lude.Maybe FlexMatchMode) (\s a -> s {flexMatchMode = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcFlexMatchMode "Use generic-lens or generic-optics with 'flexMatchMode' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- /Note:/ Consider using 'gameSessionQueueARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameSessionQueueARNs :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe [Lude.Text])
cmcGameSessionQueueARNs = Lens.lens (gameSessionQueueARNs :: CreateMatchmakingConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {gameSessionQueueARNs = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcGameSessionQueueARNs "Use generic-lens or generic-optics with 'gameSessionQueueARNs' instead." #-}

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcName :: Lens.Lens' CreateMatchmakingConfiguration Lude.Text
cmcName = Lens.lens (name :: CreateMatchmakingConfiguration -> Lude.Text) (\s a -> s {name = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information to be added to all events related to this matchmaking configuration.
--
-- /Note:/ Consider using 'customEventData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcCustomEventData :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Text)
cmcCustomEventData = Lens.lens (customEventData :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {customEventData = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcCustomEventData "Use generic-lens or generic-optics with 'customEventData' instead." #-}

-- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAcceptanceRequired :: Lens.Lens' CreateMatchmakingConfiguration Lude.Bool
cmcAcceptanceRequired = Lens.lens (acceptanceRequired :: CreateMatchmakingConfiguration -> Lude.Bool) (\s a -> s {acceptanceRequired = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcGameSessionData :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Text)
cmcGameSessionData = Lens.lens (gameSessionData :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A human-readable description of the matchmaking configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcDescription :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Text)
cmcDescription = Lens.lens (description :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcTags :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe [Tag])
cmcTags = Lens.lens (tags :: CreateMatchmakingConfiguration -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- /Note:/ Consider using 'additionalPlayerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcAdditionalPlayerCount :: Lens.Lens' CreateMatchmakingConfiguration (Lude.Maybe Lude.Natural)
cmcAdditionalPlayerCount = Lens.lens (additionalPlayerCount :: CreateMatchmakingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {additionalPlayerCount = a} :: CreateMatchmakingConfiguration)
{-# DEPRECATED cmcAdditionalPlayerCount "Use generic-lens or generic-optics with 'additionalPlayerCount' instead." #-}

instance Lude.AWSRequest CreateMatchmakingConfiguration where
  type
    Rs CreateMatchmakingConfiguration =
      CreateMatchmakingConfigurationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMatchmakingConfigurationResponse'
            Lude.<$> (x Lude..?> "Configuration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMatchmakingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateMatchmakingConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BackfillMode" Lude..=) Lude.<$> backfillMode,
            ("GameProperties" Lude..=) Lude.<$> gameProperties,
            Lude.Just ("RuleSetName" Lude..= ruleSetName),
            ("AcceptanceTimeoutSeconds" Lude..=)
              Lude.<$> acceptanceTimeoutSeconds,
            Lude.Just ("RequestTimeoutSeconds" Lude..= requestTimeoutSeconds),
            ("NotificationTarget" Lude..=) Lude.<$> notificationTarget,
            ("FlexMatchMode" Lude..=) Lude.<$> flexMatchMode,
            ("GameSessionQueueArns" Lude..=) Lude.<$> gameSessionQueueARNs,
            Lude.Just ("Name" Lude..= name),
            ("CustomEventData" Lude..=) Lude.<$> customEventData,
            Lude.Just ("AcceptanceRequired" Lude..= acceptanceRequired),
            ("GameSessionData" Lude..=) Lude.<$> gameSessionData,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            ("AdditionalPlayerCount" Lude..=) Lude.<$> additionalPlayerCount
          ]
      )

instance Lude.ToPath CreateMatchmakingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMatchmakingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'
  { -- | Object that describes the newly created matchmaking configuration.
    configuration :: Lude.Maybe MatchmakingConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'configuration' - Object that describes the newly created matchmaking configuration.
-- * 'responseStatus' - The response status code.
mkCreateMatchmakingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMatchmakingConfigurationResponse
mkCreateMatchmakingConfigurationResponse pResponseStatus_ =
  CreateMatchmakingConfigurationResponse'
    { configuration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly created matchmaking configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcrsConfiguration :: Lens.Lens' CreateMatchmakingConfigurationResponse (Lude.Maybe MatchmakingConfiguration)
cmcrsConfiguration = Lens.lens (configuration :: CreateMatchmakingConfigurationResponse -> Lude.Maybe MatchmakingConfiguration) (\s a -> s {configuration = a} :: CreateMatchmakingConfigurationResponse)
{-# DEPRECATED cmcrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcrsResponseStatus :: Lens.Lens' CreateMatchmakingConfigurationResponse Lude.Int
cmcrsResponseStatus = Lens.lens (responseStatus :: CreateMatchmakingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMatchmakingConfigurationResponse)
{-# DEPRECATED cmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
