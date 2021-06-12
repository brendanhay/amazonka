{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateMatchmakingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a new matchmaking configuration for use with FlexMatch. Whether
-- your are using FlexMatch with GameLift hosting or as a standalone
-- matchmaking service, the matchmaking configuration sets out rules for
-- matching players and forming teams. If you\'re also using GameLift
-- hosting, it defines how to start game sessions for each match. Your
-- matchmaking system can use multiple configurations to handle different
-- game scenarios. All matchmaking requests (StartMatchmaking or
-- StartMatchBackfill) identify the matchmaking configuration to use and
-- provide player attributes consistent with that configuration.
--
-- To create a matchmaking configuration, you must provide the following:
-- configuration name and FlexMatch mode (with or without GameLift
-- hosting); a rule set that specifies how to evaluate players and find
-- acceptable matches; whether player acceptance is required; and the
-- maximum time allowed for a matchmaking attempt. When using FlexMatch
-- with GameLift hosting, you also need to identify the game session queue
-- to use when starting a game session for the match.
--
-- In addition, you must set up an Amazon Simple Notification Service (SNS)
-- to receive matchmaking notifications, and provide the topic ARN in the
-- matchmaking configuration. An alternative method, continuously polling
-- ticket status with DescribeMatchmaking, is only suitable for games in
-- development with low matchmaking usage.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html FlexMatch Developer Guide>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch Matchmaker>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
--
-- __Related operations__
--
-- -   CreateMatchmakingConfiguration
--
-- -   DescribeMatchmakingConfigurations
--
-- -   UpdateMatchmakingConfiguration
--
-- -   DeleteMatchmakingConfiguration
--
-- -   CreateMatchmakingRuleSet
--
-- -   DescribeMatchmakingRuleSets
--
-- -   ValidateMatchmakingRuleSet
--
-- -   DeleteMatchmakingRuleSet
module Network.AWS.GameLift.CreateMatchmakingConfiguration
  ( -- * Creating a Request
    CreateMatchmakingConfiguration (..),
    newCreateMatchmakingConfiguration,

    -- * Request Lenses
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_name,
    createMatchmakingConfiguration_requestTimeoutSeconds,
    createMatchmakingConfiguration_acceptanceRequired,
    createMatchmakingConfiguration_ruleSetName,

    -- * Destructuring the Response
    CreateMatchmakingConfigurationResponse (..),
    newCreateMatchmakingConfigurationResponse,

    -- * Response Lenses
    createMatchmakingConfigurationResponse_configuration,
    createMatchmakingConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { -- | Information to be added to all events related to this matchmaking
    -- configuration.
    customEventData :: Core.Maybe Core.Text,
    -- | A set of custom properties for a game session, formatted as key-value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new GameSession object that is created
    -- for a successful match. This parameter is not used if @FlexMatchMode@ is
    -- set to @STANDALONE@.
    gameProperties :: Core.Maybe [GameProperty],
    -- | Indicates whether this matchmaking configuration is being used with
    -- GameLift hosting or as a standalone matchmaking solution.
    --
    -- -   __STANDALONE__ - FlexMatch forms matches and returns match
    --     information, including players and team assignments, in a
    --     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
    --     event.
    --
    -- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
    --     GameLift queue to start a game session for the match.
    flexMatchMode :: Core.Maybe FlexMatchMode,
    -- | The method used to backfill game sessions that are created with this
    -- matchmaking configuration. Specify @MANUAL@ when your game manages
    -- backfill requests manually or does not use the match backfill feature.
    -- Specify @AUTOMATIC@ to have GameLift create a StartMatchBackfill request
    -- whenever a game session has one or more open slots. Learn more about
    -- manual and automatic backfill in
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
    -- Automatic backfill is not available when @FlexMatchMode@ is set to
    -- @STANDALONE@.
    backfillMode :: Core.Maybe BackfillMode,
    -- | The number of player slots in a match to keep open for future players.
    -- For example, assume that the configuration\'s rule set specifies a match
    -- for a single 12-person team. If the additional player count is set to 2,
    -- only 10 players are initially selected for the match. This parameter is
    -- not used if @FlexMatchMode@ is set to @STANDALONE@.
    additionalPlayerCount :: Core.Maybe Core.Natural,
    -- | The length of time (in seconds) to wait for players to accept a proposed
    -- match, if acceptance is required. If any player rejects the match or
    -- fails to accept before the timeout, the tickets are returned to the
    -- ticket pool and continue to be evaluated for an acceptable match.
    acceptanceTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new GameSession object that is created
    -- for a successful match. This parameter is not used if @FlexMatchMode@ is
    -- set to @STANDALONE@.
    gameSessionData :: Core.Maybe Core.Text,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift game session queue resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Queues can be located
    -- in any Region. Queues are used to start new GameLift-hosted game
    -- sessions for matches that are created with this matchmaking
    -- configuration. If @FlexMatchMode@ is set to @STANDALONE@, do not set
    -- this parameter.
    gameSessionQueueArns :: Core.Maybe [Core.Text],
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    notificationTarget :: Core.Maybe Core.Text,
    -- | A list of labels to assign to the new matchmaking configuration
    -- resource. Tags are developer-defined key-value pairs. Tagging AWS
    -- resources are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Tag],
    -- | A human-readable description of the matchmaking configuration.
    description :: Core.Maybe Core.Text,
    -- | A unique identifier for a matchmaking configuration. This name is used
    -- to identify the configuration associated with a matchmaking request or
    -- ticket.
    name :: Core.Text,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain
    -- in process before timing out. Requests that fail due to timing out can
    -- be resubmitted as needed.
    requestTimeoutSeconds :: Core.Natural,
    -- | A flag that determines whether a match that was created with this
    -- configuration must be accepted by the matched players. To require
    -- acceptance, set to @TRUE@. With this option enabled, matchmaking tickets
    -- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
    -- potential match is waiting for player acceptance.
    acceptanceRequired :: Core.Bool,
    -- | A unique identifier for a matchmaking rule set to use with this
    -- configuration. You can use either the rule set name or ARN value. A
    -- matchmaking configuration can only use rule sets that are defined in the
    -- same Region.
    ruleSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMatchmakingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEventData', 'createMatchmakingConfiguration_customEventData' - Information to be added to all events related to this matchmaking
-- configuration.
--
-- 'gameProperties', 'createMatchmakingConfiguration_gameProperties' - A set of custom properties for a game session, formatted as key-value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
--
-- 'flexMatchMode', 'createMatchmakingConfiguration_flexMatchMode' - Indicates whether this matchmaking configuration is being used with
-- GameLift hosting or as a standalone matchmaking solution.
--
-- -   __STANDALONE__ - FlexMatch forms matches and returns match
--     information, including players and team assignments, in a
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
--     event.
--
-- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
--     GameLift queue to start a game session for the match.
--
-- 'backfillMode', 'createMatchmakingConfiguration_backfillMode' - The method used to backfill game sessions that are created with this
-- matchmaking configuration. Specify @MANUAL@ when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify @AUTOMATIC@ to have GameLift create a StartMatchBackfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
--
-- 'additionalPlayerCount', 'createMatchmakingConfiguration_additionalPlayerCount' - The number of player slots in a match to keep open for future players.
-- For example, assume that the configuration\'s rule set specifies a match
-- for a single 12-person team. If the additional player count is set to 2,
-- only 10 players are initially selected for the match. This parameter is
-- not used if @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'acceptanceTimeoutSeconds', 'createMatchmakingConfiguration_acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required. If any player rejects the match or
-- fails to accept before the timeout, the tickets are returned to the
-- ticket pool and continue to be evaluated for an acceptable match.
--
-- 'gameSessionData', 'createMatchmakingConfiguration_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
--
-- 'gameSessionQueueArns', 'createMatchmakingConfiguration_gameSessionQueueArns' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Queues can be located
-- in any Region. Queues are used to start new GameLift-hosted game
-- sessions for matches that are created with this matchmaking
-- configuration. If @FlexMatchMode@ is set to @STANDALONE@, do not set
-- this parameter.
--
-- 'notificationTarget', 'createMatchmakingConfiguration_notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- 'tags', 'createMatchmakingConfiguration_tags' - A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging AWS
-- resources are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'description', 'createMatchmakingConfiguration_description' - A human-readable description of the matchmaking configuration.
--
-- 'name', 'createMatchmakingConfiguration_name' - A unique identifier for a matchmaking configuration. This name is used
-- to identify the configuration associated with a matchmaking request or
-- ticket.
--
-- 'requestTimeoutSeconds', 'createMatchmakingConfiguration_requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
--
-- 'acceptanceRequired', 'createMatchmakingConfiguration_acceptanceRequired' - A flag that determines whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to @TRUE@. With this option enabled, matchmaking tickets
-- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
-- potential match is waiting for player acceptance.
--
-- 'ruleSetName', 'createMatchmakingConfiguration_ruleSetName' - A unique identifier for a matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
newCreateMatchmakingConfiguration ::
  -- | 'name'
  Core.Text ->
  -- | 'requestTimeoutSeconds'
  Core.Natural ->
  -- | 'acceptanceRequired'
  Core.Bool ->
  -- | 'ruleSetName'
  Core.Text ->
  CreateMatchmakingConfiguration
newCreateMatchmakingConfiguration
  pName_
  pRequestTimeoutSeconds_
  pAcceptanceRequired_
  pRuleSetName_ =
    CreateMatchmakingConfiguration'
      { customEventData =
          Core.Nothing,
        gameProperties = Core.Nothing,
        flexMatchMode = Core.Nothing,
        backfillMode = Core.Nothing,
        additionalPlayerCount = Core.Nothing,
        acceptanceTimeoutSeconds = Core.Nothing,
        gameSessionData = Core.Nothing,
        gameSessionQueueArns = Core.Nothing,
        notificationTarget = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        requestTimeoutSeconds =
          pRequestTimeoutSeconds_,
        acceptanceRequired = pAcceptanceRequired_,
        ruleSetName = pRuleSetName_
      }

-- | Information to be added to all events related to this matchmaking
-- configuration.
createMatchmakingConfiguration_customEventData :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Text)
createMatchmakingConfiguration_customEventData = Lens.lens (\CreateMatchmakingConfiguration' {customEventData} -> customEventData) (\s@CreateMatchmakingConfiguration' {} a -> s {customEventData = a} :: CreateMatchmakingConfiguration)

-- | A set of custom properties for a game session, formatted as key-value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
createMatchmakingConfiguration_gameProperties :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [GameProperty])
createMatchmakingConfiguration_gameProperties = Lens.lens (\CreateMatchmakingConfiguration' {gameProperties} -> gameProperties) (\s@CreateMatchmakingConfiguration' {} a -> s {gameProperties = a} :: CreateMatchmakingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether this matchmaking configuration is being used with
-- GameLift hosting or as a standalone matchmaking solution.
--
-- -   __STANDALONE__ - FlexMatch forms matches and returns match
--     information, including players and team assignments, in a
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
--     event.
--
-- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
--     GameLift queue to start a game session for the match.
createMatchmakingConfiguration_flexMatchMode :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe FlexMatchMode)
createMatchmakingConfiguration_flexMatchMode = Lens.lens (\CreateMatchmakingConfiguration' {flexMatchMode} -> flexMatchMode) (\s@CreateMatchmakingConfiguration' {} a -> s {flexMatchMode = a} :: CreateMatchmakingConfiguration)

-- | The method used to backfill game sessions that are created with this
-- matchmaking configuration. Specify @MANUAL@ when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify @AUTOMATIC@ to have GameLift create a StartMatchBackfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
createMatchmakingConfiguration_backfillMode :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe BackfillMode)
createMatchmakingConfiguration_backfillMode = Lens.lens (\CreateMatchmakingConfiguration' {backfillMode} -> backfillMode) (\s@CreateMatchmakingConfiguration' {} a -> s {backfillMode = a} :: CreateMatchmakingConfiguration)

-- | The number of player slots in a match to keep open for future players.
-- For example, assume that the configuration\'s rule set specifies a match
-- for a single 12-person team. If the additional player count is set to 2,
-- only 10 players are initially selected for the match. This parameter is
-- not used if @FlexMatchMode@ is set to @STANDALONE@.
createMatchmakingConfiguration_additionalPlayerCount :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Natural)
createMatchmakingConfiguration_additionalPlayerCount = Lens.lens (\CreateMatchmakingConfiguration' {additionalPlayerCount} -> additionalPlayerCount) (\s@CreateMatchmakingConfiguration' {} a -> s {additionalPlayerCount = a} :: CreateMatchmakingConfiguration)

-- | The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required. If any player rejects the match or
-- fails to accept before the timeout, the tickets are returned to the
-- ticket pool and continue to be evaluated for an acceptable match.
createMatchmakingConfiguration_acceptanceTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Natural)
createMatchmakingConfiguration_acceptanceTimeoutSeconds = Lens.lens (\CreateMatchmakingConfiguration' {acceptanceTimeoutSeconds} -> acceptanceTimeoutSeconds) (\s@CreateMatchmakingConfiguration' {} a -> s {acceptanceTimeoutSeconds = a} :: CreateMatchmakingConfiguration)

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
createMatchmakingConfiguration_gameSessionData :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Text)
createMatchmakingConfiguration_gameSessionData = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionData} -> gameSessionData) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionData = a} :: CreateMatchmakingConfiguration)

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Queues can be located
-- in any Region. Queues are used to start new GameLift-hosted game
-- sessions for matches that are created with this matchmaking
-- configuration. If @FlexMatchMode@ is set to @STANDALONE@, do not set
-- this parameter.
createMatchmakingConfiguration_gameSessionQueueArns :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [Core.Text])
createMatchmakingConfiguration_gameSessionQueueArns = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionQueueArns} -> gameSessionQueueArns) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionQueueArns = a} :: CreateMatchmakingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
createMatchmakingConfiguration_notificationTarget :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Text)
createMatchmakingConfiguration_notificationTarget = Lens.lens (\CreateMatchmakingConfiguration' {notificationTarget} -> notificationTarget) (\s@CreateMatchmakingConfiguration' {} a -> s {notificationTarget = a} :: CreateMatchmakingConfiguration)

-- | A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging AWS
-- resources are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createMatchmakingConfiguration_tags :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe [Tag])
createMatchmakingConfiguration_tags = Lens.lens (\CreateMatchmakingConfiguration' {tags} -> tags) (\s@CreateMatchmakingConfiguration' {} a -> s {tags = a} :: CreateMatchmakingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | A human-readable description of the matchmaking configuration.
createMatchmakingConfiguration_description :: Lens.Lens' CreateMatchmakingConfiguration (Core.Maybe Core.Text)
createMatchmakingConfiguration_description = Lens.lens (\CreateMatchmakingConfiguration' {description} -> description) (\s@CreateMatchmakingConfiguration' {} a -> s {description = a} :: CreateMatchmakingConfiguration)

-- | A unique identifier for a matchmaking configuration. This name is used
-- to identify the configuration associated with a matchmaking request or
-- ticket.
createMatchmakingConfiguration_name :: Lens.Lens' CreateMatchmakingConfiguration Core.Text
createMatchmakingConfiguration_name = Lens.lens (\CreateMatchmakingConfiguration' {name} -> name) (\s@CreateMatchmakingConfiguration' {} a -> s {name = a} :: CreateMatchmakingConfiguration)

-- | The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
createMatchmakingConfiguration_requestTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration Core.Natural
createMatchmakingConfiguration_requestTimeoutSeconds = Lens.lens (\CreateMatchmakingConfiguration' {requestTimeoutSeconds} -> requestTimeoutSeconds) (\s@CreateMatchmakingConfiguration' {} a -> s {requestTimeoutSeconds = a} :: CreateMatchmakingConfiguration)

-- | A flag that determines whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to @TRUE@. With this option enabled, matchmaking tickets
-- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
-- potential match is waiting for player acceptance.
createMatchmakingConfiguration_acceptanceRequired :: Lens.Lens' CreateMatchmakingConfiguration Core.Bool
createMatchmakingConfiguration_acceptanceRequired = Lens.lens (\CreateMatchmakingConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@CreateMatchmakingConfiguration' {} a -> s {acceptanceRequired = a} :: CreateMatchmakingConfiguration)

-- | A unique identifier for a matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
createMatchmakingConfiguration_ruleSetName :: Lens.Lens' CreateMatchmakingConfiguration Core.Text
createMatchmakingConfiguration_ruleSetName = Lens.lens (\CreateMatchmakingConfiguration' {ruleSetName} -> ruleSetName) (\s@CreateMatchmakingConfiguration' {} a -> s {ruleSetName = a} :: CreateMatchmakingConfiguration)

instance
  Core.AWSRequest
    CreateMatchmakingConfiguration
  where
  type
    AWSResponse CreateMatchmakingConfiguration =
      CreateMatchmakingConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMatchmakingConfigurationResponse'
            Core.<$> (x Core..?> "Configuration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMatchmakingConfiguration

instance Core.NFData CreateMatchmakingConfiguration

instance
  Core.ToHeaders
    CreateMatchmakingConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateMatchmakingConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomEventData" Core..=)
              Core.<$> customEventData,
            ("GameProperties" Core..=) Core.<$> gameProperties,
            ("FlexMatchMode" Core..=) Core.<$> flexMatchMode,
            ("BackfillMode" Core..=) Core.<$> backfillMode,
            ("AdditionalPlayerCount" Core..=)
              Core.<$> additionalPlayerCount,
            ("AcceptanceTimeoutSeconds" Core..=)
              Core.<$> acceptanceTimeoutSeconds,
            ("GameSessionData" Core..=) Core.<$> gameSessionData,
            ("GameSessionQueueArns" Core..=)
              Core.<$> gameSessionQueueArns,
            ("NotificationTarget" Core..=)
              Core.<$> notificationTarget,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ( "RequestTimeoutSeconds"
                  Core..= requestTimeoutSeconds
              ),
            Core.Just
              ("AcceptanceRequired" Core..= acceptanceRequired),
            Core.Just ("RuleSetName" Core..= ruleSetName)
          ]
      )

instance Core.ToPath CreateMatchmakingConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery CreateMatchmakingConfiguration where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'
  { -- | Object that describes the newly created matchmaking configuration.
    configuration :: Core.Maybe MatchmakingConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMatchmakingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'createMatchmakingConfigurationResponse_configuration' - Object that describes the newly created matchmaking configuration.
--
-- 'httpStatus', 'createMatchmakingConfigurationResponse_httpStatus' - The response's http status code.
newCreateMatchmakingConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateMatchmakingConfigurationResponse
newCreateMatchmakingConfigurationResponse
  pHttpStatus_ =
    CreateMatchmakingConfigurationResponse'
      { configuration =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Object that describes the newly created matchmaking configuration.
createMatchmakingConfigurationResponse_configuration :: Lens.Lens' CreateMatchmakingConfigurationResponse (Core.Maybe MatchmakingConfiguration)
createMatchmakingConfigurationResponse_configuration = Lens.lens (\CreateMatchmakingConfigurationResponse' {configuration} -> configuration) (\s@CreateMatchmakingConfigurationResponse' {} a -> s {configuration = a} :: CreateMatchmakingConfigurationResponse)

-- | The response's http status code.
createMatchmakingConfigurationResponse_httpStatus :: Lens.Lens' CreateMatchmakingConfigurationResponse Core.Int
createMatchmakingConfigurationResponse_httpStatus = Lens.lens (\CreateMatchmakingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateMatchmakingConfigurationResponse' {} a -> s {httpStatus = a} :: CreateMatchmakingConfigurationResponse)

instance
  Core.NFData
    CreateMatchmakingConfigurationResponse
