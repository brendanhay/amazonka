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
-- Module      : Amazonka.GameLift.CreateMatchmakingConfiguration
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
-- topic to receive matchmaking notifications. Provide the topic ARN in the
-- matchmaking configuration. An alternative method, continuously polling
-- ticket status with DescribeMatchmaking, is only suitable for games in
-- development with low matchmaking usage.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch matchmaker>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set up FlexMatch event notification>
--
-- __Related actions__
--
-- CreateMatchmakingConfiguration | DescribeMatchmakingConfigurations |
-- UpdateMatchmakingConfiguration | DeleteMatchmakingConfiguration |
-- CreateMatchmakingRuleSet | DescribeMatchmakingRuleSets |
-- ValidateMatchmakingRuleSet | DeleteMatchmakingRuleSet |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateMatchmakingConfiguration
  ( -- * Creating a Request
    CreateMatchmakingConfiguration (..),
    newCreateMatchmakingConfiguration,

    -- * Request Lenses
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_additionalPlayerCount,
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

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { -- | The method used to backfill game sessions that are created with this
    -- matchmaking configuration. Specify @MANUAL@ when your game manages
    -- backfill requests manually or does not use the match backfill feature.
    -- Specify @AUTOMATIC@ to have GameLift create a StartMatchBackfill request
    -- whenever a game session has one or more open slots. Learn more about
    -- manual and automatic backfill in
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
    -- Automatic backfill is not available when @FlexMatchMode@ is set to
    -- @STANDALONE@.
    backfillMode :: Prelude.Maybe BackfillMode,
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new GameSession object that is created
    -- for a successful match. This parameter is not used if @FlexMatchMode@ is
    -- set to @STANDALONE@.
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | The length of time (in seconds) to wait for players to accept a proposed
    -- match, if acceptance is required.
    acceptanceTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    -- See
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
    -- for more information.
    notificationTarget :: Prelude.Maybe Prelude.Text,
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
    flexMatchMode :: Prelude.Maybe FlexMatchMode,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift game session queue resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
    -- can be located in any Region. Queues are used to start new
    -- GameLift-hosted game sessions for matches that are created with this
    -- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
    -- not set this parameter.
    gameSessionQueueArns :: Prelude.Maybe [Prelude.Text],
    -- | Information to be added to all events related to this matchmaking
    -- configuration.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new GameSession object that is created
    -- for a successful match. This parameter is not used if @FlexMatchMode@ is
    -- set to @STANDALONE@.
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the matchmaking configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of labels to assign to the new matchmaking configuration
    -- resource. Tags are developer-defined key-value pairs. Tagging AWS
    -- resources are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Prelude.Maybe [Tag],
    -- | The number of player slots in a match to keep open for future players.
    -- For example, if the configuration\'s rule set specifies a match for a
    -- single 12-person team, and the additional player count is set to 2, only
    -- 10 players are selected for the match. This parameter is not used if
    -- @FlexMatchMode@ is set to @STANDALONE@.
    additionalPlayerCount :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the matchmaking configuration. This name is used
    -- to identify the configuration associated with a matchmaking request or
    -- ticket.
    name :: Prelude.Text,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain
    -- in process before timing out. Requests that fail due to timing out can
    -- be resubmitted as needed.
    requestTimeoutSeconds :: Prelude.Natural,
    -- | A flag that determines whether a match that was created with this
    -- configuration must be accepted by the matched players. To require
    -- acceptance, set to @TRUE@. With this option enabled, matchmaking tickets
    -- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
    -- potential match is waiting for player acceptance.
    acceptanceRequired :: Prelude.Bool,
    -- | A unique identifier for the matchmaking rule set to use with this
    -- configuration. You can use either the rule set name or ARN value. A
    -- matchmaking configuration can only use rule sets that are defined in the
    -- same Region.
    ruleSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMatchmakingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'gameProperties', 'createMatchmakingConfiguration_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
--
-- 'acceptanceTimeoutSeconds', 'createMatchmakingConfiguration_acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
--
-- 'notificationTarget', 'createMatchmakingConfiguration_notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
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
-- 'gameSessionQueueArns', 'createMatchmakingConfiguration_gameSessionQueueArns' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
--
-- 'customEventData', 'createMatchmakingConfiguration_customEventData' - Information to be added to all events related to this matchmaking
-- configuration.
--
-- 'gameSessionData', 'createMatchmakingConfiguration_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
--
-- 'description', 'createMatchmakingConfiguration_description' - A human-readable description of the matchmaking configuration.
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
-- 'additionalPlayerCount', 'createMatchmakingConfiguration_additionalPlayerCount' - The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'name', 'createMatchmakingConfiguration_name' - A unique identifier for the matchmaking configuration. This name is used
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
-- 'ruleSetName', 'createMatchmakingConfiguration_ruleSetName' - A unique identifier for the matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
newCreateMatchmakingConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'requestTimeoutSeconds'
  Prelude.Natural ->
  -- | 'acceptanceRequired'
  Prelude.Bool ->
  -- | 'ruleSetName'
  Prelude.Text ->
  CreateMatchmakingConfiguration
newCreateMatchmakingConfiguration
  pName_
  pRequestTimeoutSeconds_
  pAcceptanceRequired_
  pRuleSetName_ =
    CreateMatchmakingConfiguration'
      { backfillMode =
          Prelude.Nothing,
        gameProperties = Prelude.Nothing,
        acceptanceTimeoutSeconds = Prelude.Nothing,
        notificationTarget = Prelude.Nothing,
        flexMatchMode = Prelude.Nothing,
        gameSessionQueueArns = Prelude.Nothing,
        customEventData = Prelude.Nothing,
        gameSessionData = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        additionalPlayerCount = Prelude.Nothing,
        name = pName_,
        requestTimeoutSeconds =
          pRequestTimeoutSeconds_,
        acceptanceRequired = pAcceptanceRequired_,
        ruleSetName = pRuleSetName_
      }

-- | The method used to backfill game sessions that are created with this
-- matchmaking configuration. Specify @MANUAL@ when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify @AUTOMATIC@ to have GameLift create a StartMatchBackfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
createMatchmakingConfiguration_backfillMode :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe BackfillMode)
createMatchmakingConfiguration_backfillMode = Lens.lens (\CreateMatchmakingConfiguration' {backfillMode} -> backfillMode) (\s@CreateMatchmakingConfiguration' {} a -> s {backfillMode = a} :: CreateMatchmakingConfiguration)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
createMatchmakingConfiguration_gameProperties :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [GameProperty])
createMatchmakingConfiguration_gameProperties = Lens.lens (\CreateMatchmakingConfiguration' {gameProperties} -> gameProperties) (\s@CreateMatchmakingConfiguration' {} a -> s {gameProperties = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
createMatchmakingConfiguration_acceptanceTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
createMatchmakingConfiguration_acceptanceTimeoutSeconds = Lens.lens (\CreateMatchmakingConfiguration' {acceptanceTimeoutSeconds} -> acceptanceTimeoutSeconds) (\s@CreateMatchmakingConfiguration' {} a -> s {acceptanceTimeoutSeconds = a} :: CreateMatchmakingConfiguration)

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
createMatchmakingConfiguration_notificationTarget :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_notificationTarget = Lens.lens (\CreateMatchmakingConfiguration' {notificationTarget} -> notificationTarget) (\s@CreateMatchmakingConfiguration' {} a -> s {notificationTarget = a} :: CreateMatchmakingConfiguration)

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
createMatchmakingConfiguration_flexMatchMode :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe FlexMatchMode)
createMatchmakingConfiguration_flexMatchMode = Lens.lens (\CreateMatchmakingConfiguration' {flexMatchMode} -> flexMatchMode) (\s@CreateMatchmakingConfiguration' {} a -> s {flexMatchMode = a} :: CreateMatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
createMatchmakingConfiguration_gameSessionQueueArns :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [Prelude.Text])
createMatchmakingConfiguration_gameSessionQueueArns = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionQueueArns} -> gameSessionQueueArns) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionQueueArns = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Information to be added to all events related to this matchmaking
-- configuration.
createMatchmakingConfiguration_customEventData :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_customEventData = Lens.lens (\CreateMatchmakingConfiguration' {customEventData} -> customEventData) (\s@CreateMatchmakingConfiguration' {} a -> s {customEventData = a} :: CreateMatchmakingConfiguration)

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new GameSession object that is created
-- for a successful match. This parameter is not used if @FlexMatchMode@ is
-- set to @STANDALONE@.
createMatchmakingConfiguration_gameSessionData :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_gameSessionData = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionData} -> gameSessionData) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionData = a} :: CreateMatchmakingConfiguration)

-- | A human-readable description of the matchmaking configuration.
createMatchmakingConfiguration_description :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_description = Lens.lens (\CreateMatchmakingConfiguration' {description} -> description) (\s@CreateMatchmakingConfiguration' {} a -> s {description = a} :: CreateMatchmakingConfiguration)

-- | A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging AWS
-- resources are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createMatchmakingConfiguration_tags :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [Tag])
createMatchmakingConfiguration_tags = Lens.lens (\CreateMatchmakingConfiguration' {tags} -> tags) (\s@CreateMatchmakingConfiguration' {} a -> s {tags = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
createMatchmakingConfiguration_additionalPlayerCount :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
createMatchmakingConfiguration_additionalPlayerCount = Lens.lens (\CreateMatchmakingConfiguration' {additionalPlayerCount} -> additionalPlayerCount) (\s@CreateMatchmakingConfiguration' {} a -> s {additionalPlayerCount = a} :: CreateMatchmakingConfiguration)

-- | A unique identifier for the matchmaking configuration. This name is used
-- to identify the configuration associated with a matchmaking request or
-- ticket.
createMatchmakingConfiguration_name :: Lens.Lens' CreateMatchmakingConfiguration Prelude.Text
createMatchmakingConfiguration_name = Lens.lens (\CreateMatchmakingConfiguration' {name} -> name) (\s@CreateMatchmakingConfiguration' {} a -> s {name = a} :: CreateMatchmakingConfiguration)

-- | The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
createMatchmakingConfiguration_requestTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration Prelude.Natural
createMatchmakingConfiguration_requestTimeoutSeconds = Lens.lens (\CreateMatchmakingConfiguration' {requestTimeoutSeconds} -> requestTimeoutSeconds) (\s@CreateMatchmakingConfiguration' {} a -> s {requestTimeoutSeconds = a} :: CreateMatchmakingConfiguration)

-- | A flag that determines whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to @TRUE@. With this option enabled, matchmaking tickets
-- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
-- potential match is waiting for player acceptance.
createMatchmakingConfiguration_acceptanceRequired :: Lens.Lens' CreateMatchmakingConfiguration Prelude.Bool
createMatchmakingConfiguration_acceptanceRequired = Lens.lens (\CreateMatchmakingConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@CreateMatchmakingConfiguration' {} a -> s {acceptanceRequired = a} :: CreateMatchmakingConfiguration)

-- | A unique identifier for the matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
createMatchmakingConfiguration_ruleSetName :: Lens.Lens' CreateMatchmakingConfiguration Prelude.Text
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
            Prelude.<$> (x Core..?> "Configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMatchmakingConfiguration
  where
  hashWithSalt
    _salt
    CreateMatchmakingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` backfillMode
        `Prelude.hashWithSalt` gameProperties
        `Prelude.hashWithSalt` acceptanceTimeoutSeconds
        `Prelude.hashWithSalt` notificationTarget
        `Prelude.hashWithSalt` flexMatchMode
        `Prelude.hashWithSalt` gameSessionQueueArns
        `Prelude.hashWithSalt` customEventData
        `Prelude.hashWithSalt` gameSessionData
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` additionalPlayerCount
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` requestTimeoutSeconds
        `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` ruleSetName

instance
  Prelude.NFData
    CreateMatchmakingConfiguration
  where
  rnf CreateMatchmakingConfiguration' {..} =
    Prelude.rnf backfillMode
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf acceptanceTimeoutSeconds
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf flexMatchMode
      `Prelude.seq` Prelude.rnf gameSessionQueueArns
      `Prelude.seq` Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf additionalPlayerCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestTimeoutSeconds
      `Prelude.seq` Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf ruleSetName

instance
  Core.ToHeaders
    CreateMatchmakingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateMatchmakingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BackfillMode" Core..=) Prelude.<$> backfillMode,
            ("GameProperties" Core..=)
              Prelude.<$> gameProperties,
            ("AcceptanceTimeoutSeconds" Core..=)
              Prelude.<$> acceptanceTimeoutSeconds,
            ("NotificationTarget" Core..=)
              Prelude.<$> notificationTarget,
            ("FlexMatchMode" Core..=) Prelude.<$> flexMatchMode,
            ("GameSessionQueueArns" Core..=)
              Prelude.<$> gameSessionQueueArns,
            ("CustomEventData" Core..=)
              Prelude.<$> customEventData,
            ("GameSessionData" Core..=)
              Prelude.<$> gameSessionData,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            ("AdditionalPlayerCount" Core..=)
              Prelude.<$> additionalPlayerCount,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ( "RequestTimeoutSeconds"
                  Core..= requestTimeoutSeconds
              ),
            Prelude.Just
              ("AcceptanceRequired" Core..= acceptanceRequired),
            Prelude.Just ("RuleSetName" Core..= ruleSetName)
          ]
      )

instance Core.ToPath CreateMatchmakingConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateMatchmakingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'
  { -- | Object that describes the newly created matchmaking configuration.
    configuration :: Prelude.Maybe MatchmakingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateMatchmakingConfigurationResponse
newCreateMatchmakingConfigurationResponse
  pHttpStatus_ =
    CreateMatchmakingConfigurationResponse'
      { configuration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Object that describes the newly created matchmaking configuration.
createMatchmakingConfigurationResponse_configuration :: Lens.Lens' CreateMatchmakingConfigurationResponse (Prelude.Maybe MatchmakingConfiguration)
createMatchmakingConfigurationResponse_configuration = Lens.lens (\CreateMatchmakingConfigurationResponse' {configuration} -> configuration) (\s@CreateMatchmakingConfigurationResponse' {} a -> s {configuration = a} :: CreateMatchmakingConfigurationResponse)

-- | The response's http status code.
createMatchmakingConfigurationResponse_httpStatus :: Lens.Lens' CreateMatchmakingConfigurationResponse Prelude.Int
createMatchmakingConfigurationResponse_httpStatus = Lens.lens (\CreateMatchmakingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateMatchmakingConfigurationResponse' {} a -> s {httpStatus = a} :: CreateMatchmakingConfigurationResponse)

instance
  Prelude.NFData
    CreateMatchmakingConfigurationResponse
  where
  rnf CreateMatchmakingConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf httpStatus
