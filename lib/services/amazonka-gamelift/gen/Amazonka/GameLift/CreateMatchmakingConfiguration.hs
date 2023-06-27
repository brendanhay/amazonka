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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a new matchmaking configuration for use with FlexMatch. Whether
-- your are using FlexMatch with Amazon GameLift hosting or as a standalone
-- matchmaking service, the matchmaking configuration sets out rules for
-- matching players and forming teams. If you\'re also using Amazon
-- GameLift hosting, it defines how to start game sessions for each match.
-- Your matchmaking system can use multiple configurations to handle
-- different game scenarios. All matchmaking requests identify the
-- matchmaking configuration to use and provide player attributes
-- consistent with that configuration.
--
-- To create a matchmaking configuration, you must provide the following:
-- configuration name and FlexMatch mode (with or without Amazon GameLift
-- hosting); a rule set that specifies how to evaluate players and find
-- acceptable matches; whether player acceptance is required; and the
-- maximum time allowed for a matchmaking attempt. When using FlexMatch
-- with Amazon GameLift hosting, you also need to identify the game session
-- queue to use when starting a game session for the match.
--
-- In addition, you must set up an Amazon Simple Notification Service topic
-- to receive matchmaking notifications. Provide the topic ARN in the
-- matchmaking configuration.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch matchmaker>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set up FlexMatch event notification>
module Amazonka.GameLift.CreateMatchmakingConfiguration
  ( -- * Creating a Request
    CreateMatchmakingConfiguration (..),
    newCreateMatchmakingConfiguration,

    -- * Request Lenses
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_tags,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { -- | The length of time (in seconds) to wait for players to accept a proposed
    -- match, if acceptance is required.
    acceptanceTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of player slots in a match to keep open for future players.
    -- For example, if the configuration\'s rule set specifies a match for a
    -- single 12-person team, and the additional player count is set to 2, only
    -- 10 players are selected for the match. This parameter is not used if
    -- @FlexMatchMode@ is set to @STANDALONE@.
    additionalPlayerCount :: Prelude.Maybe Prelude.Natural,
    -- | The method used to backfill game sessions that are created with this
    -- matchmaking configuration. Specify @MANUAL@ when your game manages
    -- backfill requests manually or does not use the match backfill feature.
    -- Specify @AUTOMATIC@ to have Amazon GameLift create a backfill request
    -- whenever a game session has one or more open slots. Learn more about
    -- manual and automatic backfill in
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
    -- Automatic backfill is not available when @FlexMatchMode@ is set to
    -- @STANDALONE@.
    backfillMode :: Prelude.Maybe BackfillMode,
    -- | Information to be added to all events related to this matchmaking
    -- configuration.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the matchmaking configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this matchmaking configuration is being used with
    -- Amazon GameLift hosting or as a standalone matchmaking solution.
    --
    -- -   __STANDALONE__ - FlexMatch forms matches and returns match
    --     information, including players and team assignments, in a
    --     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
    --     event.
    --
    -- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
    --     Amazon GameLift queue to start a game session for the match.
    flexMatchMode :: Prelude.Maybe FlexMatchMode,
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process with a
    -- request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new @GameSession@ object that is
    -- created for a successful match. This parameter is not used if
    -- @FlexMatchMode@ is set to @STANDALONE@.
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process with a request to
    -- start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new @GameSession@ object that is
    -- created for a successful match. This parameter is not used if
    -- @FlexMatchMode@ is set to @STANDALONE@.
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift game session queue resource and
    -- uniquely identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
    -- can be located in any Region. Queues are used to start new Amazon
    -- GameLift-hosted game sessions for matches that are created with this
    -- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
    -- not set this parameter.
    gameSessionQueueArns :: Prelude.Maybe [Prelude.Text],
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    -- See
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
    -- for more information.
    notificationTarget :: Prelude.Maybe Prelude.Text,
    -- | A list of labels to assign to the new matchmaking configuration
    -- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
    -- Services resources are useful for resource management, access management
    -- and cost allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe [Tag],
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
-- 'acceptanceTimeoutSeconds', 'createMatchmakingConfiguration_acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
--
-- 'additionalPlayerCount', 'createMatchmakingConfiguration_additionalPlayerCount' - The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'backfillMode', 'createMatchmakingConfiguration_backfillMode' - The method used to backfill game sessions that are created with this
-- matchmaking configuration. Specify @MANUAL@ when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify @AUTOMATIC@ to have Amazon GameLift create a backfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
--
-- 'customEventData', 'createMatchmakingConfiguration_customEventData' - Information to be added to all events related to this matchmaking
-- configuration.
--
-- 'description', 'createMatchmakingConfiguration_description' - A human-readable description of the matchmaking configuration.
--
-- 'flexMatchMode', 'createMatchmakingConfiguration_flexMatchMode' - Indicates whether this matchmaking configuration is being used with
-- Amazon GameLift hosting or as a standalone matchmaking solution.
--
-- -   __STANDALONE__ - FlexMatch forms matches and returns match
--     information, including players and team assignments, in a
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
--     event.
--
-- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
--     Amazon GameLift queue to start a game session for the match.
--
-- 'gameProperties', 'createMatchmakingConfiguration_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'gameSessionData', 'createMatchmakingConfiguration_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'gameSessionQueueArns', 'createMatchmakingConfiguration_gameSessionQueueArns' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift game session queue resource and
-- uniquely identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new Amazon
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
--
-- 'notificationTarget', 'createMatchmakingConfiguration_notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
--
-- 'tags', 'createMatchmakingConfiguration_tags' - A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
-- Services resources are useful for resource management, access management
-- and cost allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
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
      { acceptanceTimeoutSeconds =
          Prelude.Nothing,
        additionalPlayerCount = Prelude.Nothing,
        backfillMode = Prelude.Nothing,
        customEventData = Prelude.Nothing,
        description = Prelude.Nothing,
        flexMatchMode = Prelude.Nothing,
        gameProperties = Prelude.Nothing,
        gameSessionData = Prelude.Nothing,
        gameSessionQueueArns = Prelude.Nothing,
        notificationTarget = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        requestTimeoutSeconds =
          pRequestTimeoutSeconds_,
        acceptanceRequired = pAcceptanceRequired_,
        ruleSetName = pRuleSetName_
      }

-- | The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
createMatchmakingConfiguration_acceptanceTimeoutSeconds :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
createMatchmakingConfiguration_acceptanceTimeoutSeconds = Lens.lens (\CreateMatchmakingConfiguration' {acceptanceTimeoutSeconds} -> acceptanceTimeoutSeconds) (\s@CreateMatchmakingConfiguration' {} a -> s {acceptanceTimeoutSeconds = a} :: CreateMatchmakingConfiguration)

-- | The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
createMatchmakingConfiguration_additionalPlayerCount :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
createMatchmakingConfiguration_additionalPlayerCount = Lens.lens (\CreateMatchmakingConfiguration' {additionalPlayerCount} -> additionalPlayerCount) (\s@CreateMatchmakingConfiguration' {} a -> s {additionalPlayerCount = a} :: CreateMatchmakingConfiguration)

-- | The method used to backfill game sessions that are created with this
-- matchmaking configuration. Specify @MANUAL@ when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify @AUTOMATIC@ to have Amazon GameLift create a backfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
createMatchmakingConfiguration_backfillMode :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe BackfillMode)
createMatchmakingConfiguration_backfillMode = Lens.lens (\CreateMatchmakingConfiguration' {backfillMode} -> backfillMode) (\s@CreateMatchmakingConfiguration' {} a -> s {backfillMode = a} :: CreateMatchmakingConfiguration)

-- | Information to be added to all events related to this matchmaking
-- configuration.
createMatchmakingConfiguration_customEventData :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_customEventData = Lens.lens (\CreateMatchmakingConfiguration' {customEventData} -> customEventData) (\s@CreateMatchmakingConfiguration' {} a -> s {customEventData = a} :: CreateMatchmakingConfiguration)

-- | A human-readable description of the matchmaking configuration.
createMatchmakingConfiguration_description :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_description = Lens.lens (\CreateMatchmakingConfiguration' {description} -> description) (\s@CreateMatchmakingConfiguration' {} a -> s {description = a} :: CreateMatchmakingConfiguration)

-- | Indicates whether this matchmaking configuration is being used with
-- Amazon GameLift hosting or as a standalone matchmaking solution.
--
-- -   __STANDALONE__ - FlexMatch forms matches and returns match
--     information, including players and team assignments, in a
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded>
--     event.
--
-- -   __WITH_QUEUE__ - FlexMatch forms matches and uses the specified
--     Amazon GameLift queue to start a game session for the match.
createMatchmakingConfiguration_flexMatchMode :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe FlexMatchMode)
createMatchmakingConfiguration_flexMatchMode = Lens.lens (\CreateMatchmakingConfiguration' {flexMatchMode} -> flexMatchMode) (\s@CreateMatchmakingConfiguration' {} a -> s {flexMatchMode = a} :: CreateMatchmakingConfiguration)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
createMatchmakingConfiguration_gameProperties :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [GameProperty])
createMatchmakingConfiguration_gameProperties = Lens.lens (\CreateMatchmakingConfiguration' {gameProperties} -> gameProperties) (\s@CreateMatchmakingConfiguration' {} a -> s {gameProperties = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
createMatchmakingConfiguration_gameSessionData :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_gameSessionData = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionData} -> gameSessionData) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionData = a} :: CreateMatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift game session queue resource and
-- uniquely identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new Amazon
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
createMatchmakingConfiguration_gameSessionQueueArns :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [Prelude.Text])
createMatchmakingConfiguration_gameSessionQueueArns = Lens.lens (\CreateMatchmakingConfiguration' {gameSessionQueueArns} -> gameSessionQueueArns) (\s@CreateMatchmakingConfiguration' {} a -> s {gameSessionQueueArns = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
createMatchmakingConfiguration_notificationTarget :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
createMatchmakingConfiguration_notificationTarget = Lens.lens (\CreateMatchmakingConfiguration' {notificationTarget} -> notificationTarget) (\s@CreateMatchmakingConfiguration' {} a -> s {notificationTarget = a} :: CreateMatchmakingConfiguration)

-- | A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
-- Services resources are useful for resource management, access management
-- and cost allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
createMatchmakingConfiguration_tags :: Lens.Lens' CreateMatchmakingConfiguration (Prelude.Maybe [Tag])
createMatchmakingConfiguration_tags = Lens.lens (\CreateMatchmakingConfiguration' {tags} -> tags) (\s@CreateMatchmakingConfiguration' {} a -> s {tags = a} :: CreateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMatchmakingConfigurationResponse'
            Prelude.<$> (x Data..?> "Configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMatchmakingConfiguration
  where
  hashWithSalt
    _salt
    CreateMatchmakingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` acceptanceTimeoutSeconds
        `Prelude.hashWithSalt` additionalPlayerCount
        `Prelude.hashWithSalt` backfillMode
        `Prelude.hashWithSalt` customEventData
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` flexMatchMode
        `Prelude.hashWithSalt` gameProperties
        `Prelude.hashWithSalt` gameSessionData
        `Prelude.hashWithSalt` gameSessionQueueArns
        `Prelude.hashWithSalt` notificationTarget
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` requestTimeoutSeconds
        `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` ruleSetName

instance
  Prelude.NFData
    CreateMatchmakingConfiguration
  where
  rnf CreateMatchmakingConfiguration' {..} =
    Prelude.rnf acceptanceTimeoutSeconds
      `Prelude.seq` Prelude.rnf additionalPlayerCount
      `Prelude.seq` Prelude.rnf backfillMode
      `Prelude.seq` Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf flexMatchMode
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf gameSessionQueueArns
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestTimeoutSeconds
      `Prelude.seq` Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf ruleSetName

instance
  Data.ToHeaders
    CreateMatchmakingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreateMatchmakingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptanceTimeoutSeconds" Data..=)
              Prelude.<$> acceptanceTimeoutSeconds,
            ("AdditionalPlayerCount" Data..=)
              Prelude.<$> additionalPlayerCount,
            ("BackfillMode" Data..=) Prelude.<$> backfillMode,
            ("CustomEventData" Data..=)
              Prelude.<$> customEventData,
            ("Description" Data..=) Prelude.<$> description,
            ("FlexMatchMode" Data..=) Prelude.<$> flexMatchMode,
            ("GameProperties" Data..=)
              Prelude.<$> gameProperties,
            ("GameSessionData" Data..=)
              Prelude.<$> gameSessionData,
            ("GameSessionQueueArns" Data..=)
              Prelude.<$> gameSessionQueueArns,
            ("NotificationTarget" Data..=)
              Prelude.<$> notificationTarget,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "RequestTimeoutSeconds"
                  Data..= requestTimeoutSeconds
              ),
            Prelude.Just
              ("AcceptanceRequired" Data..= acceptanceRequired),
            Prelude.Just ("RuleSetName" Data..= ruleSetName)
          ]
      )

instance Data.ToPath CreateMatchmakingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMatchmakingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMatchmakingConfigurationResponse' smart constructor.
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
