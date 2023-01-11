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
-- Module      : Amazonka.GameLift.UpdateMatchmakingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a FlexMatch matchmaking configuration. These
-- changes affect all matches and game sessions that are created after the
-- update. To update settings, specify the configuration name to be updated
-- and provide the new settings.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch matchmaker>
module Amazonka.GameLift.UpdateMatchmakingConfiguration
  ( -- * Creating a Request
    UpdateMatchmakingConfiguration (..),
    newUpdateMatchmakingConfiguration,

    -- * Request Lenses
    updateMatchmakingConfiguration_acceptanceRequired,
    updateMatchmakingConfiguration_acceptanceTimeoutSeconds,
    updateMatchmakingConfiguration_additionalPlayerCount,
    updateMatchmakingConfiguration_backfillMode,
    updateMatchmakingConfiguration_customEventData,
    updateMatchmakingConfiguration_description,
    updateMatchmakingConfiguration_flexMatchMode,
    updateMatchmakingConfiguration_gameProperties,
    updateMatchmakingConfiguration_gameSessionData,
    updateMatchmakingConfiguration_gameSessionQueueArns,
    updateMatchmakingConfiguration_notificationTarget,
    updateMatchmakingConfiguration_requestTimeoutSeconds,
    updateMatchmakingConfiguration_ruleSetName,
    updateMatchmakingConfiguration_name,

    -- * Destructuring the Response
    UpdateMatchmakingConfigurationResponse (..),
    newUpdateMatchmakingConfigurationResponse,

    -- * Response Lenses
    updateMatchmakingConfigurationResponse_configuration,
    updateMatchmakingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMatchmakingConfiguration' smart constructor.
data UpdateMatchmakingConfiguration = UpdateMatchmakingConfiguration'
  { -- | A flag that indicates whether a match that was created with this
    -- configuration must be accepted by the matched players. To require
    -- acceptance, set to TRUE. With this option enabled, matchmaking tickets
    -- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
    -- potential match is waiting for player acceptance.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The length of time (in seconds) to wait for players to accept a proposed
    -- match, if acceptance is required.
    acceptanceTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of player slots in a match to keep open for future players.
    -- For example, if the configuration\'s rule set specifies a match for a
    -- single 12-person team, and the additional player count is set to 2, only
    -- 10 players are selected for the match. This parameter is not used if
    -- @FlexMatchMode@ is set to @STANDALONE@.
    additionalPlayerCount :: Prelude.Maybe Prelude.Natural,
    -- | The method that is used to backfill game sessions created with this
    -- matchmaking configuration. Specify MANUAL when your game manages
    -- backfill requests manually or does not use the match backfill feature.
    -- Specify AUTOMATIC to have GameLift create a match backfill request
    -- whenever a game session has one or more open slots. Learn more about
    -- manual and automatic backfill in
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
    -- Automatic backfill is not available when @FlexMatchMode@ is set to
    -- @STANDALONE@.
    backfillMode :: Prelude.Maybe BackfillMode,
    -- | Information to add to all events related to the matchmaking
    -- configuration.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A description for the matchmaking configuration.
    description :: Prelude.Maybe Prelude.Text,
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
    -- This information is added to the game session that is created for a
    -- successful match. This parameter is not used if @FlexMatchMode@ is set
    -- to @STANDALONE@.
    gameSessionData :: Prelude.Maybe Prelude.Text,
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
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    -- See
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
    -- for more information.
    notificationTarget :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain
    -- in process before timing out. Requests that fail due to timing out can
    -- be resubmitted as needed.
    requestTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the matchmaking rule set to use with this
    -- configuration. You can use either the rule set name or ARN value. A
    -- matchmaking configuration can only use rule sets that are defined in the
    -- same Region.
    ruleSetName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the matchmaking configuration to update. You can
    -- use either the configuration name or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMatchmakingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptanceRequired', 'updateMatchmakingConfiguration_acceptanceRequired' - A flag that indicates whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to TRUE. With this option enabled, matchmaking tickets
-- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
-- potential match is waiting for player acceptance.
--
-- 'acceptanceTimeoutSeconds', 'updateMatchmakingConfiguration_acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
--
-- 'additionalPlayerCount', 'updateMatchmakingConfiguration_additionalPlayerCount' - The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'backfillMode', 'updateMatchmakingConfiguration_backfillMode' - The method that is used to backfill game sessions created with this
-- matchmaking configuration. Specify MANUAL when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify AUTOMATIC to have GameLift create a match backfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
--
-- 'customEventData', 'updateMatchmakingConfiguration_customEventData' - Information to add to all events related to the matchmaking
-- configuration.
--
-- 'description', 'updateMatchmakingConfiguration_description' - A description for the matchmaking configuration.
--
-- 'flexMatchMode', 'updateMatchmakingConfiguration_flexMatchMode' - Indicates whether this matchmaking configuration is being used with
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
-- 'gameProperties', 'updateMatchmakingConfiguration_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'gameSessionData', 'updateMatchmakingConfiguration_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the game session that is created for a
-- successful match. This parameter is not used if @FlexMatchMode@ is set
-- to @STANDALONE@.
--
-- 'gameSessionQueueArns', 'updateMatchmakingConfiguration_gameSessionQueueArns' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
--
-- 'notificationTarget', 'updateMatchmakingConfiguration_notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
--
-- 'requestTimeoutSeconds', 'updateMatchmakingConfiguration_requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
--
-- 'ruleSetName', 'updateMatchmakingConfiguration_ruleSetName' - A unique identifier for the matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
--
-- 'name', 'updateMatchmakingConfiguration_name' - A unique identifier for the matchmaking configuration to update. You can
-- use either the configuration name or ARN value.
newUpdateMatchmakingConfiguration ::
  -- | 'name'
  Prelude.Text ->
  UpdateMatchmakingConfiguration
newUpdateMatchmakingConfiguration pName_ =
  UpdateMatchmakingConfiguration'
    { acceptanceRequired =
        Prelude.Nothing,
      acceptanceTimeoutSeconds = Prelude.Nothing,
      additionalPlayerCount = Prelude.Nothing,
      backfillMode = Prelude.Nothing,
      customEventData = Prelude.Nothing,
      description = Prelude.Nothing,
      flexMatchMode = Prelude.Nothing,
      gameProperties = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      gameSessionQueueArns = Prelude.Nothing,
      notificationTarget = Prelude.Nothing,
      requestTimeoutSeconds = Prelude.Nothing,
      ruleSetName = Prelude.Nothing,
      name = pName_
    }

-- | A flag that indicates whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to TRUE. With this option enabled, matchmaking tickets
-- use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed
-- potential match is waiting for player acceptance.
updateMatchmakingConfiguration_acceptanceRequired :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Bool)
updateMatchmakingConfiguration_acceptanceRequired = Lens.lens (\UpdateMatchmakingConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@UpdateMatchmakingConfiguration' {} a -> s {acceptanceRequired = a} :: UpdateMatchmakingConfiguration)

-- | The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required.
updateMatchmakingConfiguration_acceptanceTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
updateMatchmakingConfiguration_acceptanceTimeoutSeconds = Lens.lens (\UpdateMatchmakingConfiguration' {acceptanceTimeoutSeconds} -> acceptanceTimeoutSeconds) (\s@UpdateMatchmakingConfiguration' {} a -> s {acceptanceTimeoutSeconds = a} :: UpdateMatchmakingConfiguration)

-- | The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
updateMatchmakingConfiguration_additionalPlayerCount :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
updateMatchmakingConfiguration_additionalPlayerCount = Lens.lens (\UpdateMatchmakingConfiguration' {additionalPlayerCount} -> additionalPlayerCount) (\s@UpdateMatchmakingConfiguration' {} a -> s {additionalPlayerCount = a} :: UpdateMatchmakingConfiguration)

-- | The method that is used to backfill game sessions created with this
-- matchmaking configuration. Specify MANUAL when your game manages
-- backfill requests manually or does not use the match backfill feature.
-- Specify AUTOMATIC to have GameLift create a match backfill request
-- whenever a game session has one or more open slots. Learn more about
-- manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
updateMatchmakingConfiguration_backfillMode :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe BackfillMode)
updateMatchmakingConfiguration_backfillMode = Lens.lens (\UpdateMatchmakingConfiguration' {backfillMode} -> backfillMode) (\s@UpdateMatchmakingConfiguration' {} a -> s {backfillMode = a} :: UpdateMatchmakingConfiguration)

-- | Information to add to all events related to the matchmaking
-- configuration.
updateMatchmakingConfiguration_customEventData :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
updateMatchmakingConfiguration_customEventData = Lens.lens (\UpdateMatchmakingConfiguration' {customEventData} -> customEventData) (\s@UpdateMatchmakingConfiguration' {} a -> s {customEventData = a} :: UpdateMatchmakingConfiguration)

-- | A description for the matchmaking configuration.
updateMatchmakingConfiguration_description :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
updateMatchmakingConfiguration_description = Lens.lens (\UpdateMatchmakingConfiguration' {description} -> description) (\s@UpdateMatchmakingConfiguration' {} a -> s {description = a} :: UpdateMatchmakingConfiguration)

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
updateMatchmakingConfiguration_flexMatchMode :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe FlexMatchMode)
updateMatchmakingConfiguration_flexMatchMode = Lens.lens (\UpdateMatchmakingConfiguration' {flexMatchMode} -> flexMatchMode) (\s@UpdateMatchmakingConfiguration' {} a -> s {flexMatchMode = a} :: UpdateMatchmakingConfiguration)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used if
-- @FlexMatchMode@ is set to @STANDALONE@.
updateMatchmakingConfiguration_gameProperties :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe [GameProperty])
updateMatchmakingConfiguration_gameProperties = Lens.lens (\UpdateMatchmakingConfiguration' {gameProperties} -> gameProperties) (\s@UpdateMatchmakingConfiguration' {} a -> s {gameProperties = a} :: UpdateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the game session that is created for a
-- successful match. This parameter is not used if @FlexMatchMode@ is set
-- to @STANDALONE@.
updateMatchmakingConfiguration_gameSessionData :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
updateMatchmakingConfiguration_gameSessionData = Lens.lens (\UpdateMatchmakingConfiguration' {gameSessionData} -> gameSessionData) (\s@UpdateMatchmakingConfiguration' {} a -> s {gameSessionData = a} :: UpdateMatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@, do
-- not set this parameter.
updateMatchmakingConfiguration_gameSessionQueueArns :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe [Prelude.Text])
updateMatchmakingConfiguration_gameSessionQueueArns = Lens.lens (\UpdateMatchmakingConfiguration' {gameSessionQueueArns} -> gameSessionQueueArns) (\s@UpdateMatchmakingConfiguration' {} a -> s {gameSessionQueueArns = a} :: UpdateMatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
-- See
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up notifications for matchmaking>
-- for more information.
updateMatchmakingConfiguration_notificationTarget :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
updateMatchmakingConfiguration_notificationTarget = Lens.lens (\UpdateMatchmakingConfiguration' {notificationTarget} -> notificationTarget) (\s@UpdateMatchmakingConfiguration' {} a -> s {notificationTarget = a} :: UpdateMatchmakingConfiguration)

-- | The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
updateMatchmakingConfiguration_requestTimeoutSeconds :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
updateMatchmakingConfiguration_requestTimeoutSeconds = Lens.lens (\UpdateMatchmakingConfiguration' {requestTimeoutSeconds} -> requestTimeoutSeconds) (\s@UpdateMatchmakingConfiguration' {} a -> s {requestTimeoutSeconds = a} :: UpdateMatchmakingConfiguration)

-- | A unique identifier for the matchmaking rule set to use with this
-- configuration. You can use either the rule set name or ARN value. A
-- matchmaking configuration can only use rule sets that are defined in the
-- same Region.
updateMatchmakingConfiguration_ruleSetName :: Lens.Lens' UpdateMatchmakingConfiguration (Prelude.Maybe Prelude.Text)
updateMatchmakingConfiguration_ruleSetName = Lens.lens (\UpdateMatchmakingConfiguration' {ruleSetName} -> ruleSetName) (\s@UpdateMatchmakingConfiguration' {} a -> s {ruleSetName = a} :: UpdateMatchmakingConfiguration)

-- | A unique identifier for the matchmaking configuration to update. You can
-- use either the configuration name or ARN value.
updateMatchmakingConfiguration_name :: Lens.Lens' UpdateMatchmakingConfiguration Prelude.Text
updateMatchmakingConfiguration_name = Lens.lens (\UpdateMatchmakingConfiguration' {name} -> name) (\s@UpdateMatchmakingConfiguration' {} a -> s {name = a} :: UpdateMatchmakingConfiguration)

instance
  Core.AWSRequest
    UpdateMatchmakingConfiguration
  where
  type
    AWSResponse UpdateMatchmakingConfiguration =
      UpdateMatchmakingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMatchmakingConfigurationResponse'
            Prelude.<$> (x Data..?> "Configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateMatchmakingConfiguration
  where
  hashWithSalt
    _salt
    UpdateMatchmakingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` acceptanceRequired
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
        `Prelude.hashWithSalt` requestTimeoutSeconds
        `Prelude.hashWithSalt` ruleSetName
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    UpdateMatchmakingConfiguration
  where
  rnf UpdateMatchmakingConfiguration' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf acceptanceTimeoutSeconds
      `Prelude.seq` Prelude.rnf additionalPlayerCount
      `Prelude.seq` Prelude.rnf backfillMode
      `Prelude.seq` Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf flexMatchMode
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf gameSessionQueueArns
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf requestTimeoutSeconds
      `Prelude.seq` Prelude.rnf ruleSetName
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    UpdateMatchmakingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.UpdateMatchmakingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMatchmakingConfiguration where
  toJSON UpdateMatchmakingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptanceRequired" Data..=)
              Prelude.<$> acceptanceRequired,
            ("AcceptanceTimeoutSeconds" Data..=)
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
            ("RequestTimeoutSeconds" Data..=)
              Prelude.<$> requestTimeoutSeconds,
            ("RuleSetName" Data..=) Prelude.<$> ruleSetName,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateMatchmakingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMatchmakingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMatchmakingConfigurationResponse' smart constructor.
data UpdateMatchmakingConfigurationResponse = UpdateMatchmakingConfigurationResponse'
  { -- | The updated matchmaking configuration.
    configuration :: Prelude.Maybe MatchmakingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMatchmakingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateMatchmakingConfigurationResponse_configuration' - The updated matchmaking configuration.
--
-- 'httpStatus', 'updateMatchmakingConfigurationResponse_httpStatus' - The response's http status code.
newUpdateMatchmakingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMatchmakingConfigurationResponse
newUpdateMatchmakingConfigurationResponse
  pHttpStatus_ =
    UpdateMatchmakingConfigurationResponse'
      { configuration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated matchmaking configuration.
updateMatchmakingConfigurationResponse_configuration :: Lens.Lens' UpdateMatchmakingConfigurationResponse (Prelude.Maybe MatchmakingConfiguration)
updateMatchmakingConfigurationResponse_configuration = Lens.lens (\UpdateMatchmakingConfigurationResponse' {configuration} -> configuration) (\s@UpdateMatchmakingConfigurationResponse' {} a -> s {configuration = a} :: UpdateMatchmakingConfigurationResponse)

-- | The response's http status code.
updateMatchmakingConfigurationResponse_httpStatus :: Lens.Lens' UpdateMatchmakingConfigurationResponse Prelude.Int
updateMatchmakingConfigurationResponse_httpStatus = Lens.lens (\UpdateMatchmakingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateMatchmakingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateMatchmakingConfigurationResponse)

instance
  Prelude.NFData
    UpdateMatchmakingConfigurationResponse
  where
  rnf UpdateMatchmakingConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf httpStatus
