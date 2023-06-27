{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types.MatchmakingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.MatchmakingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.BackfillMode
import Amazonka.GameLift.Types.FlexMatchMode
import Amazonka.GameLift.Types.GameProperty
import qualified Amazonka.Prelude as Prelude

-- | Guidelines for use with FlexMatch to match players into games. All
-- matchmaking requests must specify a matchmaking configuration.
--
-- /See:/ 'newMatchmakingConfiguration' smart constructor.
data MatchmakingConfiguration = MatchmakingConfiguration'
  { -- | A flag that indicates whether a match that was created with this
    -- configuration must be accepted by the matched players. To require
    -- acceptance, set to TRUE. When this option is enabled, matchmaking
    -- tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a
    -- completed potential match is waiting for player acceptance.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The length of time (in seconds) to wait for players to accept a proposed
    -- match, if acceptance is required. If any player rejects the match or
    -- fails to accept before the timeout, the ticket continues to look for an
    -- acceptable match.
    acceptanceTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of player slots in a match to keep open for future players.
    -- For example, if the configuration\'s rule set specifies a match for a
    -- single 12-person team, and the additional player count is set to 2, only
    -- 10 players are selected for the match. This parameter is not used when
    -- @FlexMatchMode@ is set to @STANDALONE@.
    additionalPlayerCount :: Prelude.Maybe Prelude.Natural,
    -- | The method used to backfill game sessions created with this matchmaking
    -- configuration. MANUAL indicates that the game makes backfill requests or
    -- does not use the match backfill feature. AUTOMATIC indicates that
    -- GameLift creates backfill requests whenever a game session has one or
    -- more open slots. Learn more about manual and automatic backfill in
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill existing games with FlexMatch>.
    -- Automatic backfill is not available when @FlexMatchMode@ is set to
    -- @STANDALONE@.
    backfillMode :: Prelude.Maybe BackfillMode,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift matchmaking configuration resource
    -- and uniquely identifies it. ARNs are unique across all Regions. Format
    -- is
    -- @arn:aws:gamelift:\<region>::matchmakingconfiguration\/\<matchmaking configuration name>@.
    -- In a Amazon GameLift configuration ARN, the resource ID matches the
    -- /Name/ value.
    configurationArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Information to attach to all events related to the matchmaking
    -- configuration.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with matchmaking configuration.
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
    -- created for a successful match. This parameter is not used when
    -- @FlexMatchMode@ is set to @STANDALONE@.
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process with a request to
    -- start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    -- This information is added to the new @GameSession@ object that is
    -- created for a successful match. This parameter is not used when
    -- @FlexMatchMode@ is set to @STANDALONE@.
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift game session queue resource and
    -- uniquely identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
    -- can be located in any Region. Queues are used to start new Amazon
    -- GameLift-hosted game sessions for matches that are created with this
    -- matchmaking configuration. This property is not set when @FlexMatchMode@
    -- is set to @STANDALONE@.
    gameSessionQueueArns :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for the matchmaking configuration. This name is used
    -- to identify the configuration associated with a matchmaking request or
    -- ticket.
    name :: Prelude.Maybe Prelude.Text,
    -- | An SNS topic ARN that is set up to receive matchmaking notifications.
    notificationTarget :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration, in seconds, that a matchmaking ticket can remain
    -- in process before timing out. Requests that fail due to timing out can
    -- be resubmitted as needed.
    requestTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift matchmaking rule set resource that this
    -- configuration uses.
    ruleSetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the matchmaking rule set to use with this
    -- configuration. A matchmaking configuration can only use rule sets that
    -- are defined in the same Region.
    ruleSetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchmakingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptanceRequired', 'matchmakingConfiguration_acceptanceRequired' - A flag that indicates whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to TRUE. When this option is enabled, matchmaking
-- tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a
-- completed potential match is waiting for player acceptance.
--
-- 'acceptanceTimeoutSeconds', 'matchmakingConfiguration_acceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required. If any player rejects the match or
-- fails to accept before the timeout, the ticket continues to look for an
-- acceptable match.
--
-- 'additionalPlayerCount', 'matchmakingConfiguration_additionalPlayerCount' - The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'backfillMode', 'matchmakingConfiguration_backfillMode' - The method used to backfill game sessions created with this matchmaking
-- configuration. MANUAL indicates that the game makes backfill requests or
-- does not use the match backfill feature. AUTOMATIC indicates that
-- GameLift creates backfill requests whenever a game session has one or
-- more open slots. Learn more about manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill existing games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
--
-- 'configurationArn', 'matchmakingConfiguration_configurationArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift matchmaking configuration resource
-- and uniquely identifies it. ARNs are unique across all Regions. Format
-- is
-- @arn:aws:gamelift:\<region>::matchmakingconfiguration\/\<matchmaking configuration name>@.
-- In a Amazon GameLift configuration ARN, the resource ID matches the
-- /Name/ value.
--
-- 'creationTime', 'matchmakingConfiguration_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'customEventData', 'matchmakingConfiguration_customEventData' - Information to attach to all events related to the matchmaking
-- configuration.
--
-- 'description', 'matchmakingConfiguration_description' - A descriptive label that is associated with matchmaking configuration.
--
-- 'flexMatchMode', 'matchmakingConfiguration_flexMatchMode' - Indicates whether this matchmaking configuration is being used with
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
-- 'gameProperties', 'matchmakingConfiguration_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'gameSessionData', 'matchmakingConfiguration_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
--
-- 'gameSessionQueueArns', 'matchmakingConfiguration_gameSessionQueueArns' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift game session queue resource and
-- uniquely identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new Amazon
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. This property is not set when @FlexMatchMode@
-- is set to @STANDALONE@.
--
-- 'name', 'matchmakingConfiguration_name' - A unique identifier for the matchmaking configuration. This name is used
-- to identify the configuration associated with a matchmaking request or
-- ticket.
--
-- 'notificationTarget', 'matchmakingConfiguration_notificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- 'requestTimeoutSeconds', 'matchmakingConfiguration_requestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
--
-- 'ruleSetArn', 'matchmakingConfiguration_ruleSetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift matchmaking rule set resource that this
-- configuration uses.
--
-- 'ruleSetName', 'matchmakingConfiguration_ruleSetName' - A unique identifier for the matchmaking rule set to use with this
-- configuration. A matchmaking configuration can only use rule sets that
-- are defined in the same Region.
newMatchmakingConfiguration ::
  MatchmakingConfiguration
newMatchmakingConfiguration =
  MatchmakingConfiguration'
    { acceptanceRequired =
        Prelude.Nothing,
      acceptanceTimeoutSeconds = Prelude.Nothing,
      additionalPlayerCount = Prelude.Nothing,
      backfillMode = Prelude.Nothing,
      configurationArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      customEventData = Prelude.Nothing,
      description = Prelude.Nothing,
      flexMatchMode = Prelude.Nothing,
      gameProperties = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      gameSessionQueueArns = Prelude.Nothing,
      name = Prelude.Nothing,
      notificationTarget = Prelude.Nothing,
      requestTimeoutSeconds = Prelude.Nothing,
      ruleSetArn = Prelude.Nothing,
      ruleSetName = Prelude.Nothing
    }

-- | A flag that indicates whether a match that was created with this
-- configuration must be accepted by the matched players. To require
-- acceptance, set to TRUE. When this option is enabled, matchmaking
-- tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a
-- completed potential match is waiting for player acceptance.
matchmakingConfiguration_acceptanceRequired :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Bool)
matchmakingConfiguration_acceptanceRequired = Lens.lens (\MatchmakingConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@MatchmakingConfiguration' {} a -> s {acceptanceRequired = a} :: MatchmakingConfiguration)

-- | The length of time (in seconds) to wait for players to accept a proposed
-- match, if acceptance is required. If any player rejects the match or
-- fails to accept before the timeout, the ticket continues to look for an
-- acceptable match.
matchmakingConfiguration_acceptanceTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
matchmakingConfiguration_acceptanceTimeoutSeconds = Lens.lens (\MatchmakingConfiguration' {acceptanceTimeoutSeconds} -> acceptanceTimeoutSeconds) (\s@MatchmakingConfiguration' {} a -> s {acceptanceTimeoutSeconds = a} :: MatchmakingConfiguration)

-- | The number of player slots in a match to keep open for future players.
-- For example, if the configuration\'s rule set specifies a match for a
-- single 12-person team, and the additional player count is set to 2, only
-- 10 players are selected for the match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
matchmakingConfiguration_additionalPlayerCount :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
matchmakingConfiguration_additionalPlayerCount = Lens.lens (\MatchmakingConfiguration' {additionalPlayerCount} -> additionalPlayerCount) (\s@MatchmakingConfiguration' {} a -> s {additionalPlayerCount = a} :: MatchmakingConfiguration)

-- | The method used to backfill game sessions created with this matchmaking
-- configuration. MANUAL indicates that the game makes backfill requests or
-- does not use the match backfill feature. AUTOMATIC indicates that
-- GameLift creates backfill requests whenever a game session has one or
-- more open slots. Learn more about manual and automatic backfill in
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill existing games with FlexMatch>.
-- Automatic backfill is not available when @FlexMatchMode@ is set to
-- @STANDALONE@.
matchmakingConfiguration_backfillMode :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe BackfillMode)
matchmakingConfiguration_backfillMode = Lens.lens (\MatchmakingConfiguration' {backfillMode} -> backfillMode) (\s@MatchmakingConfiguration' {} a -> s {backfillMode = a} :: MatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift matchmaking configuration resource
-- and uniquely identifies it. ARNs are unique across all Regions. Format
-- is
-- @arn:aws:gamelift:\<region>::matchmakingconfiguration\/\<matchmaking configuration name>@.
-- In a Amazon GameLift configuration ARN, the resource ID matches the
-- /Name/ value.
matchmakingConfiguration_configurationArn :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_configurationArn = Lens.lens (\MatchmakingConfiguration' {configurationArn} -> configurationArn) (\s@MatchmakingConfiguration' {} a -> s {configurationArn = a} :: MatchmakingConfiguration)

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
matchmakingConfiguration_creationTime :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.UTCTime)
matchmakingConfiguration_creationTime = Lens.lens (\MatchmakingConfiguration' {creationTime} -> creationTime) (\s@MatchmakingConfiguration' {} a -> s {creationTime = a} :: MatchmakingConfiguration) Prelude.. Lens.mapping Data._Time

-- | Information to attach to all events related to the matchmaking
-- configuration.
matchmakingConfiguration_customEventData :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_customEventData = Lens.lens (\MatchmakingConfiguration' {customEventData} -> customEventData) (\s@MatchmakingConfiguration' {} a -> s {customEventData = a} :: MatchmakingConfiguration)

-- | A descriptive label that is associated with matchmaking configuration.
matchmakingConfiguration_description :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_description = Lens.lens (\MatchmakingConfiguration' {description} -> description) (\s@MatchmakingConfiguration' {} a -> s {description = a} :: MatchmakingConfiguration)

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
matchmakingConfiguration_flexMatchMode :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe FlexMatchMode)
matchmakingConfiguration_flexMatchMode = Lens.lens (\MatchmakingConfiguration' {flexMatchMode} -> flexMatchMode) (\s@MatchmakingConfiguration' {} a -> s {flexMatchMode = a} :: MatchmakingConfiguration)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
matchmakingConfiguration_gameProperties :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe [GameProperty])
matchmakingConfiguration_gameProperties = Lens.lens (\MatchmakingConfiguration' {gameProperties} -> gameProperties) (\s@MatchmakingConfiguration' {} a -> s {gameProperties = a} :: MatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process with a request to
-- start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
-- This information is added to the new @GameSession@ object that is
-- created for a successful match. This parameter is not used when
-- @FlexMatchMode@ is set to @STANDALONE@.
matchmakingConfiguration_gameSessionData :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_gameSessionData = Lens.lens (\MatchmakingConfiguration' {gameSessionData} -> gameSessionData) (\s@MatchmakingConfiguration' {} a -> s {gameSessionData = a} :: MatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift game session queue resource and
-- uniquely identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. Queues
-- can be located in any Region. Queues are used to start new Amazon
-- GameLift-hosted game sessions for matches that are created with this
-- matchmaking configuration. This property is not set when @FlexMatchMode@
-- is set to @STANDALONE@.
matchmakingConfiguration_gameSessionQueueArns :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe [Prelude.Text])
matchmakingConfiguration_gameSessionQueueArns = Lens.lens (\MatchmakingConfiguration' {gameSessionQueueArns} -> gameSessionQueueArns) (\s@MatchmakingConfiguration' {} a -> s {gameSessionQueueArns = a} :: MatchmakingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the matchmaking configuration. This name is used
-- to identify the configuration associated with a matchmaking request or
-- ticket.
matchmakingConfiguration_name :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_name = Lens.lens (\MatchmakingConfiguration' {name} -> name) (\s@MatchmakingConfiguration' {} a -> s {name = a} :: MatchmakingConfiguration)

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
matchmakingConfiguration_notificationTarget :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_notificationTarget = Lens.lens (\MatchmakingConfiguration' {notificationTarget} -> notificationTarget) (\s@MatchmakingConfiguration' {} a -> s {notificationTarget = a} :: MatchmakingConfiguration)

-- | The maximum duration, in seconds, that a matchmaking ticket can remain
-- in process before timing out. Requests that fail due to timing out can
-- be resubmitted as needed.
matchmakingConfiguration_requestTimeoutSeconds :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Natural)
matchmakingConfiguration_requestTimeoutSeconds = Lens.lens (\MatchmakingConfiguration' {requestTimeoutSeconds} -> requestTimeoutSeconds) (\s@MatchmakingConfiguration' {} a -> s {requestTimeoutSeconds = a} :: MatchmakingConfiguration)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift matchmaking rule set resource that this
-- configuration uses.
matchmakingConfiguration_ruleSetArn :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_ruleSetArn = Lens.lens (\MatchmakingConfiguration' {ruleSetArn} -> ruleSetArn) (\s@MatchmakingConfiguration' {} a -> s {ruleSetArn = a} :: MatchmakingConfiguration)

-- | A unique identifier for the matchmaking rule set to use with this
-- configuration. A matchmaking configuration can only use rule sets that
-- are defined in the same Region.
matchmakingConfiguration_ruleSetName :: Lens.Lens' MatchmakingConfiguration (Prelude.Maybe Prelude.Text)
matchmakingConfiguration_ruleSetName = Lens.lens (\MatchmakingConfiguration' {ruleSetName} -> ruleSetName) (\s@MatchmakingConfiguration' {} a -> s {ruleSetName = a} :: MatchmakingConfiguration)

instance Data.FromJSON MatchmakingConfiguration where
  parseJSON =
    Data.withObject
      "MatchmakingConfiguration"
      ( \x ->
          MatchmakingConfiguration'
            Prelude.<$> (x Data..:? "AcceptanceRequired")
            Prelude.<*> (x Data..:? "AcceptanceTimeoutSeconds")
            Prelude.<*> (x Data..:? "AdditionalPlayerCount")
            Prelude.<*> (x Data..:? "BackfillMode")
            Prelude.<*> (x Data..:? "ConfigurationArn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CustomEventData")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FlexMatchMode")
            Prelude.<*> (x Data..:? "GameProperties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GameSessionData")
            Prelude.<*> ( x
                            Data..:? "GameSessionQueueArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotificationTarget")
            Prelude.<*> (x Data..:? "RequestTimeoutSeconds")
            Prelude.<*> (x Data..:? "RuleSetArn")
            Prelude.<*> (x Data..:? "RuleSetName")
      )

instance Prelude.Hashable MatchmakingConfiguration where
  hashWithSalt _salt MatchmakingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` acceptanceRequired
      `Prelude.hashWithSalt` acceptanceTimeoutSeconds
      `Prelude.hashWithSalt` additionalPlayerCount
      `Prelude.hashWithSalt` backfillMode
      `Prelude.hashWithSalt` configurationArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` customEventData
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` flexMatchMode
      `Prelude.hashWithSalt` gameProperties
      `Prelude.hashWithSalt` gameSessionData
      `Prelude.hashWithSalt` gameSessionQueueArns
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notificationTarget
      `Prelude.hashWithSalt` requestTimeoutSeconds
      `Prelude.hashWithSalt` ruleSetArn
      `Prelude.hashWithSalt` ruleSetName

instance Prelude.NFData MatchmakingConfiguration where
  rnf MatchmakingConfiguration' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf acceptanceTimeoutSeconds
      `Prelude.seq` Prelude.rnf additionalPlayerCount
      `Prelude.seq` Prelude.rnf backfillMode
      `Prelude.seq` Prelude.rnf configurationArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf flexMatchMode
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf gameSessionQueueArns
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf requestTimeoutSeconds
      `Prelude.seq` Prelude.rnf ruleSetArn
      `Prelude.seq` Prelude.rnf ruleSetName
