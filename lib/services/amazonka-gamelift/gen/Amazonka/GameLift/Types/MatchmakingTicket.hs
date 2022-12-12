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
-- Module      : Amazonka.GameLift.Types.MatchmakingTicket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.MatchmakingTicket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.GameSessionConnectionInfo
import Amazonka.GameLift.Types.MatchmakingConfigurationStatus
import Amazonka.GameLift.Types.Player
import qualified Amazonka.Prelude as Prelude

-- | Ticket generated to track the progress of a matchmaking request. Each
-- ticket is uniquely identified by a ticket ID, supplied by the requester,
-- when creating a matchmaking request.
--
-- /See:/ 'newMatchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift matchmaking configuration resource that is
    -- used with this ticket.
    configurationArn :: Prelude.Maybe Prelude.Text,
    -- | Name of the matchmaking configuration that is used with this ticket.
    -- Matchmaking configurations determine how players are grouped into a
    -- match and how a new game session is created for the match.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when the matchmaking request stopped being
    -- processed due to successful completion, timeout, or cancellation. Format
    -- is a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Average amount of time (in seconds) that players are currently waiting
    -- for a match. If there is not enough recent data, this property may be
    -- empty.
    estimatedWaitTime :: Prelude.Maybe Prelude.Natural,
    -- | Connection information for a new game session. Once a match is made, the
    -- FlexMatch engine creates a new game session for it. This information is
    -- added to the matchmaking ticket, which you can be retrieve by calling
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeMatchmaking.html DescribeMatchmaking>
    -- .
    gameSessionConnectionInfo :: Prelude.Maybe GameSessionConnectionInfo,
    -- | A set of @Player@ objects, each representing a player to find matches
    -- for. Players are identified by a unique player ID and may include
    -- latency data for use during matchmaking. If the ticket is in status
    -- @COMPLETED@, the @Player@ objects include the team the players were
    -- assigned to in the resulting match.
    players :: Prelude.Maybe [Player],
    -- | Time stamp indicating when this matchmaking request was received. Format
    -- is a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Current status of the matchmaking request.
    --
    -- -   __QUEUED__ -- The matchmaking request has been received and is
    --     currently waiting to be processed.
    --
    -- -   __SEARCHING__ -- The matchmaking request is currently being
    --     processed.
    --
    -- -   __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players
    --     must accept the match. This status is used only with requests that
    --     use a matchmaking configuration with a player acceptance
    --     requirement.
    --
    -- -   __PLACING__ -- The FlexMatch engine has matched players and is in
    --     the process of placing a new game session for the match.
    --
    -- -   __COMPLETED__ -- Players have been matched and a game session is
    --     ready to host the players. A ticket in this state contains the
    --     necessary connection information for players.
    --
    -- -   __FAILED__ -- The matchmaking request was not completed.
    --
    -- -   __CANCELLED__ -- The matchmaking request was canceled. This may be
    --     the result of a @StopMatchmaking@ operation or a proposed match that
    --     one or more players failed to accept.
    --
    -- -   __TIMED_OUT__ -- The matchmaking request was not successful within
    --     the duration specified in the matchmaking configuration.
    --
    -- Matchmaking requests that fail to successfully complete (statuses
    -- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
    -- new ticket IDs.
    status :: Prelude.Maybe MatchmakingConfigurationStatus,
    -- | Additional information about the current status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Code to explain the current status. For example, a status reason may
    -- indicate when a ticket has returned to @SEARCHING@ status after a
    -- proposed match fails to receive player acceptances.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a matchmaking ticket.
    ticketId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchmakingTicket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationArn', 'matchmakingTicket_configurationArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift matchmaking configuration resource that is
-- used with this ticket.
--
-- 'configurationName', 'matchmakingTicket_configurationName' - Name of the matchmaking configuration that is used with this ticket.
-- Matchmaking configurations determine how players are grouped into a
-- match and how a new game session is created for the match.
--
-- 'endTime', 'matchmakingTicket_endTime' - Time stamp indicating when the matchmaking request stopped being
-- processed due to successful completion, timeout, or cancellation. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'estimatedWaitTime', 'matchmakingTicket_estimatedWaitTime' - Average amount of time (in seconds) that players are currently waiting
-- for a match. If there is not enough recent data, this property may be
-- empty.
--
-- 'gameSessionConnectionInfo', 'matchmakingTicket_gameSessionConnectionInfo' - Connection information for a new game session. Once a match is made, the
-- FlexMatch engine creates a new game session for it. This information is
-- added to the matchmaking ticket, which you can be retrieve by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeMatchmaking.html DescribeMatchmaking>
-- .
--
-- 'players', 'matchmakingTicket_players' - A set of @Player@ objects, each representing a player to find matches
-- for. Players are identified by a unique player ID and may include
-- latency data for use during matchmaking. If the ticket is in status
-- @COMPLETED@, the @Player@ objects include the team the players were
-- assigned to in the resulting match.
--
-- 'startTime', 'matchmakingTicket_startTime' - Time stamp indicating when this matchmaking request was received. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'status', 'matchmakingTicket_status' - Current status of the matchmaking request.
--
-- -   __QUEUED__ -- The matchmaking request has been received and is
--     currently waiting to be processed.
--
-- -   __SEARCHING__ -- The matchmaking request is currently being
--     processed.
--
-- -   __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players
--     must accept the match. This status is used only with requests that
--     use a matchmaking configuration with a player acceptance
--     requirement.
--
-- -   __PLACING__ -- The FlexMatch engine has matched players and is in
--     the process of placing a new game session for the match.
--
-- -   __COMPLETED__ -- Players have been matched and a game session is
--     ready to host the players. A ticket in this state contains the
--     necessary connection information for players.
--
-- -   __FAILED__ -- The matchmaking request was not completed.
--
-- -   __CANCELLED__ -- The matchmaking request was canceled. This may be
--     the result of a @StopMatchmaking@ operation or a proposed match that
--     one or more players failed to accept.
--
-- -   __TIMED_OUT__ -- The matchmaking request was not successful within
--     the duration specified in the matchmaking configuration.
--
-- Matchmaking requests that fail to successfully complete (statuses
-- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
-- new ticket IDs.
--
-- 'statusMessage', 'matchmakingTicket_statusMessage' - Additional information about the current status.
--
-- 'statusReason', 'matchmakingTicket_statusReason' - Code to explain the current status. For example, a status reason may
-- indicate when a ticket has returned to @SEARCHING@ status after a
-- proposed match fails to receive player acceptances.
--
-- 'ticketId', 'matchmakingTicket_ticketId' - A unique identifier for a matchmaking ticket.
newMatchmakingTicket ::
  MatchmakingTicket
newMatchmakingTicket =
  MatchmakingTicket'
    { configurationArn =
        Prelude.Nothing,
      configurationName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      estimatedWaitTime = Prelude.Nothing,
      gameSessionConnectionInfo = Prelude.Nothing,
      players = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      ticketId = Prelude.Nothing
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift matchmaking configuration resource that is
-- used with this ticket.
matchmakingTicket_configurationArn :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Text)
matchmakingTicket_configurationArn = Lens.lens (\MatchmakingTicket' {configurationArn} -> configurationArn) (\s@MatchmakingTicket' {} a -> s {configurationArn = a} :: MatchmakingTicket)

-- | Name of the matchmaking configuration that is used with this ticket.
-- Matchmaking configurations determine how players are grouped into a
-- match and how a new game session is created for the match.
matchmakingTicket_configurationName :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Text)
matchmakingTicket_configurationName = Lens.lens (\MatchmakingTicket' {configurationName} -> configurationName) (\s@MatchmakingTicket' {} a -> s {configurationName = a} :: MatchmakingTicket)

-- | Time stamp indicating when the matchmaking request stopped being
-- processed due to successful completion, timeout, or cancellation. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
matchmakingTicket_endTime :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.UTCTime)
matchmakingTicket_endTime = Lens.lens (\MatchmakingTicket' {endTime} -> endTime) (\s@MatchmakingTicket' {} a -> s {endTime = a} :: MatchmakingTicket) Prelude.. Lens.mapping Data._Time

-- | Average amount of time (in seconds) that players are currently waiting
-- for a match. If there is not enough recent data, this property may be
-- empty.
matchmakingTicket_estimatedWaitTime :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Natural)
matchmakingTicket_estimatedWaitTime = Lens.lens (\MatchmakingTicket' {estimatedWaitTime} -> estimatedWaitTime) (\s@MatchmakingTicket' {} a -> s {estimatedWaitTime = a} :: MatchmakingTicket)

-- | Connection information for a new game session. Once a match is made, the
-- FlexMatch engine creates a new game session for it. This information is
-- added to the matchmaking ticket, which you can be retrieve by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeMatchmaking.html DescribeMatchmaking>
-- .
matchmakingTicket_gameSessionConnectionInfo :: Lens.Lens' MatchmakingTicket (Prelude.Maybe GameSessionConnectionInfo)
matchmakingTicket_gameSessionConnectionInfo = Lens.lens (\MatchmakingTicket' {gameSessionConnectionInfo} -> gameSessionConnectionInfo) (\s@MatchmakingTicket' {} a -> s {gameSessionConnectionInfo = a} :: MatchmakingTicket)

-- | A set of @Player@ objects, each representing a player to find matches
-- for. Players are identified by a unique player ID and may include
-- latency data for use during matchmaking. If the ticket is in status
-- @COMPLETED@, the @Player@ objects include the team the players were
-- assigned to in the resulting match.
matchmakingTicket_players :: Lens.Lens' MatchmakingTicket (Prelude.Maybe [Player])
matchmakingTicket_players = Lens.lens (\MatchmakingTicket' {players} -> players) (\s@MatchmakingTicket' {} a -> s {players = a} :: MatchmakingTicket) Prelude.. Lens.mapping Lens.coerced

-- | Time stamp indicating when this matchmaking request was received. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
matchmakingTicket_startTime :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.UTCTime)
matchmakingTicket_startTime = Lens.lens (\MatchmakingTicket' {startTime} -> startTime) (\s@MatchmakingTicket' {} a -> s {startTime = a} :: MatchmakingTicket) Prelude.. Lens.mapping Data._Time

-- | Current status of the matchmaking request.
--
-- -   __QUEUED__ -- The matchmaking request has been received and is
--     currently waiting to be processed.
--
-- -   __SEARCHING__ -- The matchmaking request is currently being
--     processed.
--
-- -   __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players
--     must accept the match. This status is used only with requests that
--     use a matchmaking configuration with a player acceptance
--     requirement.
--
-- -   __PLACING__ -- The FlexMatch engine has matched players and is in
--     the process of placing a new game session for the match.
--
-- -   __COMPLETED__ -- Players have been matched and a game session is
--     ready to host the players. A ticket in this state contains the
--     necessary connection information for players.
--
-- -   __FAILED__ -- The matchmaking request was not completed.
--
-- -   __CANCELLED__ -- The matchmaking request was canceled. This may be
--     the result of a @StopMatchmaking@ operation or a proposed match that
--     one or more players failed to accept.
--
-- -   __TIMED_OUT__ -- The matchmaking request was not successful within
--     the duration specified in the matchmaking configuration.
--
-- Matchmaking requests that fail to successfully complete (statuses
-- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
-- new ticket IDs.
matchmakingTicket_status :: Lens.Lens' MatchmakingTicket (Prelude.Maybe MatchmakingConfigurationStatus)
matchmakingTicket_status = Lens.lens (\MatchmakingTicket' {status} -> status) (\s@MatchmakingTicket' {} a -> s {status = a} :: MatchmakingTicket)

-- | Additional information about the current status.
matchmakingTicket_statusMessage :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Text)
matchmakingTicket_statusMessage = Lens.lens (\MatchmakingTicket' {statusMessage} -> statusMessage) (\s@MatchmakingTicket' {} a -> s {statusMessage = a} :: MatchmakingTicket)

-- | Code to explain the current status. For example, a status reason may
-- indicate when a ticket has returned to @SEARCHING@ status after a
-- proposed match fails to receive player acceptances.
matchmakingTicket_statusReason :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Text)
matchmakingTicket_statusReason = Lens.lens (\MatchmakingTicket' {statusReason} -> statusReason) (\s@MatchmakingTicket' {} a -> s {statusReason = a} :: MatchmakingTicket)

-- | A unique identifier for a matchmaking ticket.
matchmakingTicket_ticketId :: Lens.Lens' MatchmakingTicket (Prelude.Maybe Prelude.Text)
matchmakingTicket_ticketId = Lens.lens (\MatchmakingTicket' {ticketId} -> ticketId) (\s@MatchmakingTicket' {} a -> s {ticketId = a} :: MatchmakingTicket)

instance Data.FromJSON MatchmakingTicket where
  parseJSON =
    Data.withObject
      "MatchmakingTicket"
      ( \x ->
          MatchmakingTicket'
            Prelude.<$> (x Data..:? "ConfigurationArn")
            Prelude.<*> (x Data..:? "ConfigurationName")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "EstimatedWaitTime")
            Prelude.<*> (x Data..:? "GameSessionConnectionInfo")
            Prelude.<*> (x Data..:? "Players" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..:? "TicketId")
      )

instance Prelude.Hashable MatchmakingTicket where
  hashWithSalt _salt MatchmakingTicket' {..} =
    _salt `Prelude.hashWithSalt` configurationArn
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` estimatedWaitTime
      `Prelude.hashWithSalt` gameSessionConnectionInfo
      `Prelude.hashWithSalt` players
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` ticketId

instance Prelude.NFData MatchmakingTicket where
  rnf MatchmakingTicket' {..} =
    Prelude.rnf configurationArn
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf estimatedWaitTime
      `Prelude.seq` Prelude.rnf gameSessionConnectionInfo
      `Prelude.seq` Prelude.rnf players
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf ticketId
