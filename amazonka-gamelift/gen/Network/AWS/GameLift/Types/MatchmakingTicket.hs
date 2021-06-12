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
-- Module      : Network.AWS.GameLift.Types.MatchmakingTicket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingTicket where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
import Network.AWS.GameLift.Types.Player
import qualified Network.AWS.Lens as Lens

-- | Ticket generated to track the progress of a matchmaking request. Each
-- ticket is uniquely identified by a ticket ID, supplied by the requester,
-- when creating a matchmaking request with StartMatchmaking. Tickets can
-- be retrieved by calling DescribeMatchmaking with the ticket ID.
--
-- /See:/ 'newMatchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { -- | Additional information about the current status.
    statusMessage :: Core.Maybe Core.Text,
    -- | Current status of the matchmaking request.
    --
    -- -   __QUEUED__ -- The matchmaking request has been received and is
    --     currently waiting to be processed.
    --
    -- -   __SEARCHING__ -- The matchmaking request is currently being
    --     processed.
    --
    -- -   __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players
    --     must accept the match (see AcceptMatch). This status is used only
    --     with requests that use a matchmaking configuration with a player
    --     acceptance requirement.
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
    --     the result of a call to StopMatchmaking or a proposed match that one
    --     or more players failed to accept.
    --
    -- -   __TIMED_OUT__ -- The matchmaking request was not successful within
    --     the duration specified in the matchmaking configuration.
    --
    -- Matchmaking requests that fail to successfully complete (statuses
    -- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
    -- new ticket IDs.
    status :: Core.Maybe MatchmakingConfigurationStatus,
    -- | Average amount of time (in seconds) that players are currently waiting
    -- for a match. If there is not enough recent data, this property may be
    -- empty.
    estimatedWaitTime :: Core.Maybe Core.Natural,
    -- | A unique identifier for a matchmaking ticket.
    ticketId :: Core.Maybe Core.Text,
    -- | A set of @Player@ objects, each representing a player to find matches
    -- for. Players are identified by a unique player ID and may include
    -- latency data for use during matchmaking. If the ticket is in status
    -- @COMPLETED@, the @Player@ objects include the team the players were
    -- assigned to in the resulting match.
    players :: Core.Maybe [Player],
    -- | Time stamp indicating when this matchmaking request was received. Format
    -- is a number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    startTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift matchmaking configuration resource that is
    -- used with this ticket.
    configurationArn :: Core.Maybe Core.Text,
    -- | Time stamp indicating when this matchmaking request stopped being
    -- processed due to success, failure, or cancellation. Format is a number
    -- expressed in Unix time as milliseconds (for example \"1469498468.057\").
    endTime :: Core.Maybe Core.POSIX,
    -- | Name of the MatchmakingConfiguration that is used with this ticket.
    -- Matchmaking configurations determine how players are grouped into a
    -- match and how a new game session is created for the match.
    configurationName :: Core.Maybe Core.Text,
    -- | Identifier and connection information of the game session created for
    -- the match. This information is added to the ticket only after the
    -- matchmaking request has been successfully completed. This parameter is
    -- not set when FlexMatch is being used without GameLift hosting.
    gameSessionConnectionInfo :: Core.Maybe GameSessionConnectionInfo,
    -- | Code to explain the current status. For example, a status reason may
    -- indicate when a ticket has returned to @SEARCHING@ status after a
    -- proposed match fails to receive player acceptances.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MatchmakingTicket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'matchmakingTicket_statusMessage' - Additional information about the current status.
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
--     must accept the match (see AcceptMatch). This status is used only
--     with requests that use a matchmaking configuration with a player
--     acceptance requirement.
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
--     the result of a call to StopMatchmaking or a proposed match that one
--     or more players failed to accept.
--
-- -   __TIMED_OUT__ -- The matchmaking request was not successful within
--     the duration specified in the matchmaking configuration.
--
-- Matchmaking requests that fail to successfully complete (statuses
-- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
-- new ticket IDs.
--
-- 'estimatedWaitTime', 'matchmakingTicket_estimatedWaitTime' - Average amount of time (in seconds) that players are currently waiting
-- for a match. If there is not enough recent data, this property may be
-- empty.
--
-- 'ticketId', 'matchmakingTicket_ticketId' - A unique identifier for a matchmaking ticket.
--
-- 'players', 'matchmakingTicket_players' - A set of @Player@ objects, each representing a player to find matches
-- for. Players are identified by a unique player ID and may include
-- latency data for use during matchmaking. If the ticket is in status
-- @COMPLETED@, the @Player@ objects include the team the players were
-- assigned to in the resulting match.
--
-- 'startTime', 'matchmakingTicket_startTime' - Time stamp indicating when this matchmaking request was received. Format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'configurationArn', 'matchmakingTicket_configurationArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift matchmaking configuration resource that is
-- used with this ticket.
--
-- 'endTime', 'matchmakingTicket_endTime' - Time stamp indicating when this matchmaking request stopped being
-- processed due to success, failure, or cancellation. Format is a number
-- expressed in Unix time as milliseconds (for example \"1469498468.057\").
--
-- 'configurationName', 'matchmakingTicket_configurationName' - Name of the MatchmakingConfiguration that is used with this ticket.
-- Matchmaking configurations determine how players are grouped into a
-- match and how a new game session is created for the match.
--
-- 'gameSessionConnectionInfo', 'matchmakingTicket_gameSessionConnectionInfo' - Identifier and connection information of the game session created for
-- the match. This information is added to the ticket only after the
-- matchmaking request has been successfully completed. This parameter is
-- not set when FlexMatch is being used without GameLift hosting.
--
-- 'statusReason', 'matchmakingTicket_statusReason' - Code to explain the current status. For example, a status reason may
-- indicate when a ticket has returned to @SEARCHING@ status after a
-- proposed match fails to receive player acceptances.
newMatchmakingTicket ::
  MatchmakingTicket
newMatchmakingTicket =
  MatchmakingTicket'
    { statusMessage = Core.Nothing,
      status = Core.Nothing,
      estimatedWaitTime = Core.Nothing,
      ticketId = Core.Nothing,
      players = Core.Nothing,
      startTime = Core.Nothing,
      configurationArn = Core.Nothing,
      endTime = Core.Nothing,
      configurationName = Core.Nothing,
      gameSessionConnectionInfo = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | Additional information about the current status.
matchmakingTicket_statusMessage :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Text)
matchmakingTicket_statusMessage = Lens.lens (\MatchmakingTicket' {statusMessage} -> statusMessage) (\s@MatchmakingTicket' {} a -> s {statusMessage = a} :: MatchmakingTicket)

-- | Current status of the matchmaking request.
--
-- -   __QUEUED__ -- The matchmaking request has been received and is
--     currently waiting to be processed.
--
-- -   __SEARCHING__ -- The matchmaking request is currently being
--     processed.
--
-- -   __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players
--     must accept the match (see AcceptMatch). This status is used only
--     with requests that use a matchmaking configuration with a player
--     acceptance requirement.
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
--     the result of a call to StopMatchmaking or a proposed match that one
--     or more players failed to accept.
--
-- -   __TIMED_OUT__ -- The matchmaking request was not successful within
--     the duration specified in the matchmaking configuration.
--
-- Matchmaking requests that fail to successfully complete (statuses
-- FAILED, CANCELLED, TIMED_OUT) can be resubmitted as new requests with
-- new ticket IDs.
matchmakingTicket_status :: Lens.Lens' MatchmakingTicket (Core.Maybe MatchmakingConfigurationStatus)
matchmakingTicket_status = Lens.lens (\MatchmakingTicket' {status} -> status) (\s@MatchmakingTicket' {} a -> s {status = a} :: MatchmakingTicket)

-- | Average amount of time (in seconds) that players are currently waiting
-- for a match. If there is not enough recent data, this property may be
-- empty.
matchmakingTicket_estimatedWaitTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Natural)
matchmakingTicket_estimatedWaitTime = Lens.lens (\MatchmakingTicket' {estimatedWaitTime} -> estimatedWaitTime) (\s@MatchmakingTicket' {} a -> s {estimatedWaitTime = a} :: MatchmakingTicket)

-- | A unique identifier for a matchmaking ticket.
matchmakingTicket_ticketId :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Text)
matchmakingTicket_ticketId = Lens.lens (\MatchmakingTicket' {ticketId} -> ticketId) (\s@MatchmakingTicket' {} a -> s {ticketId = a} :: MatchmakingTicket)

-- | A set of @Player@ objects, each representing a player to find matches
-- for. Players are identified by a unique player ID and may include
-- latency data for use during matchmaking. If the ticket is in status
-- @COMPLETED@, the @Player@ objects include the team the players were
-- assigned to in the resulting match.
matchmakingTicket_players :: Lens.Lens' MatchmakingTicket (Core.Maybe [Player])
matchmakingTicket_players = Lens.lens (\MatchmakingTicket' {players} -> players) (\s@MatchmakingTicket' {} a -> s {players = a} :: MatchmakingTicket) Core.. Lens.mapping Lens._Coerce

-- | Time stamp indicating when this matchmaking request was received. Format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
matchmakingTicket_startTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.UTCTime)
matchmakingTicket_startTime = Lens.lens (\MatchmakingTicket' {startTime} -> startTime) (\s@MatchmakingTicket' {} a -> s {startTime = a} :: MatchmakingTicket) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift matchmaking configuration resource that is
-- used with this ticket.
matchmakingTicket_configurationArn :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Text)
matchmakingTicket_configurationArn = Lens.lens (\MatchmakingTicket' {configurationArn} -> configurationArn) (\s@MatchmakingTicket' {} a -> s {configurationArn = a} :: MatchmakingTicket)

-- | Time stamp indicating when this matchmaking request stopped being
-- processed due to success, failure, or cancellation. Format is a number
-- expressed in Unix time as milliseconds (for example \"1469498468.057\").
matchmakingTicket_endTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.UTCTime)
matchmakingTicket_endTime = Lens.lens (\MatchmakingTicket' {endTime} -> endTime) (\s@MatchmakingTicket' {} a -> s {endTime = a} :: MatchmakingTicket) Core.. Lens.mapping Core._Time

-- | Name of the MatchmakingConfiguration that is used with this ticket.
-- Matchmaking configurations determine how players are grouped into a
-- match and how a new game session is created for the match.
matchmakingTicket_configurationName :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Text)
matchmakingTicket_configurationName = Lens.lens (\MatchmakingTicket' {configurationName} -> configurationName) (\s@MatchmakingTicket' {} a -> s {configurationName = a} :: MatchmakingTicket)

-- | Identifier and connection information of the game session created for
-- the match. This information is added to the ticket only after the
-- matchmaking request has been successfully completed. This parameter is
-- not set when FlexMatch is being used without GameLift hosting.
matchmakingTicket_gameSessionConnectionInfo :: Lens.Lens' MatchmakingTicket (Core.Maybe GameSessionConnectionInfo)
matchmakingTicket_gameSessionConnectionInfo = Lens.lens (\MatchmakingTicket' {gameSessionConnectionInfo} -> gameSessionConnectionInfo) (\s@MatchmakingTicket' {} a -> s {gameSessionConnectionInfo = a} :: MatchmakingTicket)

-- | Code to explain the current status. For example, a status reason may
-- indicate when a ticket has returned to @SEARCHING@ status after a
-- proposed match fails to receive player acceptances.
matchmakingTicket_statusReason :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Text)
matchmakingTicket_statusReason = Lens.lens (\MatchmakingTicket' {statusReason} -> statusReason) (\s@MatchmakingTicket' {} a -> s {statusReason = a} :: MatchmakingTicket)

instance Core.FromJSON MatchmakingTicket where
  parseJSON =
    Core.withObject
      "MatchmakingTicket"
      ( \x ->
          MatchmakingTicket'
            Core.<$> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "EstimatedWaitTime")
            Core.<*> (x Core..:? "TicketId")
            Core.<*> (x Core..:? "Players" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "ConfigurationArn")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "ConfigurationName")
            Core.<*> (x Core..:? "GameSessionConnectionInfo")
            Core.<*> (x Core..:? "StatusReason")
      )

instance Core.Hashable MatchmakingTicket

instance Core.NFData MatchmakingTicket
