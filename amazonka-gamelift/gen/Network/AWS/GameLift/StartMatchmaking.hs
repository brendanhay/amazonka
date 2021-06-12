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
-- Module      : Network.AWS.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on
-- custom matchmaking rules. If you\'re also using GameLift hosting, a new
-- game session is started for the matched players. Each matchmaking
-- request identifies one or more players to find a match for, and
-- specifies the type of match to build, including the team configuration
-- and the rules for an acceptable match. When a matchmaking request
-- identifies a group of players who want to play together, FlexMatch finds
-- additional players to fill the match. Match type, rules, and other
-- features are defined in a @MatchmakingConfiguration@.
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking
-- configuration, and include the players to be matched. For each player,
-- you must also include the player attribute values that are required by
-- the matchmaking configuration (in the rule set). If successful, a
-- matchmaking ticket is returned with status set to @QUEUED@.
--
-- Track the status of the ticket to respond as needed. If you\'re also
-- using GameLift hosting, a successfully completed ticket contains game
-- session connection information. Ticket status updates are tracked using
-- event notification through Amazon Simple Notification Service (SNS),
-- which is defined in the matchmaking configuration.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-tasks.html FlexMatch Integration Roadmap>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch Works>
--
-- __Related operations__
--
-- -   StartMatchmaking
--
-- -   DescribeMatchmaking
--
-- -   StopMatchmaking
--
-- -   AcceptMatch
--
-- -   StartMatchBackfill
module Network.AWS.GameLift.StartMatchmaking
  ( -- * Creating a Request
    StartMatchmaking (..),
    newStartMatchmaking,

    -- * Request Lenses
    startMatchmaking_ticketId,
    startMatchmaking_configurationName,
    startMatchmaking_players,

    -- * Destructuring the Response
    StartMatchmakingResponse (..),
    newStartMatchmakingResponse,

    -- * Response Lenses
    startMatchmakingResponse_matchmakingTicket,
    startMatchmakingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStartMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { -- | A unique identifier for a matchmaking ticket. If no ticket ID is
    -- specified here, Amazon GameLift will generate one in the form of a UUID.
    -- Use this identifier to track the matchmaking ticket status and retrieve
    -- match results.
    ticketId :: Core.Maybe Core.Text,
    -- | Name of the matchmaking configuration to use for this request.
    -- Matchmaking configurations must exist in the same Region as this
    -- request. You can use either the configuration name or ARN value.
    configurationName :: Core.Text,
    -- | Information on each player to be matched. This information must include
    -- a player ID, and may contain player attributes and latency data to be
    -- used in the matchmaking process. After a successful match, @Player@
    -- objects contain the name of the team the player is assigned to.
    players :: [Player]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMatchmaking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ticketId', 'startMatchmaking_ticketId' - A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the matchmaking ticket status and retrieve
-- match results.
--
-- 'configurationName', 'startMatchmaking_configurationName' - Name of the matchmaking configuration to use for this request.
-- Matchmaking configurations must exist in the same Region as this
-- request. You can use either the configuration name or ARN value.
--
-- 'players', 'startMatchmaking_players' - Information on each player to be matched. This information must include
-- a player ID, and may contain player attributes and latency data to be
-- used in the matchmaking process. After a successful match, @Player@
-- objects contain the name of the team the player is assigned to.
newStartMatchmaking ::
  -- | 'configurationName'
  Core.Text ->
  StartMatchmaking
newStartMatchmaking pConfigurationName_ =
  StartMatchmaking'
    { ticketId = Core.Nothing,
      configurationName = pConfigurationName_,
      players = Core.mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the matchmaking ticket status and retrieve
-- match results.
startMatchmaking_ticketId :: Lens.Lens' StartMatchmaking (Core.Maybe Core.Text)
startMatchmaking_ticketId = Lens.lens (\StartMatchmaking' {ticketId} -> ticketId) (\s@StartMatchmaking' {} a -> s {ticketId = a} :: StartMatchmaking)

-- | Name of the matchmaking configuration to use for this request.
-- Matchmaking configurations must exist in the same Region as this
-- request. You can use either the configuration name or ARN value.
startMatchmaking_configurationName :: Lens.Lens' StartMatchmaking Core.Text
startMatchmaking_configurationName = Lens.lens (\StartMatchmaking' {configurationName} -> configurationName) (\s@StartMatchmaking' {} a -> s {configurationName = a} :: StartMatchmaking)

-- | Information on each player to be matched. This information must include
-- a player ID, and may contain player attributes and latency data to be
-- used in the matchmaking process. After a successful match, @Player@
-- objects contain the name of the team the player is assigned to.
startMatchmaking_players :: Lens.Lens' StartMatchmaking [Player]
startMatchmaking_players = Lens.lens (\StartMatchmaking' {players} -> players) (\s@StartMatchmaking' {} a -> s {players = a} :: StartMatchmaking) Core.. Lens._Coerce

instance Core.AWSRequest StartMatchmaking where
  type
    AWSResponse StartMatchmaking =
      StartMatchmakingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMatchmakingResponse'
            Core.<$> (x Core..?> "MatchmakingTicket")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartMatchmaking

instance Core.NFData StartMatchmaking

instance Core.ToHeaders StartMatchmaking where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.StartMatchmaking" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartMatchmaking where
  toJSON StartMatchmaking' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TicketId" Core..=) Core.<$> ticketId,
            Core.Just
              ("ConfigurationName" Core..= configurationName),
            Core.Just ("Players" Core..= players)
          ]
      )

instance Core.ToPath StartMatchmaking where
  toPath = Core.const "/"

instance Core.ToQuery StartMatchmaking where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newStartMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { -- | Ticket representing the matchmaking request. This object include the
    -- information included in the request, ticket status, and match results as
    -- generated during the matchmaking process.
    matchmakingTicket :: Core.Maybe MatchmakingTicket,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMatchmakingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchmakingTicket', 'startMatchmakingResponse_matchmakingTicket' - Ticket representing the matchmaking request. This object include the
-- information included in the request, ticket status, and match results as
-- generated during the matchmaking process.
--
-- 'httpStatus', 'startMatchmakingResponse_httpStatus' - The response's http status code.
newStartMatchmakingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartMatchmakingResponse
newStartMatchmakingResponse pHttpStatus_ =
  StartMatchmakingResponse'
    { matchmakingTicket =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Ticket representing the matchmaking request. This object include the
-- information included in the request, ticket status, and match results as
-- generated during the matchmaking process.
startMatchmakingResponse_matchmakingTicket :: Lens.Lens' StartMatchmakingResponse (Core.Maybe MatchmakingTicket)
startMatchmakingResponse_matchmakingTicket = Lens.lens (\StartMatchmakingResponse' {matchmakingTicket} -> matchmakingTicket) (\s@StartMatchmakingResponse' {} a -> s {matchmakingTicket = a} :: StartMatchmakingResponse)

-- | The response's http status code.
startMatchmakingResponse_httpStatus :: Lens.Lens' StartMatchmakingResponse Core.Int
startMatchmakingResponse_httpStatus = Lens.lens (\StartMatchmakingResponse' {httpStatus} -> httpStatus) (\s@StartMatchmakingResponse' {} a -> s {httpStatus = a} :: StartMatchmakingResponse)

instance Core.NFData StartMatchmakingResponse
