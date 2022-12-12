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
-- Module      : Amazonka.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in currently running game sessions.
-- The backfill match process is essentially identical to the process of
-- forming new matches. Backfill requests use the same matchmaker that was
-- used to make the original match, and they provide matchmaking data for
-- all players currently in the game session. FlexMatch uses this
-- information to select new players so that backfilled match continues to
-- meet the original match requirements.
--
-- When using FlexMatch with GameLift managed hosting, you can request a
-- backfill match from a client service by calling this operation with a
-- @GameSessions@ ID. You also have the option of making backfill requests
-- directly from your game server. In response to a request, FlexMatch
-- creates player sessions for the new players, updates the @GameSession@
-- resource, and sends updated matchmaking data to the game server. You can
-- request a backfill match at any point after a game session is started.
-- Each game session can have only one active backfill request at a time; a
-- subsequent request automatically replaces the earlier request.
--
-- When using FlexMatch as a standalone component, request a backfill match
-- by calling this operation without a game session identifier. As with
-- newly formed matches, matchmaking results are returned in a matchmaking
-- event so that your game can update the game session that is being
-- backfilled.
--
-- To request a backfill match, specify a unique ticket ID, the original
-- matchmaking configuration, and matchmaking data for all current players
-- in the game session being backfilled. Optionally, specify the
-- @GameSession@ ARN. If successful, a match backfill ticket is created and
-- returned with status set to QUEUED. Track the status of backfill tickets
-- using the same method for tracking tickets for new matches.
--
-- Only game sessions created by FlexMatch are supported for match
-- backfill.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill existing games with FlexMatch>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html Matchmaking events>
-- (reference)
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch works>
module Amazonka.GameLift.StartMatchBackfill
  ( -- * Creating a Request
    StartMatchBackfill (..),
    newStartMatchBackfill,

    -- * Request Lenses
    startMatchBackfill_gameSessionArn,
    startMatchBackfill_ticketId,
    startMatchBackfill_configurationName,
    startMatchBackfill_players,

    -- * Destructuring the Response
    StartMatchBackfillResponse (..),
    newStartMatchBackfillResponse,

    -- * Response Lenses
    startMatchBackfillResponse_matchmakingTicket,
    startMatchBackfillResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { -- | A unique identifier for the game session. Use the game session ID. When
    -- using FlexMatch as a standalone matchmaking solution, this parameter is
    -- not needed.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a matchmaking ticket. If no ticket ID is
    -- specified here, Amazon GameLift will generate one in the form of a UUID.
    -- Use this identifier to track the match backfill ticket status and
    -- retrieve match results.
    ticketId :: Prelude.Maybe Prelude.Text,
    -- | Name of the matchmaker to use for this request. You can use either the
    -- configuration name or ARN value. The ARN of the matchmaker that was used
    -- with the original game session is listed in the @GameSession@ object,
    -- @MatchmakerData@ property.
    configurationName :: Prelude.Text,
    -- | Match information on all players that are currently assigned to the game
    -- session. This information is used by the matchmaker to find new players
    -- and add them to the existing game.
    --
    -- You can include up to 199 @Players@ in a @StartMatchBackfill@ request.
    --
    -- -   PlayerID, PlayerAttributes, Team -- This information is maintained
    --     in the @GameSession@ object, @MatchmakerData@ property, for all
    --     players who are currently assigned to the game session. The
    --     matchmaker data is in JSON syntax, formatted as a string. For more
    --     details, see
    --     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    --
    --     The backfill request must specify the team membership for every
    --     player. Do not specify team if you are not using backfill.
    --
    -- -   LatencyInMs -- If the matchmaker uses player latency, include a
    --     latency value, in milliseconds, for the Region that the game session
    --     is currently in. Do not include latency values for any other Region.
    players :: [Player]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMatchBackfill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionArn', 'startMatchBackfill_gameSessionArn' - A unique identifier for the game session. Use the game session ID. When
-- using FlexMatch as a standalone matchmaking solution, this parameter is
-- not needed.
--
-- 'ticketId', 'startMatchBackfill_ticketId' - A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the match backfill ticket status and
-- retrieve match results.
--
-- 'configurationName', 'startMatchBackfill_configurationName' - Name of the matchmaker to use for this request. You can use either the
-- configuration name or ARN value. The ARN of the matchmaker that was used
-- with the original game session is listed in the @GameSession@ object,
-- @MatchmakerData@ property.
--
-- 'players', 'startMatchBackfill_players' - Match information on all players that are currently assigned to the game
-- session. This information is used by the matchmaker to find new players
-- and add them to the existing game.
--
-- You can include up to 199 @Players@ in a @StartMatchBackfill@ request.
--
-- -   PlayerID, PlayerAttributes, Team -- This information is maintained
--     in the @GameSession@ object, @MatchmakerData@ property, for all
--     players who are currently assigned to the game session. The
--     matchmaker data is in JSON syntax, formatted as a string. For more
--     details, see
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
--     The backfill request must specify the team membership for every
--     player. Do not specify team if you are not using backfill.
--
-- -   LatencyInMs -- If the matchmaker uses player latency, include a
--     latency value, in milliseconds, for the Region that the game session
--     is currently in. Do not include latency values for any other Region.
newStartMatchBackfill ::
  -- | 'configurationName'
  Prelude.Text ->
  StartMatchBackfill
newStartMatchBackfill pConfigurationName_ =
  StartMatchBackfill'
    { gameSessionArn =
        Prelude.Nothing,
      ticketId = Prelude.Nothing,
      configurationName = pConfigurationName_,
      players = Prelude.mempty
    }

-- | A unique identifier for the game session. Use the game session ID. When
-- using FlexMatch as a standalone matchmaking solution, this parameter is
-- not needed.
startMatchBackfill_gameSessionArn :: Lens.Lens' StartMatchBackfill (Prelude.Maybe Prelude.Text)
startMatchBackfill_gameSessionArn = Lens.lens (\StartMatchBackfill' {gameSessionArn} -> gameSessionArn) (\s@StartMatchBackfill' {} a -> s {gameSessionArn = a} :: StartMatchBackfill)

-- | A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the match backfill ticket status and
-- retrieve match results.
startMatchBackfill_ticketId :: Lens.Lens' StartMatchBackfill (Prelude.Maybe Prelude.Text)
startMatchBackfill_ticketId = Lens.lens (\StartMatchBackfill' {ticketId} -> ticketId) (\s@StartMatchBackfill' {} a -> s {ticketId = a} :: StartMatchBackfill)

-- | Name of the matchmaker to use for this request. You can use either the
-- configuration name or ARN value. The ARN of the matchmaker that was used
-- with the original game session is listed in the @GameSession@ object,
-- @MatchmakerData@ property.
startMatchBackfill_configurationName :: Lens.Lens' StartMatchBackfill Prelude.Text
startMatchBackfill_configurationName = Lens.lens (\StartMatchBackfill' {configurationName} -> configurationName) (\s@StartMatchBackfill' {} a -> s {configurationName = a} :: StartMatchBackfill)

-- | Match information on all players that are currently assigned to the game
-- session. This information is used by the matchmaker to find new players
-- and add them to the existing game.
--
-- You can include up to 199 @Players@ in a @StartMatchBackfill@ request.
--
-- -   PlayerID, PlayerAttributes, Team -- This information is maintained
--     in the @GameSession@ object, @MatchmakerData@ property, for all
--     players who are currently assigned to the game session. The
--     matchmaker data is in JSON syntax, formatted as a string. For more
--     details, see
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
--     The backfill request must specify the team membership for every
--     player. Do not specify team if you are not using backfill.
--
-- -   LatencyInMs -- If the matchmaker uses player latency, include a
--     latency value, in milliseconds, for the Region that the game session
--     is currently in. Do not include latency values for any other Region.
startMatchBackfill_players :: Lens.Lens' StartMatchBackfill [Player]
startMatchBackfill_players = Lens.lens (\StartMatchBackfill' {players} -> players) (\s@StartMatchBackfill' {} a -> s {players = a} :: StartMatchBackfill) Prelude.. Lens.coerced

instance Core.AWSRequest StartMatchBackfill where
  type
    AWSResponse StartMatchBackfill =
      StartMatchBackfillResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMatchBackfillResponse'
            Prelude.<$> (x Data..?> "MatchmakingTicket")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMatchBackfill where
  hashWithSalt _salt StartMatchBackfill' {..} =
    _salt `Prelude.hashWithSalt` gameSessionArn
      `Prelude.hashWithSalt` ticketId
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` players

instance Prelude.NFData StartMatchBackfill where
  rnf StartMatchBackfill' {..} =
    Prelude.rnf gameSessionArn
      `Prelude.seq` Prelude.rnf ticketId
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf players

instance Data.ToHeaders StartMatchBackfill where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.StartMatchBackfill" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMatchBackfill where
  toJSON StartMatchBackfill' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GameSessionArn" Data..=)
              Prelude.<$> gameSessionArn,
            ("TicketId" Data..=) Prelude.<$> ticketId,
            Prelude.Just
              ("ConfigurationName" Data..= configurationName),
            Prelude.Just ("Players" Data..= players)
          ]
      )

instance Data.ToPath StartMatchBackfill where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMatchBackfill where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMatchBackfillResponse' smart constructor.
data StartMatchBackfillResponse = StartMatchBackfillResponse'
  { -- | Ticket representing the backfill matchmaking request. This object
    -- includes the information in the request, ticket status, and match
    -- results as generated during the matchmaking process.
    matchmakingTicket :: Prelude.Maybe MatchmakingTicket,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMatchBackfillResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchmakingTicket', 'startMatchBackfillResponse_matchmakingTicket' - Ticket representing the backfill matchmaking request. This object
-- includes the information in the request, ticket status, and match
-- results as generated during the matchmaking process.
--
-- 'httpStatus', 'startMatchBackfillResponse_httpStatus' - The response's http status code.
newStartMatchBackfillResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMatchBackfillResponse
newStartMatchBackfillResponse pHttpStatus_ =
  StartMatchBackfillResponse'
    { matchmakingTicket =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Ticket representing the backfill matchmaking request. This object
-- includes the information in the request, ticket status, and match
-- results as generated during the matchmaking process.
startMatchBackfillResponse_matchmakingTicket :: Lens.Lens' StartMatchBackfillResponse (Prelude.Maybe MatchmakingTicket)
startMatchBackfillResponse_matchmakingTicket = Lens.lens (\StartMatchBackfillResponse' {matchmakingTicket} -> matchmakingTicket) (\s@StartMatchBackfillResponse' {} a -> s {matchmakingTicket = a} :: StartMatchBackfillResponse)

-- | The response's http status code.
startMatchBackfillResponse_httpStatus :: Lens.Lens' StartMatchBackfillResponse Prelude.Int
startMatchBackfillResponse_httpStatus = Lens.lens (\StartMatchBackfillResponse' {httpStatus} -> httpStatus) (\s@StartMatchBackfillResponse' {} a -> s {httpStatus = a} :: StartMatchBackfillResponse)

instance Prelude.NFData StartMatchBackfillResponse where
  rnf StartMatchBackfillResponse' {..} =
    Prelude.rnf matchmakingTicket
      `Prelude.seq` Prelude.rnf httpStatus
