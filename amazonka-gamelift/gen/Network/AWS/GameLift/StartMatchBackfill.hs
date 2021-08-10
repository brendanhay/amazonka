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
-- Module      : Network.AWS.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in an existing game session. This
-- operation can be used to add players to matched games that start with
-- fewer than the maximum number of players or to replace players when they
-- drop out. By backfilling with the same matchmaker used to create the
-- original match, you ensure that new players meet the match criteria and
-- maintain a consistent experience throughout the game session. You can
-- backfill a match anytime after a game session has been created.
--
-- To request a match backfill, specify a unique ticket ID, the existing
-- game session\'s ARN, a matchmaking configuration, and a set of data that
-- describes all current players in the game session. If successful, a
-- match backfill ticket is created and returned with status set to QUEUED.
-- The ticket is placed in the matchmaker\'s ticket pool and processed.
-- Track the status of the ticket to respond as needed.
--
-- The process of finding backfill matches is essentially identical to the
-- initial matchmaking process. The matchmaker searches the pool and groups
-- tickets together to form potential matches, allowing only one backfill
-- ticket per potential match. Once the a match is formed, the matchmaker
-- creates player sessions for the new players. All tickets in the match
-- are updated with the game session\'s connection information, and the
-- GameSession object is updated to include matchmaker data on the new
-- players. For more detail on how match backfill requests are processed,
-- see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How Amazon GameLift FlexMatch Works>.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>
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
module Network.AWS.GameLift.StartMatchBackfill
  ( -- * Creating a Request
    StartMatchBackfill (..),
    newStartMatchBackfill,

    -- * Request Lenses
    startMatchBackfill_ticketId,
    startMatchBackfill_gameSessionArn,
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

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStartMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { -- | A unique identifier for a matchmaking ticket. If no ticket ID is
    -- specified here, Amazon GameLift will generate one in the form of a UUID.
    -- Use this identifier to track the match backfill ticket status and
    -- retrieve match results.
    ticketId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a game session and uniquely identifies it. This is
    -- the same as the game session ID.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | Name of the matchmaker to use for this request. You can use either the
    -- configuration name or ARN value. The ARN of the matchmaker that was used
    -- with the original game session is listed in the GameSession object,
    -- @MatchmakerData@ property.
    configurationName :: Prelude.Text,
    -- | Match information on all players that are currently assigned to the game
    -- session. This information is used by the matchmaker to find new players
    -- and add them to the existing game.
    --
    -- -   PlayerID, PlayerAttributes, Team -\\\\- This information is
    --     maintained in the GameSession object, @MatchmakerData@ property, for
    --     all players who are currently assigned to the game session. The
    --     matchmaker data is in JSON syntax, formatted as a string. For more
    --     details, see
    --     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    --
    -- -   LatencyInMs -\\\\- If the matchmaker uses player latency, include a
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
-- 'ticketId', 'startMatchBackfill_ticketId' - A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the match backfill ticket status and
-- retrieve match results.
--
-- 'gameSessionArn', 'startMatchBackfill_gameSessionArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a game session and uniquely identifies it. This is
-- the same as the game session ID.
--
-- 'configurationName', 'startMatchBackfill_configurationName' - Name of the matchmaker to use for this request. You can use either the
-- configuration name or ARN value. The ARN of the matchmaker that was used
-- with the original game session is listed in the GameSession object,
-- @MatchmakerData@ property.
--
-- 'players', 'startMatchBackfill_players' - Match information on all players that are currently assigned to the game
-- session. This information is used by the matchmaker to find new players
-- and add them to the existing game.
--
-- -   PlayerID, PlayerAttributes, Team -\\\\- This information is
--     maintained in the GameSession object, @MatchmakerData@ property, for
--     all players who are currently assigned to the game session. The
--     matchmaker data is in JSON syntax, formatted as a string. For more
--     details, see
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
-- -   LatencyInMs -\\\\- If the matchmaker uses player latency, include a
--     latency value, in milliseconds, for the Region that the game session
--     is currently in. Do not include latency values for any other Region.
newStartMatchBackfill ::
  -- | 'configurationName'
  Prelude.Text ->
  StartMatchBackfill
newStartMatchBackfill pConfigurationName_ =
  StartMatchBackfill'
    { ticketId = Prelude.Nothing,
      gameSessionArn = Prelude.Nothing,
      configurationName = pConfigurationName_,
      players = Prelude.mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the match backfill ticket status and
-- retrieve match results.
startMatchBackfill_ticketId :: Lens.Lens' StartMatchBackfill (Prelude.Maybe Prelude.Text)
startMatchBackfill_ticketId = Lens.lens (\StartMatchBackfill' {ticketId} -> ticketId) (\s@StartMatchBackfill' {} a -> s {ticketId = a} :: StartMatchBackfill)

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a game session and uniquely identifies it. This is
-- the same as the game session ID.
startMatchBackfill_gameSessionArn :: Lens.Lens' StartMatchBackfill (Prelude.Maybe Prelude.Text)
startMatchBackfill_gameSessionArn = Lens.lens (\StartMatchBackfill' {gameSessionArn} -> gameSessionArn) (\s@StartMatchBackfill' {} a -> s {gameSessionArn = a} :: StartMatchBackfill)

-- | Name of the matchmaker to use for this request. You can use either the
-- configuration name or ARN value. The ARN of the matchmaker that was used
-- with the original game session is listed in the GameSession object,
-- @MatchmakerData@ property.
startMatchBackfill_configurationName :: Lens.Lens' StartMatchBackfill Prelude.Text
startMatchBackfill_configurationName = Lens.lens (\StartMatchBackfill' {configurationName} -> configurationName) (\s@StartMatchBackfill' {} a -> s {configurationName = a} :: StartMatchBackfill)

-- | Match information on all players that are currently assigned to the game
-- session. This information is used by the matchmaker to find new players
-- and add them to the existing game.
--
-- -   PlayerID, PlayerAttributes, Team -\\\\- This information is
--     maintained in the GameSession object, @MatchmakerData@ property, for
--     all players who are currently assigned to the game session. The
--     matchmaker data is in JSON syntax, formatted as a string. For more
--     details, see
--     <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
-- -   LatencyInMs -\\\\- If the matchmaker uses player latency, include a
--     latency value, in milliseconds, for the Region that the game session
--     is currently in. Do not include latency values for any other Region.
startMatchBackfill_players :: Lens.Lens' StartMatchBackfill [Player]
startMatchBackfill_players = Lens.lens (\StartMatchBackfill' {players} -> players) (\s@StartMatchBackfill' {} a -> s {players = a} :: StartMatchBackfill) Prelude.. Lens._Coerce

instance Core.AWSRequest StartMatchBackfill where
  type
    AWSResponse StartMatchBackfill =
      StartMatchBackfillResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMatchBackfillResponse'
            Prelude.<$> (x Core..?> "MatchmakingTicket")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMatchBackfill

instance Prelude.NFData StartMatchBackfill

instance Core.ToHeaders StartMatchBackfill where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.StartMatchBackfill" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartMatchBackfill where
  toJSON StartMatchBackfill' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TicketId" Core..=) Prelude.<$> ticketId,
            ("GameSessionArn" Core..=)
              Prelude.<$> gameSessionArn,
            Prelude.Just
              ("ConfigurationName" Core..= configurationName),
            Prelude.Just ("Players" Core..= players)
          ]
      )

instance Core.ToPath StartMatchBackfill where
  toPath = Prelude.const "/"

instance Core.ToQuery StartMatchBackfill where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newStartMatchBackfillResponse' smart constructor.
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

instance Prelude.NFData StartMatchBackfillResponse
