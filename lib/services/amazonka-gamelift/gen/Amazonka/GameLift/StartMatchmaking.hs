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
-- Module      : Amazonka.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on
-- custom matchmaking rules. With games that use GameLift managed hosting,
-- this operation also triggers GameLift to find hosting resources and
-- start a new game session for the new match. Each matchmaking request
-- includes information on one or more players and specifies the FlexMatch
-- matchmaker to use. When a request is for multiple players, FlexMatch
-- attempts to build a match that includes all players in the request,
-- placing them in the same team and finding additional players as needed
-- to fill the match.
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking
-- configuration, and include the players to be matched. You must also
-- include any player attributes that are required by the matchmaking
-- configuration\'s rule set. If successful, a matchmaking ticket is
-- returned with status set to @QUEUED@.
--
-- Track matchmaking events to respond as needed and acquire game session
-- connection information for successfully completed matches. Ticket status
-- updates are tracked using event notification through Amazon Simple
-- Notification Service, which is defined in the matchmaking configuration.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a game client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch event notification>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch works>
module Amazonka.GameLift.StartMatchmaking
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { -- | A unique identifier for a matchmaking ticket. If no ticket ID is
    -- specified here, Amazon GameLift will generate one in the form of a UUID.
    -- Use this identifier to track the matchmaking ticket status and retrieve
    -- match results.
    ticketId :: Prelude.Maybe Prelude.Text,
    -- | Name of the matchmaking configuration to use for this request.
    -- Matchmaking configurations must exist in the same Region as this
    -- request. You can use either the configuration name or ARN value.
    configurationName :: Prelude.Text,
    -- | Information on each player to be matched. This information must include
    -- a player ID, and may contain player attributes and latency data to be
    -- used in the matchmaking process. After a successful match, @Player@
    -- objects contain the name of the team the player is assigned to.
    --
    -- You can include up to 10 @Players@ in a @StartMatchmaking@ request.
    players :: [Player]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- You can include up to 10 @Players@ in a @StartMatchmaking@ request.
newStartMatchmaking ::
  -- | 'configurationName'
  Prelude.Text ->
  StartMatchmaking
newStartMatchmaking pConfigurationName_ =
  StartMatchmaking'
    { ticketId = Prelude.Nothing,
      configurationName = pConfigurationName_,
      players = Prelude.mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is
-- specified here, Amazon GameLift will generate one in the form of a UUID.
-- Use this identifier to track the matchmaking ticket status and retrieve
-- match results.
startMatchmaking_ticketId :: Lens.Lens' StartMatchmaking (Prelude.Maybe Prelude.Text)
startMatchmaking_ticketId = Lens.lens (\StartMatchmaking' {ticketId} -> ticketId) (\s@StartMatchmaking' {} a -> s {ticketId = a} :: StartMatchmaking)

-- | Name of the matchmaking configuration to use for this request.
-- Matchmaking configurations must exist in the same Region as this
-- request. You can use either the configuration name or ARN value.
startMatchmaking_configurationName :: Lens.Lens' StartMatchmaking Prelude.Text
startMatchmaking_configurationName = Lens.lens (\StartMatchmaking' {configurationName} -> configurationName) (\s@StartMatchmaking' {} a -> s {configurationName = a} :: StartMatchmaking)

-- | Information on each player to be matched. This information must include
-- a player ID, and may contain player attributes and latency data to be
-- used in the matchmaking process. After a successful match, @Player@
-- objects contain the name of the team the player is assigned to.
--
-- You can include up to 10 @Players@ in a @StartMatchmaking@ request.
startMatchmaking_players :: Lens.Lens' StartMatchmaking [Player]
startMatchmaking_players = Lens.lens (\StartMatchmaking' {players} -> players) (\s@StartMatchmaking' {} a -> s {players = a} :: StartMatchmaking) Prelude.. Lens.coerced

instance Core.AWSRequest StartMatchmaking where
  type
    AWSResponse StartMatchmaking =
      StartMatchmakingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMatchmakingResponse'
            Prelude.<$> (x Data..?> "MatchmakingTicket")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMatchmaking where
  hashWithSalt _salt StartMatchmaking' {..} =
    _salt `Prelude.hashWithSalt` ticketId
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` players

instance Prelude.NFData StartMatchmaking where
  rnf StartMatchmaking' {..} =
    Prelude.rnf ticketId
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf players

instance Data.ToHeaders StartMatchmaking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.StartMatchmaking" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMatchmaking where
  toJSON StartMatchmaking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TicketId" Data..=) Prelude.<$> ticketId,
            Prelude.Just
              ("ConfigurationName" Data..= configurationName),
            Prelude.Just ("Players" Data..= players)
          ]
      )

instance Data.ToPath StartMatchmaking where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMatchmaking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { -- | Ticket representing the matchmaking request. This object include the
    -- information included in the request, ticket status, and match results as
    -- generated during the matchmaking process.
    matchmakingTicket :: Prelude.Maybe MatchmakingTicket,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartMatchmakingResponse
newStartMatchmakingResponse pHttpStatus_ =
  StartMatchmakingResponse'
    { matchmakingTicket =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Ticket representing the matchmaking request. This object include the
-- information included in the request, ticket status, and match results as
-- generated during the matchmaking process.
startMatchmakingResponse_matchmakingTicket :: Lens.Lens' StartMatchmakingResponse (Prelude.Maybe MatchmakingTicket)
startMatchmakingResponse_matchmakingTicket = Lens.lens (\StartMatchmakingResponse' {matchmakingTicket} -> matchmakingTicket) (\s@StartMatchmakingResponse' {} a -> s {matchmakingTicket = a} :: StartMatchmakingResponse)

-- | The response's http status code.
startMatchmakingResponse_httpStatus :: Lens.Lens' StartMatchmakingResponse Prelude.Int
startMatchmakingResponse_httpStatus = Lens.lens (\StartMatchmakingResponse' {httpStatus} -> httpStatus) (\s@StartMatchmakingResponse' {} a -> s {httpStatus = a} :: StartMatchmakingResponse)

instance Prelude.NFData StartMatchmakingResponse where
  rnf StartMatchmakingResponse' {..} =
    Prelude.rnf matchmakingTicket
      `Prelude.seq` Prelude.rnf httpStatus
