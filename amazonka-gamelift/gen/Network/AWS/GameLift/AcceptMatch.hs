{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.AcceptMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a player\'s acceptance or rejection of a proposed FlexMatch
-- match. A matchmaking configuration may require player acceptance; if so,
-- then matches built with that configuration cannot be completed unless
-- all players accept the proposed match within a specified time limit.
--
-- When FlexMatch builds a match, all the matchmaking tickets involved in
-- the proposed match are placed into status @REQUIRES_ACCEPTANCE@. This is
-- a trigger for your game to get acceptance from all players in the
-- ticket. Acceptances are only valid for tickets when they are in this
-- status; all other acceptances result in an error.
--
-- To register acceptance, specify the ticket ID, a response, and one or
-- more players. Once all players have registered acceptance, the
-- matchmaking tickets advance to status @PLACING@, where a new game
-- session is created for the match.
--
-- If any player rejects the match, or if acceptances are not received
-- before a specified timeout, the proposed match is dropped. The
-- matchmaking tickets are then handled in one of two ways: For tickets
-- where one or more players rejected the match, the ticket status is
-- returned to @SEARCHING@ to find a new match. For tickets where one or
-- more players failed to respond, the ticket status is set to @CANCELLED@,
-- and processing is terminated. A new matchmaking request for these
-- players can be submitted as needed.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html FlexMatch Events Reference>
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
module Network.AWS.GameLift.AcceptMatch
  ( -- * Creating a Request
    AcceptMatch (..),
    newAcceptMatch,

    -- * Request Lenses
    acceptMatch_ticketId,
    acceptMatch_playerIds,
    acceptMatch_acceptanceType,

    -- * Destructuring the Response
    AcceptMatchResponse (..),
    newAcceptMatchResponse,

    -- * Response Lenses
    acceptMatchResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newAcceptMatch' smart constructor.
data AcceptMatch = AcceptMatch'
  { -- | A unique identifier for a matchmaking ticket. The ticket must be in
    -- status @REQUIRES_ACCEPTANCE@; otherwise this request will fail.
    ticketId :: Prelude.Text,
    -- | A unique identifier for a player delivering the response. This parameter
    -- can include one or multiple player IDs.
    playerIds :: [Prelude.Text],
    -- | Player response to the proposed match.
    acceptanceType :: AcceptanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AcceptMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ticketId', 'acceptMatch_ticketId' - A unique identifier for a matchmaking ticket. The ticket must be in
-- status @REQUIRES_ACCEPTANCE@; otherwise this request will fail.
--
-- 'playerIds', 'acceptMatch_playerIds' - A unique identifier for a player delivering the response. This parameter
-- can include one or multiple player IDs.
--
-- 'acceptanceType', 'acceptMatch_acceptanceType' - Player response to the proposed match.
newAcceptMatch ::
  -- | 'ticketId'
  Prelude.Text ->
  -- | 'acceptanceType'
  AcceptanceType ->
  AcceptMatch
newAcceptMatch pTicketId_ pAcceptanceType_ =
  AcceptMatch'
    { ticketId = pTicketId_,
      playerIds = Prelude.mempty,
      acceptanceType = pAcceptanceType_
    }

-- | A unique identifier for a matchmaking ticket. The ticket must be in
-- status @REQUIRES_ACCEPTANCE@; otherwise this request will fail.
acceptMatch_ticketId :: Lens.Lens' AcceptMatch Prelude.Text
acceptMatch_ticketId = Lens.lens (\AcceptMatch' {ticketId} -> ticketId) (\s@AcceptMatch' {} a -> s {ticketId = a} :: AcceptMatch)

-- | A unique identifier for a player delivering the response. This parameter
-- can include one or multiple player IDs.
acceptMatch_playerIds :: Lens.Lens' AcceptMatch [Prelude.Text]
acceptMatch_playerIds = Lens.lens (\AcceptMatch' {playerIds} -> playerIds) (\s@AcceptMatch' {} a -> s {playerIds = a} :: AcceptMatch) Prelude.. Prelude._Coerce

-- | Player response to the proposed match.
acceptMatch_acceptanceType :: Lens.Lens' AcceptMatch AcceptanceType
acceptMatch_acceptanceType = Lens.lens (\AcceptMatch' {acceptanceType} -> acceptanceType) (\s@AcceptMatch' {} a -> s {acceptanceType = a} :: AcceptMatch)

instance Prelude.AWSRequest AcceptMatch where
  type Rs AcceptMatch = AcceptMatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptMatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptMatch

instance Prelude.NFData AcceptMatch

instance Prelude.ToHeaders AcceptMatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.AcceptMatch" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AcceptMatch where
  toJSON AcceptMatch' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TicketId" Prelude..= ticketId),
            Prelude.Just ("PlayerIds" Prelude..= playerIds),
            Prelude.Just
              ("AcceptanceType" Prelude..= acceptanceType)
          ]
      )

instance Prelude.ToPath AcceptMatch where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AcceptMatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptMatchResponse' smart constructor.
data AcceptMatchResponse = AcceptMatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AcceptMatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptMatchResponse_httpStatus' - The response's http status code.
newAcceptMatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptMatchResponse
newAcceptMatchResponse pHttpStatus_ =
  AcceptMatchResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
acceptMatchResponse_httpStatus :: Lens.Lens' AcceptMatchResponse Prelude.Int
acceptMatchResponse_httpStatus = Lens.lens (\AcceptMatchResponse' {httpStatus} -> httpStatus) (\s@AcceptMatchResponse' {} a -> s {httpStatus = a} :: AcceptMatchResponse)

instance Prelude.NFData AcceptMatchResponse
