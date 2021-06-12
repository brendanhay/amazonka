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
-- Module      : Network.AWS.GameLift.StopMatchmaking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a matchmaking ticket or match backfill ticket that is currently
-- being processed. To stop the matchmaking operation, specify the ticket
-- ID. If successful, work on the ticket is stopped, and the ticket status
-- is changed to @CANCELLED@.
--
-- This call is also used to turn off automatic backfill for an individual
-- game session. This is for game sessions that are created with a
-- matchmaking configuration that has automatic backfill enabled. The
-- ticket ID is included in the @MatchmakerData@ of an updated game session
-- object, which is provided to the game server.
--
-- If the operation is successful, the service sends back an empty JSON
-- struct with the HTTP 200 response (not an empty HTTP body).
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
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
module Network.AWS.GameLift.StopMatchmaking
  ( -- * Creating a Request
    StopMatchmaking (..),
    newStopMatchmaking,

    -- * Request Lenses
    stopMatchmaking_ticketId,

    -- * Destructuring the Response
    StopMatchmakingResponse (..),
    newStopMatchmakingResponse,

    -- * Response Lenses
    stopMatchmakingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStopMatchmaking' smart constructor.
data StopMatchmaking = StopMatchmaking'
  { -- | A unique identifier for a matchmaking ticket.
    ticketId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopMatchmaking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ticketId', 'stopMatchmaking_ticketId' - A unique identifier for a matchmaking ticket.
newStopMatchmaking ::
  -- | 'ticketId'
  Core.Text ->
  StopMatchmaking
newStopMatchmaking pTicketId_ =
  StopMatchmaking' {ticketId = pTicketId_}

-- | A unique identifier for a matchmaking ticket.
stopMatchmaking_ticketId :: Lens.Lens' StopMatchmaking Core.Text
stopMatchmaking_ticketId = Lens.lens (\StopMatchmaking' {ticketId} -> ticketId) (\s@StopMatchmaking' {} a -> s {ticketId = a} :: StopMatchmaking)

instance Core.AWSRequest StopMatchmaking where
  type
    AWSResponse StopMatchmaking =
      StopMatchmakingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopMatchmakingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopMatchmaking

instance Core.NFData StopMatchmaking

instance Core.ToHeaders StopMatchmaking where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.StopMatchmaking" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopMatchmaking where
  toJSON StopMatchmaking' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TicketId" Core..= ticketId)]
      )

instance Core.ToPath StopMatchmaking where
  toPath = Core.const "/"

instance Core.ToQuery StopMatchmaking where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopMatchmakingResponse' smart constructor.
data StopMatchmakingResponse = StopMatchmakingResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopMatchmakingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopMatchmakingResponse_httpStatus' - The response's http status code.
newStopMatchmakingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopMatchmakingResponse
newStopMatchmakingResponse pHttpStatus_ =
  StopMatchmakingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopMatchmakingResponse_httpStatus :: Lens.Lens' StopMatchmakingResponse Core.Int
stopMatchmakingResponse_httpStatus = Lens.lens (\StopMatchmakingResponse' {httpStatus} -> httpStatus) (\s@StopMatchmakingResponse' {} a -> s {httpStatus = a} :: StopMatchmakingResponse)

instance Core.NFData StopMatchmakingResponse
