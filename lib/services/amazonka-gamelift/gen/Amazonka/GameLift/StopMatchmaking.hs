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
-- Module      : Amazonka.GameLift.StopMatchmaking
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a game client>
--
-- __Related actions__
--
-- StartMatchmaking | DescribeMatchmaking | StopMatchmaking | AcceptMatch |
-- StartMatchBackfill |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.StopMatchmaking
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStopMatchmaking' smart constructor.
data StopMatchmaking = StopMatchmaking'
  { -- | A unique identifier for a matchmaking ticket.
    ticketId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopMatchmaking
newStopMatchmaking pTicketId_ =
  StopMatchmaking' {ticketId = pTicketId_}

-- | A unique identifier for a matchmaking ticket.
stopMatchmaking_ticketId :: Lens.Lens' StopMatchmaking Prelude.Text
stopMatchmaking_ticketId = Lens.lens (\StopMatchmaking' {ticketId} -> ticketId) (\s@StopMatchmaking' {} a -> s {ticketId = a} :: StopMatchmaking)

instance Core.AWSRequest StopMatchmaking where
  type
    AWSResponse StopMatchmaking =
      StopMatchmakingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopMatchmakingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopMatchmaking where
  hashWithSalt _salt StopMatchmaking' {..} =
    _salt `Prelude.hashWithSalt` ticketId

instance Prelude.NFData StopMatchmaking where
  rnf StopMatchmaking' {..} = Prelude.rnf ticketId

instance Core.ToHeaders StopMatchmaking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.StopMatchmaking" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopMatchmaking where
  toJSON StopMatchmaking' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TicketId" Core..= ticketId)]
      )

instance Core.ToPath StopMatchmaking where
  toPath = Prelude.const "/"

instance Core.ToQuery StopMatchmaking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopMatchmakingResponse' smart constructor.
data StopMatchmakingResponse = StopMatchmakingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopMatchmakingResponse
newStopMatchmakingResponse pHttpStatus_ =
  StopMatchmakingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopMatchmakingResponse_httpStatus :: Lens.Lens' StopMatchmakingResponse Prelude.Int
stopMatchmakingResponse_httpStatus = Lens.lens (\StopMatchmakingResponse' {httpStatus} -> httpStatus) (\s@StopMatchmakingResponse' {} a -> s {httpStatus = a} :: StopMatchmakingResponse)

instance Prelude.NFData StopMatchmakingResponse where
  rnf StopMatchmakingResponse' {..} =
    Prelude.rnf httpStatus
