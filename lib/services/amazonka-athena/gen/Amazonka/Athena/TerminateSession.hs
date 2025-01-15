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
-- Module      : Amazonka.Athena.TerminateSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates an active session. A @TerminateSession@ call on a session
-- that is already inactive (for example, in a @FAILED@, @TERMINATED@ or
-- @TERMINATING@ state) succeeds but has no effect. Calculations running in
-- the session when @TerminateSession@ is called are forcefully stopped,
-- but may display as @FAILED@ instead of @STOPPED@.
module Amazonka.Athena.TerminateSession
  ( -- * Creating a Request
    TerminateSession (..),
    newTerminateSession,

    -- * Request Lenses
    terminateSession_sessionId,

    -- * Destructuring the Response
    TerminateSessionResponse (..),
    newTerminateSessionResponse,

    -- * Response Lenses
    terminateSessionResponse_state,
    terminateSessionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTerminateSession' smart constructor.
data TerminateSession = TerminateSession'
  { -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'terminateSession_sessionId' - The session ID.
newTerminateSession ::
  -- | 'sessionId'
  Prelude.Text ->
  TerminateSession
newTerminateSession pSessionId_ =
  TerminateSession' {sessionId = pSessionId_}

-- | The session ID.
terminateSession_sessionId :: Lens.Lens' TerminateSession Prelude.Text
terminateSession_sessionId = Lens.lens (\TerminateSession' {sessionId} -> sessionId) (\s@TerminateSession' {} a -> s {sessionId = a} :: TerminateSession)

instance Core.AWSRequest TerminateSession where
  type
    AWSResponse TerminateSession =
      TerminateSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateSessionResponse'
            Prelude.<$> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateSession where
  hashWithSalt _salt TerminateSession' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData TerminateSession where
  rnf TerminateSession' {..} = Prelude.rnf sessionId

instance Data.ToHeaders TerminateSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.TerminateSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateSession where
  toJSON TerminateSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath TerminateSession where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateSessionResponse' smart constructor.
data TerminateSessionResponse = TerminateSessionResponse'
  { -- | The state of the session. A description of each state follows.
    --
    -- @CREATING@ - The session is being started, including acquiring
    -- resources.
    --
    -- @CREATED@ - The session has been started.
    --
    -- @IDLE@ - The session is able to accept a calculation.
    --
    -- @BUSY@ - The session is processing another task and is unable to accept
    -- a calculation.
    --
    -- @TERMINATING@ - The session is in the process of shutting down.
    --
    -- @TERMINATED@ - The session and its resources are no longer running.
    --
    -- @DEGRADED@ - The session has no healthy coordinators.
    --
    -- @FAILED@ - Due to a failure, the session and its resources are no longer
    -- running.
    state :: Prelude.Maybe SessionState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'terminateSessionResponse_state' - The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
--
-- 'httpStatus', 'terminateSessionResponse_httpStatus' - The response's http status code.
newTerminateSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateSessionResponse
newTerminateSessionResponse pHttpStatus_ =
  TerminateSessionResponse'
    { state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
terminateSessionResponse_state :: Lens.Lens' TerminateSessionResponse (Prelude.Maybe SessionState)
terminateSessionResponse_state = Lens.lens (\TerminateSessionResponse' {state} -> state) (\s@TerminateSessionResponse' {} a -> s {state = a} :: TerminateSessionResponse)

-- | The response's http status code.
terminateSessionResponse_httpStatus :: Lens.Lens' TerminateSessionResponse Prelude.Int
terminateSessionResponse_httpStatus = Lens.lens (\TerminateSessionResponse' {httpStatus} -> httpStatus) (\s@TerminateSessionResponse' {} a -> s {httpStatus = a} :: TerminateSessionResponse)

instance Prelude.NFData TerminateSessionResponse where
  rnf TerminateSessionResponse' {..} =
    Prelude.rnf state `Prelude.seq`
      Prelude.rnf httpStatus
