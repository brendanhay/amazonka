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
-- Module      : Amazonka.Nimble.DeleteStreamingSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes streaming session resource.
--
-- After invoking this operation, use GetStreamingSession to poll the
-- resource until it transitions to a DELETED state.
--
-- A streaming session will count against your streaming session quota
-- until it is marked DELETED.
module Amazonka.Nimble.DeleteStreamingSession
  ( -- * Creating a Request
    DeleteStreamingSession (..),
    newDeleteStreamingSession,

    -- * Request Lenses
    deleteStreamingSession_clientToken,
    deleteStreamingSession_studioId,
    deleteStreamingSession_sessionId,

    -- * Destructuring the Response
    DeleteStreamingSessionResponse (..),
    newDeleteStreamingSessionResponse,

    -- * Response Lenses
    deleteStreamingSessionResponse_session,
    deleteStreamingSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStreamingSession' smart constructor.
data DeleteStreamingSession = DeleteStreamingSession'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStreamingSession_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'deleteStreamingSession_studioId' - The studio ID.
--
-- 'sessionId', 'deleteStreamingSession_sessionId' - The session ID.
newDeleteStreamingSession ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  DeleteStreamingSession
newDeleteStreamingSession pStudioId_ pSessionId_ =
  DeleteStreamingSession'
    { clientToken =
        Prelude.Nothing,
      studioId = pStudioId_,
      sessionId = pSessionId_
    }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
deleteStreamingSession_clientToken :: Lens.Lens' DeleteStreamingSession (Prelude.Maybe Prelude.Text)
deleteStreamingSession_clientToken = Lens.lens (\DeleteStreamingSession' {clientToken} -> clientToken) (\s@DeleteStreamingSession' {} a -> s {clientToken = a} :: DeleteStreamingSession)

-- | The studio ID.
deleteStreamingSession_studioId :: Lens.Lens' DeleteStreamingSession Prelude.Text
deleteStreamingSession_studioId = Lens.lens (\DeleteStreamingSession' {studioId} -> studioId) (\s@DeleteStreamingSession' {} a -> s {studioId = a} :: DeleteStreamingSession)

-- | The session ID.
deleteStreamingSession_sessionId :: Lens.Lens' DeleteStreamingSession Prelude.Text
deleteStreamingSession_sessionId = Lens.lens (\DeleteStreamingSession' {sessionId} -> sessionId) (\s@DeleteStreamingSession' {} a -> s {sessionId = a} :: DeleteStreamingSession)

instance Core.AWSRequest DeleteStreamingSession where
  type
    AWSResponse DeleteStreamingSession =
      DeleteStreamingSessionResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStreamingSessionResponse'
            Prelude.<$> (x Core..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStreamingSession

instance Prelude.NFData DeleteStreamingSession

instance Core.ToHeaders DeleteStreamingSession where
  toHeaders DeleteStreamingSession' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DeleteStreamingSession where
  toPath DeleteStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-sessions/",
        Core.toBS sessionId
      ]

instance Core.ToQuery DeleteStreamingSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamingSessionResponse' smart constructor.
data DeleteStreamingSessionResponse = DeleteStreamingSessionResponse'
  { -- | The session.
    session :: Prelude.Maybe StreamingSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'deleteStreamingSessionResponse_session' - The session.
--
-- 'httpStatus', 'deleteStreamingSessionResponse_httpStatus' - The response's http status code.
newDeleteStreamingSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStreamingSessionResponse
newDeleteStreamingSessionResponse pHttpStatus_ =
  DeleteStreamingSessionResponse'
    { session =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session.
deleteStreamingSessionResponse_session :: Lens.Lens' DeleteStreamingSessionResponse (Prelude.Maybe StreamingSession)
deleteStreamingSessionResponse_session = Lens.lens (\DeleteStreamingSessionResponse' {session} -> session) (\s@DeleteStreamingSessionResponse' {} a -> s {session = a} :: DeleteStreamingSessionResponse)

-- | The response's http status code.
deleteStreamingSessionResponse_httpStatus :: Lens.Lens' DeleteStreamingSessionResponse Prelude.Int
deleteStreamingSessionResponse_httpStatus = Lens.lens (\DeleteStreamingSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamingSessionResponse' {} a -> s {httpStatus = a} :: DeleteStreamingSessionResponse)

instance
  Prelude.NFData
    DeleteStreamingSessionResponse
