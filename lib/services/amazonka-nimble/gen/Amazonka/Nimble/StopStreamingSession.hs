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
-- Module      : Amazonka.Nimble.StopStreamingSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transitions sessions from the READY state into the STOPPED state. The
-- STOP_IN_PROGRESS state is the intermediate state between the READY and
-- STOPPED states.
module Amazonka.Nimble.StopStreamingSession
  ( -- * Creating a Request
    StopStreamingSession (..),
    newStopStreamingSession,

    -- * Request Lenses
    stopStreamingSession_clientToken,
    stopStreamingSession_sessionId,
    stopStreamingSession_studioId,

    -- * Destructuring the Response
    StopStreamingSessionResponse (..),
    newStopStreamingSessionResponse,

    -- * Response Lenses
    stopStreamingSessionResponse_session,
    stopStreamingSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopStreamingSession' smart constructor.
data StopStreamingSession = StopStreamingSession'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The streaming session ID for the StopStreamingSessionRequest.
    sessionId :: Prelude.Text,
    -- | The studioId for the StopStreamingSessionRequest.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'stopStreamingSession_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'sessionId', 'stopStreamingSession_sessionId' - The streaming session ID for the StopStreamingSessionRequest.
--
-- 'studioId', 'stopStreamingSession_studioId' - The studioId for the StopStreamingSessionRequest.
newStopStreamingSession ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  StopStreamingSession
newStopStreamingSession pSessionId_ pStudioId_ =
  StopStreamingSession'
    { clientToken =
        Prelude.Nothing,
      sessionId = pSessionId_,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
stopStreamingSession_clientToken :: Lens.Lens' StopStreamingSession (Prelude.Maybe Prelude.Text)
stopStreamingSession_clientToken = Lens.lens (\StopStreamingSession' {clientToken} -> clientToken) (\s@StopStreamingSession' {} a -> s {clientToken = a} :: StopStreamingSession)

-- | The streaming session ID for the StopStreamingSessionRequest.
stopStreamingSession_sessionId :: Lens.Lens' StopStreamingSession Prelude.Text
stopStreamingSession_sessionId = Lens.lens (\StopStreamingSession' {sessionId} -> sessionId) (\s@StopStreamingSession' {} a -> s {sessionId = a} :: StopStreamingSession)

-- | The studioId for the StopStreamingSessionRequest.
stopStreamingSession_studioId :: Lens.Lens' StopStreamingSession Prelude.Text
stopStreamingSession_studioId = Lens.lens (\StopStreamingSession' {studioId} -> studioId) (\s@StopStreamingSession' {} a -> s {studioId = a} :: StopStreamingSession)

instance Core.AWSRequest StopStreamingSession where
  type
    AWSResponse StopStreamingSession =
      StopStreamingSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopStreamingSessionResponse'
            Prelude.<$> (x Data..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopStreamingSession where
  hashWithSalt _salt StopStreamingSession' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData StopStreamingSession where
  rnf StopStreamingSession' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders StopStreamingSession where
  toHeaders StopStreamingSession' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON StopStreamingSession where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopStreamingSession where
  toPath StopStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions/",
        Data.toBS sessionId,
        "/stop"
      ]

instance Data.ToQuery StopStreamingSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopStreamingSessionResponse' smart constructor.
data StopStreamingSessionResponse = StopStreamingSessionResponse'
  { session :: Prelude.Maybe StreamingSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStreamingSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'stopStreamingSessionResponse_session' - Undocumented member.
--
-- 'httpStatus', 'stopStreamingSessionResponse_httpStatus' - The response's http status code.
newStopStreamingSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopStreamingSessionResponse
newStopStreamingSessionResponse pHttpStatus_ =
  StopStreamingSessionResponse'
    { session =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopStreamingSessionResponse_session :: Lens.Lens' StopStreamingSessionResponse (Prelude.Maybe StreamingSession)
stopStreamingSessionResponse_session = Lens.lens (\StopStreamingSessionResponse' {session} -> session) (\s@StopStreamingSessionResponse' {} a -> s {session = a} :: StopStreamingSessionResponse)

-- | The response's http status code.
stopStreamingSessionResponse_httpStatus :: Lens.Lens' StopStreamingSessionResponse Prelude.Int
stopStreamingSessionResponse_httpStatus = Lens.lens (\StopStreamingSessionResponse' {httpStatus} -> httpStatus) (\s@StopStreamingSessionResponse' {} a -> s {httpStatus = a} :: StopStreamingSessionResponse)

instance Prelude.NFData StopStreamingSessionResponse where
  rnf StopStreamingSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
