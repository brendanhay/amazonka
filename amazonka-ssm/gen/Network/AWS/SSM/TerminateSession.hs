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
-- Module      : Network.AWS.SSM.TerminateSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently ends a session and closes the data connection between the
-- Session Manager client and SSM Agent on the instance. A terminated
-- session cannot be resumed.
module Network.AWS.SSM.TerminateSession
  ( -- * Creating a Request
    TerminateSession (..),
    newTerminateSession,

    -- * Request Lenses
    terminateSession_sessionId,

    -- * Destructuring the Response
    TerminateSessionResponse (..),
    newTerminateSessionResponse,

    -- * Response Lenses
    terminateSessionResponse_sessionId,
    terminateSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newTerminateSession' smart constructor.
data TerminateSession = TerminateSession'
  { -- | The ID of the session to terminate.
    sessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'terminateSession_sessionId' - The ID of the session to terminate.
newTerminateSession ::
  -- | 'sessionId'
  Core.Text ->
  TerminateSession
newTerminateSession pSessionId_ =
  TerminateSession' {sessionId = pSessionId_}

-- | The ID of the session to terminate.
terminateSession_sessionId :: Lens.Lens' TerminateSession Core.Text
terminateSession_sessionId = Lens.lens (\TerminateSession' {sessionId} -> sessionId) (\s@TerminateSession' {} a -> s {sessionId = a} :: TerminateSession)

instance Core.AWSRequest TerminateSession where
  type
    AWSResponse TerminateSession =
      TerminateSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateSessionResponse'
            Core.<$> (x Core..?> "SessionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TerminateSession

instance Core.NFData TerminateSession

instance Core.ToHeaders TerminateSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.TerminateSession" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TerminateSession where
  toJSON TerminateSession' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SessionId" Core..= sessionId)]
      )

instance Core.ToPath TerminateSession where
  toPath = Core.const "/"

instance Core.ToQuery TerminateSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTerminateSessionResponse' smart constructor.
data TerminateSessionResponse = TerminateSessionResponse'
  { -- | The ID of the session that has been terminated.
    sessionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'terminateSessionResponse_sessionId' - The ID of the session that has been terminated.
--
-- 'httpStatus', 'terminateSessionResponse_httpStatus' - The response's http status code.
newTerminateSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TerminateSessionResponse
newTerminateSessionResponse pHttpStatus_ =
  TerminateSessionResponse'
    { sessionId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the session that has been terminated.
terminateSessionResponse_sessionId :: Lens.Lens' TerminateSessionResponse (Core.Maybe Core.Text)
terminateSessionResponse_sessionId = Lens.lens (\TerminateSessionResponse' {sessionId} -> sessionId) (\s@TerminateSessionResponse' {} a -> s {sessionId = a} :: TerminateSessionResponse)

-- | The response's http status code.
terminateSessionResponse_httpStatus :: Lens.Lens' TerminateSessionResponse Core.Int
terminateSessionResponse_httpStatus = Lens.lens (\TerminateSessionResponse' {httpStatus} -> httpStatus) (\s@TerminateSessionResponse' {} a -> s {httpStatus = a} :: TerminateSessionResponse)

instance Core.NFData TerminateSessionResponse
