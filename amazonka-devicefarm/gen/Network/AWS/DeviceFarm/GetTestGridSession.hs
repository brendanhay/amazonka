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
-- Module      : Network.AWS.DeviceFarm.GetTestGridSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A session is an instance of a browser created through a
-- @RemoteWebDriver@ with the URL from CreateTestGridUrlResult$url. You can
-- use the following to look up sessions:
--
-- -   The session ARN (GetTestGridSessionRequest$sessionArn).
--
-- -   The project ARN and a session ID
--     (GetTestGridSessionRequest$projectArn and
--     GetTestGridSessionRequest$sessionId).
module Network.AWS.DeviceFarm.GetTestGridSession
  ( -- * Creating a Request
    GetTestGridSession (..),
    newGetTestGridSession,

    -- * Request Lenses
    getTestGridSession_sessionId,
    getTestGridSession_sessionArn,
    getTestGridSession_projectArn,

    -- * Destructuring the Response
    GetTestGridSessionResponse (..),
    newGetTestGridSessionResponse,

    -- * Response Lenses
    getTestGridSessionResponse_testGridSession,
    getTestGridSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { -- | An ID associated with this session.
    sessionId :: Core.Maybe Core.Text,
    -- | An ARN that uniquely identifies a TestGridSession.
    sessionArn :: Core.Maybe Core.Text,
    -- | The ARN for the project that this session belongs to. See
    -- CreateTestGridProject and ListTestGridProjects.
    projectArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTestGridSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getTestGridSession_sessionId' - An ID associated with this session.
--
-- 'sessionArn', 'getTestGridSession_sessionArn' - An ARN that uniquely identifies a TestGridSession.
--
-- 'projectArn', 'getTestGridSession_projectArn' - The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
newGetTestGridSession ::
  GetTestGridSession
newGetTestGridSession =
  GetTestGridSession'
    { sessionId = Core.Nothing,
      sessionArn = Core.Nothing,
      projectArn = Core.Nothing
    }

-- | An ID associated with this session.
getTestGridSession_sessionId :: Lens.Lens' GetTestGridSession (Core.Maybe Core.Text)
getTestGridSession_sessionId = Lens.lens (\GetTestGridSession' {sessionId} -> sessionId) (\s@GetTestGridSession' {} a -> s {sessionId = a} :: GetTestGridSession)

-- | An ARN that uniquely identifies a TestGridSession.
getTestGridSession_sessionArn :: Lens.Lens' GetTestGridSession (Core.Maybe Core.Text)
getTestGridSession_sessionArn = Lens.lens (\GetTestGridSession' {sessionArn} -> sessionArn) (\s@GetTestGridSession' {} a -> s {sessionArn = a} :: GetTestGridSession)

-- | The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
getTestGridSession_projectArn :: Lens.Lens' GetTestGridSession (Core.Maybe Core.Text)
getTestGridSession_projectArn = Lens.lens (\GetTestGridSession' {projectArn} -> projectArn) (\s@GetTestGridSession' {} a -> s {projectArn = a} :: GetTestGridSession)

instance Core.AWSRequest GetTestGridSession where
  type
    AWSResponse GetTestGridSession =
      GetTestGridSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            Core.<$> (x Core..?> "testGridSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTestGridSession

instance Core.NFData GetTestGridSession

instance Core.ToHeaders GetTestGridSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetTestGridSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTestGridSession where
  toJSON GetTestGridSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sessionId" Core..=) Core.<$> sessionId,
            ("sessionArn" Core..=) Core.<$> sessionArn,
            ("projectArn" Core..=) Core.<$> projectArn
          ]
      )

instance Core.ToPath GetTestGridSession where
  toPath = Core.const "/"

instance Core.ToQuery GetTestGridSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { -- | The TestGridSession that was requested.
    testGridSession :: Core.Maybe TestGridSession,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTestGridSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridSession', 'getTestGridSessionResponse_testGridSession' - The TestGridSession that was requested.
--
-- 'httpStatus', 'getTestGridSessionResponse_httpStatus' - The response's http status code.
newGetTestGridSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTestGridSessionResponse
newGetTestGridSessionResponse pHttpStatus_ =
  GetTestGridSessionResponse'
    { testGridSession =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The TestGridSession that was requested.
getTestGridSessionResponse_testGridSession :: Lens.Lens' GetTestGridSessionResponse (Core.Maybe TestGridSession)
getTestGridSessionResponse_testGridSession = Lens.lens (\GetTestGridSessionResponse' {testGridSession} -> testGridSession) (\s@GetTestGridSessionResponse' {} a -> s {testGridSession = a} :: GetTestGridSessionResponse)

-- | The response's http status code.
getTestGridSessionResponse_httpStatus :: Lens.Lens' GetTestGridSessionResponse Core.Int
getTestGridSessionResponse_httpStatus = Lens.lens (\GetTestGridSessionResponse' {httpStatus} -> httpStatus) (\s@GetTestGridSessionResponse' {} a -> s {httpStatus = a} :: GetTestGridSessionResponse)

instance Core.NFData GetTestGridSessionResponse
