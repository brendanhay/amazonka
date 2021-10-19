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
    getTestGridSession_sessionArn,
    getTestGridSession_projectArn,
    getTestGridSession_sessionId,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { -- | An ARN that uniquely identifies a TestGridSession.
    sessionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the project that this session belongs to. See
    -- CreateTestGridProject and ListTestGridProjects.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | An ID associated with this session.
    sessionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestGridSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionArn', 'getTestGridSession_sessionArn' - An ARN that uniquely identifies a TestGridSession.
--
-- 'projectArn', 'getTestGridSession_projectArn' - The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
--
-- 'sessionId', 'getTestGridSession_sessionId' - An ID associated with this session.
newGetTestGridSession ::
  GetTestGridSession
newGetTestGridSession =
  GetTestGridSession'
    { sessionArn = Prelude.Nothing,
      projectArn = Prelude.Nothing,
      sessionId = Prelude.Nothing
    }

-- | An ARN that uniquely identifies a TestGridSession.
getTestGridSession_sessionArn :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_sessionArn = Lens.lens (\GetTestGridSession' {sessionArn} -> sessionArn) (\s@GetTestGridSession' {} a -> s {sessionArn = a} :: GetTestGridSession)

-- | The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
getTestGridSession_projectArn :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_projectArn = Lens.lens (\GetTestGridSession' {projectArn} -> projectArn) (\s@GetTestGridSession' {} a -> s {projectArn = a} :: GetTestGridSession)

-- | An ID associated with this session.
getTestGridSession_sessionId :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_sessionId = Lens.lens (\GetTestGridSession' {sessionId} -> sessionId) (\s@GetTestGridSession' {} a -> s {sessionId = a} :: GetTestGridSession)

instance Core.AWSRequest GetTestGridSession where
  type
    AWSResponse GetTestGridSession =
      GetTestGridSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            Prelude.<$> (x Core..?> "testGridSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTestGridSession

instance Prelude.NFData GetTestGridSession

instance Core.ToHeaders GetTestGridSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetTestGridSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTestGridSession where
  toJSON GetTestGridSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sessionArn" Core..=) Prelude.<$> sessionArn,
            ("projectArn" Core..=) Prelude.<$> projectArn,
            ("sessionId" Core..=) Prelude.<$> sessionId
          ]
      )

instance Core.ToPath GetTestGridSession where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTestGridSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { -- | The TestGridSession that was requested.
    testGridSession :: Prelude.Maybe TestGridSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTestGridSessionResponse
newGetTestGridSessionResponse pHttpStatus_ =
  GetTestGridSessionResponse'
    { testGridSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The TestGridSession that was requested.
getTestGridSessionResponse_testGridSession :: Lens.Lens' GetTestGridSessionResponse (Prelude.Maybe TestGridSession)
getTestGridSessionResponse_testGridSession = Lens.lens (\GetTestGridSessionResponse' {testGridSession} -> testGridSession) (\s@GetTestGridSessionResponse' {} a -> s {testGridSession = a} :: GetTestGridSessionResponse)

-- | The response's http status code.
getTestGridSessionResponse_httpStatus :: Lens.Lens' GetTestGridSessionResponse Prelude.Int
getTestGridSessionResponse_httpStatus = Lens.lens (\GetTestGridSessionResponse' {httpStatus} -> httpStatus) (\s@GetTestGridSessionResponse' {} a -> s {httpStatus = a} :: GetTestGridSessionResponse)

instance Prelude.NFData GetTestGridSessionResponse
