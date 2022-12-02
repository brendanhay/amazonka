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
-- Module      : Amazonka.DeviceFarm.GetTestGridSession
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DeviceFarm.GetTestGridSession
  ( -- * Creating a Request
    GetTestGridSession (..),
    newGetTestGridSession,

    -- * Request Lenses
    getTestGridSession_sessionArn,
    getTestGridSession_sessionId,
    getTestGridSession_projectArn,

    -- * Destructuring the Response
    GetTestGridSessionResponse (..),
    newGetTestGridSessionResponse,

    -- * Response Lenses
    getTestGridSessionResponse_testGridSession,
    getTestGridSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { -- | An ARN that uniquely identifies a TestGridSession.
    sessionArn :: Prelude.Maybe Prelude.Text,
    -- | An ID associated with this session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the project that this session belongs to. See
    -- CreateTestGridProject and ListTestGridProjects.
    projectArn :: Prelude.Maybe Prelude.Text
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
-- 'sessionId', 'getTestGridSession_sessionId' - An ID associated with this session.
--
-- 'projectArn', 'getTestGridSession_projectArn' - The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
newGetTestGridSession ::
  GetTestGridSession
newGetTestGridSession =
  GetTestGridSession'
    { sessionArn = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      projectArn = Prelude.Nothing
    }

-- | An ARN that uniquely identifies a TestGridSession.
getTestGridSession_sessionArn :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_sessionArn = Lens.lens (\GetTestGridSession' {sessionArn} -> sessionArn) (\s@GetTestGridSession' {} a -> s {sessionArn = a} :: GetTestGridSession)

-- | An ID associated with this session.
getTestGridSession_sessionId :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_sessionId = Lens.lens (\GetTestGridSession' {sessionId} -> sessionId) (\s@GetTestGridSession' {} a -> s {sessionId = a} :: GetTestGridSession)

-- | The ARN for the project that this session belongs to. See
-- CreateTestGridProject and ListTestGridProjects.
getTestGridSession_projectArn :: Lens.Lens' GetTestGridSession (Prelude.Maybe Prelude.Text)
getTestGridSession_projectArn = Lens.lens (\GetTestGridSession' {projectArn} -> projectArn) (\s@GetTestGridSession' {} a -> s {projectArn = a} :: GetTestGridSession)

instance Core.AWSRequest GetTestGridSession where
  type
    AWSResponse GetTestGridSession =
      GetTestGridSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            Prelude.<$> (x Data..?> "testGridSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTestGridSession where
  hashWithSalt _salt GetTestGridSession' {..} =
    _salt `Prelude.hashWithSalt` sessionArn
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData GetTestGridSession where
  rnf GetTestGridSession' {..} =
    Prelude.rnf sessionArn
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf projectArn

instance Data.ToHeaders GetTestGridSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetTestGridSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTestGridSession where
  toJSON GetTestGridSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sessionArn" Data..=) Prelude.<$> sessionArn,
            ("sessionId" Data..=) Prelude.<$> sessionId,
            ("projectArn" Data..=) Prelude.<$> projectArn
          ]
      )

instance Data.ToPath GetTestGridSession where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTestGridSession where
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

instance Prelude.NFData GetTestGridSessionResponse where
  rnf GetTestGridSessionResponse' {..} =
    Prelude.rnf testGridSession
      `Prelude.seq` Prelude.rnf httpStatus
