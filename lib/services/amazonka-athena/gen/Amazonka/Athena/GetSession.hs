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
-- Module      : Amazonka.Athena.GetSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the full details of a previously created session, including the
-- session status and configuration.
module Amazonka.Athena.GetSession
  ( -- * Creating a Request
    GetSession (..),
    newGetSession,

    -- * Request Lenses
    getSession_sessionId,

    -- * Destructuring the Response
    GetSessionResponse (..),
    newGetSessionResponse,

    -- * Response Lenses
    getSessionResponse_description,
    getSessionResponse_engineConfiguration,
    getSessionResponse_engineVersion,
    getSessionResponse_notebookVersion,
    getSessionResponse_sessionConfiguration,
    getSessionResponse_sessionId,
    getSessionResponse_statistics,
    getSessionResponse_status,
    getSessionResponse_workGroup,
    getSessionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getSession_sessionId' - The session ID.
newGetSession ::
  -- | 'sessionId'
  Prelude.Text ->
  GetSession
newGetSession pSessionId_ =
  GetSession' {sessionId = pSessionId_}

-- | The session ID.
getSession_sessionId :: Lens.Lens' GetSession Prelude.Text
getSession_sessionId = Lens.lens (\GetSession' {sessionId} -> sessionId) (\s@GetSession' {} a -> s {sessionId = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "EngineConfiguration")
            Prelude.<*> (x Data..?> "EngineVersion")
            Prelude.<*> (x Data..?> "NotebookVersion")
            Prelude.<*> (x Data..?> "SessionConfiguration")
            Prelude.<*> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "Statistics")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "WorkGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSession where
  hashWithSalt _salt GetSession' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData GetSession where
  rnf GetSession' {..} = Prelude.rnf sessionId

instance Data.ToHeaders GetSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonAthena.GetSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSession where
  toJSON GetSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath GetSession where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | The session description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Contains engine configuration information like DPU usage.
    engineConfiguration :: Prelude.Maybe EngineConfiguration,
    -- | The engine version used by the session (for example,
    -- @PySpark engine version 3@). You can get a list of engine versions by
    -- calling ListEngineVersions.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The notebook version.
    notebookVersion :: Prelude.Maybe Prelude.Text,
    -- | Contains the workgroup configuration information used by the session.
    sessionConfiguration :: Prelude.Maybe SessionConfiguration,
    -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Contains the DPU execution time.
    statistics :: Prelude.Maybe SessionStatistics,
    -- | Contains information about the status of the session.
    status :: Prelude.Maybe SessionStatus,
    -- | The workgroup to which the session belongs.
    workGroup :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSessionResponse_description' - The session description.
--
-- 'engineConfiguration', 'getSessionResponse_engineConfiguration' - Contains engine configuration information like DPU usage.
--
-- 'engineVersion', 'getSessionResponse_engineVersion' - The engine version used by the session (for example,
-- @PySpark engine version 3@). You can get a list of engine versions by
-- calling ListEngineVersions.
--
-- 'notebookVersion', 'getSessionResponse_notebookVersion' - The notebook version.
--
-- 'sessionConfiguration', 'getSessionResponse_sessionConfiguration' - Contains the workgroup configuration information used by the session.
--
-- 'sessionId', 'getSessionResponse_sessionId' - The session ID.
--
-- 'statistics', 'getSessionResponse_statistics' - Contains the DPU execution time.
--
-- 'status', 'getSessionResponse_status' - Contains information about the status of the session.
--
-- 'workGroup', 'getSessionResponse_workGroup' - The workgroup to which the session belongs.
--
-- 'httpStatus', 'getSessionResponse_httpStatus' - The response's http status code.
newGetSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionResponse
newGetSessionResponse pHttpStatus_ =
  GetSessionResponse'
    { description = Prelude.Nothing,
      engineConfiguration = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      notebookVersion = Prelude.Nothing,
      sessionConfiguration = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      statistics = Prelude.Nothing,
      status = Prelude.Nothing,
      workGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session description.
getSessionResponse_description :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_description = Lens.lens (\GetSessionResponse' {description} -> description) (\s@GetSessionResponse' {} a -> s {description = a} :: GetSessionResponse)

-- | Contains engine configuration information like DPU usage.
getSessionResponse_engineConfiguration :: Lens.Lens' GetSessionResponse (Prelude.Maybe EngineConfiguration)
getSessionResponse_engineConfiguration = Lens.lens (\GetSessionResponse' {engineConfiguration} -> engineConfiguration) (\s@GetSessionResponse' {} a -> s {engineConfiguration = a} :: GetSessionResponse)

-- | The engine version used by the session (for example,
-- @PySpark engine version 3@). You can get a list of engine versions by
-- calling ListEngineVersions.
getSessionResponse_engineVersion :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_engineVersion = Lens.lens (\GetSessionResponse' {engineVersion} -> engineVersion) (\s@GetSessionResponse' {} a -> s {engineVersion = a} :: GetSessionResponse)

-- | The notebook version.
getSessionResponse_notebookVersion :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_notebookVersion = Lens.lens (\GetSessionResponse' {notebookVersion} -> notebookVersion) (\s@GetSessionResponse' {} a -> s {notebookVersion = a} :: GetSessionResponse)

-- | Contains the workgroup configuration information used by the session.
getSessionResponse_sessionConfiguration :: Lens.Lens' GetSessionResponse (Prelude.Maybe SessionConfiguration)
getSessionResponse_sessionConfiguration = Lens.lens (\GetSessionResponse' {sessionConfiguration} -> sessionConfiguration) (\s@GetSessionResponse' {} a -> s {sessionConfiguration = a} :: GetSessionResponse)

-- | The session ID.
getSessionResponse_sessionId :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_sessionId = Lens.lens (\GetSessionResponse' {sessionId} -> sessionId) (\s@GetSessionResponse' {} a -> s {sessionId = a} :: GetSessionResponse)

-- | Contains the DPU execution time.
getSessionResponse_statistics :: Lens.Lens' GetSessionResponse (Prelude.Maybe SessionStatistics)
getSessionResponse_statistics = Lens.lens (\GetSessionResponse' {statistics} -> statistics) (\s@GetSessionResponse' {} a -> s {statistics = a} :: GetSessionResponse)

-- | Contains information about the status of the session.
getSessionResponse_status :: Lens.Lens' GetSessionResponse (Prelude.Maybe SessionStatus)
getSessionResponse_status = Lens.lens (\GetSessionResponse' {status} -> status) (\s@GetSessionResponse' {} a -> s {status = a} :: GetSessionResponse)

-- | The workgroup to which the session belongs.
getSessionResponse_workGroup :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_workGroup = Lens.lens (\GetSessionResponse' {workGroup} -> workGroup) (\s@GetSessionResponse' {} a -> s {workGroup = a} :: GetSessionResponse)

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Prelude.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Prelude.NFData GetSessionResponse where
  rnf GetSessionResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf engineConfiguration
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf notebookVersion
      `Prelude.seq` Prelude.rnf sessionConfiguration
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf httpStatus
