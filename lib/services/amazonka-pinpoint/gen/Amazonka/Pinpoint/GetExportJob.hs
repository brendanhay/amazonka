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
-- Module      : Amazonka.Pinpoint.GetExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific export
-- job for an application.
module Amazonka.Pinpoint.GetExportJob
  ( -- * Creating a Request
    GetExportJob (..),
    newGetExportJob,

    -- * Request Lenses
    getExportJob_applicationId,
    getExportJob_jobId,

    -- * Destructuring the Response
    GetExportJobResponse (..),
    newGetExportJobResponse,

    -- * Response Lenses
    getExportJobResponse_httpStatus,
    getExportJobResponse_exportJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExportJob' smart constructor.
data GetExportJob = GetExportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getExportJob_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'jobId', 'getExportJob_jobId' - The unique identifier for the job.
newGetExportJob ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetExportJob
newGetExportJob pApplicationId_ pJobId_ =
  GetExportJob'
    { applicationId = pApplicationId_,
      jobId = pJobId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getExportJob_applicationId :: Lens.Lens' GetExportJob Prelude.Text
getExportJob_applicationId = Lens.lens (\GetExportJob' {applicationId} -> applicationId) (\s@GetExportJob' {} a -> s {applicationId = a} :: GetExportJob)

-- | The unique identifier for the job.
getExportJob_jobId :: Lens.Lens' GetExportJob Prelude.Text
getExportJob_jobId = Lens.lens (\GetExportJob' {jobId} -> jobId) (\s@GetExportJob' {} a -> s {jobId = a} :: GetExportJob)

instance Core.AWSRequest GetExportJob where
  type AWSResponse GetExportJob = GetExportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetExportJob where
  hashWithSalt _salt GetExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetExportJob where
  rnf GetExportJob' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExportJob where
  toPath GetExportJob' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/jobs/export/",
        Data.toBS jobId
      ]

instance Data.ToQuery GetExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExportJobResponse' smart constructor.
data GetExportJobResponse = GetExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    exportJobResponse :: ExportJobResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getExportJobResponse_httpStatus' - The response's http status code.
--
-- 'exportJobResponse', 'getExportJobResponse_exportJobResponse' - Undocumented member.
newGetExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportJobResponse'
  ExportJobResponse ->
  GetExportJobResponse
newGetExportJobResponse
  pHttpStatus_
  pExportJobResponse_ =
    GetExportJobResponse'
      { httpStatus = pHttpStatus_,
        exportJobResponse = pExportJobResponse_
      }

-- | The response's http status code.
getExportJobResponse_httpStatus :: Lens.Lens' GetExportJobResponse Prelude.Int
getExportJobResponse_httpStatus = Lens.lens (\GetExportJobResponse' {httpStatus} -> httpStatus) (\s@GetExportJobResponse' {} a -> s {httpStatus = a} :: GetExportJobResponse)

-- | Undocumented member.
getExportJobResponse_exportJobResponse :: Lens.Lens' GetExportJobResponse ExportJobResponse
getExportJobResponse_exportJobResponse = Lens.lens (\GetExportJobResponse' {exportJobResponse} -> exportJobResponse) (\s@GetExportJobResponse' {} a -> s {exportJobResponse = a} :: GetExportJobResponse)

instance Prelude.NFData GetExportJobResponse where
  rnf GetExportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportJobResponse
