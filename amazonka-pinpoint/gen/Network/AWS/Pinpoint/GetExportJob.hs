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
-- Module      : Network.AWS.Pinpoint.GetExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific export
-- job for an application.
module Network.AWS.Pinpoint.GetExportJob
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetExportJob' smart constructor.
data GetExportJob = GetExportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  GetExportJob
newGetExportJob pApplicationId_ pJobId_ =
  GetExportJob'
    { applicationId = pApplicationId_,
      jobId = pJobId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getExportJob_applicationId :: Lens.Lens' GetExportJob Core.Text
getExportJob_applicationId = Lens.lens (\GetExportJob' {applicationId} -> applicationId) (\s@GetExportJob' {} a -> s {applicationId = a} :: GetExportJob)

-- | The unique identifier for the job.
getExportJob_jobId :: Lens.Lens' GetExportJob Core.Text
getExportJob_jobId = Lens.lens (\GetExportJob' {jobId} -> jobId) (\s@GetExportJob' {} a -> s {jobId = a} :: GetExportJob)

instance Core.AWSRequest GetExportJob where
  type AWSResponse GetExportJob = GetExportJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetExportJob

instance Core.NFData GetExportJob

instance Core.ToHeaders GetExportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetExportJob where
  toPath GetExportJob' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/export/",
        Core.toBS jobId
      ]

instance Core.ToQuery GetExportJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetExportJobResponse' smart constructor.
data GetExportJobResponse = GetExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    exportJobResponse :: ExportJobResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
getExportJobResponse_httpStatus :: Lens.Lens' GetExportJobResponse Core.Int
getExportJobResponse_httpStatus = Lens.lens (\GetExportJobResponse' {httpStatus} -> httpStatus) (\s@GetExportJobResponse' {} a -> s {httpStatus = a} :: GetExportJobResponse)

-- | Undocumented member.
getExportJobResponse_exportJobResponse :: Lens.Lens' GetExportJobResponse ExportJobResponse
getExportJobResponse_exportJobResponse = Lens.lens (\GetExportJobResponse' {exportJobResponse} -> exportJobResponse) (\s@GetExportJobResponse' {} a -> s {exportJobResponse = a} :: GetExportJobResponse)

instance Core.NFData GetExportJobResponse
