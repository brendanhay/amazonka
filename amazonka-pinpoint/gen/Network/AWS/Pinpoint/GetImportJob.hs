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
-- Module      : Network.AWS.Pinpoint.GetImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific import
-- job for an application.
module Network.AWS.Pinpoint.GetImportJob
  ( -- * Creating a Request
    GetImportJob (..),
    newGetImportJob,

    -- * Request Lenses
    getImportJob_applicationId,
    getImportJob_jobId,

    -- * Destructuring the Response
    GetImportJobResponse (..),
    newGetImportJobResponse,

    -- * Response Lenses
    getImportJobResponse_httpStatus,
    getImportJobResponse_importJobResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetImportJob' smart constructor.
data GetImportJob = GetImportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getImportJob_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'jobId', 'getImportJob_jobId' - The unique identifier for the job.
newGetImportJob ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  GetImportJob
newGetImportJob pApplicationId_ pJobId_ =
  GetImportJob'
    { applicationId = pApplicationId_,
      jobId = pJobId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getImportJob_applicationId :: Lens.Lens' GetImportJob Core.Text
getImportJob_applicationId = Lens.lens (\GetImportJob' {applicationId} -> applicationId) (\s@GetImportJob' {} a -> s {applicationId = a} :: GetImportJob)

-- | The unique identifier for the job.
getImportJob_jobId :: Lens.Lens' GetImportJob Core.Text
getImportJob_jobId = Lens.lens (\GetImportJob' {jobId} -> jobId) (\s@GetImportJob' {} a -> s {jobId = a} :: GetImportJob)

instance Core.AWSRequest GetImportJob where
  type AWSResponse GetImportJob = GetImportJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetImportJob

instance Core.NFData GetImportJob

instance Core.ToHeaders GetImportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetImportJob where
  toPath GetImportJob' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/import/",
        Core.toBS jobId
      ]

instance Core.ToQuery GetImportJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetImportJobResponse' smart constructor.
data GetImportJobResponse = GetImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    importJobResponse :: ImportJobResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getImportJobResponse_httpStatus' - The response's http status code.
--
-- 'importJobResponse', 'getImportJobResponse_importJobResponse' - Undocumented member.
newGetImportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'importJobResponse'
  ImportJobResponse ->
  GetImportJobResponse
newGetImportJobResponse
  pHttpStatus_
  pImportJobResponse_ =
    GetImportJobResponse'
      { httpStatus = pHttpStatus_,
        importJobResponse = pImportJobResponse_
      }

-- | The response's http status code.
getImportJobResponse_httpStatus :: Lens.Lens' GetImportJobResponse Core.Int
getImportJobResponse_httpStatus = Lens.lens (\GetImportJobResponse' {httpStatus} -> httpStatus) (\s@GetImportJobResponse' {} a -> s {httpStatus = a} :: GetImportJobResponse)

-- | Undocumented member.
getImportJobResponse_importJobResponse :: Lens.Lens' GetImportJobResponse ImportJobResponse
getImportJobResponse_importJobResponse = Lens.lens (\GetImportJobResponse' {importJobResponse} -> importJobResponse) (\s@GetImportJobResponse' {} a -> s {importJobResponse = a} :: GetImportJobResponse)

instance Core.NFData GetImportJobResponse
