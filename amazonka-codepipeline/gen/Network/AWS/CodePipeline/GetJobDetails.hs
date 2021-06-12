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
-- Module      : Network.AWS.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job. Used for custom actions only.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the S3 bucket used to store artifacts for the pipeline, if the
-- action requires access to that S3 bucket for input or output artifacts.
-- This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetJobDetails
  ( -- * Creating a Request
    GetJobDetails (..),
    newGetJobDetails,

    -- * Request Lenses
    getJobDetails_jobId,

    -- * Destructuring the Response
    GetJobDetailsResponse (..),
    newGetJobDetailsResponse,

    -- * Response Lenses
    getJobDetailsResponse_jobDetails,
    getJobDetailsResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetJobDetails@ action.
--
-- /See:/ 'newGetJobDetails' smart constructor.
data GetJobDetails = GetJobDetails'
  { -- | The unique system-generated ID for the job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getJobDetails_jobId' - The unique system-generated ID for the job.
newGetJobDetails ::
  -- | 'jobId'
  Core.Text ->
  GetJobDetails
newGetJobDetails pJobId_ =
  GetJobDetails' {jobId = pJobId_}

-- | The unique system-generated ID for the job.
getJobDetails_jobId :: Lens.Lens' GetJobDetails Core.Text
getJobDetails_jobId = Lens.lens (\GetJobDetails' {jobId} -> jobId) (\s@GetJobDetails' {} a -> s {jobId = a} :: GetJobDetails)

instance Core.AWSRequest GetJobDetails where
  type
    AWSResponse GetJobDetails =
      GetJobDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobDetailsResponse'
            Core.<$> (x Core..?> "jobDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobDetails

instance Core.NFData GetJobDetails

instance Core.ToHeaders GetJobDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetJobDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJobDetails where
  toJSON GetJobDetails' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("jobId" Core..= jobId)])

instance Core.ToPath GetJobDetails where
  toPath = Core.const "/"

instance Core.ToQuery GetJobDetails where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetJobDetails@ action.
--
-- /See:/ 'newGetJobDetailsResponse' smart constructor.
data GetJobDetailsResponse = GetJobDetailsResponse'
  { -- | The details of the job.
    --
    -- If AWSSessionCredentials is used, a long-running job can call
    -- @GetJobDetails@ again to obtain new credentials.
    jobDetails :: Core.Maybe JobDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDetails', 'getJobDetailsResponse_jobDetails' - The details of the job.
--
-- If AWSSessionCredentials is used, a long-running job can call
-- @GetJobDetails@ again to obtain new credentials.
--
-- 'httpStatus', 'getJobDetailsResponse_httpStatus' - The response's http status code.
newGetJobDetailsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetJobDetailsResponse
newGetJobDetailsResponse pHttpStatus_ =
  GetJobDetailsResponse'
    { jobDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the job.
--
-- If AWSSessionCredentials is used, a long-running job can call
-- @GetJobDetails@ again to obtain new credentials.
getJobDetailsResponse_jobDetails :: Lens.Lens' GetJobDetailsResponse (Core.Maybe JobDetails)
getJobDetailsResponse_jobDetails = Lens.lens (\GetJobDetailsResponse' {jobDetails} -> jobDetails) (\s@GetJobDetailsResponse' {} a -> s {jobDetails = a} :: GetJobDetailsResponse)

-- | The response's http status code.
getJobDetailsResponse_httpStatus :: Lens.Lens' GetJobDetailsResponse Core.Int
getJobDetailsResponse_httpStatus = Lens.lens (\GetJobDetailsResponse' {httpStatus} -> httpStatus) (\s@GetJobDetailsResponse' {} a -> s {httpStatus = a} :: GetJobDetailsResponse)

instance Core.NFData GetJobDetailsResponse
