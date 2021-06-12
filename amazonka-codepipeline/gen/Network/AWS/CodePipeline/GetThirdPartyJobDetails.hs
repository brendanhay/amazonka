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
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Used for partner
-- actions only.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the S3 bucket used to store artifacts for the pipeline, if the
-- action requires access to that S3 bucket for input or output artifacts.
-- This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
  ( -- * Creating a Request
    GetThirdPartyJobDetails (..),
    newGetThirdPartyJobDetails,

    -- * Request Lenses
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,

    -- * Destructuring the Response
    GetThirdPartyJobDetailsResponse (..),
    newGetThirdPartyJobDetailsResponse,

    -- * Response Lenses
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'newGetThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { -- | The unique system-generated ID used for identifying the job.
    jobId :: Core.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to
    -- verify that the calling entity is allowed access to the job and its
    -- details.
    clientToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetThirdPartyJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getThirdPartyJobDetails_jobId' - The unique system-generated ID used for identifying the job.
--
-- 'clientToken', 'getThirdPartyJobDetails_clientToken' - The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
newGetThirdPartyJobDetails ::
  -- | 'jobId'
  Core.Text ->
  -- | 'clientToken'
  Core.Text ->
  GetThirdPartyJobDetails
newGetThirdPartyJobDetails pJobId_ pClientToken_ =
  GetThirdPartyJobDetails'
    { jobId = pJobId_,
      clientToken = pClientToken_
    }

-- | The unique system-generated ID used for identifying the job.
getThirdPartyJobDetails_jobId :: Lens.Lens' GetThirdPartyJobDetails Core.Text
getThirdPartyJobDetails_jobId = Lens.lens (\GetThirdPartyJobDetails' {jobId} -> jobId) (\s@GetThirdPartyJobDetails' {} a -> s {jobId = a} :: GetThirdPartyJobDetails)

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
getThirdPartyJobDetails_clientToken :: Lens.Lens' GetThirdPartyJobDetails Core.Text
getThirdPartyJobDetails_clientToken = Lens.lens (\GetThirdPartyJobDetails' {clientToken} -> clientToken) (\s@GetThirdPartyJobDetails' {} a -> s {clientToken = a} :: GetThirdPartyJobDetails)

instance Core.AWSRequest GetThirdPartyJobDetails where
  type
    AWSResponse GetThirdPartyJobDetails =
      GetThirdPartyJobDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThirdPartyJobDetailsResponse'
            Core.<$> (x Core..?> "jobDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetThirdPartyJobDetails

instance Core.NFData GetThirdPartyJobDetails

instance Core.ToHeaders GetThirdPartyJobDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetThirdPartyJobDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetThirdPartyJobDetails where
  toJSON GetThirdPartyJobDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath GetThirdPartyJobDetails where
  toPath = Core.const "/"

instance Core.ToQuery GetThirdPartyJobDetails where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'newGetThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { -- | The details of the job, including any protected values defined for the
    -- job.
    jobDetails :: Core.Maybe ThirdPartyJobDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetThirdPartyJobDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDetails', 'getThirdPartyJobDetailsResponse_jobDetails' - The details of the job, including any protected values defined for the
-- job.
--
-- 'httpStatus', 'getThirdPartyJobDetailsResponse_httpStatus' - The response's http status code.
newGetThirdPartyJobDetailsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetThirdPartyJobDetailsResponse
newGetThirdPartyJobDetailsResponse pHttpStatus_ =
  GetThirdPartyJobDetailsResponse'
    { jobDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the job, including any protected values defined for the
-- job.
getThirdPartyJobDetailsResponse_jobDetails :: Lens.Lens' GetThirdPartyJobDetailsResponse (Core.Maybe ThirdPartyJobDetails)
getThirdPartyJobDetailsResponse_jobDetails = Lens.lens (\GetThirdPartyJobDetailsResponse' {jobDetails} -> jobDetails) (\s@GetThirdPartyJobDetailsResponse' {} a -> s {jobDetails = a} :: GetThirdPartyJobDetailsResponse)

-- | The response's http status code.
getThirdPartyJobDetailsResponse_httpStatus :: Lens.Lens' GetThirdPartyJobDetailsResponse Core.Int
getThirdPartyJobDetailsResponse_httpStatus = Lens.lens (\GetThirdPartyJobDetailsResponse' {httpStatus} -> httpStatus) (\s@GetThirdPartyJobDetailsResponse' {} a -> s {httpStatus = a} :: GetThirdPartyJobDetailsResponse)

instance Core.NFData GetThirdPartyJobDetailsResponse
