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
-- Module      : Amazonka.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodePipeline.GetJobDetails
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

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetJobDetails@ action.
--
-- /See:/ 'newGetJobDetails' smart constructor.
data GetJobDetails = GetJobDetails'
  { -- | The unique system-generated ID for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetJobDetails
newGetJobDetails pJobId_ =
  GetJobDetails' {jobId = pJobId_}

-- | The unique system-generated ID for the job.
getJobDetails_jobId :: Lens.Lens' GetJobDetails Prelude.Text
getJobDetails_jobId = Lens.lens (\GetJobDetails' {jobId} -> jobId) (\s@GetJobDetails' {} a -> s {jobId = a} :: GetJobDetails)

instance Core.AWSRequest GetJobDetails where
  type
    AWSResponse GetJobDetails =
      GetJobDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobDetailsResponse'
            Prelude.<$> (x Data..?> "jobDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobDetails where
  hashWithSalt _salt GetJobDetails' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetJobDetails where
  rnf GetJobDetails' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetJobDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.GetJobDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJobDetails where
  toJSON GetJobDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobId" Data..= jobId)]
      )

instance Data.ToPath GetJobDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobDetails where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetJobDetails@ action.
--
-- /See:/ 'newGetJobDetailsResponse' smart constructor.
data GetJobDetailsResponse = GetJobDetailsResponse'
  { -- | The details of the job.
    --
    -- If AWSSessionCredentials is used, a long-running job can call
    -- @GetJobDetails@ again to obtain new credentials.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetJobDetailsResponse
newGetJobDetailsResponse pHttpStatus_ =
  GetJobDetailsResponse'
    { jobDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the job.
--
-- If AWSSessionCredentials is used, a long-running job can call
-- @GetJobDetails@ again to obtain new credentials.
getJobDetailsResponse_jobDetails :: Lens.Lens' GetJobDetailsResponse (Prelude.Maybe JobDetails)
getJobDetailsResponse_jobDetails = Lens.lens (\GetJobDetailsResponse' {jobDetails} -> jobDetails) (\s@GetJobDetailsResponse' {} a -> s {jobDetails = a} :: GetJobDetailsResponse)

-- | The response's http status code.
getJobDetailsResponse_httpStatus :: Lens.Lens' GetJobDetailsResponse Prelude.Int
getJobDetailsResponse_httpStatus = Lens.lens (\GetJobDetailsResponse' {httpStatus} -> httpStatus) (\s@GetJobDetailsResponse' {} a -> s {httpStatus = a} :: GetJobDetailsResponse)

instance Prelude.NFData GetJobDetailsResponse where
  rnf GetJobDetailsResponse' {..} =
    Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf httpStatus
