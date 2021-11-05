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
-- Module      : Network.AWS.Amplify.GetJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a job for a branch of an Amplify app.
module Network.AWS.Amplify.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_appId,
    getJob_branchName,
    getJob_jobId,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_httpStatus,
    getJobResponse_job,
  )
where

import Network.AWS.Amplify.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request structure for the get job request.
--
-- /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The branch name for the job.
    branchName :: Prelude.Text,
    -- | The unique ID for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getJob_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'getJob_branchName' - The branch name for the job.
--
-- 'jobId', 'getJob_jobId' - The unique ID for the job.
newGetJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetJob
newGetJob pAppId_ pBranchName_ pJobId_ =
  GetJob'
    { appId = pAppId_,
      branchName = pBranchName_,
      jobId = pJobId_
    }

-- | The unique ID for an Amplify app.
getJob_appId :: Lens.Lens' GetJob Prelude.Text
getJob_appId = Lens.lens (\GetJob' {appId} -> appId) (\s@GetJob' {} a -> s {appId = a} :: GetJob)

-- | The branch name for the job.
getJob_branchName :: Lens.Lens' GetJob Prelude.Text
getJob_branchName = Lens.lens (\GetJob' {branchName} -> branchName) (\s@GetJob' {} a -> s {branchName = a} :: GetJob)

-- | The unique ID for the job.
getJob_jobId :: Lens.Lens' GetJob Prelude.Text
getJob_jobId = Lens.lens (\GetJob' {jobId} -> jobId) (\s@GetJob' {} a -> s {jobId = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "job")
      )

instance Prelude.Hashable GetJob

instance Prelude.NFData GetJob

instance Core.ToHeaders GetJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetJob where
  toPath GetJob' {..} =
    Prelude.mconcat
      [ "/apps/",
        Core.toBS appId,
        "/branches/",
        Core.toBS branchName,
        "/jobs/",
        Core.toBS jobId
      ]

instance Core.ToQuery GetJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    job :: Job
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJobResponse_httpStatus' - The response's http status code.
--
-- 'job', 'getJobResponse_job' - Undocumented member.
newGetJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'job'
  Job ->
  GetJobResponse
newGetJobResponse pHttpStatus_ pJob_ =
  GetJobResponse'
    { httpStatus = pHttpStatus_,
      job = pJob_
    }

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Prelude.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

-- | Undocumented member.
getJobResponse_job :: Lens.Lens' GetJobResponse Job
getJobResponse_job = Lens.lens (\GetJobResponse' {job} -> job) (\s@GetJobResponse' {} a -> s {job = a} :: GetJobResponse)

instance Prelude.NFData GetJobResponse
