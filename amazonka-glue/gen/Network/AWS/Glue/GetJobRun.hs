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
-- Module      : Network.AWS.Glue.GetJobRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given job run.
module Network.AWS.Glue.GetJobRun
  ( -- * Creating a Request
    GetJobRun (..),
    newGetJobRun,

    -- * Request Lenses
    getJobRun_predecessorsIncluded,
    getJobRun_jobName,
    getJobRun_runId,

    -- * Destructuring the Response
    GetJobRunResponse (..),
    newGetJobRunResponse,

    -- * Response Lenses
    getJobRunResponse_jobRun,
    getJobRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobRun' smart constructor.
data GetJobRun = GetJobRun'
  { -- | True if a list of predecessor runs should be returned.
    predecessorsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | Name of the job definition being run.
    jobName :: Prelude.Text,
    -- | The ID of the job run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predecessorsIncluded', 'getJobRun_predecessorsIncluded' - True if a list of predecessor runs should be returned.
--
-- 'jobName', 'getJobRun_jobName' - Name of the job definition being run.
--
-- 'runId', 'getJobRun_runId' - The ID of the job run.
newGetJobRun ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  GetJobRun
newGetJobRun pJobName_ pRunId_ =
  GetJobRun'
    { predecessorsIncluded = Prelude.Nothing,
      jobName = pJobName_,
      runId = pRunId_
    }

-- | True if a list of predecessor runs should be returned.
getJobRun_predecessorsIncluded :: Lens.Lens' GetJobRun (Prelude.Maybe Prelude.Bool)
getJobRun_predecessorsIncluded = Lens.lens (\GetJobRun' {predecessorsIncluded} -> predecessorsIncluded) (\s@GetJobRun' {} a -> s {predecessorsIncluded = a} :: GetJobRun)

-- | Name of the job definition being run.
getJobRun_jobName :: Lens.Lens' GetJobRun Prelude.Text
getJobRun_jobName = Lens.lens (\GetJobRun' {jobName} -> jobName) (\s@GetJobRun' {} a -> s {jobName = a} :: GetJobRun)

-- | The ID of the job run.
getJobRun_runId :: Lens.Lens' GetJobRun Prelude.Text
getJobRun_runId = Lens.lens (\GetJobRun' {runId} -> runId) (\s@GetJobRun' {} a -> s {runId = a} :: GetJobRun)

instance Core.AWSRequest GetJobRun where
  type AWSResponse GetJobRun = GetJobRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunResponse'
            Prelude.<$> (x Core..?> "JobRun")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobRun

instance Prelude.NFData GetJobRun

instance Core.ToHeaders GetJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetJobRun" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetJobRun where
  toJSON GetJobRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PredecessorsIncluded" Core..=)
              Prelude.<$> predecessorsIncluded,
            Prelude.Just ("JobName" Core..= jobName),
            Prelude.Just ("RunId" Core..= runId)
          ]
      )

instance Core.ToPath GetJobRun where
  toPath = Prelude.const "/"

instance Core.ToQuery GetJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobRunResponse' smart constructor.
data GetJobRunResponse = GetJobRunResponse'
  { -- | The requested job-run metadata.
    jobRun :: Prelude.Maybe JobRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRun', 'getJobRunResponse_jobRun' - The requested job-run metadata.
--
-- 'httpStatus', 'getJobRunResponse_httpStatus' - The response's http status code.
newGetJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobRunResponse
newGetJobRunResponse pHttpStatus_ =
  GetJobRunResponse'
    { jobRun = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested job-run metadata.
getJobRunResponse_jobRun :: Lens.Lens' GetJobRunResponse (Prelude.Maybe JobRun)
getJobRunResponse_jobRun = Lens.lens (\GetJobRunResponse' {jobRun} -> jobRun) (\s@GetJobRunResponse' {} a -> s {jobRun = a} :: GetJobRunResponse)

-- | The response's http status code.
getJobRunResponse_httpStatus :: Lens.Lens' GetJobRunResponse Prelude.Int
getJobRunResponse_httpStatus = Lens.lens (\GetJobRunResponse' {httpStatus} -> httpStatus) (\s@GetJobRunResponse' {} a -> s {httpStatus = a} :: GetJobRunResponse)

instance Prelude.NFData GetJobRunResponse
