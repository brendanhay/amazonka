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
-- Module      : Amazonka.Amplify.StopJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a job that is in progress for a branch of an Amplify app.
module Amazonka.Amplify.StopJob
  ( -- * Creating a Request
    StopJob (..),
    newStopJob,

    -- * Request Lenses
    stopJob_appId,
    stopJob_branchName,
    stopJob_jobId,

    -- * Destructuring the Response
    StopJobResponse (..),
    newStopJobResponse,

    -- * Response Lenses
    stopJobResponse_httpStatus,
    stopJobResponse_jobSummary,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the stop job request.
--
-- /See:/ 'newStopJob' smart constructor.
data StopJob = StopJob'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch, for the job.
    branchName :: Prelude.Text,
    -- | The unique id for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'stopJob_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'stopJob_branchName' - The name for the branch, for the job.
--
-- 'jobId', 'stopJob_jobId' - The unique id for the job.
newStopJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  StopJob
newStopJob pAppId_ pBranchName_ pJobId_ =
  StopJob'
    { appId = pAppId_,
      branchName = pBranchName_,
      jobId = pJobId_
    }

-- | The unique ID for an Amplify app.
stopJob_appId :: Lens.Lens' StopJob Prelude.Text
stopJob_appId = Lens.lens (\StopJob' {appId} -> appId) (\s@StopJob' {} a -> s {appId = a} :: StopJob)

-- | The name for the branch, for the job.
stopJob_branchName :: Lens.Lens' StopJob Prelude.Text
stopJob_branchName = Lens.lens (\StopJob' {branchName} -> branchName) (\s@StopJob' {} a -> s {branchName = a} :: StopJob)

-- | The unique id for the job.
stopJob_jobId :: Lens.Lens' StopJob Prelude.Text
stopJob_jobId = Lens.lens (\StopJob' {jobId} -> jobId) (\s@StopJob' {} a -> s {jobId = a} :: StopJob)

instance Core.AWSRequest StopJob where
  type AWSResponse StopJob = StopJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobSummary")
      )

instance Prelude.Hashable StopJob where
  hashWithSalt _salt StopJob' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopJob where
  rnf StopJob' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders StopJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath StopJob where
  toPath StopJob' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName,
        "/jobs/",
        Data.toBS jobId,
        "/stop"
      ]

instance Data.ToQuery StopJob where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the stop job request.
--
-- /See:/ 'newStopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary for the job.
    jobSummary :: JobSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopJobResponse_httpStatus' - The response's http status code.
--
-- 'jobSummary', 'stopJobResponse_jobSummary' - The summary for the job.
newStopJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobSummary'
  JobSummary ->
  StopJobResponse
newStopJobResponse pHttpStatus_ pJobSummary_ =
  StopJobResponse'
    { httpStatus = pHttpStatus_,
      jobSummary = pJobSummary_
    }

-- | The response's http status code.
stopJobResponse_httpStatus :: Lens.Lens' StopJobResponse Prelude.Int
stopJobResponse_httpStatus = Lens.lens (\StopJobResponse' {httpStatus} -> httpStatus) (\s@StopJobResponse' {} a -> s {httpStatus = a} :: StopJobResponse)

-- | The summary for the job.
stopJobResponse_jobSummary :: Lens.Lens' StopJobResponse JobSummary
stopJobResponse_jobSummary = Lens.lens (\StopJobResponse' {jobSummary} -> jobSummary) (\s@StopJobResponse' {} a -> s {jobSummary = a} :: StopJobResponse)

instance Prelude.NFData StopJobResponse where
  rnf StopJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobSummary
