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
-- Module      : Amazonka.Amplify.DeleteJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job for a branch of an Amplify app.
module Amazonka.Amplify.DeleteJob
  ( -- * Creating a Request
    DeleteJob (..),
    newDeleteJob,

    -- * Request Lenses
    deleteJob_appId,
    deleteJob_branchName,
    deleteJob_jobId,

    -- * Destructuring the Response
    DeleteJobResponse (..),
    newDeleteJobResponse,

    -- * Response Lenses
    deleteJobResponse_httpStatus,
    deleteJobResponse_jobSummary,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the delete job request.
--
-- /See:/ 'newDeleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch, for the job.
    branchName :: Prelude.Text,
    -- | The unique ID for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteJob_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'deleteJob_branchName' - The name for the branch, for the job.
--
-- 'jobId', 'deleteJob_jobId' - The unique ID for the job.
newDeleteJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DeleteJob
newDeleteJob pAppId_ pBranchName_ pJobId_ =
  DeleteJob'
    { appId = pAppId_,
      branchName = pBranchName_,
      jobId = pJobId_
    }

-- | The unique ID for an Amplify app.
deleteJob_appId :: Lens.Lens' DeleteJob Prelude.Text
deleteJob_appId = Lens.lens (\DeleteJob' {appId} -> appId) (\s@DeleteJob' {} a -> s {appId = a} :: DeleteJob)

-- | The name for the branch, for the job.
deleteJob_branchName :: Lens.Lens' DeleteJob Prelude.Text
deleteJob_branchName = Lens.lens (\DeleteJob' {branchName} -> branchName) (\s@DeleteJob' {} a -> s {branchName = a} :: DeleteJob)

-- | The unique ID for the job.
deleteJob_jobId :: Lens.Lens' DeleteJob Prelude.Text
deleteJob_jobId = Lens.lens (\DeleteJob' {jobId} -> jobId) (\s@DeleteJob' {} a -> s {jobId = a} :: DeleteJob)

instance Core.AWSRequest DeleteJob where
  type AWSResponse DeleteJob = DeleteJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobSummary")
      )

instance Prelude.Hashable DeleteJob where
  hashWithSalt _salt DeleteJob' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DeleteJob where
  rnf DeleteJob' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DeleteJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteJob where
  toPath DeleteJob' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName,
        "/jobs/",
        Data.toBS jobId
      ]

instance Data.ToQuery DeleteJob where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the delete job request.
--
-- /See:/ 'newDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    jobSummary :: JobSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteJobResponse_httpStatus' - The response's http status code.
--
-- 'jobSummary', 'deleteJobResponse_jobSummary' - Undocumented member.
newDeleteJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobSummary'
  JobSummary ->
  DeleteJobResponse
newDeleteJobResponse pHttpStatus_ pJobSummary_ =
  DeleteJobResponse'
    { httpStatus = pHttpStatus_,
      jobSummary = pJobSummary_
    }

-- | The response's http status code.
deleteJobResponse_httpStatus :: Lens.Lens' DeleteJobResponse Prelude.Int
deleteJobResponse_httpStatus = Lens.lens (\DeleteJobResponse' {httpStatus} -> httpStatus) (\s@DeleteJobResponse' {} a -> s {httpStatus = a} :: DeleteJobResponse)

-- | Undocumented member.
deleteJobResponse_jobSummary :: Lens.Lens' DeleteJobResponse JobSummary
deleteJobResponse_jobSummary = Lens.lens (\DeleteJobResponse' {jobSummary} -> jobSummary) (\s@DeleteJobResponse' {} a -> s {jobSummary = a} :: DeleteJobResponse)

instance Prelude.NFData DeleteJobResponse where
  rnf DeleteJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobSummary
