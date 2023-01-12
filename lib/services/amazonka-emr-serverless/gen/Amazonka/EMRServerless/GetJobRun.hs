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
-- Module      : Amazonka.EMRServerless.GetJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays detailed information about a job run.
module Amazonka.EMRServerless.GetJobRun
  ( -- * Creating a Request
    GetJobRun (..),
    newGetJobRun,

    -- * Request Lenses
    getJobRun_applicationId,
    getJobRun_jobRunId,

    -- * Destructuring the Response
    GetJobRunResponse (..),
    newGetJobRunResponse,

    -- * Response Lenses
    getJobRunResponse_httpStatus,
    getJobRunResponse_jobRun,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJobRun' smart constructor.
data GetJobRun = GetJobRun'
  { -- | The ID of the application on which the job run is submitted.
    applicationId :: Prelude.Text,
    -- | The ID of the job run.
    jobRunId :: Prelude.Text
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
-- 'applicationId', 'getJobRun_applicationId' - The ID of the application on which the job run is submitted.
--
-- 'jobRunId', 'getJobRun_jobRunId' - The ID of the job run.
newGetJobRun ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  GetJobRun
newGetJobRun pApplicationId_ pJobRunId_ =
  GetJobRun'
    { applicationId = pApplicationId_,
      jobRunId = pJobRunId_
    }

-- | The ID of the application on which the job run is submitted.
getJobRun_applicationId :: Lens.Lens' GetJobRun Prelude.Text
getJobRun_applicationId = Lens.lens (\GetJobRun' {applicationId} -> applicationId) (\s@GetJobRun' {} a -> s {applicationId = a} :: GetJobRun)

-- | The ID of the job run.
getJobRun_jobRunId :: Lens.Lens' GetJobRun Prelude.Text
getJobRun_jobRunId = Lens.lens (\GetJobRun' {jobRunId} -> jobRunId) (\s@GetJobRun' {} a -> s {jobRunId = a} :: GetJobRun)

instance Core.AWSRequest GetJobRun where
  type AWSResponse GetJobRun = GetJobRunResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobRun")
      )

instance Prelude.Hashable GetJobRun where
  hashWithSalt _salt GetJobRun' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobRunId

instance Prelude.NFData GetJobRun where
  rnf GetJobRun' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobRunId

instance Data.ToHeaders GetJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJobRun where
  toPath GetJobRun' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/jobruns/",
        Data.toBS jobRunId
      ]

instance Data.ToQuery GetJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobRunResponse' smart constructor.
data GetJobRunResponse = GetJobRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output displays information about the job run.
    jobRun :: JobRun
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJobRunResponse_httpStatus' - The response's http status code.
--
-- 'jobRun', 'getJobRunResponse_jobRun' - The output displays information about the job run.
newGetJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobRun'
  JobRun ->
  GetJobRunResponse
newGetJobRunResponse pHttpStatus_ pJobRun_ =
  GetJobRunResponse'
    { httpStatus = pHttpStatus_,
      jobRun = pJobRun_
    }

-- | The response's http status code.
getJobRunResponse_httpStatus :: Lens.Lens' GetJobRunResponse Prelude.Int
getJobRunResponse_httpStatus = Lens.lens (\GetJobRunResponse' {httpStatus} -> httpStatus) (\s@GetJobRunResponse' {} a -> s {httpStatus = a} :: GetJobRunResponse)

-- | The output displays information about the job run.
getJobRunResponse_jobRun :: Lens.Lens' GetJobRunResponse JobRun
getJobRunResponse_jobRun = Lens.lens (\GetJobRunResponse' {jobRun} -> jobRun) (\s@GetJobRunResponse' {} a -> s {jobRun = a} :: GetJobRunResponse)

instance Prelude.NFData GetJobRunResponse where
  rnf GetJobRunResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobRun
