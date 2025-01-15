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
-- Module      : Amazonka.Glue.GetJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an existing job definition.
module Amazonka.Glue.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_jobName,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_job,
    getJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | The name of the job definition to retrieve.
    jobName :: Prelude.Text
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
-- 'jobName', 'getJob_jobName' - The name of the job definition to retrieve.
newGetJob ::
  -- | 'jobName'
  Prelude.Text ->
  GetJob
newGetJob pJobName_ = GetJob' {jobName = pJobName_}

-- | The name of the job definition to retrieve.
getJob_jobName :: Lens.Lens' GetJob Prelude.Text
getJob_jobName = Lens.lens (\GetJob' {jobName} -> jobName) (\s@GetJob' {} a -> s {jobName = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Prelude.<$> (x Data..?> "Job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJob where
  hashWithSalt _salt GetJob' {..} =
    _salt `Prelude.hashWithSalt` jobName

instance Prelude.NFData GetJob where
  rnf GetJob' {..} = Prelude.rnf jobName

instance Data.ToHeaders GetJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetJob" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJob where
  toJSON GetJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobName" Data..= jobName)]
      )

instance Data.ToPath GetJob where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | The requested job definition.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'getJobResponse_job' - The requested job definition.
--
-- 'httpStatus', 'getJobResponse_httpStatus' - The response's http status code.
newGetJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobResponse
newGetJobResponse pHttpStatus_ =
  GetJobResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested job definition.
getJobResponse_job :: Lens.Lens' GetJobResponse (Prelude.Maybe Job)
getJobResponse_job = Lens.lens (\GetJobResponse' {job} -> job) (\s@GetJobResponse' {} a -> s {job = a} :: GetJobResponse)

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Prelude.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

instance Prelude.NFData GetJobResponse where
  rnf GetJobResponse' {..} =
    Prelude.rnf job `Prelude.seq`
      Prelude.rnf httpStatus
