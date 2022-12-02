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
-- Module      : Amazonka.Translate.StopTextTranslationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an asynchronous batch translation job that is in progress.
--
-- If the job\'s state is @IN_PROGRESS@, the job will be marked for
-- termination and put into the @STOP_REQUESTED@ state. If the job
-- completes before it can be stopped, it is put into the @COMPLETED@
-- state. Otherwise, the job is put into the @STOPPED@ state.
--
-- Asynchronous batch translation jobs are started with the
-- StartTextTranslationJob operation. You can use the
-- DescribeTextTranslationJob or ListTextTranslationJobs operations to get
-- a batch translation job\'s @JobId@.
module Amazonka.Translate.StopTextTranslationJob
  ( -- * Creating a Request
    StopTextTranslationJob (..),
    newStopTextTranslationJob,

    -- * Request Lenses
    stopTextTranslationJob_jobId,

    -- * Destructuring the Response
    StopTextTranslationJobResponse (..),
    newStopTextTranslationJobResponse,

    -- * Response Lenses
    stopTextTranslationJobResponse_jobStatus,
    stopTextTranslationJobResponse_jobId,
    stopTextTranslationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newStopTextTranslationJob' smart constructor.
data StopTextTranslationJob = StopTextTranslationJob'
  { -- | The job ID of the job to be stopped.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTextTranslationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopTextTranslationJob_jobId' - The job ID of the job to be stopped.
newStopTextTranslationJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopTextTranslationJob
newStopTextTranslationJob pJobId_ =
  StopTextTranslationJob' {jobId = pJobId_}

-- | The job ID of the job to be stopped.
stopTextTranslationJob_jobId :: Lens.Lens' StopTextTranslationJob Prelude.Text
stopTextTranslationJob_jobId = Lens.lens (\StopTextTranslationJob' {jobId} -> jobId) (\s@StopTextTranslationJob' {} a -> s {jobId = a} :: StopTextTranslationJob)

instance Core.AWSRequest StopTextTranslationJob where
  type
    AWSResponse StopTextTranslationJob =
      StopTextTranslationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTextTranslationJobResponse'
            Prelude.<$> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopTextTranslationJob where
  hashWithSalt _salt StopTextTranslationJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopTextTranslationJob where
  rnf StopTextTranslationJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopTextTranslationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.StopTextTranslationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopTextTranslationJob where
  toJSON StopTextTranslationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopTextTranslationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopTextTranslationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTextTranslationJobResponse' smart constructor.
data StopTextTranslationJobResponse = StopTextTranslationJobResponse'
  { -- | The status of the designated job. Upon successful completion, the job\'s
    -- status will be @STOPPED@.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The job ID of the stopped batch translation job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTextTranslationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopTextTranslationJobResponse_jobStatus' - The status of the designated job. Upon successful completion, the job\'s
-- status will be @STOPPED@.
--
-- 'jobId', 'stopTextTranslationJobResponse_jobId' - The job ID of the stopped batch translation job.
--
-- 'httpStatus', 'stopTextTranslationJobResponse_httpStatus' - The response's http status code.
newStopTextTranslationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopTextTranslationJobResponse
newStopTextTranslationJobResponse pHttpStatus_ =
  StopTextTranslationJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the designated job. Upon successful completion, the job\'s
-- status will be @STOPPED@.
stopTextTranslationJobResponse_jobStatus :: Lens.Lens' StopTextTranslationJobResponse (Prelude.Maybe JobStatus)
stopTextTranslationJobResponse_jobStatus = Lens.lens (\StopTextTranslationJobResponse' {jobStatus} -> jobStatus) (\s@StopTextTranslationJobResponse' {} a -> s {jobStatus = a} :: StopTextTranslationJobResponse)

-- | The job ID of the stopped batch translation job.
stopTextTranslationJobResponse_jobId :: Lens.Lens' StopTextTranslationJobResponse (Prelude.Maybe Prelude.Text)
stopTextTranslationJobResponse_jobId = Lens.lens (\StopTextTranslationJobResponse' {jobId} -> jobId) (\s@StopTextTranslationJobResponse' {} a -> s {jobId = a} :: StopTextTranslationJobResponse)

-- | The response's http status code.
stopTextTranslationJobResponse_httpStatus :: Lens.Lens' StopTextTranslationJobResponse Prelude.Int
stopTextTranslationJobResponse_httpStatus = Lens.lens (\StopTextTranslationJobResponse' {httpStatus} -> httpStatus) (\s@StopTextTranslationJobResponse' {} a -> s {httpStatus = a} :: StopTextTranslationJobResponse)

instance
  Prelude.NFData
    StopTextTranslationJobResponse
  where
  rnf StopTextTranslationJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
