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
-- Module      : Amazonka.IoT.DescribeJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeJob>
-- action.
module Amazonka.IoT.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_jobId,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_documentSource,
    describeJobResponse_job,
    describeJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeJob_jobId' - The unique identifier you assigned to this job when it was created.
newDescribeJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeJob
newDescribeJob pJobId_ =
  DescribeJob' {jobId = pJobId_}

-- | The unique identifier you assigned to this job when it was created.
describeJob_jobId :: Lens.Lens' DescribeJob Prelude.Text
describeJob_jobId = Lens.lens (\DescribeJob' {jobId} -> jobId) (\s@DescribeJob' {} a -> s {jobId = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = DescribeJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Prelude.<$> (x Data..?> "documentSource")
            Prelude.<*> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJob where
  hashWithSalt _salt DescribeJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeJob where
  rnf DescribeJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DescribeJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Prelude.mconcat ["/jobs/", Data.toBS jobId]

instance Data.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | An S3 link to the job document.
    documentSource :: Prelude.Maybe Prelude.Text,
    -- | Information about the job.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentSource', 'describeJobResponse_documentSource' - An S3 link to the job document.
--
-- 'job', 'describeJobResponse_job' - Information about the job.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ =
  DescribeJobResponse'
    { documentSource =
        Prelude.Nothing,
      job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An S3 link to the job document.
describeJobResponse_documentSource :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_documentSource = Lens.lens (\DescribeJobResponse' {documentSource} -> documentSource) (\s@DescribeJobResponse' {} a -> s {documentSource = a} :: DescribeJobResponse)

-- | Information about the job.
describeJobResponse_job :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Job)
describeJobResponse_job = Lens.lens (\DescribeJobResponse' {job} -> job) (\s@DescribeJobResponse' {} a -> s {job = a} :: DescribeJobResponse)

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Prelude.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

instance Prelude.NFData DescribeJobResponse where
  rnf DescribeJobResponse' {..} =
    Prelude.rnf documentSource
      `Prelude.seq` Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
