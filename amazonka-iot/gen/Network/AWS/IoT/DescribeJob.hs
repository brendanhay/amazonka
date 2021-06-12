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
-- Module      : Network.AWS.IoT.DescribeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job.
module Network.AWS.IoT.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_jobId,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_job,
    describeJobResponse_documentSource,
    describeJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeJob
newDescribeJob pJobId_ =
  DescribeJob' {jobId = pJobId_}

-- | The unique identifier you assigned to this job when it was created.
describeJob_jobId :: Lens.Lens' DescribeJob Core.Text
describeJob_jobId = Lens.lens (\DescribeJob' {jobId} -> jobId) (\s@DescribeJob' {} a -> s {jobId = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = DescribeJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Core.<$> (x Core..?> "job")
            Core.<*> (x Core..?> "documentSource")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJob

instance Core.NFData DescribeJob

instance Core.ToHeaders DescribeJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Core.mconcat ["/jobs/", Core.toBS jobId]

instance Core.ToQuery DescribeJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | Information about the job.
    job :: Core.Maybe Job,
    -- | An S3 link to the job document.
    documentSource :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'describeJobResponse_job' - Information about the job.
--
-- 'documentSource', 'describeJobResponse_documentSource' - An S3 link to the job document.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ =
  DescribeJobResponse'
    { job = Core.Nothing,
      documentSource = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the job.
describeJobResponse_job :: Lens.Lens' DescribeJobResponse (Core.Maybe Job)
describeJobResponse_job = Lens.lens (\DescribeJobResponse' {job} -> job) (\s@DescribeJobResponse' {} a -> s {job = a} :: DescribeJobResponse)

-- | An S3 link to the job document.
describeJobResponse_documentSource :: Lens.Lens' DescribeJobResponse (Core.Maybe Core.Text)
describeJobResponse_documentSource = Lens.lens (\DescribeJobResponse' {documentSource} -> documentSource) (\s@DescribeJobResponse' {} a -> s {documentSource = a} :: DescribeJobResponse)

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Core.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

instance Core.NFData DescribeJobResponse
