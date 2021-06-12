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
-- Module      : Network.AWS.Batch.TerminateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a job in a job queue. Jobs that are in the @STARTING@ or
-- @RUNNING@ state are terminated, which causes them to transition to
-- @FAILED@. Jobs that have not progressed to the @STARTING@ state are
-- cancelled.
module Network.AWS.Batch.TerminateJob
  ( -- * Creating a Request
    TerminateJob (..),
    newTerminateJob,

    -- * Request Lenses
    terminateJob_jobId,
    terminateJob_reason,

    -- * Destructuring the Response
    TerminateJobResponse (..),
    newTerminateJobResponse,

    -- * Response Lenses
    terminateJobResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @TerminateJob@.
--
-- /See:/ 'newTerminateJob' smart constructor.
data TerminateJob = TerminateJob'
  { -- | The AWS Batch job ID of the job to terminate.
    jobId :: Core.Text,
    -- | A message to attach to the job that explains the reason for canceling
    -- it. This message is returned by future DescribeJobs operations on the
    -- job. This message is also recorded in the AWS Batch activity logs.
    reason :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'terminateJob_jobId' - The AWS Batch job ID of the job to terminate.
--
-- 'reason', 'terminateJob_reason' - A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the AWS Batch activity logs.
newTerminateJob ::
  -- | 'jobId'
  Core.Text ->
  -- | 'reason'
  Core.Text ->
  TerminateJob
newTerminateJob pJobId_ pReason_ =
  TerminateJob' {jobId = pJobId_, reason = pReason_}

-- | The AWS Batch job ID of the job to terminate.
terminateJob_jobId :: Lens.Lens' TerminateJob Core.Text
terminateJob_jobId = Lens.lens (\TerminateJob' {jobId} -> jobId) (\s@TerminateJob' {} a -> s {jobId = a} :: TerminateJob)

-- | A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the AWS Batch activity logs.
terminateJob_reason :: Lens.Lens' TerminateJob Core.Text
terminateJob_reason = Lens.lens (\TerminateJob' {reason} -> reason) (\s@TerminateJob' {} a -> s {reason = a} :: TerminateJob)

instance Core.AWSRequest TerminateJob where
  type AWSResponse TerminateJob = TerminateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TerminateJob

instance Core.NFData TerminateJob

instance Core.ToHeaders TerminateJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TerminateJob where
  toJSON TerminateJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("reason" Core..= reason)
          ]
      )

instance Core.ToPath TerminateJob where
  toPath = Core.const "/v1/terminatejob"

instance Core.ToQuery TerminateJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTerminateJobResponse' smart constructor.
data TerminateJobResponse = TerminateJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'terminateJobResponse_httpStatus' - The response's http status code.
newTerminateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TerminateJobResponse
newTerminateJobResponse pHttpStatus_ =
  TerminateJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
terminateJobResponse_httpStatus :: Lens.Lens' TerminateJobResponse Core.Int
terminateJobResponse_httpStatus = Lens.lens (\TerminateJobResponse' {httpStatus} -> httpStatus) (\s@TerminateJobResponse' {} a -> s {httpStatus = a} :: TerminateJobResponse)

instance Core.NFData TerminateJobResponse
