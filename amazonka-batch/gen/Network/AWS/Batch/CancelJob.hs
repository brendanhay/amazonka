{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.CancelJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job in an AWS Batch job queue. Jobs that are in the
-- @SUBMITTED@, @PENDING@, or @RUNNABLE@ state are canceled. Jobs that have
-- progressed to @STARTING@ or @RUNNING@ are not canceled (but the API
-- operation still succeeds, even if no job is canceled); these jobs must
-- be terminated with the TerminateJob operation.
module Network.AWS.Batch.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_jobId,
    cancelJob_reason,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @CancelJob@.
--
-- /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | The AWS Batch job ID of the job to cancel.
    jobId :: Prelude.Text,
    -- | A message to attach to the job that explains the reason for canceling
    -- it. This message is returned by future DescribeJobs operations on the
    -- job. This message is also recorded in the AWS Batch activity logs.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelJob_jobId' - The AWS Batch job ID of the job to cancel.
--
-- 'reason', 'cancelJob_reason' - A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the AWS Batch activity logs.
newCancelJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'reason'
  Prelude.Text ->
  CancelJob
newCancelJob pJobId_ pReason_ =
  CancelJob' {jobId = pJobId_, reason = pReason_}

-- | The AWS Batch job ID of the job to cancel.
cancelJob_jobId :: Lens.Lens' CancelJob Prelude.Text
cancelJob_jobId = Lens.lens (\CancelJob' {jobId} -> jobId) (\s@CancelJob' {} a -> s {jobId = a} :: CancelJob)

-- | A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the AWS Batch activity logs.
cancelJob_reason :: Lens.Lens' CancelJob Prelude.Text
cancelJob_reason = Lens.lens (\CancelJob' {reason} -> reason) (\s@CancelJob' {} a -> s {reason = a} :: CancelJob)

instance Prelude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelJob

instance Prelude.NFData CancelJob

instance Prelude.ToHeaders CancelJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Prelude..= jobId),
            Prelude.Just ("reason" Prelude..= reason)
          ]
      )

instance Prelude.ToPath CancelJob where
  toPath = Prelude.const "/v1/canceljob"

instance Prelude.ToQuery CancelJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
newCancelJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelJobResponse
newCancelJobResponse pHttpStatus_ =
  CancelJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Prelude.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

instance Prelude.NFData CancelJobResponse
