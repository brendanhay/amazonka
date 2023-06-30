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
-- Module      : Amazonka.Batch.TerminateJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a job in a job queue. Jobs that are in the @STARTING@ or
-- @RUNNING@ state are terminated, which causes them to transition to
-- @FAILED@. Jobs that have not progressed to the @STARTING@ state are
-- cancelled.
module Amazonka.Batch.TerminateJob
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

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @TerminateJob@.
--
-- /See:/ 'newTerminateJob' smart constructor.
data TerminateJob = TerminateJob'
  { -- | The Batch job ID of the job to terminate.
    jobId :: Prelude.Text,
    -- | A message to attach to the job that explains the reason for canceling
    -- it. This message is returned by future DescribeJobs operations on the
    -- job. This message is also recorded in the Batch activity logs.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'terminateJob_jobId' - The Batch job ID of the job to terminate.
--
-- 'reason', 'terminateJob_reason' - A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the Batch activity logs.
newTerminateJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'reason'
  Prelude.Text ->
  TerminateJob
newTerminateJob pJobId_ pReason_ =
  TerminateJob' {jobId = pJobId_, reason = pReason_}

-- | The Batch job ID of the job to terminate.
terminateJob_jobId :: Lens.Lens' TerminateJob Prelude.Text
terminateJob_jobId = Lens.lens (\TerminateJob' {jobId} -> jobId) (\s@TerminateJob' {} a -> s {jobId = a} :: TerminateJob)

-- | A message to attach to the job that explains the reason for canceling
-- it. This message is returned by future DescribeJobs operations on the
-- job. This message is also recorded in the Batch activity logs.
terminateJob_reason :: Lens.Lens' TerminateJob Prelude.Text
terminateJob_reason = Lens.lens (\TerminateJob' {reason} -> reason) (\s@TerminateJob' {} a -> s {reason = a} :: TerminateJob)

instance Core.AWSRequest TerminateJob where
  type AWSResponse TerminateJob = TerminateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateJob where
  hashWithSalt _salt TerminateJob' {..} =
    _salt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData TerminateJob where
  rnf TerminateJob' {..} =
    Prelude.rnf jobId `Prelude.seq` Prelude.rnf reason

instance Data.ToHeaders TerminateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateJob where
  toJSON TerminateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Data..= jobId),
            Prelude.Just ("reason" Data..= reason)
          ]
      )

instance Data.ToPath TerminateJob where
  toPath = Prelude.const "/v1/terminatejob"

instance Data.ToQuery TerminateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateJobResponse' smart constructor.
data TerminateJobResponse = TerminateJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  TerminateJobResponse
newTerminateJobResponse pHttpStatus_ =
  TerminateJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
terminateJobResponse_httpStatus :: Lens.Lens' TerminateJobResponse Prelude.Int
terminateJobResponse_httpStatus = Lens.lens (\TerminateJobResponse' {httpStatus} -> httpStatus) (\s@TerminateJobResponse' {} a -> s {httpStatus = a} :: TerminateJobResponse)

instance Prelude.NFData TerminateJobResponse where
  rnf TerminateJobResponse' {..} =
    Prelude.rnf httpStatus
