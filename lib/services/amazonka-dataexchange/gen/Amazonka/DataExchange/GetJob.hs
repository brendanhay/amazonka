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
-- Module      : Amazonka.DataExchange.GetJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job.
module Amazonka.DataExchange.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_jobId,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_state,
    getJobResponse_arn,
    getJobResponse_createdAt,
    getJobResponse_details,
    getJobResponse_id,
    getJobResponse_type,
    getJobResponse_updatedAt,
    getJobResponse_errors,
    getJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | The unique identifier for a job.
    jobId :: Prelude.Text
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
-- 'jobId', 'getJob_jobId' - The unique identifier for a job.
newGetJob ::
  -- | 'jobId'
  Prelude.Text ->
  GetJob
newGetJob pJobId_ = GetJob' {jobId = pJobId_}

-- | The unique identifier for a job.
getJob_jobId :: Lens.Lens' GetJob Prelude.Text
getJob_jobId = Lens.lens (\GetJob' {jobId} -> jobId) (\s@GetJob' {} a -> s {jobId = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Prelude.<$> (x Core..?> "State")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "Details")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJob where
  hashWithSalt salt' GetJob' {..} =
    salt' `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetJob where
  rnf GetJob' {..} = Prelude.rnf jobId

instance Core.ToHeaders GetJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetJob where
  toPath GetJob' {..} =
    Prelude.mconcat ["/v1/jobs/", Core.toBS jobId]

instance Core.ToQuery GetJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | The state of the job.
    state :: Prelude.Maybe State,
    -- | The ARN for the job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the job was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Details about the job.
    details :: Prelude.Maybe ResponseDetails,
    -- | The unique identifier for the job.
    id :: Prelude.Maybe Prelude.Text,
    -- | The job type.
    type' :: Prelude.Maybe Type,
    -- | The date and time that the job was last updated, in ISO 8601 format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The errors associated with jobs.
    errors :: Prelude.Maybe [JobError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'getJobResponse_state' - The state of the job.
--
-- 'arn', 'getJobResponse_arn' - The ARN for the job.
--
-- 'createdAt', 'getJobResponse_createdAt' - The date and time that the job was created, in ISO 8601 format.
--
-- 'details', 'getJobResponse_details' - Details about the job.
--
-- 'id', 'getJobResponse_id' - The unique identifier for the job.
--
-- 'type'', 'getJobResponse_type' - The job type.
--
-- 'updatedAt', 'getJobResponse_updatedAt' - The date and time that the job was last updated, in ISO 8601 format.
--
-- 'errors', 'getJobResponse_errors' - The errors associated with jobs.
--
-- 'httpStatus', 'getJobResponse_httpStatus' - The response's http status code.
newGetJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobResponse
newGetJobResponse pHttpStatus_ =
  GetJobResponse'
    { state = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      details = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the job.
getJobResponse_state :: Lens.Lens' GetJobResponse (Prelude.Maybe State)
getJobResponse_state = Lens.lens (\GetJobResponse' {state} -> state) (\s@GetJobResponse' {} a -> s {state = a} :: GetJobResponse)

-- | The ARN for the job.
getJobResponse_arn :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.Text)
getJobResponse_arn = Lens.lens (\GetJobResponse' {arn} -> arn) (\s@GetJobResponse' {} a -> s {arn = a} :: GetJobResponse)

-- | The date and time that the job was created, in ISO 8601 format.
getJobResponse_createdAt :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.UTCTime)
getJobResponse_createdAt = Lens.lens (\GetJobResponse' {createdAt} -> createdAt) (\s@GetJobResponse' {} a -> s {createdAt = a} :: GetJobResponse) Prelude.. Lens.mapping Core._Time

-- | Details about the job.
getJobResponse_details :: Lens.Lens' GetJobResponse (Prelude.Maybe ResponseDetails)
getJobResponse_details = Lens.lens (\GetJobResponse' {details} -> details) (\s@GetJobResponse' {} a -> s {details = a} :: GetJobResponse)

-- | The unique identifier for the job.
getJobResponse_id :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.Text)
getJobResponse_id = Lens.lens (\GetJobResponse' {id} -> id) (\s@GetJobResponse' {} a -> s {id = a} :: GetJobResponse)

-- | The job type.
getJobResponse_type :: Lens.Lens' GetJobResponse (Prelude.Maybe Type)
getJobResponse_type = Lens.lens (\GetJobResponse' {type'} -> type') (\s@GetJobResponse' {} a -> s {type' = a} :: GetJobResponse)

-- | The date and time that the job was last updated, in ISO 8601 format.
getJobResponse_updatedAt :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.UTCTime)
getJobResponse_updatedAt = Lens.lens (\GetJobResponse' {updatedAt} -> updatedAt) (\s@GetJobResponse' {} a -> s {updatedAt = a} :: GetJobResponse) Prelude.. Lens.mapping Core._Time

-- | The errors associated with jobs.
getJobResponse_errors :: Lens.Lens' GetJobResponse (Prelude.Maybe [JobError])
getJobResponse_errors = Lens.lens (\GetJobResponse' {errors} -> errors) (\s@GetJobResponse' {} a -> s {errors = a} :: GetJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Prelude.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

instance Prelude.NFData GetJobResponse where
  rnf GetJobResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
