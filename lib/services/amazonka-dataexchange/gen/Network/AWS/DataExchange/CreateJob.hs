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
-- Module      : Network.AWS.DataExchange.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a job.
module Network.AWS.DataExchange.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_type,
    createJob_details,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_state,
    createJobResponse_arn,
    createJobResponse_createdAt,
    createJobResponse_details,
    createJobResponse_id,
    createJobResponse_type,
    createJobResponse_updatedAt,
    createJobResponse_errors,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request body for CreateJob.
--
-- /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The type of job to be created.
    type' :: Type,
    -- | The details for the CreateJob request.
    details :: RequestDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'createJob_type' - The type of job to be created.
--
-- 'details', 'createJob_details' - The details for the CreateJob request.
newCreateJob ::
  -- | 'type''
  Type ->
  -- | 'details'
  RequestDetails ->
  CreateJob
newCreateJob pType_ pDetails_ =
  CreateJob' {type' = pType_, details = pDetails_}

-- | The type of job to be created.
createJob_type :: Lens.Lens' CreateJob Type
createJob_type = Lens.lens (\CreateJob' {type'} -> type') (\s@CreateJob' {} a -> s {type' = a} :: CreateJob)

-- | The details for the CreateJob request.
createJob_details :: Lens.Lens' CreateJob RequestDetails
createJob_details = Lens.lens (\CreateJob' {details} -> details) (\s@CreateJob' {} a -> s {details = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
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

instance Prelude.Hashable CreateJob

instance Prelude.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Details" Core..= details)
          ]
      )

instance Core.ToPath CreateJob where
  toPath = Prelude.const "/v1/jobs"

instance Core.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
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
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'createJobResponse_state' - The state of the job.
--
-- 'arn', 'createJobResponse_arn' - The ARN for the job.
--
-- 'createdAt', 'createJobResponse_createdAt' - The date and time that the job was created, in ISO 8601 format.
--
-- 'details', 'createJobResponse_details' - Details about the job.
--
-- 'id', 'createJobResponse_id' - The unique identifier for the job.
--
-- 'type'', 'createJobResponse_type' - The job type.
--
-- 'updatedAt', 'createJobResponse_updatedAt' - The date and time that the job was last updated, in ISO 8601 format.
--
-- 'errors', 'createJobResponse_errors' - The errors associated with jobs.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
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
createJobResponse_state :: Lens.Lens' CreateJobResponse (Prelude.Maybe State)
createJobResponse_state = Lens.lens (\CreateJobResponse' {state} -> state) (\s@CreateJobResponse' {} a -> s {state = a} :: CreateJobResponse)

-- | The ARN for the job.
createJobResponse_arn :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_arn = Lens.lens (\CreateJobResponse' {arn} -> arn) (\s@CreateJobResponse' {} a -> s {arn = a} :: CreateJobResponse)

-- | The date and time that the job was created, in ISO 8601 format.
createJobResponse_createdAt :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.UTCTime)
createJobResponse_createdAt = Lens.lens (\CreateJobResponse' {createdAt} -> createdAt) (\s@CreateJobResponse' {} a -> s {createdAt = a} :: CreateJobResponse) Prelude.. Lens.mapping Core._Time

-- | Details about the job.
createJobResponse_details :: Lens.Lens' CreateJobResponse (Prelude.Maybe ResponseDetails)
createJobResponse_details = Lens.lens (\CreateJobResponse' {details} -> details) (\s@CreateJobResponse' {} a -> s {details = a} :: CreateJobResponse)

-- | The unique identifier for the job.
createJobResponse_id :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_id = Lens.lens (\CreateJobResponse' {id} -> id) (\s@CreateJobResponse' {} a -> s {id = a} :: CreateJobResponse)

-- | The job type.
createJobResponse_type :: Lens.Lens' CreateJobResponse (Prelude.Maybe Type)
createJobResponse_type = Lens.lens (\CreateJobResponse' {type'} -> type') (\s@CreateJobResponse' {} a -> s {type' = a} :: CreateJobResponse)

-- | The date and time that the job was last updated, in ISO 8601 format.
createJobResponse_updatedAt :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.UTCTime)
createJobResponse_updatedAt = Lens.lens (\CreateJobResponse' {updatedAt} -> updatedAt) (\s@CreateJobResponse' {} a -> s {updatedAt = a} :: CreateJobResponse) Prelude.. Lens.mapping Core._Time

-- | The errors associated with jobs.
createJobResponse_errors :: Lens.Lens' CreateJobResponse (Prelude.Maybe [JobError])
createJobResponse_errors = Lens.lens (\CreateJobResponse' {errors} -> errors) (\s@CreateJobResponse' {} a -> s {errors = a} :: CreateJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse
