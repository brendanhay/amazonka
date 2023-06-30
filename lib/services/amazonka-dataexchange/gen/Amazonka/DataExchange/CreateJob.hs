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
-- Module      : Amazonka.DataExchange.CreateJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a job.
module Amazonka.DataExchange.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_details,
    createJob_type,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_arn,
    createJobResponse_createdAt,
    createJobResponse_details,
    createJobResponse_errors,
    createJobResponse_id,
    createJobResponse_state,
    createJobResponse_type,
    createJobResponse_updatedAt,
    createJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The details for the CreateJob request.
    details :: RequestDetails,
    -- | The type of job to be created.
    type' :: Type
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
-- 'details', 'createJob_details' - The details for the CreateJob request.
--
-- 'type'', 'createJob_type' - The type of job to be created.
newCreateJob ::
  -- | 'details'
  RequestDetails ->
  -- | 'type''
  Type ->
  CreateJob
newCreateJob pDetails_ pType_ =
  CreateJob' {details = pDetails_, type' = pType_}

-- | The details for the CreateJob request.
createJob_details :: Lens.Lens' CreateJob RequestDetails
createJob_details = Lens.lens (\CreateJob' {details} -> details) (\s@CreateJob' {} a -> s {details = a} :: CreateJob)

-- | The type of job to be created.
createJob_type :: Lens.Lens' CreateJob Type
createJob_type = Lens.lens (\CreateJob' {type'} -> type') (\s@CreateJob' {} a -> s {type' = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Details")
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf details `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Details" Data..= details),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateJob where
  toPath = Prelude.const "/v1/jobs"

instance Data.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The ARN for the job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the job was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | Details about the job.
    details :: Prelude.Maybe ResponseDetails,
    -- | The errors associated with jobs.
    errors :: Prelude.Maybe [JobError],
    -- | The unique identifier for the job.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the job.
    state :: Prelude.Maybe State,
    -- | The job type.
    type' :: Prelude.Maybe Type,
    -- | The date and time that the job was last updated, in ISO 8601 format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
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
-- 'arn', 'createJobResponse_arn' - The ARN for the job.
--
-- 'createdAt', 'createJobResponse_createdAt' - The date and time that the job was created, in ISO 8601 format.
--
-- 'details', 'createJobResponse_details' - Details about the job.
--
-- 'errors', 'createJobResponse_errors' - The errors associated with jobs.
--
-- 'id', 'createJobResponse_id' - The unique identifier for the job.
--
-- 'state', 'createJobResponse_state' - The state of the job.
--
-- 'type'', 'createJobResponse_type' - The job type.
--
-- 'updatedAt', 'createJobResponse_updatedAt' - The date and time that the job was last updated, in ISO 8601 format.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      details = Prelude.Nothing,
      errors = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the job.
createJobResponse_arn :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_arn = Lens.lens (\CreateJobResponse' {arn} -> arn) (\s@CreateJobResponse' {} a -> s {arn = a} :: CreateJobResponse)

-- | The date and time that the job was created, in ISO 8601 format.
createJobResponse_createdAt :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.UTCTime)
createJobResponse_createdAt = Lens.lens (\CreateJobResponse' {createdAt} -> createdAt) (\s@CreateJobResponse' {} a -> s {createdAt = a} :: CreateJobResponse) Prelude.. Lens.mapping Data._Time

-- | Details about the job.
createJobResponse_details :: Lens.Lens' CreateJobResponse (Prelude.Maybe ResponseDetails)
createJobResponse_details = Lens.lens (\CreateJobResponse' {details} -> details) (\s@CreateJobResponse' {} a -> s {details = a} :: CreateJobResponse)

-- | The errors associated with jobs.
createJobResponse_errors :: Lens.Lens' CreateJobResponse (Prelude.Maybe [JobError])
createJobResponse_errors = Lens.lens (\CreateJobResponse' {errors} -> errors) (\s@CreateJobResponse' {} a -> s {errors = a} :: CreateJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the job.
createJobResponse_id :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_id = Lens.lens (\CreateJobResponse' {id} -> id) (\s@CreateJobResponse' {} a -> s {id = a} :: CreateJobResponse)

-- | The state of the job.
createJobResponse_state :: Lens.Lens' CreateJobResponse (Prelude.Maybe State)
createJobResponse_state = Lens.lens (\CreateJobResponse' {state} -> state) (\s@CreateJobResponse' {} a -> s {state = a} :: CreateJobResponse)

-- | The job type.
createJobResponse_type :: Lens.Lens' CreateJobResponse (Prelude.Maybe Type)
createJobResponse_type = Lens.lens (\CreateJobResponse' {type'} -> type') (\s@CreateJobResponse' {} a -> s {type' = a} :: CreateJobResponse)

-- | The date and time that the job was last updated, in ISO 8601 format.
createJobResponse_updatedAt :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.UTCTime)
createJobResponse_updatedAt = Lens.lens (\CreateJobResponse' {updatedAt} -> updatedAt) (\s@CreateJobResponse' {} a -> s {updatedAt = a} :: CreateJobResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
