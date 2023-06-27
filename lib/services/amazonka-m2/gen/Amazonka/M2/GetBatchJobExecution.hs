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
-- Module      : Amazonka.M2.GetBatchJobExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a specific batch job execution for a specific
-- application.
module Amazonka.M2.GetBatchJobExecution
  ( -- * Creating a Request
    GetBatchJobExecution (..),
    newGetBatchJobExecution,

    -- * Request Lenses
    getBatchJobExecution_applicationId,
    getBatchJobExecution_executionId,

    -- * Destructuring the Response
    GetBatchJobExecutionResponse (..),
    newGetBatchJobExecutionResponse,

    -- * Response Lenses
    getBatchJobExecutionResponse_batchJobIdentifier,
    getBatchJobExecutionResponse_endTime,
    getBatchJobExecutionResponse_jobId,
    getBatchJobExecutionResponse_jobName,
    getBatchJobExecutionResponse_jobType,
    getBatchJobExecutionResponse_jobUser,
    getBatchJobExecutionResponse_returnCode,
    getBatchJobExecutionResponse_statusReason,
    getBatchJobExecutionResponse_httpStatus,
    getBatchJobExecutionResponse_applicationId,
    getBatchJobExecutionResponse_executionId,
    getBatchJobExecutionResponse_startTime,
    getBatchJobExecutionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBatchJobExecution' smart constructor.
data GetBatchJobExecution = GetBatchJobExecution'
  { -- | The identifier of the application.
    applicationId :: Prelude.Text,
    -- | The unique identifier of the batch job execution.
    executionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBatchJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getBatchJobExecution_applicationId' - The identifier of the application.
--
-- 'executionId', 'getBatchJobExecution_executionId' - The unique identifier of the batch job execution.
newGetBatchJobExecution ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'executionId'
  Prelude.Text ->
  GetBatchJobExecution
newGetBatchJobExecution pApplicationId_ pExecutionId_ =
  GetBatchJobExecution'
    { applicationId =
        pApplicationId_,
      executionId = pExecutionId_
    }

-- | The identifier of the application.
getBatchJobExecution_applicationId :: Lens.Lens' GetBatchJobExecution Prelude.Text
getBatchJobExecution_applicationId = Lens.lens (\GetBatchJobExecution' {applicationId} -> applicationId) (\s@GetBatchJobExecution' {} a -> s {applicationId = a} :: GetBatchJobExecution)

-- | The unique identifier of the batch job execution.
getBatchJobExecution_executionId :: Lens.Lens' GetBatchJobExecution Prelude.Text
getBatchJobExecution_executionId = Lens.lens (\GetBatchJobExecution' {executionId} -> executionId) (\s@GetBatchJobExecution' {} a -> s {executionId = a} :: GetBatchJobExecution)

instance Core.AWSRequest GetBatchJobExecution where
  type
    AWSResponse GetBatchJobExecution =
      GetBatchJobExecutionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBatchJobExecutionResponse'
            Prelude.<$> (x Data..?> "batchJobIdentifier")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "jobName")
            Prelude.<*> (x Data..?> "jobType")
            Prelude.<*> (x Data..?> "jobUser")
            Prelude.<*> (x Data..?> "returnCode")
            Prelude.<*> (x Data..?> "statusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "executionId")
            Prelude.<*> (x Data..:> "startTime")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetBatchJobExecution where
  hashWithSalt _salt GetBatchJobExecution' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` executionId

instance Prelude.NFData GetBatchJobExecution where
  rnf GetBatchJobExecution' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf executionId

instance Data.ToHeaders GetBatchJobExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBatchJobExecution where
  toPath GetBatchJobExecution' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/batch-job-executions/",
        Data.toBS executionId
      ]

instance Data.ToQuery GetBatchJobExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBatchJobExecutionResponse' smart constructor.
data GetBatchJobExecutionResponse = GetBatchJobExecutionResponse'
  { -- | The unique identifier of this batch job.
    batchJobIdentifier :: Prelude.Maybe BatchJobIdentifier,
    -- | The timestamp when the batch job execution ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for this batch job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of this batch job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The type of job.
    jobType :: Prelude.Maybe BatchJobType,
    -- | The user for the job.
    jobUser :: Prelude.Maybe Prelude.Text,
    -- | The batch job return code from either the Blu Age or Micro Focus runtime
    -- engines. For more information, see
    -- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
    -- in the /IBM WebSphere Application Server/ documentation.
    returnCode :: Prelude.Maybe Prelude.Text,
    -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the application.
    applicationId :: Prelude.Text,
    -- | The unique identifier for this batch job execution.
    executionId :: Prelude.Text,
    -- | The timestamp when the batch job execution started.
    startTime :: Data.POSIX,
    -- | The status of the batch job execution.
    status :: BatchJobExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBatchJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchJobIdentifier', 'getBatchJobExecutionResponse_batchJobIdentifier' - The unique identifier of this batch job.
--
-- 'endTime', 'getBatchJobExecutionResponse_endTime' - The timestamp when the batch job execution ended.
--
-- 'jobId', 'getBatchJobExecutionResponse_jobId' - The unique identifier for this batch job.
--
-- 'jobName', 'getBatchJobExecutionResponse_jobName' - The name of this batch job.
--
-- 'jobType', 'getBatchJobExecutionResponse_jobType' - The type of job.
--
-- 'jobUser', 'getBatchJobExecutionResponse_jobUser' - The user for the job.
--
-- 'returnCode', 'getBatchJobExecutionResponse_returnCode' - The batch job return code from either the Blu Age or Micro Focus runtime
-- engines. For more information, see
-- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
-- in the /IBM WebSphere Application Server/ documentation.
--
-- 'statusReason', 'getBatchJobExecutionResponse_statusReason' - The reason for the reported status.
--
-- 'httpStatus', 'getBatchJobExecutionResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'getBatchJobExecutionResponse_applicationId' - The identifier of the application.
--
-- 'executionId', 'getBatchJobExecutionResponse_executionId' - The unique identifier for this batch job execution.
--
-- 'startTime', 'getBatchJobExecutionResponse_startTime' - The timestamp when the batch job execution started.
--
-- 'status', 'getBatchJobExecutionResponse_status' - The status of the batch job execution.
newGetBatchJobExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'executionId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'status'
  BatchJobExecutionStatus ->
  GetBatchJobExecutionResponse
newGetBatchJobExecutionResponse
  pHttpStatus_
  pApplicationId_
  pExecutionId_
  pStartTime_
  pStatus_ =
    GetBatchJobExecutionResponse'
      { batchJobIdentifier =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        jobId = Prelude.Nothing,
        jobName = Prelude.Nothing,
        jobType = Prelude.Nothing,
        jobUser = Prelude.Nothing,
        returnCode = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        executionId = pExecutionId_,
        startTime = Data._Time Lens.# pStartTime_,
        status = pStatus_
      }

-- | The unique identifier of this batch job.
getBatchJobExecutionResponse_batchJobIdentifier :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe BatchJobIdentifier)
getBatchJobExecutionResponse_batchJobIdentifier = Lens.lens (\GetBatchJobExecutionResponse' {batchJobIdentifier} -> batchJobIdentifier) (\s@GetBatchJobExecutionResponse' {} a -> s {batchJobIdentifier = a} :: GetBatchJobExecutionResponse)

-- | The timestamp when the batch job execution ended.
getBatchJobExecutionResponse_endTime :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchJobExecutionResponse_endTime = Lens.lens (\GetBatchJobExecutionResponse' {endTime} -> endTime) (\s@GetBatchJobExecutionResponse' {} a -> s {endTime = a} :: GetBatchJobExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for this batch job.
getBatchJobExecutionResponse_jobId :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.Text)
getBatchJobExecutionResponse_jobId = Lens.lens (\GetBatchJobExecutionResponse' {jobId} -> jobId) (\s@GetBatchJobExecutionResponse' {} a -> s {jobId = a} :: GetBatchJobExecutionResponse)

-- | The name of this batch job.
getBatchJobExecutionResponse_jobName :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.Text)
getBatchJobExecutionResponse_jobName = Lens.lens (\GetBatchJobExecutionResponse' {jobName} -> jobName) (\s@GetBatchJobExecutionResponse' {} a -> s {jobName = a} :: GetBatchJobExecutionResponse)

-- | The type of job.
getBatchJobExecutionResponse_jobType :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe BatchJobType)
getBatchJobExecutionResponse_jobType = Lens.lens (\GetBatchJobExecutionResponse' {jobType} -> jobType) (\s@GetBatchJobExecutionResponse' {} a -> s {jobType = a} :: GetBatchJobExecutionResponse)

-- | The user for the job.
getBatchJobExecutionResponse_jobUser :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.Text)
getBatchJobExecutionResponse_jobUser = Lens.lens (\GetBatchJobExecutionResponse' {jobUser} -> jobUser) (\s@GetBatchJobExecutionResponse' {} a -> s {jobUser = a} :: GetBatchJobExecutionResponse)

-- | The batch job return code from either the Blu Age or Micro Focus runtime
-- engines. For more information, see
-- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
-- in the /IBM WebSphere Application Server/ documentation.
getBatchJobExecutionResponse_returnCode :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.Text)
getBatchJobExecutionResponse_returnCode = Lens.lens (\GetBatchJobExecutionResponse' {returnCode} -> returnCode) (\s@GetBatchJobExecutionResponse' {} a -> s {returnCode = a} :: GetBatchJobExecutionResponse)

-- | The reason for the reported status.
getBatchJobExecutionResponse_statusReason :: Lens.Lens' GetBatchJobExecutionResponse (Prelude.Maybe Prelude.Text)
getBatchJobExecutionResponse_statusReason = Lens.lens (\GetBatchJobExecutionResponse' {statusReason} -> statusReason) (\s@GetBatchJobExecutionResponse' {} a -> s {statusReason = a} :: GetBatchJobExecutionResponse)

-- | The response's http status code.
getBatchJobExecutionResponse_httpStatus :: Lens.Lens' GetBatchJobExecutionResponse Prelude.Int
getBatchJobExecutionResponse_httpStatus = Lens.lens (\GetBatchJobExecutionResponse' {httpStatus} -> httpStatus) (\s@GetBatchJobExecutionResponse' {} a -> s {httpStatus = a} :: GetBatchJobExecutionResponse)

-- | The identifier of the application.
getBatchJobExecutionResponse_applicationId :: Lens.Lens' GetBatchJobExecutionResponse Prelude.Text
getBatchJobExecutionResponse_applicationId = Lens.lens (\GetBatchJobExecutionResponse' {applicationId} -> applicationId) (\s@GetBatchJobExecutionResponse' {} a -> s {applicationId = a} :: GetBatchJobExecutionResponse)

-- | The unique identifier for this batch job execution.
getBatchJobExecutionResponse_executionId :: Lens.Lens' GetBatchJobExecutionResponse Prelude.Text
getBatchJobExecutionResponse_executionId = Lens.lens (\GetBatchJobExecutionResponse' {executionId} -> executionId) (\s@GetBatchJobExecutionResponse' {} a -> s {executionId = a} :: GetBatchJobExecutionResponse)

-- | The timestamp when the batch job execution started.
getBatchJobExecutionResponse_startTime :: Lens.Lens' GetBatchJobExecutionResponse Prelude.UTCTime
getBatchJobExecutionResponse_startTime = Lens.lens (\GetBatchJobExecutionResponse' {startTime} -> startTime) (\s@GetBatchJobExecutionResponse' {} a -> s {startTime = a} :: GetBatchJobExecutionResponse) Prelude.. Data._Time

-- | The status of the batch job execution.
getBatchJobExecutionResponse_status :: Lens.Lens' GetBatchJobExecutionResponse BatchJobExecutionStatus
getBatchJobExecutionResponse_status = Lens.lens (\GetBatchJobExecutionResponse' {status} -> status) (\s@GetBatchJobExecutionResponse' {} a -> s {status = a} :: GetBatchJobExecutionResponse)

instance Prelude.NFData GetBatchJobExecutionResponse where
  rnf GetBatchJobExecutionResponse' {..} =
    Prelude.rnf batchJobIdentifier
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf jobUser
      `Prelude.seq` Prelude.rnf returnCode
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
