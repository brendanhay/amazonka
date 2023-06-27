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
-- Module      : Amazonka.QuickSight.DescribeAssetBundleImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing import job.
--
-- Poll job descriptions after starting a job to know when it has succeeded
-- or failed. Job descriptions are available for 14 days after job starts.
module Amazonka.QuickSight.DescribeAssetBundleImportJob
  ( -- * Creating a Request
    DescribeAssetBundleImportJob (..),
    newDescribeAssetBundleImportJob,

    -- * Request Lenses
    describeAssetBundleImportJob_awsAccountId,
    describeAssetBundleImportJob_assetBundleImportJobId,

    -- * Destructuring the Response
    DescribeAssetBundleImportJobResponse (..),
    newDescribeAssetBundleImportJobResponse,

    -- * Response Lenses
    describeAssetBundleImportJobResponse_arn,
    describeAssetBundleImportJobResponse_assetBundleImportJobId,
    describeAssetBundleImportJobResponse_assetBundleImportSource,
    describeAssetBundleImportJobResponse_awsAccountId,
    describeAssetBundleImportJobResponse_createdTime,
    describeAssetBundleImportJobResponse_errors,
    describeAssetBundleImportJobResponse_failureAction,
    describeAssetBundleImportJobResponse_jobStatus,
    describeAssetBundleImportJobResponse_overrideParameters,
    describeAssetBundleImportJobResponse_requestId,
    describeAssetBundleImportJobResponse_rollbackErrors,
    describeAssetBundleImportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAssetBundleImportJob' smart constructor.
data DescribeAssetBundleImportJob = DescribeAssetBundleImportJob'
  { -- | The ID of the Amazon Web Services account the import job was executed
    -- in.
    awsAccountId :: Prelude.Text,
    -- | The ID of the job. The job ID is set when you start a new job with a
    -- @StartAssetBundleImportJob@ API call.
    assetBundleImportJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetBundleImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAssetBundleImportJob_awsAccountId' - The ID of the Amazon Web Services account the import job was executed
-- in.
--
-- 'assetBundleImportJobId', 'describeAssetBundleImportJob_assetBundleImportJobId' - The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleImportJob@ API call.
newDescribeAssetBundleImportJob ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assetBundleImportJobId'
  Prelude.Text ->
  DescribeAssetBundleImportJob
newDescribeAssetBundleImportJob
  pAwsAccountId_
  pAssetBundleImportJobId_ =
    DescribeAssetBundleImportJob'
      { awsAccountId =
          pAwsAccountId_,
        assetBundleImportJobId =
          pAssetBundleImportJobId_
      }

-- | The ID of the Amazon Web Services account the import job was executed
-- in.
describeAssetBundleImportJob_awsAccountId :: Lens.Lens' DescribeAssetBundleImportJob Prelude.Text
describeAssetBundleImportJob_awsAccountId = Lens.lens (\DescribeAssetBundleImportJob' {awsAccountId} -> awsAccountId) (\s@DescribeAssetBundleImportJob' {} a -> s {awsAccountId = a} :: DescribeAssetBundleImportJob)

-- | The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleImportJob@ API call.
describeAssetBundleImportJob_assetBundleImportJobId :: Lens.Lens' DescribeAssetBundleImportJob Prelude.Text
describeAssetBundleImportJob_assetBundleImportJobId = Lens.lens (\DescribeAssetBundleImportJob' {assetBundleImportJobId} -> assetBundleImportJobId) (\s@DescribeAssetBundleImportJob' {} a -> s {assetBundleImportJobId = a} :: DescribeAssetBundleImportJob)

instance Core.AWSRequest DescribeAssetBundleImportJob where
  type
    AWSResponse DescribeAssetBundleImportJob =
      DescribeAssetBundleImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetBundleImportJobResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetBundleImportJobId")
            Prelude.<*> (x Data..?> "AssetBundleImportSource")
            Prelude.<*> (x Data..?> "AwsAccountId")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "FailureAction")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "OverrideParameters")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "RollbackErrors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAssetBundleImportJob
  where
  hashWithSalt _salt DescribeAssetBundleImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assetBundleImportJobId

instance Prelude.NFData DescribeAssetBundleImportJob where
  rnf DescribeAssetBundleImportJob' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assetBundleImportJobId

instance Data.ToHeaders DescribeAssetBundleImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAssetBundleImportJob where
  toPath DescribeAssetBundleImportJob' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-import-jobs/",
        Data.toBS assetBundleImportJobId
      ]

instance Data.ToQuery DescribeAssetBundleImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssetBundleImportJobResponse' smart constructor.
data DescribeAssetBundleImportJobResponse = DescribeAssetBundleImportJobResponse'
  { -- | The Amazon Resource Name (ARN) for the import job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job. The job ID is set when you start a new job with a
    -- @StartAssetBundleImportJob@ API call.
    assetBundleImportJobId :: Prelude.Maybe Prelude.Text,
    -- | The source of the asset bundle zip file that contains the data that is
    -- imported by the job.
    assetBundleImportSource :: Prelude.Maybe AssetBundleImportSourceDescription,
    -- | The ID of the Amazon Web Services account the import job was executed
    -- in.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The time that the import job was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | An array of error records that describes any failures that occurred
    -- during the export job processing.
    --
    -- Error records accumulate while the job is still running. The complete
    -- set of error records is available after the job has completed and
    -- failed.
    errors :: Prelude.Maybe [AssetBundleImportJobError],
    -- | The failure action for the import job.
    failureAction :: Prelude.Maybe AssetBundleImportFailureAction,
    -- | Indicates the status of a job through its queuing and execution.
    --
    -- Poll the @DescribeAssetBundleImport@ API until @JobStatus@ returns one
    -- of the following values:
    --
    -- -   @SUCCESSFUL@
    --
    -- -   @FAILED@
    --
    -- -   @FAILED_ROLLBACK_COMPLETED@
    --
    -- -   @FAILED_ROLLBACK_ERROR@
    jobStatus :: Prelude.Maybe AssetBundleImportJobStatus,
    -- | Optional overrides to be applied to the resource configuration before
    -- import.
    overrideParameters :: Prelude.Maybe AssetBundleImportJobOverrideParameters,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An array of error records that describes any failures that occurred
    -- while an import job was attempting a rollback.
    --
    -- Error records accumulate while the job is still running. The complete
    -- set of error records is available after the job has completed and
    -- failed.
    rollbackErrors :: Prelude.Maybe [AssetBundleImportJobError],
    -- | The HTTP status of the response.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetBundleImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeAssetBundleImportJobResponse_arn' - The Amazon Resource Name (ARN) for the import job.
--
-- 'assetBundleImportJobId', 'describeAssetBundleImportJobResponse_assetBundleImportJobId' - The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleImportJob@ API call.
--
-- 'assetBundleImportSource', 'describeAssetBundleImportJobResponse_assetBundleImportSource' - The source of the asset bundle zip file that contains the data that is
-- imported by the job.
--
-- 'awsAccountId', 'describeAssetBundleImportJobResponse_awsAccountId' - The ID of the Amazon Web Services account the import job was executed
-- in.
--
-- 'createdTime', 'describeAssetBundleImportJobResponse_createdTime' - The time that the import job was created.
--
-- 'errors', 'describeAssetBundleImportJobResponse_errors' - An array of error records that describes any failures that occurred
-- during the export job processing.
--
-- Error records accumulate while the job is still running. The complete
-- set of error records is available after the job has completed and
-- failed.
--
-- 'failureAction', 'describeAssetBundleImportJobResponse_failureAction' - The failure action for the import job.
--
-- 'jobStatus', 'describeAssetBundleImportJobResponse_jobStatus' - Indicates the status of a job through its queuing and execution.
--
-- Poll the @DescribeAssetBundleImport@ API until @JobStatus@ returns one
-- of the following values:
--
-- -   @SUCCESSFUL@
--
-- -   @FAILED@
--
-- -   @FAILED_ROLLBACK_COMPLETED@
--
-- -   @FAILED_ROLLBACK_ERROR@
--
-- 'overrideParameters', 'describeAssetBundleImportJobResponse_overrideParameters' - Optional overrides to be applied to the resource configuration before
-- import.
--
-- 'requestId', 'describeAssetBundleImportJobResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'rollbackErrors', 'describeAssetBundleImportJobResponse_rollbackErrors' - An array of error records that describes any failures that occurred
-- while an import job was attempting a rollback.
--
-- Error records accumulate while the job is still running. The complete
-- set of error records is available after the job has completed and
-- failed.
--
-- 'status', 'describeAssetBundleImportJobResponse_status' - The HTTP status of the response.
newDescribeAssetBundleImportJobResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAssetBundleImportJobResponse
newDescribeAssetBundleImportJobResponse pStatus_ =
  DescribeAssetBundleImportJobResponse'
    { arn =
        Prelude.Nothing,
      assetBundleImportJobId =
        Prelude.Nothing,
      assetBundleImportSource =
        Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      errors = Prelude.Nothing,
      failureAction = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      overrideParameters = Prelude.Nothing,
      requestId = Prelude.Nothing,
      rollbackErrors = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the import job.
describeAssetBundleImportJobResponse_arn :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleImportJobResponse_arn = Lens.lens (\DescribeAssetBundleImportJobResponse' {arn} -> arn) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {arn = a} :: DescribeAssetBundleImportJobResponse)

-- | The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleImportJob@ API call.
describeAssetBundleImportJobResponse_assetBundleImportJobId :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleImportJobResponse_assetBundleImportJobId = Lens.lens (\DescribeAssetBundleImportJobResponse' {assetBundleImportJobId} -> assetBundleImportJobId) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {assetBundleImportJobId = a} :: DescribeAssetBundleImportJobResponse)

-- | The source of the asset bundle zip file that contains the data that is
-- imported by the job.
describeAssetBundleImportJobResponse_assetBundleImportSource :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe AssetBundleImportSourceDescription)
describeAssetBundleImportJobResponse_assetBundleImportSource = Lens.lens (\DescribeAssetBundleImportJobResponse' {assetBundleImportSource} -> assetBundleImportSource) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {assetBundleImportSource = a} :: DescribeAssetBundleImportJobResponse)

-- | The ID of the Amazon Web Services account the import job was executed
-- in.
describeAssetBundleImportJobResponse_awsAccountId :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleImportJobResponse_awsAccountId = Lens.lens (\DescribeAssetBundleImportJobResponse' {awsAccountId} -> awsAccountId) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {awsAccountId = a} :: DescribeAssetBundleImportJobResponse)

-- | The time that the import job was created.
describeAssetBundleImportJobResponse_createdTime :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeAssetBundleImportJobResponse_createdTime = Lens.lens (\DescribeAssetBundleImportJobResponse' {createdTime} -> createdTime) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {createdTime = a} :: DescribeAssetBundleImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | An array of error records that describes any failures that occurred
-- during the export job processing.
--
-- Error records accumulate while the job is still running. The complete
-- set of error records is available after the job has completed and
-- failed.
describeAssetBundleImportJobResponse_errors :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe [AssetBundleImportJobError])
describeAssetBundleImportJobResponse_errors = Lens.lens (\DescribeAssetBundleImportJobResponse' {errors} -> errors) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {errors = a} :: DescribeAssetBundleImportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The failure action for the import job.
describeAssetBundleImportJobResponse_failureAction :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe AssetBundleImportFailureAction)
describeAssetBundleImportJobResponse_failureAction = Lens.lens (\DescribeAssetBundleImportJobResponse' {failureAction} -> failureAction) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {failureAction = a} :: DescribeAssetBundleImportJobResponse)

-- | Indicates the status of a job through its queuing and execution.
--
-- Poll the @DescribeAssetBundleImport@ API until @JobStatus@ returns one
-- of the following values:
--
-- -   @SUCCESSFUL@
--
-- -   @FAILED@
--
-- -   @FAILED_ROLLBACK_COMPLETED@
--
-- -   @FAILED_ROLLBACK_ERROR@
describeAssetBundleImportJobResponse_jobStatus :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe AssetBundleImportJobStatus)
describeAssetBundleImportJobResponse_jobStatus = Lens.lens (\DescribeAssetBundleImportJobResponse' {jobStatus} -> jobStatus) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {jobStatus = a} :: DescribeAssetBundleImportJobResponse)

-- | Optional overrides to be applied to the resource configuration before
-- import.
describeAssetBundleImportJobResponse_overrideParameters :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe AssetBundleImportJobOverrideParameters)
describeAssetBundleImportJobResponse_overrideParameters = Lens.lens (\DescribeAssetBundleImportJobResponse' {overrideParameters} -> overrideParameters) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {overrideParameters = a} :: DescribeAssetBundleImportJobResponse)

-- | The Amazon Web Services request ID for this operation.
describeAssetBundleImportJobResponse_requestId :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleImportJobResponse_requestId = Lens.lens (\DescribeAssetBundleImportJobResponse' {requestId} -> requestId) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {requestId = a} :: DescribeAssetBundleImportJobResponse)

-- | An array of error records that describes any failures that occurred
-- while an import job was attempting a rollback.
--
-- Error records accumulate while the job is still running. The complete
-- set of error records is available after the job has completed and
-- failed.
describeAssetBundleImportJobResponse_rollbackErrors :: Lens.Lens' DescribeAssetBundleImportJobResponse (Prelude.Maybe [AssetBundleImportJobError])
describeAssetBundleImportJobResponse_rollbackErrors = Lens.lens (\DescribeAssetBundleImportJobResponse' {rollbackErrors} -> rollbackErrors) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {rollbackErrors = a} :: DescribeAssetBundleImportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the response.
describeAssetBundleImportJobResponse_status :: Lens.Lens' DescribeAssetBundleImportJobResponse Prelude.Int
describeAssetBundleImportJobResponse_status = Lens.lens (\DescribeAssetBundleImportJobResponse' {status} -> status) (\s@DescribeAssetBundleImportJobResponse' {} a -> s {status = a} :: DescribeAssetBundleImportJobResponse)

instance
  Prelude.NFData
    DescribeAssetBundleImportJobResponse
  where
  rnf DescribeAssetBundleImportJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleImportJobId
      `Prelude.seq` Prelude.rnf assetBundleImportSource
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf failureAction
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf overrideParameters
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf rollbackErrors
      `Prelude.seq` Prelude.rnf status
