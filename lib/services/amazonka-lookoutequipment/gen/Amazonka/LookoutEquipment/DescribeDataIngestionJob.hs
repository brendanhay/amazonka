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
-- Module      : Amazonka.LookoutEquipment.DescribeDataIngestionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information on a specific data ingestion job such as creation
-- time, dataset ARN, status, and so on.
module Amazonka.LookoutEquipment.DescribeDataIngestionJob
  ( -- * Creating a Request
    DescribeDataIngestionJob (..),
    newDescribeDataIngestionJob,

    -- * Request Lenses
    describeDataIngestionJob_jobId,

    -- * Destructuring the Response
    DescribeDataIngestionJobResponse (..),
    newDescribeDataIngestionJobResponse,

    -- * Response Lenses
    describeDataIngestionJobResponse_ingestionInputConfiguration,
    describeDataIngestionJobResponse_status,
    describeDataIngestionJobResponse_datasetArn,
    describeDataIngestionJobResponse_failedReason,
    describeDataIngestionJobResponse_jobId,
    describeDataIngestionJobResponse_createdAt,
    describeDataIngestionJobResponse_roleArn,
    describeDataIngestionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataIngestionJob' smart constructor.
data DescribeDataIngestionJob = DescribeDataIngestionJob'
  { -- | The job ID of the data ingestion job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataIngestionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeDataIngestionJob_jobId' - The job ID of the data ingestion job.
newDescribeDataIngestionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeDataIngestionJob
newDescribeDataIngestionJob pJobId_ =
  DescribeDataIngestionJob' {jobId = pJobId_}

-- | The job ID of the data ingestion job.
describeDataIngestionJob_jobId :: Lens.Lens' DescribeDataIngestionJob Prelude.Text
describeDataIngestionJob_jobId = Lens.lens (\DescribeDataIngestionJob' {jobId} -> jobId) (\s@DescribeDataIngestionJob' {} a -> s {jobId = a} :: DescribeDataIngestionJob)

instance Core.AWSRequest DescribeDataIngestionJob where
  type
    AWSResponse DescribeDataIngestionJob =
      DescribeDataIngestionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataIngestionJobResponse'
            Prelude.<$> (x Core..?> "IngestionInputConfiguration")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "DatasetArn")
            Prelude.<*> (x Core..?> "FailedReason")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataIngestionJob where
  hashWithSalt salt' DescribeDataIngestionJob' {..} =
    salt' `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeDataIngestionJob where
  rnf DescribeDataIngestionJob' {..} = Prelude.rnf jobId

instance Core.ToHeaders DescribeDataIngestionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.DescribeDataIngestionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDataIngestionJob where
  toJSON DescribeDataIngestionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeDataIngestionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDataIngestionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataIngestionJobResponse' smart constructor.
data DescribeDataIngestionJobResponse = DescribeDataIngestionJobResponse'
  { -- | Specifies the S3 location configuration for the data input for the data
    -- ingestion job.
    ingestionInputConfiguration :: Prelude.Maybe IngestionInputConfiguration,
    -- | Indicates the status of the @DataIngestionJob@ operation.
    status :: Prelude.Maybe IngestionJobStatus,
    -- | The Amazon Resource Name (ARN) of the dataset being used in the data
    -- ingestion job.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the reason for failure when a data ingestion job has failed.
    failedReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates the job ID of the data ingestion job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the data ingestion job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an IAM role with permission to access
    -- the data source being ingested.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataIngestionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestionInputConfiguration', 'describeDataIngestionJobResponse_ingestionInputConfiguration' - Specifies the S3 location configuration for the data input for the data
-- ingestion job.
--
-- 'status', 'describeDataIngestionJobResponse_status' - Indicates the status of the @DataIngestionJob@ operation.
--
-- 'datasetArn', 'describeDataIngestionJobResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset being used in the data
-- ingestion job.
--
-- 'failedReason', 'describeDataIngestionJobResponse_failedReason' - Specifies the reason for failure when a data ingestion job has failed.
--
-- 'jobId', 'describeDataIngestionJobResponse_jobId' - Indicates the job ID of the data ingestion job.
--
-- 'createdAt', 'describeDataIngestionJobResponse_createdAt' - The time at which the data ingestion job was created.
--
-- 'roleArn', 'describeDataIngestionJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source being ingested.
--
-- 'httpStatus', 'describeDataIngestionJobResponse_httpStatus' - The response's http status code.
newDescribeDataIngestionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataIngestionJobResponse
newDescribeDataIngestionJobResponse pHttpStatus_ =
  DescribeDataIngestionJobResponse'
    { ingestionInputConfiguration =
        Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      failedReason = Prelude.Nothing,
      jobId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the S3 location configuration for the data input for the data
-- ingestion job.
describeDataIngestionJobResponse_ingestionInputConfiguration :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe IngestionInputConfiguration)
describeDataIngestionJobResponse_ingestionInputConfiguration = Lens.lens (\DescribeDataIngestionJobResponse' {ingestionInputConfiguration} -> ingestionInputConfiguration) (\s@DescribeDataIngestionJobResponse' {} a -> s {ingestionInputConfiguration = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the status of the @DataIngestionJob@ operation.
describeDataIngestionJobResponse_status :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe IngestionJobStatus)
describeDataIngestionJobResponse_status = Lens.lens (\DescribeDataIngestionJobResponse' {status} -> status) (\s@DescribeDataIngestionJobResponse' {} a -> s {status = a} :: DescribeDataIngestionJobResponse)

-- | The Amazon Resource Name (ARN) of the dataset being used in the data
-- ingestion job.
describeDataIngestionJobResponse_datasetArn :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_datasetArn = Lens.lens (\DescribeDataIngestionJobResponse' {datasetArn} -> datasetArn) (\s@DescribeDataIngestionJobResponse' {} a -> s {datasetArn = a} :: DescribeDataIngestionJobResponse)

-- | Specifies the reason for failure when a data ingestion job has failed.
describeDataIngestionJobResponse_failedReason :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_failedReason = Lens.lens (\DescribeDataIngestionJobResponse' {failedReason} -> failedReason) (\s@DescribeDataIngestionJobResponse' {} a -> s {failedReason = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the job ID of the data ingestion job.
describeDataIngestionJobResponse_jobId :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_jobId = Lens.lens (\DescribeDataIngestionJobResponse' {jobId} -> jobId) (\s@DescribeDataIngestionJobResponse' {} a -> s {jobId = a} :: DescribeDataIngestionJobResponse)

-- | The time at which the data ingestion job was created.
describeDataIngestionJobResponse_createdAt :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDataIngestionJobResponse_createdAt = Lens.lens (\DescribeDataIngestionJobResponse' {createdAt} -> createdAt) (\s@DescribeDataIngestionJobResponse' {} a -> s {createdAt = a} :: DescribeDataIngestionJobResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source being ingested.
describeDataIngestionJobResponse_roleArn :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_roleArn = Lens.lens (\DescribeDataIngestionJobResponse' {roleArn} -> roleArn) (\s@DescribeDataIngestionJobResponse' {} a -> s {roleArn = a} :: DescribeDataIngestionJobResponse)

-- | The response's http status code.
describeDataIngestionJobResponse_httpStatus :: Lens.Lens' DescribeDataIngestionJobResponse Prelude.Int
describeDataIngestionJobResponse_httpStatus = Lens.lens (\DescribeDataIngestionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDataIngestionJobResponse' {} a -> s {httpStatus = a} :: DescribeDataIngestionJobResponse)

instance
  Prelude.NFData
    DescribeDataIngestionJobResponse
  where
  rnf DescribeDataIngestionJobResponse' {..} =
    Prelude.rnf ingestionInputConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf failedReason
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf status
