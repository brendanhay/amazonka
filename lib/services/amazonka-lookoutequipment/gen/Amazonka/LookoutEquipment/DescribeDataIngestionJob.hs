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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information on a specific data ingestion job such as creation
-- time, dataset ARN, and status.
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
    describeDataIngestionJobResponse_createdAt,
    describeDataIngestionJobResponse_dataEndTime,
    describeDataIngestionJobResponse_dataQualitySummary,
    describeDataIngestionJobResponse_dataStartTime,
    describeDataIngestionJobResponse_datasetArn,
    describeDataIngestionJobResponse_failedReason,
    describeDataIngestionJobResponse_ingestedDataSize,
    describeDataIngestionJobResponse_ingestedFilesSummary,
    describeDataIngestionJobResponse_ingestionInputConfiguration,
    describeDataIngestionJobResponse_jobId,
    describeDataIngestionJobResponse_roleArn,
    describeDataIngestionJobResponse_status,
    describeDataIngestionJobResponse_statusDetail,
    describeDataIngestionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataIngestionJobResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "DataEndTime")
            Prelude.<*> (x Data..?> "DataQualitySummary")
            Prelude.<*> (x Data..?> "DataStartTime")
            Prelude.<*> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "FailedReason")
            Prelude.<*> (x Data..?> "IngestedDataSize")
            Prelude.<*> (x Data..?> "IngestedFilesSummary")
            Prelude.<*> (x Data..?> "IngestionInputConfiguration")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataIngestionJob where
  hashWithSalt _salt DescribeDataIngestionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeDataIngestionJob where
  rnf DescribeDataIngestionJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DescribeDataIngestionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DescribeDataIngestionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataIngestionJob where
  toJSON DescribeDataIngestionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeDataIngestionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataIngestionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataIngestionJobResponse' smart constructor.
data DescribeDataIngestionJobResponse = DescribeDataIngestionJobResponse'
  { -- | The time at which the data ingestion job was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Indicates the latest timestamp corresponding to data that was
    -- successfully ingested during this specific ingestion job.
    dataEndTime :: Prelude.Maybe Data.POSIX,
    -- | Gives statistics about a completed ingestion job. These statistics
    -- primarily relate to quantifying incorrect data such as
    -- MissingCompleteSensorData, MissingSensorData, UnsupportedDateFormats,
    -- InsufficientSensorData, and DuplicateTimeStamps.
    dataQualitySummary :: Prelude.Maybe DataQualitySummary,
    -- | Indicates the earliest timestamp corresponding to data that was
    -- successfully ingested during this specific ingestion job.
    dataStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset being used in the data
    -- ingestion job.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the reason for failure when a data ingestion job has failed.
    failedReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates the size of the ingested dataset.
    ingestedDataSize :: Prelude.Maybe Prelude.Natural,
    ingestedFilesSummary :: Prelude.Maybe IngestedFilesSummary,
    -- | Specifies the S3 location configuration for the data input for the data
    -- ingestion job.
    ingestionInputConfiguration :: Prelude.Maybe IngestionInputConfiguration,
    -- | Indicates the job ID of the data ingestion job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role with permission to access
    -- the data source being ingested.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the @DataIngestionJob@ operation.
    status :: Prelude.Maybe IngestionJobStatus,
    -- | Provides details about status of the ingestion job that is currently in
    -- progress.
    statusDetail :: Prelude.Maybe Prelude.Text,
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
-- 'createdAt', 'describeDataIngestionJobResponse_createdAt' - The time at which the data ingestion job was created.
--
-- 'dataEndTime', 'describeDataIngestionJobResponse_dataEndTime' - Indicates the latest timestamp corresponding to data that was
-- successfully ingested during this specific ingestion job.
--
-- 'dataQualitySummary', 'describeDataIngestionJobResponse_dataQualitySummary' - Gives statistics about a completed ingestion job. These statistics
-- primarily relate to quantifying incorrect data such as
-- MissingCompleteSensorData, MissingSensorData, UnsupportedDateFormats,
-- InsufficientSensorData, and DuplicateTimeStamps.
--
-- 'dataStartTime', 'describeDataIngestionJobResponse_dataStartTime' - Indicates the earliest timestamp corresponding to data that was
-- successfully ingested during this specific ingestion job.
--
-- 'datasetArn', 'describeDataIngestionJobResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset being used in the data
-- ingestion job.
--
-- 'failedReason', 'describeDataIngestionJobResponse_failedReason' - Specifies the reason for failure when a data ingestion job has failed.
--
-- 'ingestedDataSize', 'describeDataIngestionJobResponse_ingestedDataSize' - Indicates the size of the ingested dataset.
--
-- 'ingestedFilesSummary', 'describeDataIngestionJobResponse_ingestedFilesSummary' - Undocumented member.
--
-- 'ingestionInputConfiguration', 'describeDataIngestionJobResponse_ingestionInputConfiguration' - Specifies the S3 location configuration for the data input for the data
-- ingestion job.
--
-- 'jobId', 'describeDataIngestionJobResponse_jobId' - Indicates the job ID of the data ingestion job.
--
-- 'roleArn', 'describeDataIngestionJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source being ingested.
--
-- 'status', 'describeDataIngestionJobResponse_status' - Indicates the status of the @DataIngestionJob@ operation.
--
-- 'statusDetail', 'describeDataIngestionJobResponse_statusDetail' - Provides details about status of the ingestion job that is currently in
-- progress.
--
-- 'httpStatus', 'describeDataIngestionJobResponse_httpStatus' - The response's http status code.
newDescribeDataIngestionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataIngestionJobResponse
newDescribeDataIngestionJobResponse pHttpStatus_ =
  DescribeDataIngestionJobResponse'
    { createdAt =
        Prelude.Nothing,
      dataEndTime = Prelude.Nothing,
      dataQualitySummary = Prelude.Nothing,
      dataStartTime = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      failedReason = Prelude.Nothing,
      ingestedDataSize = Prelude.Nothing,
      ingestedFilesSummary = Prelude.Nothing,
      ingestionInputConfiguration =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the data ingestion job was created.
describeDataIngestionJobResponse_createdAt :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDataIngestionJobResponse_createdAt = Lens.lens (\DescribeDataIngestionJobResponse' {createdAt} -> createdAt) (\s@DescribeDataIngestionJobResponse' {} a -> s {createdAt = a} :: DescribeDataIngestionJobResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates the latest timestamp corresponding to data that was
-- successfully ingested during this specific ingestion job.
describeDataIngestionJobResponse_dataEndTime :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDataIngestionJobResponse_dataEndTime = Lens.lens (\DescribeDataIngestionJobResponse' {dataEndTime} -> dataEndTime) (\s@DescribeDataIngestionJobResponse' {} a -> s {dataEndTime = a} :: DescribeDataIngestionJobResponse) Prelude.. Lens.mapping Data._Time

-- | Gives statistics about a completed ingestion job. These statistics
-- primarily relate to quantifying incorrect data such as
-- MissingCompleteSensorData, MissingSensorData, UnsupportedDateFormats,
-- InsufficientSensorData, and DuplicateTimeStamps.
describeDataIngestionJobResponse_dataQualitySummary :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe DataQualitySummary)
describeDataIngestionJobResponse_dataQualitySummary = Lens.lens (\DescribeDataIngestionJobResponse' {dataQualitySummary} -> dataQualitySummary) (\s@DescribeDataIngestionJobResponse' {} a -> s {dataQualitySummary = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the earliest timestamp corresponding to data that was
-- successfully ingested during this specific ingestion job.
describeDataIngestionJobResponse_dataStartTime :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDataIngestionJobResponse_dataStartTime = Lens.lens (\DescribeDataIngestionJobResponse' {dataStartTime} -> dataStartTime) (\s@DescribeDataIngestionJobResponse' {} a -> s {dataStartTime = a} :: DescribeDataIngestionJobResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset being used in the data
-- ingestion job.
describeDataIngestionJobResponse_datasetArn :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_datasetArn = Lens.lens (\DescribeDataIngestionJobResponse' {datasetArn} -> datasetArn) (\s@DescribeDataIngestionJobResponse' {} a -> s {datasetArn = a} :: DescribeDataIngestionJobResponse)

-- | Specifies the reason for failure when a data ingestion job has failed.
describeDataIngestionJobResponse_failedReason :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_failedReason = Lens.lens (\DescribeDataIngestionJobResponse' {failedReason} -> failedReason) (\s@DescribeDataIngestionJobResponse' {} a -> s {failedReason = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the size of the ingested dataset.
describeDataIngestionJobResponse_ingestedDataSize :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Natural)
describeDataIngestionJobResponse_ingestedDataSize = Lens.lens (\DescribeDataIngestionJobResponse' {ingestedDataSize} -> ingestedDataSize) (\s@DescribeDataIngestionJobResponse' {} a -> s {ingestedDataSize = a} :: DescribeDataIngestionJobResponse)

-- | Undocumented member.
describeDataIngestionJobResponse_ingestedFilesSummary :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe IngestedFilesSummary)
describeDataIngestionJobResponse_ingestedFilesSummary = Lens.lens (\DescribeDataIngestionJobResponse' {ingestedFilesSummary} -> ingestedFilesSummary) (\s@DescribeDataIngestionJobResponse' {} a -> s {ingestedFilesSummary = a} :: DescribeDataIngestionJobResponse)

-- | Specifies the S3 location configuration for the data input for the data
-- ingestion job.
describeDataIngestionJobResponse_ingestionInputConfiguration :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe IngestionInputConfiguration)
describeDataIngestionJobResponse_ingestionInputConfiguration = Lens.lens (\DescribeDataIngestionJobResponse' {ingestionInputConfiguration} -> ingestionInputConfiguration) (\s@DescribeDataIngestionJobResponse' {} a -> s {ingestionInputConfiguration = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the job ID of the data ingestion job.
describeDataIngestionJobResponse_jobId :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_jobId = Lens.lens (\DescribeDataIngestionJobResponse' {jobId} -> jobId) (\s@DescribeDataIngestionJobResponse' {} a -> s {jobId = a} :: DescribeDataIngestionJobResponse)

-- | The Amazon Resource Name (ARN) of an IAM role with permission to access
-- the data source being ingested.
describeDataIngestionJobResponse_roleArn :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_roleArn = Lens.lens (\DescribeDataIngestionJobResponse' {roleArn} -> roleArn) (\s@DescribeDataIngestionJobResponse' {} a -> s {roleArn = a} :: DescribeDataIngestionJobResponse)

-- | Indicates the status of the @DataIngestionJob@ operation.
describeDataIngestionJobResponse_status :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe IngestionJobStatus)
describeDataIngestionJobResponse_status = Lens.lens (\DescribeDataIngestionJobResponse' {status} -> status) (\s@DescribeDataIngestionJobResponse' {} a -> s {status = a} :: DescribeDataIngestionJobResponse)

-- | Provides details about status of the ingestion job that is currently in
-- progress.
describeDataIngestionJobResponse_statusDetail :: Lens.Lens' DescribeDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
describeDataIngestionJobResponse_statusDetail = Lens.lens (\DescribeDataIngestionJobResponse' {statusDetail} -> statusDetail) (\s@DescribeDataIngestionJobResponse' {} a -> s {statusDetail = a} :: DescribeDataIngestionJobResponse)

-- | The response's http status code.
describeDataIngestionJobResponse_httpStatus :: Lens.Lens' DescribeDataIngestionJobResponse Prelude.Int
describeDataIngestionJobResponse_httpStatus = Lens.lens (\DescribeDataIngestionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDataIngestionJobResponse' {} a -> s {httpStatus = a} :: DescribeDataIngestionJobResponse)

instance
  Prelude.NFData
    DescribeDataIngestionJobResponse
  where
  rnf DescribeDataIngestionJobResponse' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf dataEndTime `Prelude.seq`
        Prelude.rnf dataQualitySummary `Prelude.seq`
          Prelude.rnf dataStartTime `Prelude.seq`
            Prelude.rnf datasetArn `Prelude.seq`
              Prelude.rnf failedReason `Prelude.seq`
                Prelude.rnf ingestedDataSize `Prelude.seq`
                  Prelude.rnf ingestedFilesSummary `Prelude.seq`
                    Prelude.rnf ingestionInputConfiguration `Prelude.seq`
                      Prelude.rnf jobId `Prelude.seq`
                        Prelude.rnf roleArn `Prelude.seq`
                          Prelude.rnf status `Prelude.seq`
                            Prelude.rnf statusDetail `Prelude.seq`
                              Prelude.rnf httpStatus
