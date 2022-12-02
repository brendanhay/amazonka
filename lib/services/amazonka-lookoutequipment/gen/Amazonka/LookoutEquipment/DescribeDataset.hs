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
-- Module      : Amazonka.LookoutEquipment.DescribeDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a JSON description of the data in each time series dataset,
-- including names, column names, and data types.
module Amazonka.LookoutEquipment.DescribeDataset
  ( -- * Creating a Request
    DescribeDataset (..),
    newDescribeDataset,

    -- * Request Lenses
    describeDataset_datasetName,

    -- * Destructuring the Response
    DescribeDatasetResponse (..),
    newDescribeDatasetResponse,

    -- * Response Lenses
    describeDatasetResponse_dataStartTime,
    describeDatasetResponse_serverSideKmsKeyId,
    describeDatasetResponse_roleArn,
    describeDatasetResponse_lastUpdatedAt,
    describeDatasetResponse_dataEndTime,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_status,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_ingestionInputConfiguration,
    describeDatasetResponse_ingestedFilesSummary,
    describeDatasetResponse_schema,
    describeDatasetResponse_dataQualitySummary,
    describeDatasetResponse_createdAt,
    describeDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | The name of the dataset to be described.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetName', 'describeDataset_datasetName' - The name of the dataset to be described.
newDescribeDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  DescribeDataset
newDescribeDataset pDatasetName_ =
  DescribeDataset' {datasetName = pDatasetName_}

-- | The name of the dataset to be described.
describeDataset_datasetName :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_datasetName = Lens.lens (\DescribeDataset' {datasetName} -> datasetName) (\s@DescribeDataset' {} a -> s {datasetName = a} :: DescribeDataset)

instance Core.AWSRequest DescribeDataset where
  type
    AWSResponse DescribeDataset =
      DescribeDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Prelude.<$> (x Data..?> "DataStartTime")
            Prelude.<*> (x Data..?> "ServerSideKmsKeyId")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "DataEndTime")
            Prelude.<*> (x Data..?> "DatasetName")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "IngestionInputConfiguration")
            Prelude.<*> (x Data..?> "IngestedFilesSummary")
            Prelude.<*> (x Data..?> "Schema")
            Prelude.<*> (x Data..?> "DataQualitySummary")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataset where
  hashWithSalt _salt DescribeDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetName

instance Prelude.NFData DescribeDataset where
  rnf DescribeDataset' {..} = Prelude.rnf datasetName

instance Data.ToHeaders DescribeDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DescribeDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataset where
  toJSON DescribeDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatasetName" Data..= datasetName)]
      )

instance Data.ToPath DescribeDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | Indicates the earliest timestamp corresponding to data that was
    -- successfully ingested during the most recent ingestion of this
    -- particular dataset.
    dataStartTime :: Prelude.Maybe Data.POSIX,
    -- | Provides the identifier of the KMS key used to encrypt dataset data by
    -- Amazon Lookout for Equipment.
    serverSideKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that you are using for
    -- this the data ingestion job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time the dataset was last updated, if it was.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Indicates the latest timestamp corresponding to data that was
    -- successfully ingested during the most recent ingestion of this
    -- particular dataset.
    dataEndTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the dataset being described.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The Amazon Resource Name (ARN) of the dataset being described.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the S3 location configuration for the data input for the data
    -- ingestion job.
    ingestionInputConfiguration :: Prelude.Maybe IngestionInputConfiguration,
    -- | IngestedFilesSummary associated with the given dataset for the latest
    -- successful associated ingestion job id.
    ingestedFilesSummary :: Prelude.Maybe IngestedFilesSummary,
    -- | A JSON description of the data that is in each time series dataset,
    -- including names, column names, and data types.
    schema :: Prelude.Maybe Prelude.Text,
    -- | Gives statistics associated with the given dataset for the latest
    -- successful associated ingestion job id. These statistics primarily
    -- relate to quantifying incorrect data such as MissingCompleteSensorData,
    -- MissingSensorData, UnsupportedDateFormats, InsufficientSensorData, and
    -- DuplicateTimeStamps.
    dataQualitySummary :: Prelude.Maybe DataQualitySummary,
    -- | Specifies the time the dataset was created in Lookout for Equipment.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataStartTime', 'describeDatasetResponse_dataStartTime' - Indicates the earliest timestamp corresponding to data that was
-- successfully ingested during the most recent ingestion of this
-- particular dataset.
--
-- 'serverSideKmsKeyId', 'describeDatasetResponse_serverSideKmsKeyId' - Provides the identifier of the KMS key used to encrypt dataset data by
-- Amazon Lookout for Equipment.
--
-- 'roleArn', 'describeDatasetResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that you are using for
-- this the data ingestion job.
--
-- 'lastUpdatedAt', 'describeDatasetResponse_lastUpdatedAt' - Specifies the time the dataset was last updated, if it was.
--
-- 'dataEndTime', 'describeDatasetResponse_dataEndTime' - Indicates the latest timestamp corresponding to data that was
-- successfully ingested during the most recent ingestion of this
-- particular dataset.
--
-- 'datasetName', 'describeDatasetResponse_datasetName' - The name of the dataset being described.
--
-- 'status', 'describeDatasetResponse_status' - Indicates the status of the dataset.
--
-- 'datasetArn', 'describeDatasetResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset being described.
--
-- 'ingestionInputConfiguration', 'describeDatasetResponse_ingestionInputConfiguration' - Specifies the S3 location configuration for the data input for the data
-- ingestion job.
--
-- 'ingestedFilesSummary', 'describeDatasetResponse_ingestedFilesSummary' - IngestedFilesSummary associated with the given dataset for the latest
-- successful associated ingestion job id.
--
-- 'schema', 'describeDatasetResponse_schema' - A JSON description of the data that is in each time series dataset,
-- including names, column names, and data types.
--
-- 'dataQualitySummary', 'describeDatasetResponse_dataQualitySummary' - Gives statistics associated with the given dataset for the latest
-- successful associated ingestion job id. These statistics primarily
-- relate to quantifying incorrect data such as MissingCompleteSensorData,
-- MissingSensorData, UnsupportedDateFormats, InsufficientSensorData, and
-- DuplicateTimeStamps.
--
-- 'createdAt', 'describeDatasetResponse_createdAt' - Specifies the time the dataset was created in Lookout for Equipment.
--
-- 'httpStatus', 'describeDatasetResponse_httpStatus' - The response's http status code.
newDescribeDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetResponse
newDescribeDatasetResponse pHttpStatus_ =
  DescribeDatasetResponse'
    { dataStartTime =
        Prelude.Nothing,
      serverSideKmsKeyId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      dataEndTime = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      ingestionInputConfiguration = Prelude.Nothing,
      ingestedFilesSummary = Prelude.Nothing,
      schema = Prelude.Nothing,
      dataQualitySummary = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the earliest timestamp corresponding to data that was
-- successfully ingested during the most recent ingestion of this
-- particular dataset.
describeDatasetResponse_dataStartTime :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_dataStartTime = Lens.lens (\DescribeDatasetResponse' {dataStartTime} -> dataStartTime) (\s@DescribeDatasetResponse' {} a -> s {dataStartTime = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | Provides the identifier of the KMS key used to encrypt dataset data by
-- Amazon Lookout for Equipment.
describeDatasetResponse_serverSideKmsKeyId :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_serverSideKmsKeyId = Lens.lens (\DescribeDatasetResponse' {serverSideKmsKeyId} -> serverSideKmsKeyId) (\s@DescribeDatasetResponse' {} a -> s {serverSideKmsKeyId = a} :: DescribeDatasetResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you are using for
-- this the data ingestion job.
describeDatasetResponse_roleArn :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_roleArn = Lens.lens (\DescribeDatasetResponse' {roleArn} -> roleArn) (\s@DescribeDatasetResponse' {} a -> s {roleArn = a} :: DescribeDatasetResponse)

-- | Specifies the time the dataset was last updated, if it was.
describeDatasetResponse_lastUpdatedAt :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_lastUpdatedAt = Lens.lens (\DescribeDatasetResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeDatasetResponse' {} a -> s {lastUpdatedAt = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates the latest timestamp corresponding to data that was
-- successfully ingested during the most recent ingestion of this
-- particular dataset.
describeDatasetResponse_dataEndTime :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_dataEndTime = Lens.lens (\DescribeDatasetResponse' {dataEndTime} -> dataEndTime) (\s@DescribeDatasetResponse' {} a -> s {dataEndTime = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the dataset being described.
describeDatasetResponse_datasetName :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_datasetName = Lens.lens (\DescribeDatasetResponse' {datasetName} -> datasetName) (\s@DescribeDatasetResponse' {} a -> s {datasetName = a} :: DescribeDatasetResponse)

-- | Indicates the status of the dataset.
describeDatasetResponse_status :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe DatasetStatus)
describeDatasetResponse_status = Lens.lens (\DescribeDatasetResponse' {status} -> status) (\s@DescribeDatasetResponse' {} a -> s {status = a} :: DescribeDatasetResponse)

-- | The Amazon Resource Name (ARN) of the dataset being described.
describeDatasetResponse_datasetArn :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_datasetArn = Lens.lens (\DescribeDatasetResponse' {datasetArn} -> datasetArn) (\s@DescribeDatasetResponse' {} a -> s {datasetArn = a} :: DescribeDatasetResponse)

-- | Specifies the S3 location configuration for the data input for the data
-- ingestion job.
describeDatasetResponse_ingestionInputConfiguration :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe IngestionInputConfiguration)
describeDatasetResponse_ingestionInputConfiguration = Lens.lens (\DescribeDatasetResponse' {ingestionInputConfiguration} -> ingestionInputConfiguration) (\s@DescribeDatasetResponse' {} a -> s {ingestionInputConfiguration = a} :: DescribeDatasetResponse)

-- | IngestedFilesSummary associated with the given dataset for the latest
-- successful associated ingestion job id.
describeDatasetResponse_ingestedFilesSummary :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe IngestedFilesSummary)
describeDatasetResponse_ingestedFilesSummary = Lens.lens (\DescribeDatasetResponse' {ingestedFilesSummary} -> ingestedFilesSummary) (\s@DescribeDatasetResponse' {} a -> s {ingestedFilesSummary = a} :: DescribeDatasetResponse)

-- | A JSON description of the data that is in each time series dataset,
-- including names, column names, and data types.
describeDatasetResponse_schema :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_schema = Lens.lens (\DescribeDatasetResponse' {schema} -> schema) (\s@DescribeDatasetResponse' {} a -> s {schema = a} :: DescribeDatasetResponse)

-- | Gives statistics associated with the given dataset for the latest
-- successful associated ingestion job id. These statistics primarily
-- relate to quantifying incorrect data such as MissingCompleteSensorData,
-- MissingSensorData, UnsupportedDateFormats, InsufficientSensorData, and
-- DuplicateTimeStamps.
describeDatasetResponse_dataQualitySummary :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe DataQualitySummary)
describeDatasetResponse_dataQualitySummary = Lens.lens (\DescribeDatasetResponse' {dataQualitySummary} -> dataQualitySummary) (\s@DescribeDatasetResponse' {} a -> s {dataQualitySummary = a} :: DescribeDatasetResponse)

-- | Specifies the time the dataset was created in Lookout for Equipment.
describeDatasetResponse_createdAt :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_createdAt = Lens.lens (\DescribeDatasetResponse' {createdAt} -> createdAt) (\s@DescribeDatasetResponse' {} a -> s {createdAt = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeDatasetResponse_httpStatus :: Lens.Lens' DescribeDatasetResponse Prelude.Int
describeDatasetResponse_httpStatus = Lens.lens (\DescribeDatasetResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetResponse' {} a -> s {httpStatus = a} :: DescribeDatasetResponse)

instance Prelude.NFData DescribeDatasetResponse where
  rnf DescribeDatasetResponse' {..} =
    Prelude.rnf dataStartTime
      `Prelude.seq` Prelude.rnf serverSideKmsKeyId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf dataEndTime
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf ingestionInputConfiguration
      `Prelude.seq` Prelude.rnf ingestedFilesSummary
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf dataQualitySummary
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
