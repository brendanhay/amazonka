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
-- Module      : Amazonka.Forecast.DescribeDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon Forecast dataset created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation.
--
-- In addition to listing the parameters specified in the @CreateDataset@
-- request, this operation includes the following dataset properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Status@
module Amazonka.Forecast.DescribeDataset
  ( -- * Creating a Request
    DescribeDataset (..),
    newDescribeDataset,

    -- * Request Lenses
    describeDataset_datasetArn,

    -- * Destructuring the Response
    DescribeDatasetResponse (..),
    newDescribeDatasetResponse,

    -- * Response Lenses
    describeDatasetResponse_creationTime,
    describeDatasetResponse_dataFrequency,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_datasetType,
    describeDatasetResponse_domain,
    describeDatasetResponse_encryptionConfig,
    describeDatasetResponse_lastModificationTime,
    describeDatasetResponse_schema,
    describeDatasetResponse_status,
    describeDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Text
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
-- 'datasetArn', 'describeDataset_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
newDescribeDataset ::
  -- | 'datasetArn'
  Prelude.Text ->
  DescribeDataset
newDescribeDataset pDatasetArn_ =
  DescribeDataset' {datasetArn = pDatasetArn_}

-- | The Amazon Resource Name (ARN) of the dataset.
describeDataset_datasetArn :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_datasetArn = Lens.lens (\DescribeDataset' {datasetArn} -> datasetArn) (\s@DescribeDataset' {} a -> s {datasetArn = a} :: DescribeDataset)

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
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DataFrequency")
            Prelude.<*> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "DatasetName")
            Prelude.<*> (x Data..?> "DatasetType")
            Prelude.<*> (x Data..?> "Domain")
            Prelude.<*> (x Data..?> "EncryptionConfig")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Schema")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataset where
  hashWithSalt _salt DescribeDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData DescribeDataset where
  rnf DescribeDataset' {..} = Prelude.rnf datasetArn

instance Data.ToHeaders DescribeDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataset where
  toJSON DescribeDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatasetArn" Data..= datasetArn)]
      )

instance Data.ToPath DescribeDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | When the dataset was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The frequency of data collection.
    --
    -- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
    -- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
    -- minutes), and 1min (1 minute). For example, \"M\" indicates every month
    -- and \"30min\" indicates every 30 minutes.
    dataFrequency :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The dataset type.
    datasetType :: Prelude.Maybe DatasetType,
    -- | The domain associated with the dataset.
    domain :: Prelude.Maybe Domain,
    -- | The AWS Key Management Service (KMS) key and the AWS Identity and Access
    -- Management (IAM) role that Amazon Forecast can assume to access the key.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | When you create a dataset, @LastModificationTime@ is the same as
    -- @CreationTime@. While data is being imported to the dataset,
    -- @LastModificationTime@ is the current time of the @DescribeDataset@
    -- call. After a
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
    -- operation has finished, @LastModificationTime@ is when the import job
    -- completed or failed.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | An array of @SchemaAttribute@ objects that specify the dataset fields.
    -- Each @SchemaAttribute@ specifies the name and data type of a field.
    schema :: Prelude.Maybe Schema,
    -- | The status of the dataset. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
    --
    -- The @UPDATE@ states apply while data is imported to the dataset from a
    -- call to the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
    -- operation and reflect the status of the dataset import job. For example,
    -- when the import job status is @CREATE_IN_PROGRESS@, the status of the
    -- dataset is @UPDATE_IN_PROGRESS@.
    --
    -- The @Status@ of the dataset must be @ACTIVE@ before you can import
    -- training data.
    status :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'describeDatasetResponse_creationTime' - When the dataset was created.
--
-- 'dataFrequency', 'describeDatasetResponse_dataFrequency' - The frequency of data collection.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"M\" indicates every month
-- and \"30min\" indicates every 30 minutes.
--
-- 'datasetArn', 'describeDatasetResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'datasetName', 'describeDatasetResponse_datasetName' - The name of the dataset.
--
-- 'datasetType', 'describeDatasetResponse_datasetType' - The dataset type.
--
-- 'domain', 'describeDatasetResponse_domain' - The domain associated with the dataset.
--
-- 'encryptionConfig', 'describeDatasetResponse_encryptionConfig' - The AWS Key Management Service (KMS) key and the AWS Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
--
-- 'lastModificationTime', 'describeDatasetResponse_lastModificationTime' - When you create a dataset, @LastModificationTime@ is the same as
-- @CreationTime@. While data is being imported to the dataset,
-- @LastModificationTime@ is the current time of the @DescribeDataset@
-- call. After a
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation has finished, @LastModificationTime@ is when the import job
-- completed or failed.
--
-- 'schema', 'describeDatasetResponse_schema' - An array of @SchemaAttribute@ objects that specify the dataset fields.
-- Each @SchemaAttribute@ specifies the name and data type of a field.
--
-- 'status', 'describeDatasetResponse_status' - The status of the dataset. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
--
-- The @UPDATE@ states apply while data is imported to the dataset from a
-- call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation and reflect the status of the dataset import job. For example,
-- when the import job status is @CREATE_IN_PROGRESS@, the status of the
-- dataset is @UPDATE_IN_PROGRESS@.
--
-- The @Status@ of the dataset must be @ACTIVE@ before you can import
-- training data.
--
-- 'httpStatus', 'describeDatasetResponse_httpStatus' - The response's http status code.
newDescribeDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetResponse
newDescribeDatasetResponse pHttpStatus_ =
  DescribeDatasetResponse'
    { creationTime =
        Prelude.Nothing,
      dataFrequency = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      domain = Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      schema = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the dataset was created.
describeDatasetResponse_creationTime :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_creationTime = Lens.lens (\DescribeDatasetResponse' {creationTime} -> creationTime) (\s@DescribeDatasetResponse' {} a -> s {creationTime = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | The frequency of data collection.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"M\" indicates every month
-- and \"30min\" indicates every 30 minutes.
describeDatasetResponse_dataFrequency :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_dataFrequency = Lens.lens (\DescribeDatasetResponse' {dataFrequency} -> dataFrequency) (\s@DescribeDatasetResponse' {} a -> s {dataFrequency = a} :: DescribeDatasetResponse)

-- | The Amazon Resource Name (ARN) of the dataset.
describeDatasetResponse_datasetArn :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_datasetArn = Lens.lens (\DescribeDatasetResponse' {datasetArn} -> datasetArn) (\s@DescribeDatasetResponse' {} a -> s {datasetArn = a} :: DescribeDatasetResponse)

-- | The name of the dataset.
describeDatasetResponse_datasetName :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_datasetName = Lens.lens (\DescribeDatasetResponse' {datasetName} -> datasetName) (\s@DescribeDatasetResponse' {} a -> s {datasetName = a} :: DescribeDatasetResponse)

-- | The dataset type.
describeDatasetResponse_datasetType :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe DatasetType)
describeDatasetResponse_datasetType = Lens.lens (\DescribeDatasetResponse' {datasetType} -> datasetType) (\s@DescribeDatasetResponse' {} a -> s {datasetType = a} :: DescribeDatasetResponse)

-- | The domain associated with the dataset.
describeDatasetResponse_domain :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Domain)
describeDatasetResponse_domain = Lens.lens (\DescribeDatasetResponse' {domain} -> domain) (\s@DescribeDatasetResponse' {} a -> s {domain = a} :: DescribeDatasetResponse)

-- | The AWS Key Management Service (KMS) key and the AWS Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
describeDatasetResponse_encryptionConfig :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe EncryptionConfig)
describeDatasetResponse_encryptionConfig = Lens.lens (\DescribeDatasetResponse' {encryptionConfig} -> encryptionConfig) (\s@DescribeDatasetResponse' {} a -> s {encryptionConfig = a} :: DescribeDatasetResponse)

-- | When you create a dataset, @LastModificationTime@ is the same as
-- @CreationTime@. While data is being imported to the dataset,
-- @LastModificationTime@ is the current time of the @DescribeDataset@
-- call. After a
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation has finished, @LastModificationTime@ is when the import job
-- completed or failed.
describeDatasetResponse_lastModificationTime :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_lastModificationTime = Lens.lens (\DescribeDatasetResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeDatasetResponse' {} a -> s {lastModificationTime = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Data._Time

-- | An array of @SchemaAttribute@ objects that specify the dataset fields.
-- Each @SchemaAttribute@ specifies the name and data type of a field.
describeDatasetResponse_schema :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Schema)
describeDatasetResponse_schema = Lens.lens (\DescribeDatasetResponse' {schema} -> schema) (\s@DescribeDatasetResponse' {} a -> s {schema = a} :: DescribeDatasetResponse)

-- | The status of the dataset. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
--
-- The @UPDATE@ states apply while data is imported to the dataset from a
-- call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation and reflect the status of the dataset import job. For example,
-- when the import job status is @CREATE_IN_PROGRESS@, the status of the
-- dataset is @UPDATE_IN_PROGRESS@.
--
-- The @Status@ of the dataset must be @ACTIVE@ before you can import
-- training data.
describeDatasetResponse_status :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_status = Lens.lens (\DescribeDatasetResponse' {status} -> status) (\s@DescribeDatasetResponse' {} a -> s {status = a} :: DescribeDatasetResponse)

-- | The response's http status code.
describeDatasetResponse_httpStatus :: Lens.Lens' DescribeDatasetResponse Prelude.Int
describeDatasetResponse_httpStatus = Lens.lens (\DescribeDatasetResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetResponse' {} a -> s {httpStatus = a} :: DescribeDatasetResponse)

instance Prelude.NFData DescribeDatasetResponse where
  rnf DescribeDatasetResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataFrequency
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
