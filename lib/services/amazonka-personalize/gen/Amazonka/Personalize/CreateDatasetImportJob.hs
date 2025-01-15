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
-- Module      : Amazonka.Personalize.CreateDatasetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that imports training data from your data source (an
-- Amazon S3 bucket) to an Amazon Personalize dataset. To allow Amazon
-- Personalize to import the training data, you must specify an IAM service
-- role that has permission to read from the data source, as Amazon
-- Personalize makes a copy of your data and processes it internally. For
-- information on granting access to your Amazon S3 bucket, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/granting-personalize-s3-access.html Giving Amazon Personalize Access to Amazon S3 Resources>.
--
-- By default, a dataset import job replaces any existing data in the
-- dataset that you imported in bulk. To add new records without replacing
-- existing data, specify INCREMENTAL for the import mode in the
-- CreateDatasetImportJob operation.
--
-- __Status__
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- To get the status of the import job, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>,
-- providing the Amazon Resource Name (ARN) of the dataset import job. The
-- dataset import is complete when the status shows as ACTIVE. If the
-- status shows as CREATE FAILED, the response includes a @failureReason@
-- key, which describes why the job failed.
--
-- Importing takes time. You must wait until the status shows as ACTIVE
-- before training a model using the dataset.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListDatasetImportJobs.html ListDatasetImportJobs>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>
module Amazonka.Personalize.CreateDatasetImportJob
  ( -- * Creating a Request
    CreateDatasetImportJob (..),
    newCreateDatasetImportJob,

    -- * Request Lenses
    createDatasetImportJob_importMode,
    createDatasetImportJob_publishAttributionMetricsToS3,
    createDatasetImportJob_tags,
    createDatasetImportJob_jobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJob_roleArn,

    -- * Destructuring the Response
    CreateDatasetImportJobResponse (..),
    newCreateDatasetImportJobResponse,

    -- * Response Lenses
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetImportJob' smart constructor.
data CreateDatasetImportJob = CreateDatasetImportJob'
  { -- | Specify how to add the new records to an existing dataset. The default
    -- import mode is @FULL@. If you haven\'t imported bulk records into the
    -- dataset previously, you can only specify @FULL@.
    --
    -- -   Specify @FULL@ to overwrite all existing bulk data in your dataset.
    --     Data you imported individually is not replaced.
    --
    -- -   Specify @INCREMENTAL@ to append the new records to the existing data
    --     in your dataset. Amazon Personalize replaces any record with the
    --     same ID with the new one.
    importMode :: Prelude.Maybe ImportMode,
    -- | If you created a metric attribution, specify whether to publish metrics
    -- for this import job to Amazon S3
    publishAttributionMetricsToS3 :: Prelude.Maybe Prelude.Bool,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the dataset import job.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the dataset import job.
    jobName :: Prelude.Text,
    -- | The ARN of the dataset that receives the imported data.
    datasetArn :: Prelude.Text,
    -- | The Amazon S3 bucket that contains the training data to import.
    dataSource :: DataSource,
    -- | The ARN of the IAM role that has permissions to read from the Amazon S3
    -- data source.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importMode', 'createDatasetImportJob_importMode' - Specify how to add the new records to an existing dataset. The default
-- import mode is @FULL@. If you haven\'t imported bulk records into the
-- dataset previously, you can only specify @FULL@.
--
-- -   Specify @FULL@ to overwrite all existing bulk data in your dataset.
--     Data you imported individually is not replaced.
--
-- -   Specify @INCREMENTAL@ to append the new records to the existing data
--     in your dataset. Amazon Personalize replaces any record with the
--     same ID with the new one.
--
-- 'publishAttributionMetricsToS3', 'createDatasetImportJob_publishAttributionMetricsToS3' - If you created a metric attribution, specify whether to publish metrics
-- for this import job to Amazon S3
--
-- 'tags', 'createDatasetImportJob_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the dataset import job.
--
-- 'jobName', 'createDatasetImportJob_jobName' - The name for the dataset import job.
--
-- 'datasetArn', 'createDatasetImportJob_datasetArn' - The ARN of the dataset that receives the imported data.
--
-- 'dataSource', 'createDatasetImportJob_dataSource' - The Amazon S3 bucket that contains the training data to import.
--
-- 'roleArn', 'createDatasetImportJob_roleArn' - The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
newCreateDatasetImportJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'dataSource'
  DataSource ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateDatasetImportJob
newCreateDatasetImportJob
  pJobName_
  pDatasetArn_
  pDataSource_
  pRoleArn_ =
    CreateDatasetImportJob'
      { importMode =
          Prelude.Nothing,
        publishAttributionMetricsToS3 = Prelude.Nothing,
        tags = Prelude.Nothing,
        jobName = pJobName_,
        datasetArn = pDatasetArn_,
        dataSource = pDataSource_,
        roleArn = pRoleArn_
      }

-- | Specify how to add the new records to an existing dataset. The default
-- import mode is @FULL@. If you haven\'t imported bulk records into the
-- dataset previously, you can only specify @FULL@.
--
-- -   Specify @FULL@ to overwrite all existing bulk data in your dataset.
--     Data you imported individually is not replaced.
--
-- -   Specify @INCREMENTAL@ to append the new records to the existing data
--     in your dataset. Amazon Personalize replaces any record with the
--     same ID with the new one.
createDatasetImportJob_importMode :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe ImportMode)
createDatasetImportJob_importMode = Lens.lens (\CreateDatasetImportJob' {importMode} -> importMode) (\s@CreateDatasetImportJob' {} a -> s {importMode = a} :: CreateDatasetImportJob)

-- | If you created a metric attribution, specify whether to publish metrics
-- for this import job to Amazon S3
createDatasetImportJob_publishAttributionMetricsToS3 :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Bool)
createDatasetImportJob_publishAttributionMetricsToS3 = Lens.lens (\CreateDatasetImportJob' {publishAttributionMetricsToS3} -> publishAttributionMetricsToS3) (\s@CreateDatasetImportJob' {} a -> s {publishAttributionMetricsToS3 = a} :: CreateDatasetImportJob)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the dataset import job.
createDatasetImportJob_tags :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe [Tag])
createDatasetImportJob_tags = Lens.lens (\CreateDatasetImportJob' {tags} -> tags) (\s@CreateDatasetImportJob' {} a -> s {tags = a} :: CreateDatasetImportJob) Prelude.. Lens.mapping Lens.coerced

-- | The name for the dataset import job.
createDatasetImportJob_jobName :: Lens.Lens' CreateDatasetImportJob Prelude.Text
createDatasetImportJob_jobName = Lens.lens (\CreateDatasetImportJob' {jobName} -> jobName) (\s@CreateDatasetImportJob' {} a -> s {jobName = a} :: CreateDatasetImportJob)

-- | The ARN of the dataset that receives the imported data.
createDatasetImportJob_datasetArn :: Lens.Lens' CreateDatasetImportJob Prelude.Text
createDatasetImportJob_datasetArn = Lens.lens (\CreateDatasetImportJob' {datasetArn} -> datasetArn) (\s@CreateDatasetImportJob' {} a -> s {datasetArn = a} :: CreateDatasetImportJob)

-- | The Amazon S3 bucket that contains the training data to import.
createDatasetImportJob_dataSource :: Lens.Lens' CreateDatasetImportJob DataSource
createDatasetImportJob_dataSource = Lens.lens (\CreateDatasetImportJob' {dataSource} -> dataSource) (\s@CreateDatasetImportJob' {} a -> s {dataSource = a} :: CreateDatasetImportJob)

-- | The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
createDatasetImportJob_roleArn :: Lens.Lens' CreateDatasetImportJob Prelude.Text
createDatasetImportJob_roleArn = Lens.lens (\CreateDatasetImportJob' {roleArn} -> roleArn) (\s@CreateDatasetImportJob' {} a -> s {roleArn = a} :: CreateDatasetImportJob)

instance Core.AWSRequest CreateDatasetImportJob where
  type
    AWSResponse CreateDatasetImportJob =
      CreateDatasetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetImportJobResponse'
            Prelude.<$> (x Data..?> "datasetImportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetImportJob where
  hashWithSalt _salt CreateDatasetImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` publishAttributionMetricsToS3
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateDatasetImportJob where
  rnf CreateDatasetImportJob' {..} =
    Prelude.rnf importMode `Prelude.seq`
      Prelude.rnf publishAttributionMetricsToS3 `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf jobName `Prelude.seq`
            Prelude.rnf datasetArn `Prelude.seq`
              Prelude.rnf dataSource `Prelude.seq`
                Prelude.rnf roleArn

instance Data.ToHeaders CreateDatasetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateDatasetImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatasetImportJob where
  toJSON CreateDatasetImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("importMode" Data..=) Prelude.<$> importMode,
            ("publishAttributionMetricsToS3" Data..=)
              Prelude.<$> publishAttributionMetricsToS3,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just ("datasetArn" Data..= datasetArn),
            Prelude.Just ("dataSource" Data..= dataSource),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateDatasetImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDatasetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetImportJobResponse' smart constructor.
data CreateDatasetImportJobResponse = CreateDatasetImportJobResponse'
  { -- | The ARN of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'createDatasetImportJobResponse_datasetImportJobArn' - The ARN of the dataset import job.
--
-- 'httpStatus', 'createDatasetImportJobResponse_httpStatus' - The response's http status code.
newCreateDatasetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetImportJobResponse
newCreateDatasetImportJobResponse pHttpStatus_ =
  CreateDatasetImportJobResponse'
    { datasetImportJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the dataset import job.
createDatasetImportJobResponse_datasetImportJobArn :: Lens.Lens' CreateDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
createDatasetImportJobResponse_datasetImportJobArn = Lens.lens (\CreateDatasetImportJobResponse' {datasetImportJobArn} -> datasetImportJobArn) (\s@CreateDatasetImportJobResponse' {} a -> s {datasetImportJobArn = a} :: CreateDatasetImportJobResponse)

-- | The response's http status code.
createDatasetImportJobResponse_httpStatus :: Lens.Lens' CreateDatasetImportJobResponse Prelude.Int
createDatasetImportJobResponse_httpStatus = Lens.lens (\CreateDatasetImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetImportJobResponse' {} a -> s {httpStatus = a} :: CreateDatasetImportJobResponse)

instance
  Prelude.NFData
    CreateDatasetImportJobResponse
  where
  rnf CreateDatasetImportJobResponse' {..} =
    Prelude.rnf datasetImportJobArn `Prelude.seq`
      Prelude.rnf httpStatus
