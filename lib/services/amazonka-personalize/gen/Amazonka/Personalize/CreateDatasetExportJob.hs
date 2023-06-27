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
-- Module      : Amazonka.Personalize.CreateDatasetExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that exports data from your dataset to an Amazon S3
-- bucket. To allow Amazon Personalize to export the training data, you
-- must specify an service-linked IAM role that gives Amazon Personalize
-- @PutObject@ permissions for your Amazon S3 bucket. For information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/export-data.html Exporting a dataset>
-- in the Amazon Personalize developer guide.
--
-- __Status__
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- To get the status of the export job, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetExportJob.html DescribeDatasetExportJob>,
-- and specify the Amazon Resource Name (ARN) of the dataset export job.
-- The dataset export is complete when the status shows as ACTIVE. If the
-- status shows as CREATE FAILED, the response includes a @failureReason@
-- key, which describes why the job failed.
module Amazonka.Personalize.CreateDatasetExportJob
  ( -- * Creating a Request
    CreateDatasetExportJob (..),
    newCreateDatasetExportJob,

    -- * Request Lenses
    createDatasetExportJob_ingestionMode,
    createDatasetExportJob_tags,
    createDatasetExportJob_jobName,
    createDatasetExportJob_datasetArn,
    createDatasetExportJob_roleArn,
    createDatasetExportJob_jobOutput,

    -- * Destructuring the Response
    CreateDatasetExportJobResponse (..),
    newCreateDatasetExportJobResponse,

    -- * Response Lenses
    createDatasetExportJobResponse_datasetExportJobArn,
    createDatasetExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetExportJob' smart constructor.
data CreateDatasetExportJob = CreateDatasetExportJob'
  { -- | The data to export, based on how you imported the data. You can choose
    -- to export only @BULK@ data that you imported using a dataset import job,
    -- only @PUT@ data that you imported incrementally (using the console,
    -- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
    -- The default value is @PUT@.
    ingestionMode :: Prelude.Maybe IngestionMode,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
    -- to apply to the dataset export job.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the dataset export job.
    jobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset that contains the data to
    -- export.
    datasetArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that has
    -- permissions to add data to your output Amazon S3 bucket.
    roleArn :: Prelude.Text,
    -- | The path to the Amazon S3 bucket where the job\'s output is stored.
    jobOutput :: DatasetExportJobOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestionMode', 'createDatasetExportJob_ingestionMode' - The data to export, based on how you imported the data. You can choose
-- to export only @BULK@ data that you imported using a dataset import job,
-- only @PUT@ data that you imported incrementally (using the console,
-- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
-- The default value is @PUT@.
--
-- 'tags', 'createDatasetExportJob_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
-- to apply to the dataset export job.
--
-- 'jobName', 'createDatasetExportJob_jobName' - The name for the dataset export job.
--
-- 'datasetArn', 'createDatasetExportJob_datasetArn' - The Amazon Resource Name (ARN) of the dataset that contains the data to
-- export.
--
-- 'roleArn', 'createDatasetExportJob_roleArn' - The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket.
--
-- 'jobOutput', 'createDatasetExportJob_jobOutput' - The path to the Amazon S3 bucket where the job\'s output is stored.
newCreateDatasetExportJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'jobOutput'
  DatasetExportJobOutput ->
  CreateDatasetExportJob
newCreateDatasetExportJob
  pJobName_
  pDatasetArn_
  pRoleArn_
  pJobOutput_ =
    CreateDatasetExportJob'
      { ingestionMode =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        jobName = pJobName_,
        datasetArn = pDatasetArn_,
        roleArn = pRoleArn_,
        jobOutput = pJobOutput_
      }

-- | The data to export, based on how you imported the data. You can choose
-- to export only @BULK@ data that you imported using a dataset import job,
-- only @PUT@ data that you imported incrementally (using the console,
-- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
-- The default value is @PUT@.
createDatasetExportJob_ingestionMode :: Lens.Lens' CreateDatasetExportJob (Prelude.Maybe IngestionMode)
createDatasetExportJob_ingestionMode = Lens.lens (\CreateDatasetExportJob' {ingestionMode} -> ingestionMode) (\s@CreateDatasetExportJob' {} a -> s {ingestionMode = a} :: CreateDatasetExportJob)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
-- to apply to the dataset export job.
createDatasetExportJob_tags :: Lens.Lens' CreateDatasetExportJob (Prelude.Maybe [Tag])
createDatasetExportJob_tags = Lens.lens (\CreateDatasetExportJob' {tags} -> tags) (\s@CreateDatasetExportJob' {} a -> s {tags = a} :: CreateDatasetExportJob) Prelude.. Lens.mapping Lens.coerced

-- | The name for the dataset export job.
createDatasetExportJob_jobName :: Lens.Lens' CreateDatasetExportJob Prelude.Text
createDatasetExportJob_jobName = Lens.lens (\CreateDatasetExportJob' {jobName} -> jobName) (\s@CreateDatasetExportJob' {} a -> s {jobName = a} :: CreateDatasetExportJob)

-- | The Amazon Resource Name (ARN) of the dataset that contains the data to
-- export.
createDatasetExportJob_datasetArn :: Lens.Lens' CreateDatasetExportJob Prelude.Text
createDatasetExportJob_datasetArn = Lens.lens (\CreateDatasetExportJob' {datasetArn} -> datasetArn) (\s@CreateDatasetExportJob' {} a -> s {datasetArn = a} :: CreateDatasetExportJob)

-- | The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket.
createDatasetExportJob_roleArn :: Lens.Lens' CreateDatasetExportJob Prelude.Text
createDatasetExportJob_roleArn = Lens.lens (\CreateDatasetExportJob' {roleArn} -> roleArn) (\s@CreateDatasetExportJob' {} a -> s {roleArn = a} :: CreateDatasetExportJob)

-- | The path to the Amazon S3 bucket where the job\'s output is stored.
createDatasetExportJob_jobOutput :: Lens.Lens' CreateDatasetExportJob DatasetExportJobOutput
createDatasetExportJob_jobOutput = Lens.lens (\CreateDatasetExportJob' {jobOutput} -> jobOutput) (\s@CreateDatasetExportJob' {} a -> s {jobOutput = a} :: CreateDatasetExportJob)

instance Core.AWSRequest CreateDatasetExportJob where
  type
    AWSResponse CreateDatasetExportJob =
      CreateDatasetExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetExportJobResponse'
            Prelude.<$> (x Data..?> "datasetExportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetExportJob where
  hashWithSalt _salt CreateDatasetExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` ingestionMode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` jobOutput

instance Prelude.NFData CreateDatasetExportJob where
  rnf CreateDatasetExportJob' {..} =
    Prelude.rnf ingestionMode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf jobOutput

instance Data.ToHeaders CreateDatasetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateDatasetExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatasetExportJob where
  toJSON CreateDatasetExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ingestionMode" Data..=) Prelude.<$> ingestionMode,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just ("datasetArn" Data..= datasetArn),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("jobOutput" Data..= jobOutput)
          ]
      )

instance Data.ToPath CreateDatasetExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDatasetExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetExportJobResponse' smart constructor.
data CreateDatasetExportJobResponse = CreateDatasetExportJobResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset export job.
    datasetExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetExportJobArn', 'createDatasetExportJobResponse_datasetExportJobArn' - The Amazon Resource Name (ARN) of the dataset export job.
--
-- 'httpStatus', 'createDatasetExportJobResponse_httpStatus' - The response's http status code.
newCreateDatasetExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetExportJobResponse
newCreateDatasetExportJobResponse pHttpStatus_ =
  CreateDatasetExportJobResponse'
    { datasetExportJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset export job.
createDatasetExportJobResponse_datasetExportJobArn :: Lens.Lens' CreateDatasetExportJobResponse (Prelude.Maybe Prelude.Text)
createDatasetExportJobResponse_datasetExportJobArn = Lens.lens (\CreateDatasetExportJobResponse' {datasetExportJobArn} -> datasetExportJobArn) (\s@CreateDatasetExportJobResponse' {} a -> s {datasetExportJobArn = a} :: CreateDatasetExportJobResponse)

-- | The response's http status code.
createDatasetExportJobResponse_httpStatus :: Lens.Lens' CreateDatasetExportJobResponse Prelude.Int
createDatasetExportJobResponse_httpStatus = Lens.lens (\CreateDatasetExportJobResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetExportJobResponse' {} a -> s {httpStatus = a} :: CreateDatasetExportJobResponse)

instance
  Prelude.NFData
    CreateDatasetExportJobResponse
  where
  rnf CreateDatasetExportJobResponse' {..} =
    Prelude.rnf datasetExportJobArn
      `Prelude.seq` Prelude.rnf httpStatus
