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
-- Module      : Network.AWS.Personalize.CreateDatasetExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- To get the status of the export job, call DescribeDatasetExportJob, and
-- specify the Amazon Resource Name (ARN) of the dataset export job. The
-- dataset export is complete when the status shows as ACTIVE. If the
-- status shows as CREATE FAILED, the response includes a @failureReason@
-- key, which describes why the job failed.
module Network.AWS.Personalize.CreateDatasetExportJob
  ( -- * Creating a Request
    CreateDatasetExportJob (..),
    newCreateDatasetExportJob,

    -- * Request Lenses
    createDatasetExportJob_ingestionMode,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDatasetExportJob' smart constructor.
data CreateDatasetExportJob = CreateDatasetExportJob'
  { -- | The data to export, based on how you imported the data. You can choose
    -- to export only @BULK@ data that you imported using a dataset import job,
    -- only @PUT@ data that you imported incrementally (using the console,
    -- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
    -- The default value is @PUT@.
    ingestionMode :: Prelude.Maybe IngestionMode,
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetExportJobResponse'
            Prelude.<$> (x Core..?> "datasetExportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetExportJob

instance Prelude.NFData CreateDatasetExportJob

instance Core.ToHeaders CreateDatasetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.CreateDatasetExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDatasetExportJob where
  toJSON CreateDatasetExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ingestionMode" Core..=) Prelude.<$> ingestionMode,
            Prelude.Just ("jobName" Core..= jobName),
            Prelude.Just ("datasetArn" Core..= datasetArn),
            Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("jobOutput" Core..= jobOutput)
          ]
      )

instance Core.ToPath CreateDatasetExportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDatasetExportJob where
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
