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
-- Module      : Amazonka.IoTSiteWise.CreateBulkImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a job to ingest data to IoT SiteWise from Amazon S3. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/CreateBulkImportJob.html Create a bulk import job (CLI)>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- You must enable IoT SiteWise to export data to Amazon S3 before you
-- create a bulk import job. For more information about how to configure
-- storage settings, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_PutStorageConfiguration.html PutStorageConfiguration>.
module Amazonka.IoTSiteWise.CreateBulkImportJob
  ( -- * Creating a Request
    CreateBulkImportJob (..),
    newCreateBulkImportJob,

    -- * Request Lenses
    createBulkImportJob_jobName,
    createBulkImportJob_jobRoleArn,
    createBulkImportJob_files,
    createBulkImportJob_errorReportLocation,
    createBulkImportJob_jobConfiguration,

    -- * Destructuring the Response
    CreateBulkImportJobResponse (..),
    newCreateBulkImportJobResponse,

    -- * Response Lenses
    createBulkImportJobResponse_httpStatus,
    createBulkImportJobResponse_jobId,
    createBulkImportJobResponse_jobName,
    createBulkImportJobResponse_jobStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBulkImportJob' smart constructor.
data CreateBulkImportJob = CreateBulkImportJob'
  { -- | The unique name that helps identify the job request.
    jobName :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IAM role that allows IoT SiteWise to read Amazon S3 data.
    jobRoleArn :: Prelude.Text,
    -- | The files in the specified Amazon S3 bucket that contain your data.
    files :: [File],
    -- | The Amazon S3 destination where errors associated with the job creation
    -- request are saved.
    errorReportLocation :: ErrorReportLocation,
    -- | Contains the configuration information of a job, such as the file format
    -- used to save data in Amazon S3.
    jobConfiguration :: JobConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBulkImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'createBulkImportJob_jobName' - The unique name that helps identify the job request.
--
-- 'jobRoleArn', 'createBulkImportJob_jobRoleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows IoT SiteWise to read Amazon S3 data.
--
-- 'files', 'createBulkImportJob_files' - The files in the specified Amazon S3 bucket that contain your data.
--
-- 'errorReportLocation', 'createBulkImportJob_errorReportLocation' - The Amazon S3 destination where errors associated with the job creation
-- request are saved.
--
-- 'jobConfiguration', 'createBulkImportJob_jobConfiguration' - Contains the configuration information of a job, such as the file format
-- used to save data in Amazon S3.
newCreateBulkImportJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobRoleArn'
  Prelude.Text ->
  -- | 'errorReportLocation'
  ErrorReportLocation ->
  -- | 'jobConfiguration'
  JobConfiguration ->
  CreateBulkImportJob
newCreateBulkImportJob
  pJobName_
  pJobRoleArn_
  pErrorReportLocation_
  pJobConfiguration_ =
    CreateBulkImportJob'
      { jobName = pJobName_,
        jobRoleArn = pJobRoleArn_,
        files = Prelude.mempty,
        errorReportLocation = pErrorReportLocation_,
        jobConfiguration = pJobConfiguration_
      }

-- | The unique name that helps identify the job request.
createBulkImportJob_jobName :: Lens.Lens' CreateBulkImportJob Prelude.Text
createBulkImportJob_jobName = Lens.lens (\CreateBulkImportJob' {jobName} -> jobName) (\s@CreateBulkImportJob' {} a -> s {jobName = a} :: CreateBulkImportJob)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows IoT SiteWise to read Amazon S3 data.
createBulkImportJob_jobRoleArn :: Lens.Lens' CreateBulkImportJob Prelude.Text
createBulkImportJob_jobRoleArn = Lens.lens (\CreateBulkImportJob' {jobRoleArn} -> jobRoleArn) (\s@CreateBulkImportJob' {} a -> s {jobRoleArn = a} :: CreateBulkImportJob)

-- | The files in the specified Amazon S3 bucket that contain your data.
createBulkImportJob_files :: Lens.Lens' CreateBulkImportJob [File]
createBulkImportJob_files = Lens.lens (\CreateBulkImportJob' {files} -> files) (\s@CreateBulkImportJob' {} a -> s {files = a} :: CreateBulkImportJob) Prelude.. Lens.coerced

-- | The Amazon S3 destination where errors associated with the job creation
-- request are saved.
createBulkImportJob_errorReportLocation :: Lens.Lens' CreateBulkImportJob ErrorReportLocation
createBulkImportJob_errorReportLocation = Lens.lens (\CreateBulkImportJob' {errorReportLocation} -> errorReportLocation) (\s@CreateBulkImportJob' {} a -> s {errorReportLocation = a} :: CreateBulkImportJob)

-- | Contains the configuration information of a job, such as the file format
-- used to save data in Amazon S3.
createBulkImportJob_jobConfiguration :: Lens.Lens' CreateBulkImportJob JobConfiguration
createBulkImportJob_jobConfiguration = Lens.lens (\CreateBulkImportJob' {jobConfiguration} -> jobConfiguration) (\s@CreateBulkImportJob' {} a -> s {jobConfiguration = a} :: CreateBulkImportJob)

instance Core.AWSRequest CreateBulkImportJob where
  type
    AWSResponse CreateBulkImportJob =
      CreateBulkImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBulkImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobId")
            Prelude.<*> (x Data..:> "jobName")
            Prelude.<*> (x Data..:> "jobStatus")
      )

instance Prelude.Hashable CreateBulkImportJob where
  hashWithSalt _salt CreateBulkImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRoleArn
      `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` errorReportLocation
      `Prelude.hashWithSalt` jobConfiguration

instance Prelude.NFData CreateBulkImportJob where
  rnf CreateBulkImportJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRoleArn
      `Prelude.seq` Prelude.rnf files
      `Prelude.seq` Prelude.rnf errorReportLocation
      `Prelude.seq` Prelude.rnf jobConfiguration

instance Data.ToHeaders CreateBulkImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBulkImportJob where
  toJSON CreateBulkImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just ("jobRoleArn" Data..= jobRoleArn),
            Prelude.Just ("files" Data..= files),
            Prelude.Just
              ("errorReportLocation" Data..= errorReportLocation),
            Prelude.Just
              ("jobConfiguration" Data..= jobConfiguration)
          ]
      )

instance Data.ToPath CreateBulkImportJob where
  toPath = Prelude.const "/jobs"

instance Data.ToQuery CreateBulkImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBulkImportJobResponse' smart constructor.
data CreateBulkImportJobResponse = CreateBulkImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the job.
    jobId :: Prelude.Text,
    -- | The unique name that helps identify the job request.
    jobName :: Prelude.Text,
    -- | The status of the bulk import job can be one of following values.
    --
    -- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
    --     to finish.
    --
    -- -   @CANCELLED@ – The bulk import job has been canceled.
    --
    -- -   @RUNNING@ – IoT SiteWise is processing your request to import your
    --     data from Amazon S3.
    --
    -- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
    --     import data from Amazon S3.
    --
    -- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
    --     data from Amazon S3. You can use logs saved in the specified error
    --     report location in Amazon S3 to troubleshoot issues.
    --
    -- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
    --     import data from Amazon S3 with errors. You can use logs saved in
    --     the specified error report location in Amazon S3 to troubleshoot
    --     issues.
    jobStatus :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBulkImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBulkImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'createBulkImportJobResponse_jobId' - The ID of the job.
--
-- 'jobName', 'createBulkImportJobResponse_jobName' - The unique name that helps identify the job request.
--
-- 'jobStatus', 'createBulkImportJobResponse_jobStatus' - The status of the bulk import job can be one of following values.
--
-- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
--     to finish.
--
-- -   @CANCELLED@ – The bulk import job has been canceled.
--
-- -   @RUNNING@ – IoT SiteWise is processing your request to import your
--     data from Amazon S3.
--
-- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
--     import data from Amazon S3.
--
-- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
--     data from Amazon S3. You can use logs saved in the specified error
--     report location in Amazon S3 to troubleshoot issues.
--
-- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
--     import data from Amazon S3 with errors. You can use logs saved in
--     the specified error report location in Amazon S3 to troubleshoot
--     issues.
newCreateBulkImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  CreateBulkImportJobResponse
newCreateBulkImportJobResponse
  pHttpStatus_
  pJobId_
  pJobName_
  pJobStatus_ =
    CreateBulkImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_,
        jobName = pJobName_,
        jobStatus = pJobStatus_
      }

-- | The response's http status code.
createBulkImportJobResponse_httpStatus :: Lens.Lens' CreateBulkImportJobResponse Prelude.Int
createBulkImportJobResponse_httpStatus = Lens.lens (\CreateBulkImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateBulkImportJobResponse' {} a -> s {httpStatus = a} :: CreateBulkImportJobResponse)

-- | The ID of the job.
createBulkImportJobResponse_jobId :: Lens.Lens' CreateBulkImportJobResponse Prelude.Text
createBulkImportJobResponse_jobId = Lens.lens (\CreateBulkImportJobResponse' {jobId} -> jobId) (\s@CreateBulkImportJobResponse' {} a -> s {jobId = a} :: CreateBulkImportJobResponse)

-- | The unique name that helps identify the job request.
createBulkImportJobResponse_jobName :: Lens.Lens' CreateBulkImportJobResponse Prelude.Text
createBulkImportJobResponse_jobName = Lens.lens (\CreateBulkImportJobResponse' {jobName} -> jobName) (\s@CreateBulkImportJobResponse' {} a -> s {jobName = a} :: CreateBulkImportJobResponse)

-- | The status of the bulk import job can be one of following values.
--
-- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
--     to finish.
--
-- -   @CANCELLED@ – The bulk import job has been canceled.
--
-- -   @RUNNING@ – IoT SiteWise is processing your request to import your
--     data from Amazon S3.
--
-- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
--     import data from Amazon S3.
--
-- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
--     data from Amazon S3. You can use logs saved in the specified error
--     report location in Amazon S3 to troubleshoot issues.
--
-- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
--     import data from Amazon S3 with errors. You can use logs saved in
--     the specified error report location in Amazon S3 to troubleshoot
--     issues.
createBulkImportJobResponse_jobStatus :: Lens.Lens' CreateBulkImportJobResponse JobStatus
createBulkImportJobResponse_jobStatus = Lens.lens (\CreateBulkImportJobResponse' {jobStatus} -> jobStatus) (\s@CreateBulkImportJobResponse' {} a -> s {jobStatus = a} :: CreateBulkImportJobResponse)

instance Prelude.NFData CreateBulkImportJobResponse where
  rnf CreateBulkImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
