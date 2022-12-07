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
-- Module      : Amazonka.IoTSiteWise.DescribeBulkImportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a bulk import job request. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/DescribeBulkImportJob.html Describe a bulk import job (CLI)>
-- in the /Amazon Simple Storage Service User Guide/.
module Amazonka.IoTSiteWise.DescribeBulkImportJob
  ( -- * Creating a Request
    DescribeBulkImportJob (..),
    newDescribeBulkImportJob,

    -- * Request Lenses
    describeBulkImportJob_jobId,

    -- * Destructuring the Response
    DescribeBulkImportJobResponse (..),
    newDescribeBulkImportJobResponse,

    -- * Response Lenses
    describeBulkImportJobResponse_httpStatus,
    describeBulkImportJobResponse_jobId,
    describeBulkImportJobResponse_jobName,
    describeBulkImportJobResponse_jobStatus,
    describeBulkImportJobResponse_jobRoleArn,
    describeBulkImportJobResponse_files,
    describeBulkImportJobResponse_errorReportLocation,
    describeBulkImportJobResponse_jobConfiguration,
    describeBulkImportJobResponse_jobCreationDate,
    describeBulkImportJobResponse_jobLastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBulkImportJob' smart constructor.
data DescribeBulkImportJob = DescribeBulkImportJob'
  { -- | The ID of the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBulkImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeBulkImportJob_jobId' - The ID of the job.
newDescribeBulkImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeBulkImportJob
newDescribeBulkImportJob pJobId_ =
  DescribeBulkImportJob' {jobId = pJobId_}

-- | The ID of the job.
describeBulkImportJob_jobId :: Lens.Lens' DescribeBulkImportJob Prelude.Text
describeBulkImportJob_jobId = Lens.lens (\DescribeBulkImportJob' {jobId} -> jobId) (\s@DescribeBulkImportJob' {} a -> s {jobId = a} :: DescribeBulkImportJob)

instance Core.AWSRequest DescribeBulkImportJob where
  type
    AWSResponse DescribeBulkImportJob =
      DescribeBulkImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBulkImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobId")
            Prelude.<*> (x Data..:> "jobName")
            Prelude.<*> (x Data..:> "jobStatus")
            Prelude.<*> (x Data..:> "jobRoleArn")
            Prelude.<*> (x Data..?> "files" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "errorReportLocation")
            Prelude.<*> (x Data..:> "jobConfiguration")
            Prelude.<*> (x Data..:> "jobCreationDate")
            Prelude.<*> (x Data..:> "jobLastUpdateDate")
      )

instance Prelude.Hashable DescribeBulkImportJob where
  hashWithSalt _salt DescribeBulkImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeBulkImportJob where
  rnf DescribeBulkImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DescribeBulkImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeBulkImportJob where
  toPath DescribeBulkImportJob' {..} =
    Prelude.mconcat ["/jobs/", Data.toBS jobId]

instance Data.ToQuery DescribeBulkImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBulkImportJobResponse' smart constructor.
data DescribeBulkImportJobResponse = DescribeBulkImportJobResponse'
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
    jobStatus :: JobStatus,
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
    jobConfiguration :: JobConfiguration,
    -- | The date the job was created, in Unix epoch TIME.
    jobCreationDate :: Data.POSIX,
    -- | The date the job was last updated, in Unix epoch time.
    jobLastUpdateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBulkImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeBulkImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'describeBulkImportJobResponse_jobId' - The ID of the job.
--
-- 'jobName', 'describeBulkImportJobResponse_jobName' - The unique name that helps identify the job request.
--
-- 'jobStatus', 'describeBulkImportJobResponse_jobStatus' - The status of the bulk import job can be one of following values.
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
--
-- 'jobRoleArn', 'describeBulkImportJobResponse_jobRoleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows IoT SiteWise to read Amazon S3 data.
--
-- 'files', 'describeBulkImportJobResponse_files' - The files in the specified Amazon S3 bucket that contain your data.
--
-- 'errorReportLocation', 'describeBulkImportJobResponse_errorReportLocation' - The Amazon S3 destination where errors associated with the job creation
-- request are saved.
--
-- 'jobConfiguration', 'describeBulkImportJobResponse_jobConfiguration' - Contains the configuration information of a job, such as the file format
-- used to save data in Amazon S3.
--
-- 'jobCreationDate', 'describeBulkImportJobResponse_jobCreationDate' - The date the job was created, in Unix epoch TIME.
--
-- 'jobLastUpdateDate', 'describeBulkImportJobResponse_jobLastUpdateDate' - The date the job was last updated, in Unix epoch time.
newDescribeBulkImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  -- | 'jobRoleArn'
  Prelude.Text ->
  -- | 'errorReportLocation'
  ErrorReportLocation ->
  -- | 'jobConfiguration'
  JobConfiguration ->
  -- | 'jobCreationDate'
  Prelude.UTCTime ->
  -- | 'jobLastUpdateDate'
  Prelude.UTCTime ->
  DescribeBulkImportJobResponse
newDescribeBulkImportJobResponse
  pHttpStatus_
  pJobId_
  pJobName_
  pJobStatus_
  pJobRoleArn_
  pErrorReportLocation_
  pJobConfiguration_
  pJobCreationDate_
  pJobLastUpdateDate_ =
    DescribeBulkImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_,
        jobName = pJobName_,
        jobStatus = pJobStatus_,
        jobRoleArn = pJobRoleArn_,
        files = Prelude.mempty,
        errorReportLocation = pErrorReportLocation_,
        jobConfiguration = pJobConfiguration_,
        jobCreationDate =
          Data._Time Lens.# pJobCreationDate_,
        jobLastUpdateDate =
          Data._Time Lens.# pJobLastUpdateDate_
      }

-- | The response's http status code.
describeBulkImportJobResponse_httpStatus :: Lens.Lens' DescribeBulkImportJobResponse Prelude.Int
describeBulkImportJobResponse_httpStatus = Lens.lens (\DescribeBulkImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeBulkImportJobResponse' {} a -> s {httpStatus = a} :: DescribeBulkImportJobResponse)

-- | The ID of the job.
describeBulkImportJobResponse_jobId :: Lens.Lens' DescribeBulkImportJobResponse Prelude.Text
describeBulkImportJobResponse_jobId = Lens.lens (\DescribeBulkImportJobResponse' {jobId} -> jobId) (\s@DescribeBulkImportJobResponse' {} a -> s {jobId = a} :: DescribeBulkImportJobResponse)

-- | The unique name that helps identify the job request.
describeBulkImportJobResponse_jobName :: Lens.Lens' DescribeBulkImportJobResponse Prelude.Text
describeBulkImportJobResponse_jobName = Lens.lens (\DescribeBulkImportJobResponse' {jobName} -> jobName) (\s@DescribeBulkImportJobResponse' {} a -> s {jobName = a} :: DescribeBulkImportJobResponse)

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
describeBulkImportJobResponse_jobStatus :: Lens.Lens' DescribeBulkImportJobResponse JobStatus
describeBulkImportJobResponse_jobStatus = Lens.lens (\DescribeBulkImportJobResponse' {jobStatus} -> jobStatus) (\s@DescribeBulkImportJobResponse' {} a -> s {jobStatus = a} :: DescribeBulkImportJobResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows IoT SiteWise to read Amazon S3 data.
describeBulkImportJobResponse_jobRoleArn :: Lens.Lens' DescribeBulkImportJobResponse Prelude.Text
describeBulkImportJobResponse_jobRoleArn = Lens.lens (\DescribeBulkImportJobResponse' {jobRoleArn} -> jobRoleArn) (\s@DescribeBulkImportJobResponse' {} a -> s {jobRoleArn = a} :: DescribeBulkImportJobResponse)

-- | The files in the specified Amazon S3 bucket that contain your data.
describeBulkImportJobResponse_files :: Lens.Lens' DescribeBulkImportJobResponse [File]
describeBulkImportJobResponse_files = Lens.lens (\DescribeBulkImportJobResponse' {files} -> files) (\s@DescribeBulkImportJobResponse' {} a -> s {files = a} :: DescribeBulkImportJobResponse) Prelude.. Lens.coerced

-- | The Amazon S3 destination where errors associated with the job creation
-- request are saved.
describeBulkImportJobResponse_errorReportLocation :: Lens.Lens' DescribeBulkImportJobResponse ErrorReportLocation
describeBulkImportJobResponse_errorReportLocation = Lens.lens (\DescribeBulkImportJobResponse' {errorReportLocation} -> errorReportLocation) (\s@DescribeBulkImportJobResponse' {} a -> s {errorReportLocation = a} :: DescribeBulkImportJobResponse)

-- | Contains the configuration information of a job, such as the file format
-- used to save data in Amazon S3.
describeBulkImportJobResponse_jobConfiguration :: Lens.Lens' DescribeBulkImportJobResponse JobConfiguration
describeBulkImportJobResponse_jobConfiguration = Lens.lens (\DescribeBulkImportJobResponse' {jobConfiguration} -> jobConfiguration) (\s@DescribeBulkImportJobResponse' {} a -> s {jobConfiguration = a} :: DescribeBulkImportJobResponse)

-- | The date the job was created, in Unix epoch TIME.
describeBulkImportJobResponse_jobCreationDate :: Lens.Lens' DescribeBulkImportJobResponse Prelude.UTCTime
describeBulkImportJobResponse_jobCreationDate = Lens.lens (\DescribeBulkImportJobResponse' {jobCreationDate} -> jobCreationDate) (\s@DescribeBulkImportJobResponse' {} a -> s {jobCreationDate = a} :: DescribeBulkImportJobResponse) Prelude.. Data._Time

-- | The date the job was last updated, in Unix epoch time.
describeBulkImportJobResponse_jobLastUpdateDate :: Lens.Lens' DescribeBulkImportJobResponse Prelude.UTCTime
describeBulkImportJobResponse_jobLastUpdateDate = Lens.lens (\DescribeBulkImportJobResponse' {jobLastUpdateDate} -> jobLastUpdateDate) (\s@DescribeBulkImportJobResponse' {} a -> s {jobLastUpdateDate = a} :: DescribeBulkImportJobResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeBulkImportJobResponse where
  rnf DescribeBulkImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobRoleArn
      `Prelude.seq` Prelude.rnf files
      `Prelude.seq` Prelude.rnf errorReportLocation
      `Prelude.seq` Prelude.rnf jobConfiguration
      `Prelude.seq` Prelude.rnf jobCreationDate
      `Prelude.seq` Prelude.rnf jobLastUpdateDate
