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
-- Module      : Amazonka.QuickSight.DescribeAssetBundleExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing export job.
--
-- Poll job descriptions after a job starts to know the status of the job.
-- When a job succeeds, a URL is provided to download the exported assets\'
-- data from. Download URLs are valid for five minutes after they are
-- generated. You can call the @DescribeAssetBundleExportJob@ API for a new
-- download URL as needed.
--
-- Job descriptions are available for 14 days after the job starts.
module Amazonka.QuickSight.DescribeAssetBundleExportJob
  ( -- * Creating a Request
    DescribeAssetBundleExportJob (..),
    newDescribeAssetBundleExportJob,

    -- * Request Lenses
    describeAssetBundleExportJob_awsAccountId,
    describeAssetBundleExportJob_assetBundleExportJobId,

    -- * Destructuring the Response
    DescribeAssetBundleExportJobResponse (..),
    newDescribeAssetBundleExportJobResponse,

    -- * Response Lenses
    describeAssetBundleExportJobResponse_arn,
    describeAssetBundleExportJobResponse_assetBundleExportJobId,
    describeAssetBundleExportJobResponse_awsAccountId,
    describeAssetBundleExportJobResponse_cloudFormationOverridePropertyConfiguration,
    describeAssetBundleExportJobResponse_createdTime,
    describeAssetBundleExportJobResponse_downloadUrl,
    describeAssetBundleExportJobResponse_errors,
    describeAssetBundleExportJobResponse_exportFormat,
    describeAssetBundleExportJobResponse_includeAllDependencies,
    describeAssetBundleExportJobResponse_jobStatus,
    describeAssetBundleExportJobResponse_requestId,
    describeAssetBundleExportJobResponse_resourceArns,
    describeAssetBundleExportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAssetBundleExportJob' smart constructor.
data DescribeAssetBundleExportJob = DescribeAssetBundleExportJob'
  { -- | The ID of the Amazon Web Services account the export job is executed in.
    awsAccountId :: Prelude.Text,
    -- | The ID of the job that you want described. The job ID is set when you
    -- start a new job with a @StartAssetBundleExportJob@ API call.
    assetBundleExportJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetBundleExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAssetBundleExportJob_awsAccountId' - The ID of the Amazon Web Services account the export job is executed in.
--
-- 'assetBundleExportJobId', 'describeAssetBundleExportJob_assetBundleExportJobId' - The ID of the job that you want described. The job ID is set when you
-- start a new job with a @StartAssetBundleExportJob@ API call.
newDescribeAssetBundleExportJob ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assetBundleExportJobId'
  Prelude.Text ->
  DescribeAssetBundleExportJob
newDescribeAssetBundleExportJob
  pAwsAccountId_
  pAssetBundleExportJobId_ =
    DescribeAssetBundleExportJob'
      { awsAccountId =
          pAwsAccountId_,
        assetBundleExportJobId =
          pAssetBundleExportJobId_
      }

-- | The ID of the Amazon Web Services account the export job is executed in.
describeAssetBundleExportJob_awsAccountId :: Lens.Lens' DescribeAssetBundleExportJob Prelude.Text
describeAssetBundleExportJob_awsAccountId = Lens.lens (\DescribeAssetBundleExportJob' {awsAccountId} -> awsAccountId) (\s@DescribeAssetBundleExportJob' {} a -> s {awsAccountId = a} :: DescribeAssetBundleExportJob)

-- | The ID of the job that you want described. The job ID is set when you
-- start a new job with a @StartAssetBundleExportJob@ API call.
describeAssetBundleExportJob_assetBundleExportJobId :: Lens.Lens' DescribeAssetBundleExportJob Prelude.Text
describeAssetBundleExportJob_assetBundleExportJobId = Lens.lens (\DescribeAssetBundleExportJob' {assetBundleExportJobId} -> assetBundleExportJobId) (\s@DescribeAssetBundleExportJob' {} a -> s {assetBundleExportJobId = a} :: DescribeAssetBundleExportJob)

instance Core.AWSRequest DescribeAssetBundleExportJob where
  type
    AWSResponse DescribeAssetBundleExportJob =
      DescribeAssetBundleExportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetBundleExportJobResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetBundleExportJobId")
            Prelude.<*> (x Data..?> "AwsAccountId")
            Prelude.<*> ( x
                            Data..?> "CloudFormationOverridePropertyConfiguration"
                        )
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "DownloadUrl")
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ExportFormat")
            Prelude.<*> (x Data..?> "IncludeAllDependencies")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ResourceArns")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAssetBundleExportJob
  where
  hashWithSalt _salt DescribeAssetBundleExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assetBundleExportJobId

instance Prelude.NFData DescribeAssetBundleExportJob where
  rnf DescribeAssetBundleExportJob' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assetBundleExportJobId

instance Data.ToHeaders DescribeAssetBundleExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAssetBundleExportJob where
  toPath DescribeAssetBundleExportJob' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-export-jobs/",
        Data.toBS assetBundleExportJobId
      ]

instance Data.ToQuery DescribeAssetBundleExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssetBundleExportJobResponse' smart constructor.
data DescribeAssetBundleExportJobResponse = DescribeAssetBundleExportJobResponse'
  { -- | The Amazon Resource Name (ARN) for the export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job. The job ID is set when you start a new job with a
    -- @StartAssetBundleExportJob@ API call.
    assetBundleExportJobId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that the export job was
    -- executed in.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The CloudFormation override property configuration for the export job.
    cloudFormationOverridePropertyConfiguration :: Prelude.Maybe AssetBundleCloudFormationOverridePropertyConfiguration,
    -- | The time that the export job was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The URL to download the exported asset bundle data from.
    --
    -- This URL is available only after the job has succeeded. This URL is
    -- valid for 5 minutes after issuance. Call @DescribeAssetBundleExportJob@
    -- again for a fresh URL if needed.
    --
    -- The downloaded asset bundle is a zip file named
    -- @assetbundle-{jobId}.qs@. The file has a @.qs@ extension.
    --
    -- This URL can\'t be used in a @StartAssetBundleImportJob@ API call and
    -- should only be used for download purposes.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | An array of error records that describes any failures that occurred
    -- during the export job processing.
    --
    -- Error records accumulate while the job runs. The complete set of error
    -- records is available after the job has completed and failed.
    errors :: Prelude.Maybe [AssetBundleExportJobError],
    -- | The format of the export.
    exportFormat :: Prelude.Maybe AssetBundleExportFormat,
    -- | The include dependencies flag.
    includeAllDependencies :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the status of a job through its queuing and execution.
    --
    -- Poll this @DescribeAssetBundleExportApi@ until @JobStatus@ is either
    -- @SUCCESSFUL@ or @FAILED@.
    jobStatus :: Prelude.Maybe AssetBundleExportJobStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A list of resource ARNs that exported with the job.
    resourceArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The HTTP status of the response.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetBundleExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeAssetBundleExportJobResponse_arn' - The Amazon Resource Name (ARN) for the export job.
--
-- 'assetBundleExportJobId', 'describeAssetBundleExportJobResponse_assetBundleExportJobId' - The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleExportJob@ API call.
--
-- 'awsAccountId', 'describeAssetBundleExportJobResponse_awsAccountId' - The ID of the Amazon Web Services account that the export job was
-- executed in.
--
-- 'cloudFormationOverridePropertyConfiguration', 'describeAssetBundleExportJobResponse_cloudFormationOverridePropertyConfiguration' - The CloudFormation override property configuration for the export job.
--
-- 'createdTime', 'describeAssetBundleExportJobResponse_createdTime' - The time that the export job was created.
--
-- 'downloadUrl', 'describeAssetBundleExportJobResponse_downloadUrl' - The URL to download the exported asset bundle data from.
--
-- This URL is available only after the job has succeeded. This URL is
-- valid for 5 minutes after issuance. Call @DescribeAssetBundleExportJob@
-- again for a fresh URL if needed.
--
-- The downloaded asset bundle is a zip file named
-- @assetbundle-{jobId}.qs@. The file has a @.qs@ extension.
--
-- This URL can\'t be used in a @StartAssetBundleImportJob@ API call and
-- should only be used for download purposes.
--
-- 'errors', 'describeAssetBundleExportJobResponse_errors' - An array of error records that describes any failures that occurred
-- during the export job processing.
--
-- Error records accumulate while the job runs. The complete set of error
-- records is available after the job has completed and failed.
--
-- 'exportFormat', 'describeAssetBundleExportJobResponse_exportFormat' - The format of the export.
--
-- 'includeAllDependencies', 'describeAssetBundleExportJobResponse_includeAllDependencies' - The include dependencies flag.
--
-- 'jobStatus', 'describeAssetBundleExportJobResponse_jobStatus' - Indicates the status of a job through its queuing and execution.
--
-- Poll this @DescribeAssetBundleExportApi@ until @JobStatus@ is either
-- @SUCCESSFUL@ or @FAILED@.
--
-- 'requestId', 'describeAssetBundleExportJobResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'resourceArns', 'describeAssetBundleExportJobResponse_resourceArns' - A list of resource ARNs that exported with the job.
--
-- 'status', 'describeAssetBundleExportJobResponse_status' - The HTTP status of the response.
newDescribeAssetBundleExportJobResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAssetBundleExportJobResponse
newDescribeAssetBundleExportJobResponse pStatus_ =
  DescribeAssetBundleExportJobResponse'
    { arn =
        Prelude.Nothing,
      assetBundleExportJobId =
        Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      cloudFormationOverridePropertyConfiguration =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      downloadUrl = Prelude.Nothing,
      errors = Prelude.Nothing,
      exportFormat = Prelude.Nothing,
      includeAllDependencies =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the export job.
describeAssetBundleExportJobResponse_arn :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleExportJobResponse_arn = Lens.lens (\DescribeAssetBundleExportJobResponse' {arn} -> arn) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {arn = a} :: DescribeAssetBundleExportJobResponse)

-- | The ID of the job. The job ID is set when you start a new job with a
-- @StartAssetBundleExportJob@ API call.
describeAssetBundleExportJobResponse_assetBundleExportJobId :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleExportJobResponse_assetBundleExportJobId = Lens.lens (\DescribeAssetBundleExportJobResponse' {assetBundleExportJobId} -> assetBundleExportJobId) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {assetBundleExportJobId = a} :: DescribeAssetBundleExportJobResponse)

-- | The ID of the Amazon Web Services account that the export job was
-- executed in.
describeAssetBundleExportJobResponse_awsAccountId :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleExportJobResponse_awsAccountId = Lens.lens (\DescribeAssetBundleExportJobResponse' {awsAccountId} -> awsAccountId) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {awsAccountId = a} :: DescribeAssetBundleExportJobResponse)

-- | The CloudFormation override property configuration for the export job.
describeAssetBundleExportJobResponse_cloudFormationOverridePropertyConfiguration :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe AssetBundleCloudFormationOverridePropertyConfiguration)
describeAssetBundleExportJobResponse_cloudFormationOverridePropertyConfiguration = Lens.lens (\DescribeAssetBundleExportJobResponse' {cloudFormationOverridePropertyConfiguration} -> cloudFormationOverridePropertyConfiguration) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {cloudFormationOverridePropertyConfiguration = a} :: DescribeAssetBundleExportJobResponse)

-- | The time that the export job was created.
describeAssetBundleExportJobResponse_createdTime :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeAssetBundleExportJobResponse_createdTime = Lens.lens (\DescribeAssetBundleExportJobResponse' {createdTime} -> createdTime) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {createdTime = a} :: DescribeAssetBundleExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The URL to download the exported asset bundle data from.
--
-- This URL is available only after the job has succeeded. This URL is
-- valid for 5 minutes after issuance. Call @DescribeAssetBundleExportJob@
-- again for a fresh URL if needed.
--
-- The downloaded asset bundle is a zip file named
-- @assetbundle-{jobId}.qs@. The file has a @.qs@ extension.
--
-- This URL can\'t be used in a @StartAssetBundleImportJob@ API call and
-- should only be used for download purposes.
describeAssetBundleExportJobResponse_downloadUrl :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleExportJobResponse_downloadUrl = Lens.lens (\DescribeAssetBundleExportJobResponse' {downloadUrl} -> downloadUrl) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {downloadUrl = a} :: DescribeAssetBundleExportJobResponse)

-- | An array of error records that describes any failures that occurred
-- during the export job processing.
--
-- Error records accumulate while the job runs. The complete set of error
-- records is available after the job has completed and failed.
describeAssetBundleExportJobResponse_errors :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe [AssetBundleExportJobError])
describeAssetBundleExportJobResponse_errors = Lens.lens (\DescribeAssetBundleExportJobResponse' {errors} -> errors) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {errors = a} :: DescribeAssetBundleExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The format of the export.
describeAssetBundleExportJobResponse_exportFormat :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe AssetBundleExportFormat)
describeAssetBundleExportJobResponse_exportFormat = Lens.lens (\DescribeAssetBundleExportJobResponse' {exportFormat} -> exportFormat) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {exportFormat = a} :: DescribeAssetBundleExportJobResponse)

-- | The include dependencies flag.
describeAssetBundleExportJobResponse_includeAllDependencies :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Bool)
describeAssetBundleExportJobResponse_includeAllDependencies = Lens.lens (\DescribeAssetBundleExportJobResponse' {includeAllDependencies} -> includeAllDependencies) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {includeAllDependencies = a} :: DescribeAssetBundleExportJobResponse)

-- | Indicates the status of a job through its queuing and execution.
--
-- Poll this @DescribeAssetBundleExportApi@ until @JobStatus@ is either
-- @SUCCESSFUL@ or @FAILED@.
describeAssetBundleExportJobResponse_jobStatus :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe AssetBundleExportJobStatus)
describeAssetBundleExportJobResponse_jobStatus = Lens.lens (\DescribeAssetBundleExportJobResponse' {jobStatus} -> jobStatus) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {jobStatus = a} :: DescribeAssetBundleExportJobResponse)

-- | The Amazon Web Services request ID for this operation.
describeAssetBundleExportJobResponse_requestId :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
describeAssetBundleExportJobResponse_requestId = Lens.lens (\DescribeAssetBundleExportJobResponse' {requestId} -> requestId) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {requestId = a} :: DescribeAssetBundleExportJobResponse)

-- | A list of resource ARNs that exported with the job.
describeAssetBundleExportJobResponse_resourceArns :: Lens.Lens' DescribeAssetBundleExportJobResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeAssetBundleExportJobResponse_resourceArns = Lens.lens (\DescribeAssetBundleExportJobResponse' {resourceArns} -> resourceArns) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {resourceArns = a} :: DescribeAssetBundleExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the response.
describeAssetBundleExportJobResponse_status :: Lens.Lens' DescribeAssetBundleExportJobResponse Prelude.Int
describeAssetBundleExportJobResponse_status = Lens.lens (\DescribeAssetBundleExportJobResponse' {status} -> status) (\s@DescribeAssetBundleExportJobResponse' {} a -> s {status = a} :: DescribeAssetBundleExportJobResponse)

instance
  Prelude.NFData
    DescribeAssetBundleExportJobResponse
  where
  rnf DescribeAssetBundleExportJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleExportJobId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf
        cloudFormationOverridePropertyConfiguration
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf downloadUrl
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf exportFormat
      `Prelude.seq` Prelude.rnf includeAllDependencies
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf status
