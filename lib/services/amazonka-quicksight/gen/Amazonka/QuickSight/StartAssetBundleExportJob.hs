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
-- Module      : Amazonka.QuickSight.StartAssetBundleExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Asset Bundle export job.
--
-- An Asset Bundle export job exports specified Amazon QuickSight assets.
-- You can also choose to export any asset dependencies in the same job.
-- Export jobs run asynchronously and can be polled with a
-- @DescribeAssetBundleExportJob@ API call. When a job is successfully
-- completed, a download URL that contains the exported assets is returned.
-- The URL is valid for 5 minutes and can be refreshed with a
-- @DescribeAssetBundleExportJob@ API call. Each Amazon QuickSight account
-- can run up to 10 export jobs concurrently.
--
-- The API caller must have the necessary permissions in their IAM role to
-- access each resource before the resources can be exported.
module Amazonka.QuickSight.StartAssetBundleExportJob
  ( -- * Creating a Request
    StartAssetBundleExportJob (..),
    newStartAssetBundleExportJob,

    -- * Request Lenses
    startAssetBundleExportJob_cloudFormationOverridePropertyConfiguration,
    startAssetBundleExportJob_includeAllDependencies,
    startAssetBundleExportJob_awsAccountId,
    startAssetBundleExportJob_assetBundleExportJobId,
    startAssetBundleExportJob_resourceArns,
    startAssetBundleExportJob_exportFormat,

    -- * Destructuring the Response
    StartAssetBundleExportJobResponse (..),
    newStartAssetBundleExportJobResponse,

    -- * Response Lenses
    startAssetBundleExportJobResponse_arn,
    startAssetBundleExportJobResponse_assetBundleExportJobId,
    startAssetBundleExportJobResponse_requestId,
    startAssetBundleExportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAssetBundleExportJob' smart constructor.
data StartAssetBundleExportJob = StartAssetBundleExportJob'
  { -- | An optional collection of structures that generate CloudFormation
    -- parameters to override the existing resource property values when the
    -- resource is exported to a new CloudFormation template.
    --
    -- Use this field if the @ExportFormat@ field of a
    -- @StartAssetBundleExportJobRequest@ API call is set to
    -- @CLOUDFORMATION_JSON@.
    cloudFormationOverridePropertyConfiguration :: Prelude.Maybe AssetBundleCloudFormationOverridePropertyConfiguration,
    -- | A Boolean that determines whether all dependencies of each resource ARN
    -- are recursively exported with the job. For example, say you provided a
    -- Dashboard ARN to the @ResourceArns@ parameter. If you set
    -- @IncludeAllDependencies@ to @TRUE@, any theme, dataset, and data source
    -- resource that is a dependency of the dashboard is also exported.
    includeAllDependencies :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services account to export assets from.
    awsAccountId :: Prelude.Text,
    -- | The ID of the job. This ID is unique while the job is running. After the
    -- job is completed, you can reuse this ID for another job.
    assetBundleExportJobId :: Prelude.Text,
    -- | An array of resource ARNs to export. The following resources are
    -- supported.
    --
    -- -   @Analysis@
    --
    -- -   @Dashboard@
    --
    -- -   @DataSet@
    --
    -- -   @DataSource@
    --
    -- -   @RefreshSchedule@
    --
    -- -   @Theme@
    --
    -- -   @VPCConnection@
    --
    -- The API caller must have the necessary permissions in their IAM role to
    -- access each resource before the resources can be exported.
    resourceArns :: Prelude.NonEmpty Prelude.Text,
    -- | The export data format.
    exportFormat :: AssetBundleExportFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssetBundleExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormationOverridePropertyConfiguration', 'startAssetBundleExportJob_cloudFormationOverridePropertyConfiguration' - An optional collection of structures that generate CloudFormation
-- parameters to override the existing resource property values when the
-- resource is exported to a new CloudFormation template.
--
-- Use this field if the @ExportFormat@ field of a
-- @StartAssetBundleExportJobRequest@ API call is set to
-- @CLOUDFORMATION_JSON@.
--
-- 'includeAllDependencies', 'startAssetBundleExportJob_includeAllDependencies' - A Boolean that determines whether all dependencies of each resource ARN
-- are recursively exported with the job. For example, say you provided a
-- Dashboard ARN to the @ResourceArns@ parameter. If you set
-- @IncludeAllDependencies@ to @TRUE@, any theme, dataset, and data source
-- resource that is a dependency of the dashboard is also exported.
--
-- 'awsAccountId', 'startAssetBundleExportJob_awsAccountId' - The ID of the Amazon Web Services account to export assets from.
--
-- 'assetBundleExportJobId', 'startAssetBundleExportJob_assetBundleExportJobId' - The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
--
-- 'resourceArns', 'startAssetBundleExportJob_resourceArns' - An array of resource ARNs to export. The following resources are
-- supported.
--
-- -   @Analysis@
--
-- -   @Dashboard@
--
-- -   @DataSet@
--
-- -   @DataSource@
--
-- -   @RefreshSchedule@
--
-- -   @Theme@
--
-- -   @VPCConnection@
--
-- The API caller must have the necessary permissions in their IAM role to
-- access each resource before the resources can be exported.
--
-- 'exportFormat', 'startAssetBundleExportJob_exportFormat' - The export data format.
newStartAssetBundleExportJob ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assetBundleExportJobId'
  Prelude.Text ->
  -- | 'resourceArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'exportFormat'
  AssetBundleExportFormat ->
  StartAssetBundleExportJob
newStartAssetBundleExportJob
  pAwsAccountId_
  pAssetBundleExportJobId_
  pResourceArns_
  pExportFormat_ =
    StartAssetBundleExportJob'
      { cloudFormationOverridePropertyConfiguration =
          Prelude.Nothing,
        includeAllDependencies = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        assetBundleExportJobId =
          pAssetBundleExportJobId_,
        resourceArns =
          Lens.coerced Lens.# pResourceArns_,
        exportFormat = pExportFormat_
      }

-- | An optional collection of structures that generate CloudFormation
-- parameters to override the existing resource property values when the
-- resource is exported to a new CloudFormation template.
--
-- Use this field if the @ExportFormat@ field of a
-- @StartAssetBundleExportJobRequest@ API call is set to
-- @CLOUDFORMATION_JSON@.
startAssetBundleExportJob_cloudFormationOverridePropertyConfiguration :: Lens.Lens' StartAssetBundleExportJob (Prelude.Maybe AssetBundleCloudFormationOverridePropertyConfiguration)
startAssetBundleExportJob_cloudFormationOverridePropertyConfiguration = Lens.lens (\StartAssetBundleExportJob' {cloudFormationOverridePropertyConfiguration} -> cloudFormationOverridePropertyConfiguration) (\s@StartAssetBundleExportJob' {} a -> s {cloudFormationOverridePropertyConfiguration = a} :: StartAssetBundleExportJob)

-- | A Boolean that determines whether all dependencies of each resource ARN
-- are recursively exported with the job. For example, say you provided a
-- Dashboard ARN to the @ResourceArns@ parameter. If you set
-- @IncludeAllDependencies@ to @TRUE@, any theme, dataset, and data source
-- resource that is a dependency of the dashboard is also exported.
startAssetBundleExportJob_includeAllDependencies :: Lens.Lens' StartAssetBundleExportJob (Prelude.Maybe Prelude.Bool)
startAssetBundleExportJob_includeAllDependencies = Lens.lens (\StartAssetBundleExportJob' {includeAllDependencies} -> includeAllDependencies) (\s@StartAssetBundleExportJob' {} a -> s {includeAllDependencies = a} :: StartAssetBundleExportJob)

-- | The ID of the Amazon Web Services account to export assets from.
startAssetBundleExportJob_awsAccountId :: Lens.Lens' StartAssetBundleExportJob Prelude.Text
startAssetBundleExportJob_awsAccountId = Lens.lens (\StartAssetBundleExportJob' {awsAccountId} -> awsAccountId) (\s@StartAssetBundleExportJob' {} a -> s {awsAccountId = a} :: StartAssetBundleExportJob)

-- | The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
startAssetBundleExportJob_assetBundleExportJobId :: Lens.Lens' StartAssetBundleExportJob Prelude.Text
startAssetBundleExportJob_assetBundleExportJobId = Lens.lens (\StartAssetBundleExportJob' {assetBundleExportJobId} -> assetBundleExportJobId) (\s@StartAssetBundleExportJob' {} a -> s {assetBundleExportJobId = a} :: StartAssetBundleExportJob)

-- | An array of resource ARNs to export. The following resources are
-- supported.
--
-- -   @Analysis@
--
-- -   @Dashboard@
--
-- -   @DataSet@
--
-- -   @DataSource@
--
-- -   @RefreshSchedule@
--
-- -   @Theme@
--
-- -   @VPCConnection@
--
-- The API caller must have the necessary permissions in their IAM role to
-- access each resource before the resources can be exported.
startAssetBundleExportJob_resourceArns :: Lens.Lens' StartAssetBundleExportJob (Prelude.NonEmpty Prelude.Text)
startAssetBundleExportJob_resourceArns = Lens.lens (\StartAssetBundleExportJob' {resourceArns} -> resourceArns) (\s@StartAssetBundleExportJob' {} a -> s {resourceArns = a} :: StartAssetBundleExportJob) Prelude.. Lens.coerced

-- | The export data format.
startAssetBundleExportJob_exportFormat :: Lens.Lens' StartAssetBundleExportJob AssetBundleExportFormat
startAssetBundleExportJob_exportFormat = Lens.lens (\StartAssetBundleExportJob' {exportFormat} -> exportFormat) (\s@StartAssetBundleExportJob' {} a -> s {exportFormat = a} :: StartAssetBundleExportJob)

instance Core.AWSRequest StartAssetBundleExportJob where
  type
    AWSResponse StartAssetBundleExportJob =
      StartAssetBundleExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssetBundleExportJobResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetBundleExportJobId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAssetBundleExportJob where
  hashWithSalt _salt StartAssetBundleExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` cloudFormationOverridePropertyConfiguration
      `Prelude.hashWithSalt` includeAllDependencies
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assetBundleExportJobId
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` exportFormat

instance Prelude.NFData StartAssetBundleExportJob where
  rnf StartAssetBundleExportJob' {..} =
    Prelude.rnf
      cloudFormationOverridePropertyConfiguration
      `Prelude.seq` Prelude.rnf includeAllDependencies
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assetBundleExportJobId
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf exportFormat

instance Data.ToHeaders StartAssetBundleExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAssetBundleExportJob where
  toJSON StartAssetBundleExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "CloudFormationOverridePropertyConfiguration"
                Data..=
            )
              Prelude.<$> cloudFormationOverridePropertyConfiguration,
            ("IncludeAllDependencies" Data..=)
              Prelude.<$> includeAllDependencies,
            Prelude.Just
              ( "AssetBundleExportJobId"
                  Data..= assetBundleExportJobId
              ),
            Prelude.Just ("ResourceArns" Data..= resourceArns),
            Prelude.Just ("ExportFormat" Data..= exportFormat)
          ]
      )

instance Data.ToPath StartAssetBundleExportJob where
  toPath StartAssetBundleExportJob' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-export-jobs/export"
      ]

instance Data.ToQuery StartAssetBundleExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAssetBundleExportJobResponse' smart constructor.
data StartAssetBundleExportJobResponse = StartAssetBundleExportJobResponse'
  { -- | The Amazon Resource Name (ARN) for the export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job. This ID is unique while the job is running. After the
    -- job is completed, you can reuse this ID for another job.
    assetBundleExportJobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services response ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the response.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssetBundleExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startAssetBundleExportJobResponse_arn' - The Amazon Resource Name (ARN) for the export job.
--
-- 'assetBundleExportJobId', 'startAssetBundleExportJobResponse_assetBundleExportJobId' - The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
--
-- 'requestId', 'startAssetBundleExportJobResponse_requestId' - The Amazon Web Services response ID for this operation.
--
-- 'status', 'startAssetBundleExportJobResponse_status' - The HTTP status of the response.
newStartAssetBundleExportJobResponse ::
  -- | 'status'
  Prelude.Int ->
  StartAssetBundleExportJobResponse
newStartAssetBundleExportJobResponse pStatus_ =
  StartAssetBundleExportJobResponse'
    { arn =
        Prelude.Nothing,
      assetBundleExportJobId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the export job.
startAssetBundleExportJobResponse_arn :: Lens.Lens' StartAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleExportJobResponse_arn = Lens.lens (\StartAssetBundleExportJobResponse' {arn} -> arn) (\s@StartAssetBundleExportJobResponse' {} a -> s {arn = a} :: StartAssetBundleExportJobResponse)

-- | The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
startAssetBundleExportJobResponse_assetBundleExportJobId :: Lens.Lens' StartAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleExportJobResponse_assetBundleExportJobId = Lens.lens (\StartAssetBundleExportJobResponse' {assetBundleExportJobId} -> assetBundleExportJobId) (\s@StartAssetBundleExportJobResponse' {} a -> s {assetBundleExportJobId = a} :: StartAssetBundleExportJobResponse)

-- | The Amazon Web Services response ID for this operation.
startAssetBundleExportJobResponse_requestId :: Lens.Lens' StartAssetBundleExportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleExportJobResponse_requestId = Lens.lens (\StartAssetBundleExportJobResponse' {requestId} -> requestId) (\s@StartAssetBundleExportJobResponse' {} a -> s {requestId = a} :: StartAssetBundleExportJobResponse)

-- | The HTTP status of the response.
startAssetBundleExportJobResponse_status :: Lens.Lens' StartAssetBundleExportJobResponse Prelude.Int
startAssetBundleExportJobResponse_status = Lens.lens (\StartAssetBundleExportJobResponse' {status} -> status) (\s@StartAssetBundleExportJobResponse' {} a -> s {status = a} :: StartAssetBundleExportJobResponse)

instance
  Prelude.NFData
    StartAssetBundleExportJobResponse
  where
  rnf StartAssetBundleExportJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleExportJobId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
