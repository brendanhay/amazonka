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
-- Module      : Amazonka.ComputeOptimizer.ExportLambdaFunctionRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports optimization recommendations for Lambda functions.
--
-- Recommendations are exported in a comma-separated values (.csv) file,
-- and its metadata in a JavaScript Object Notation (JSON) (.json) file, to
-- an existing Amazon Simple Storage Service (Amazon S3) bucket that you
-- specify. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html Exporting Recommendations>
-- in the /Compute Optimizer User Guide/.
--
-- You can have only one Lambda function export job in progress per Amazon
-- Web Services Region.
module Amazonka.ComputeOptimizer.ExportLambdaFunctionRecommendations
  ( -- * Creating a Request
    ExportLambdaFunctionRecommendations (..),
    newExportLambdaFunctionRecommendations,

    -- * Request Lenses
    exportLambdaFunctionRecommendations_accountIds,
    exportLambdaFunctionRecommendations_fieldsToExport,
    exportLambdaFunctionRecommendations_fileFormat,
    exportLambdaFunctionRecommendations_filters,
    exportLambdaFunctionRecommendations_includeMemberAccounts,
    exportLambdaFunctionRecommendations_s3DestinationConfig,

    -- * Destructuring the Response
    ExportLambdaFunctionRecommendationsResponse (..),
    newExportLambdaFunctionRecommendationsResponse,

    -- * Response Lenses
    exportLambdaFunctionRecommendationsResponse_jobId,
    exportLambdaFunctionRecommendationsResponse_s3Destination,
    exportLambdaFunctionRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportLambdaFunctionRecommendations' smart constructor.
data ExportLambdaFunctionRecommendations = ExportLambdaFunctionRecommendations'
  { -- | The IDs of the Amazon Web Services accounts for which to export Lambda
    -- function recommendations.
    --
    -- If your account is the management account of an organization, use this
    -- parameter to specify the member account for which you want to export
    -- recommendations.
    --
    -- This parameter cannot be specified together with the include member
    -- accounts parameter. The parameters are mutually exclusive.
    --
    -- Recommendations for member accounts are not included in the export if
    -- this parameter, or the include member accounts parameter, is omitted.
    --
    -- You can specify multiple account IDs per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The recommendations data to include in the export file. For more
    -- information about the fields that can be exported, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
    -- in the /Compute Optimizer User Guide/.
    fieldsToExport :: Prelude.Maybe [ExportableLambdaFunctionField],
    -- | The format of the export file.
    --
    -- The only export file format currently supported is @Csv@.
    fileFormat :: Prelude.Maybe FileFormat,
    -- | An array of objects to specify a filter that exports a more specific set
    -- of Lambda function recommendations.
    filters :: Prelude.Maybe [LambdaFunctionRecommendationFilter],
    -- | Indicates whether to include recommendations for resources in all member
    -- accounts of the organization if your account is the management account
    -- of an organization.
    --
    -- The member accounts must also be opted in to Compute Optimizer, and
    -- trusted access for Compute Optimizer must be enabled in the organization
    -- account. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
    -- in the /Compute Optimizer User Guide/.
    --
    -- Recommendations for member accounts of the organization are not included
    -- in the export file if this parameter is omitted.
    --
    -- This parameter cannot be specified together with the account IDs
    -- parameter. The parameters are mutually exclusive.
    --
    -- Recommendations for member accounts are not included in the export if
    -- this parameter, or the account IDs parameter, is omitted.
    includeMemberAccounts :: Prelude.Maybe Prelude.Bool,
    s3DestinationConfig :: S3DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportLambdaFunctionRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'exportLambdaFunctionRecommendations_accountIds' - The IDs of the Amazon Web Services accounts for which to export Lambda
-- function recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to export
-- recommendations.
--
-- This parameter cannot be specified together with the include member
-- accounts parameter. The parameters are mutually exclusive.
--
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the include member accounts parameter, is omitted.
--
-- You can specify multiple account IDs per request.
--
-- 'fieldsToExport', 'exportLambdaFunctionRecommendations_fieldsToExport' - The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
--
-- 'fileFormat', 'exportLambdaFunctionRecommendations_fileFormat' - The format of the export file.
--
-- The only export file format currently supported is @Csv@.
--
-- 'filters', 'exportLambdaFunctionRecommendations_filters' - An array of objects to specify a filter that exports a more specific set
-- of Lambda function recommendations.
--
-- 'includeMemberAccounts', 'exportLambdaFunctionRecommendations_includeMemberAccounts' - Indicates whether to include recommendations for resources in all member
-- accounts of the organization if your account is the management account
-- of an organization.
--
-- The member accounts must also be opted in to Compute Optimizer, and
-- trusted access for Compute Optimizer must be enabled in the organization
-- account. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
-- in the /Compute Optimizer User Guide/.
--
-- Recommendations for member accounts of the organization are not included
-- in the export file if this parameter is omitted.
--
-- This parameter cannot be specified together with the account IDs
-- parameter. The parameters are mutually exclusive.
--
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the account IDs parameter, is omitted.
--
-- 's3DestinationConfig', 'exportLambdaFunctionRecommendations_s3DestinationConfig' - Undocumented member.
newExportLambdaFunctionRecommendations ::
  -- | 's3DestinationConfig'
  S3DestinationConfig ->
  ExportLambdaFunctionRecommendations
newExportLambdaFunctionRecommendations
  pS3DestinationConfig_ =
    ExportLambdaFunctionRecommendations'
      { accountIds =
          Prelude.Nothing,
        fieldsToExport = Prelude.Nothing,
        fileFormat = Prelude.Nothing,
        filters = Prelude.Nothing,
        includeMemberAccounts =
          Prelude.Nothing,
        s3DestinationConfig =
          pS3DestinationConfig_
      }

-- | The IDs of the Amazon Web Services accounts for which to export Lambda
-- function recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to export
-- recommendations.
--
-- This parameter cannot be specified together with the include member
-- accounts parameter. The parameters are mutually exclusive.
--
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the include member accounts parameter, is omitted.
--
-- You can specify multiple account IDs per request.
exportLambdaFunctionRecommendations_accountIds :: Lens.Lens' ExportLambdaFunctionRecommendations (Prelude.Maybe [Prelude.Text])
exportLambdaFunctionRecommendations_accountIds = Lens.lens (\ExportLambdaFunctionRecommendations' {accountIds} -> accountIds) (\s@ExportLambdaFunctionRecommendations' {} a -> s {accountIds = a} :: ExportLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
exportLambdaFunctionRecommendations_fieldsToExport :: Lens.Lens' ExportLambdaFunctionRecommendations (Prelude.Maybe [ExportableLambdaFunctionField])
exportLambdaFunctionRecommendations_fieldsToExport = Lens.lens (\ExportLambdaFunctionRecommendations' {fieldsToExport} -> fieldsToExport) (\s@ExportLambdaFunctionRecommendations' {} a -> s {fieldsToExport = a} :: ExportLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The format of the export file.
--
-- The only export file format currently supported is @Csv@.
exportLambdaFunctionRecommendations_fileFormat :: Lens.Lens' ExportLambdaFunctionRecommendations (Prelude.Maybe FileFormat)
exportLambdaFunctionRecommendations_fileFormat = Lens.lens (\ExportLambdaFunctionRecommendations' {fileFormat} -> fileFormat) (\s@ExportLambdaFunctionRecommendations' {} a -> s {fileFormat = a} :: ExportLambdaFunctionRecommendations)

-- | An array of objects to specify a filter that exports a more specific set
-- of Lambda function recommendations.
exportLambdaFunctionRecommendations_filters :: Lens.Lens' ExportLambdaFunctionRecommendations (Prelude.Maybe [LambdaFunctionRecommendationFilter])
exportLambdaFunctionRecommendations_filters = Lens.lens (\ExportLambdaFunctionRecommendations' {filters} -> filters) (\s@ExportLambdaFunctionRecommendations' {} a -> s {filters = a} :: ExportLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to include recommendations for resources in all member
-- accounts of the organization if your account is the management account
-- of an organization.
--
-- The member accounts must also be opted in to Compute Optimizer, and
-- trusted access for Compute Optimizer must be enabled in the organization
-- account. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
-- in the /Compute Optimizer User Guide/.
--
-- Recommendations for member accounts of the organization are not included
-- in the export file if this parameter is omitted.
--
-- This parameter cannot be specified together with the account IDs
-- parameter. The parameters are mutually exclusive.
--
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the account IDs parameter, is omitted.
exportLambdaFunctionRecommendations_includeMemberAccounts :: Lens.Lens' ExportLambdaFunctionRecommendations (Prelude.Maybe Prelude.Bool)
exportLambdaFunctionRecommendations_includeMemberAccounts = Lens.lens (\ExportLambdaFunctionRecommendations' {includeMemberAccounts} -> includeMemberAccounts) (\s@ExportLambdaFunctionRecommendations' {} a -> s {includeMemberAccounts = a} :: ExportLambdaFunctionRecommendations)

-- | Undocumented member.
exportLambdaFunctionRecommendations_s3DestinationConfig :: Lens.Lens' ExportLambdaFunctionRecommendations S3DestinationConfig
exportLambdaFunctionRecommendations_s3DestinationConfig = Lens.lens (\ExportLambdaFunctionRecommendations' {s3DestinationConfig} -> s3DestinationConfig) (\s@ExportLambdaFunctionRecommendations' {} a -> s {s3DestinationConfig = a} :: ExportLambdaFunctionRecommendations)

instance
  Core.AWSRequest
    ExportLambdaFunctionRecommendations
  where
  type
    AWSResponse ExportLambdaFunctionRecommendations =
      ExportLambdaFunctionRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportLambdaFunctionRecommendationsResponse'
            Prelude.<$> (x Data..?> "jobId")
              Prelude.<*> (x Data..?> "s3Destination")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportLambdaFunctionRecommendations
  where
  hashWithSalt
    _salt
    ExportLambdaFunctionRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` fieldsToExport
        `Prelude.hashWithSalt` fileFormat
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` includeMemberAccounts
        `Prelude.hashWithSalt` s3DestinationConfig

instance
  Prelude.NFData
    ExportLambdaFunctionRecommendations
  where
  rnf ExportLambdaFunctionRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf fieldsToExport
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeMemberAccounts
      `Prelude.seq` Prelude.rnf s3DestinationConfig

instance
  Data.ToHeaders
    ExportLambdaFunctionRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.ExportLambdaFunctionRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ExportLambdaFunctionRecommendations
  where
  toJSON ExportLambdaFunctionRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("fieldsToExport" Data..=)
              Prelude.<$> fieldsToExport,
            ("fileFormat" Data..=) Prelude.<$> fileFormat,
            ("filters" Data..=) Prelude.<$> filters,
            ("includeMemberAccounts" Data..=)
              Prelude.<$> includeMemberAccounts,
            Prelude.Just
              ("s3DestinationConfig" Data..= s3DestinationConfig)
          ]
      )

instance
  Data.ToPath
    ExportLambdaFunctionRecommendations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ExportLambdaFunctionRecommendations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportLambdaFunctionRecommendationsResponse' smart constructor.
data ExportLambdaFunctionRecommendationsResponse = ExportLambdaFunctionRecommendationsResponse'
  { -- | The identification number of the export job.
    --
    -- Use the DescribeRecommendationExportJobs action, and specify the job ID
    -- to view the status of an export job.
    jobId :: Prelude.Maybe Prelude.Text,
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportLambdaFunctionRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'exportLambdaFunctionRecommendationsResponse_jobId' - The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
--
-- 's3Destination', 'exportLambdaFunctionRecommendationsResponse_s3Destination' - Undocumented member.
--
-- 'httpStatus', 'exportLambdaFunctionRecommendationsResponse_httpStatus' - The response's http status code.
newExportLambdaFunctionRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportLambdaFunctionRecommendationsResponse
newExportLambdaFunctionRecommendationsResponse
  pHttpStatus_ =
    ExportLambdaFunctionRecommendationsResponse'
      { jobId =
          Prelude.Nothing,
        s3Destination =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
exportLambdaFunctionRecommendationsResponse_jobId :: Lens.Lens' ExportLambdaFunctionRecommendationsResponse (Prelude.Maybe Prelude.Text)
exportLambdaFunctionRecommendationsResponse_jobId = Lens.lens (\ExportLambdaFunctionRecommendationsResponse' {jobId} -> jobId) (\s@ExportLambdaFunctionRecommendationsResponse' {} a -> s {jobId = a} :: ExportLambdaFunctionRecommendationsResponse)

-- | Undocumented member.
exportLambdaFunctionRecommendationsResponse_s3Destination :: Lens.Lens' ExportLambdaFunctionRecommendationsResponse (Prelude.Maybe S3Destination)
exportLambdaFunctionRecommendationsResponse_s3Destination = Lens.lens (\ExportLambdaFunctionRecommendationsResponse' {s3Destination} -> s3Destination) (\s@ExportLambdaFunctionRecommendationsResponse' {} a -> s {s3Destination = a} :: ExportLambdaFunctionRecommendationsResponse)

-- | The response's http status code.
exportLambdaFunctionRecommendationsResponse_httpStatus :: Lens.Lens' ExportLambdaFunctionRecommendationsResponse Prelude.Int
exportLambdaFunctionRecommendationsResponse_httpStatus = Lens.lens (\ExportLambdaFunctionRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ExportLambdaFunctionRecommendationsResponse' {} a -> s {httpStatus = a} :: ExportLambdaFunctionRecommendationsResponse)

instance
  Prelude.NFData
    ExportLambdaFunctionRecommendationsResponse
  where
  rnf ExportLambdaFunctionRecommendationsResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf httpStatus
