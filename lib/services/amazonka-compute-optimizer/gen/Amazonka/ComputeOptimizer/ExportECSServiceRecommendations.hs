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
-- Module      : Amazonka.ComputeOptimizer.ExportECSServiceRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports optimization recommendations for Amazon ECS services on Fargate.
--
-- Recommendations are exported in a CSV file, and its metadata in a JSON
-- file, to an existing Amazon Simple Storage Service (Amazon S3) bucket
-- that you specify. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html Exporting Recommendations>
-- in the /Compute Optimizer User Guide/.
--
-- You can only have one Amazon ECS service export job in progress per
-- Amazon Web Services Region.
module Amazonka.ComputeOptimizer.ExportECSServiceRecommendations
  ( -- * Creating a Request
    ExportECSServiceRecommendations (..),
    newExportECSServiceRecommendations,

    -- * Request Lenses
    exportECSServiceRecommendations_accountIds,
    exportECSServiceRecommendations_fieldsToExport,
    exportECSServiceRecommendations_fileFormat,
    exportECSServiceRecommendations_filters,
    exportECSServiceRecommendations_includeMemberAccounts,
    exportECSServiceRecommendations_s3DestinationConfig,

    -- * Destructuring the Response
    ExportECSServiceRecommendationsResponse (..),
    newExportECSServiceRecommendationsResponse,

    -- * Response Lenses
    exportECSServiceRecommendationsResponse_jobId,
    exportECSServiceRecommendationsResponse_s3Destination,
    exportECSServiceRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportECSServiceRecommendations' smart constructor.
data ExportECSServiceRecommendations = ExportECSServiceRecommendations'
  { -- | The Amazon Web Services account IDs for the export ECS service
    -- recommendations.
    --
    -- If your account is the management account or the delegated administrator
    -- of an organization, use this parameter to specify the member account you
    -- want to export recommendations to.
    --
    -- This parameter can\'t be specified together with the include member
    -- accounts parameter. The parameters are mutually exclusive.
    --
    -- If this parameter or the include member accounts parameter is omitted,
    -- the recommendations for member accounts aren\'t included in the export.
    --
    -- You can specify multiple account IDs per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The recommendations data to include in the export file. For more
    -- information about the fields that can be exported, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
    -- in the /Compute Optimizer User Guide/.
    fieldsToExport :: Prelude.Maybe [ExportableECSServiceField],
    -- | The format of the export file.
    --
    -- The CSV file is the only export file format currently supported.
    fileFormat :: Prelude.Maybe FileFormat,
    -- | An array of objects to specify a filter that exports a more specific set
    -- of ECS service recommendations.
    filters :: Prelude.Maybe [ECSServiceRecommendationFilter],
    -- | If your account is the management account or the delegated administrator
    -- of an organization, this parameter indicates whether to include
    -- recommendations for resources in all member accounts of the
    -- organization.
    --
    -- The member accounts must also be opted in to Compute Optimizer, and
    -- trusted access for Compute Optimizer must be enabled in the organization
    -- account. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
    -- in the /Compute Optimizer User Guide/.
    --
    -- If this parameter is omitted, recommendations for member accounts of the
    -- organization aren\'t included in the export file.
    --
    -- If this parameter or the account ID parameter is omitted,
    -- recommendations for member accounts aren\'t included in the export.
    includeMemberAccounts :: Prelude.Maybe Prelude.Bool,
    s3DestinationConfig :: S3DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportECSServiceRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'exportECSServiceRecommendations_accountIds' - The Amazon Web Services account IDs for the export ECS service
-- recommendations.
--
-- If your account is the management account or the delegated administrator
-- of an organization, use this parameter to specify the member account you
-- want to export recommendations to.
--
-- This parameter can\'t be specified together with the include member
-- accounts parameter. The parameters are mutually exclusive.
--
-- If this parameter or the include member accounts parameter is omitted,
-- the recommendations for member accounts aren\'t included in the export.
--
-- You can specify multiple account IDs per request.
--
-- 'fieldsToExport', 'exportECSServiceRecommendations_fieldsToExport' - The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
--
-- 'fileFormat', 'exportECSServiceRecommendations_fileFormat' - The format of the export file.
--
-- The CSV file is the only export file format currently supported.
--
-- 'filters', 'exportECSServiceRecommendations_filters' - An array of objects to specify a filter that exports a more specific set
-- of ECS service recommendations.
--
-- 'includeMemberAccounts', 'exportECSServiceRecommendations_includeMemberAccounts' - If your account is the management account or the delegated administrator
-- of an organization, this parameter indicates whether to include
-- recommendations for resources in all member accounts of the
-- organization.
--
-- The member accounts must also be opted in to Compute Optimizer, and
-- trusted access for Compute Optimizer must be enabled in the organization
-- account. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
-- in the /Compute Optimizer User Guide/.
--
-- If this parameter is omitted, recommendations for member accounts of the
-- organization aren\'t included in the export file.
--
-- If this parameter or the account ID parameter is omitted,
-- recommendations for member accounts aren\'t included in the export.
--
-- 's3DestinationConfig', 'exportECSServiceRecommendations_s3DestinationConfig' - Undocumented member.
newExportECSServiceRecommendations ::
  -- | 's3DestinationConfig'
  S3DestinationConfig ->
  ExportECSServiceRecommendations
newExportECSServiceRecommendations
  pS3DestinationConfig_ =
    ExportECSServiceRecommendations'
      { accountIds =
          Prelude.Nothing,
        fieldsToExport = Prelude.Nothing,
        fileFormat = Prelude.Nothing,
        filters = Prelude.Nothing,
        includeMemberAccounts = Prelude.Nothing,
        s3DestinationConfig =
          pS3DestinationConfig_
      }

-- | The Amazon Web Services account IDs for the export ECS service
-- recommendations.
--
-- If your account is the management account or the delegated administrator
-- of an organization, use this parameter to specify the member account you
-- want to export recommendations to.
--
-- This parameter can\'t be specified together with the include member
-- accounts parameter. The parameters are mutually exclusive.
--
-- If this parameter or the include member accounts parameter is omitted,
-- the recommendations for member accounts aren\'t included in the export.
--
-- You can specify multiple account IDs per request.
exportECSServiceRecommendations_accountIds :: Lens.Lens' ExportECSServiceRecommendations (Prelude.Maybe [Prelude.Text])
exportECSServiceRecommendations_accountIds = Lens.lens (\ExportECSServiceRecommendations' {accountIds} -> accountIds) (\s@ExportECSServiceRecommendations' {} a -> s {accountIds = a} :: ExportECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
exportECSServiceRecommendations_fieldsToExport :: Lens.Lens' ExportECSServiceRecommendations (Prelude.Maybe [ExportableECSServiceField])
exportECSServiceRecommendations_fieldsToExport = Lens.lens (\ExportECSServiceRecommendations' {fieldsToExport} -> fieldsToExport) (\s@ExportECSServiceRecommendations' {} a -> s {fieldsToExport = a} :: ExportECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The format of the export file.
--
-- The CSV file is the only export file format currently supported.
exportECSServiceRecommendations_fileFormat :: Lens.Lens' ExportECSServiceRecommendations (Prelude.Maybe FileFormat)
exportECSServiceRecommendations_fileFormat = Lens.lens (\ExportECSServiceRecommendations' {fileFormat} -> fileFormat) (\s@ExportECSServiceRecommendations' {} a -> s {fileFormat = a} :: ExportECSServiceRecommendations)

-- | An array of objects to specify a filter that exports a more specific set
-- of ECS service recommendations.
exportECSServiceRecommendations_filters :: Lens.Lens' ExportECSServiceRecommendations (Prelude.Maybe [ECSServiceRecommendationFilter])
exportECSServiceRecommendations_filters = Lens.lens (\ExportECSServiceRecommendations' {filters} -> filters) (\s@ExportECSServiceRecommendations' {} a -> s {filters = a} :: ExportECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | If your account is the management account or the delegated administrator
-- of an organization, this parameter indicates whether to include
-- recommendations for resources in all member accounts of the
-- organization.
--
-- The member accounts must also be opted in to Compute Optimizer, and
-- trusted access for Compute Optimizer must be enabled in the organization
-- account. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/security-iam.html#trusted-service-access Compute Optimizer and Amazon Web Services Organizations trusted access>
-- in the /Compute Optimizer User Guide/.
--
-- If this parameter is omitted, recommendations for member accounts of the
-- organization aren\'t included in the export file.
--
-- If this parameter or the account ID parameter is omitted,
-- recommendations for member accounts aren\'t included in the export.
exportECSServiceRecommendations_includeMemberAccounts :: Lens.Lens' ExportECSServiceRecommendations (Prelude.Maybe Prelude.Bool)
exportECSServiceRecommendations_includeMemberAccounts = Lens.lens (\ExportECSServiceRecommendations' {includeMemberAccounts} -> includeMemberAccounts) (\s@ExportECSServiceRecommendations' {} a -> s {includeMemberAccounts = a} :: ExportECSServiceRecommendations)

-- | Undocumented member.
exportECSServiceRecommendations_s3DestinationConfig :: Lens.Lens' ExportECSServiceRecommendations S3DestinationConfig
exportECSServiceRecommendations_s3DestinationConfig = Lens.lens (\ExportECSServiceRecommendations' {s3DestinationConfig} -> s3DestinationConfig) (\s@ExportECSServiceRecommendations' {} a -> s {s3DestinationConfig = a} :: ExportECSServiceRecommendations)

instance
  Core.AWSRequest
    ExportECSServiceRecommendations
  where
  type
    AWSResponse ExportECSServiceRecommendations =
      ExportECSServiceRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportECSServiceRecommendationsResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "s3Destination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportECSServiceRecommendations
  where
  hashWithSalt
    _salt
    ExportECSServiceRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` fieldsToExport
        `Prelude.hashWithSalt` fileFormat
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` includeMemberAccounts
        `Prelude.hashWithSalt` s3DestinationConfig

instance
  Prelude.NFData
    ExportECSServiceRecommendations
  where
  rnf ExportECSServiceRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf fieldsToExport
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeMemberAccounts
      `Prelude.seq` Prelude.rnf s3DestinationConfig

instance
  Data.ToHeaders
    ExportECSServiceRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.ExportECSServiceRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportECSServiceRecommendations where
  toJSON ExportECSServiceRecommendations' {..} =
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

instance Data.ToPath ExportECSServiceRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportECSServiceRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportECSServiceRecommendationsResponse' smart constructor.
data ExportECSServiceRecommendationsResponse = ExportECSServiceRecommendationsResponse'
  { -- | The identification number of the export job.
    --
    -- To view the status of an export job, use the
    -- DescribeRecommendationExportJobs action and specify the job ID.
    jobId :: Prelude.Maybe Prelude.Text,
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportECSServiceRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'exportECSServiceRecommendationsResponse_jobId' - The identification number of the export job.
--
-- To view the status of an export job, use the
-- DescribeRecommendationExportJobs action and specify the job ID.
--
-- 's3Destination', 'exportECSServiceRecommendationsResponse_s3Destination' - Undocumented member.
--
-- 'httpStatus', 'exportECSServiceRecommendationsResponse_httpStatus' - The response's http status code.
newExportECSServiceRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportECSServiceRecommendationsResponse
newExportECSServiceRecommendationsResponse
  pHttpStatus_ =
    ExportECSServiceRecommendationsResponse'
      { jobId =
          Prelude.Nothing,
        s3Destination = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identification number of the export job.
--
-- To view the status of an export job, use the
-- DescribeRecommendationExportJobs action and specify the job ID.
exportECSServiceRecommendationsResponse_jobId :: Lens.Lens' ExportECSServiceRecommendationsResponse (Prelude.Maybe Prelude.Text)
exportECSServiceRecommendationsResponse_jobId = Lens.lens (\ExportECSServiceRecommendationsResponse' {jobId} -> jobId) (\s@ExportECSServiceRecommendationsResponse' {} a -> s {jobId = a} :: ExportECSServiceRecommendationsResponse)

-- | Undocumented member.
exportECSServiceRecommendationsResponse_s3Destination :: Lens.Lens' ExportECSServiceRecommendationsResponse (Prelude.Maybe S3Destination)
exportECSServiceRecommendationsResponse_s3Destination = Lens.lens (\ExportECSServiceRecommendationsResponse' {s3Destination} -> s3Destination) (\s@ExportECSServiceRecommendationsResponse' {} a -> s {s3Destination = a} :: ExportECSServiceRecommendationsResponse)

-- | The response's http status code.
exportECSServiceRecommendationsResponse_httpStatus :: Lens.Lens' ExportECSServiceRecommendationsResponse Prelude.Int
exportECSServiceRecommendationsResponse_httpStatus = Lens.lens (\ExportECSServiceRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ExportECSServiceRecommendationsResponse' {} a -> s {httpStatus = a} :: ExportECSServiceRecommendationsResponse)

instance
  Prelude.NFData
    ExportECSServiceRecommendationsResponse
  where
  rnf ExportECSServiceRecommendationsResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf httpStatus
