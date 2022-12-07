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
-- Module      : Amazonka.ComputeOptimizer.ExportEBSVolumeRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports optimization recommendations for Amazon EBS volumes.
--
-- Recommendations are exported in a comma-separated values (.csv) file,
-- and its metadata in a JavaScript Object Notation (JSON) (.json) file, to
-- an existing Amazon Simple Storage Service (Amazon S3) bucket that you
-- specify. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html Exporting Recommendations>
-- in the /Compute Optimizer User Guide/.
--
-- You can have only one Amazon EBS volume export job in progress per
-- Amazon Web Services Region.
module Amazonka.ComputeOptimizer.ExportEBSVolumeRecommendations
  ( -- * Creating a Request
    ExportEBSVolumeRecommendations (..),
    newExportEBSVolumeRecommendations,

    -- * Request Lenses
    exportEBSVolumeRecommendations_accountIds,
    exportEBSVolumeRecommendations_filters,
    exportEBSVolumeRecommendations_includeMemberAccounts,
    exportEBSVolumeRecommendations_fileFormat,
    exportEBSVolumeRecommendations_fieldsToExport,
    exportEBSVolumeRecommendations_s3DestinationConfig,

    -- * Destructuring the Response
    ExportEBSVolumeRecommendationsResponse (..),
    newExportEBSVolumeRecommendationsResponse,

    -- * Response Lenses
    exportEBSVolumeRecommendationsResponse_jobId,
    exportEBSVolumeRecommendationsResponse_s3Destination,
    exportEBSVolumeRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportEBSVolumeRecommendations' smart constructor.
data ExportEBSVolumeRecommendations = ExportEBSVolumeRecommendations'
  { -- | The IDs of the Amazon Web Services accounts for which to export Amazon
    -- EBS volume recommendations.
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
    -- | An array of objects to specify a filter that exports a more specific set
    -- of Amazon EBS volume recommendations.
    filters :: Prelude.Maybe [EBSFilter],
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
    -- | The format of the export file.
    --
    -- The only export file format currently supported is @Csv@.
    fileFormat :: Prelude.Maybe FileFormat,
    -- | The recommendations data to include in the export file. For more
    -- information about the fields that can be exported, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
    -- in the /Compute Optimizer User Guide/.
    fieldsToExport :: Prelude.Maybe [ExportableVolumeField],
    s3DestinationConfig :: S3DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportEBSVolumeRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'exportEBSVolumeRecommendations_accountIds' - The IDs of the Amazon Web Services accounts for which to export Amazon
-- EBS volume recommendations.
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
-- 'filters', 'exportEBSVolumeRecommendations_filters' - An array of objects to specify a filter that exports a more specific set
-- of Amazon EBS volume recommendations.
--
-- 'includeMemberAccounts', 'exportEBSVolumeRecommendations_includeMemberAccounts' - Indicates whether to include recommendations for resources in all member
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
-- 'fileFormat', 'exportEBSVolumeRecommendations_fileFormat' - The format of the export file.
--
-- The only export file format currently supported is @Csv@.
--
-- 'fieldsToExport', 'exportEBSVolumeRecommendations_fieldsToExport' - The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
--
-- 's3DestinationConfig', 'exportEBSVolumeRecommendations_s3DestinationConfig' - Undocumented member.
newExportEBSVolumeRecommendations ::
  -- | 's3DestinationConfig'
  S3DestinationConfig ->
  ExportEBSVolumeRecommendations
newExportEBSVolumeRecommendations
  pS3DestinationConfig_ =
    ExportEBSVolumeRecommendations'
      { accountIds =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        includeMemberAccounts = Prelude.Nothing,
        fileFormat = Prelude.Nothing,
        fieldsToExport = Prelude.Nothing,
        s3DestinationConfig = pS3DestinationConfig_
      }

-- | The IDs of the Amazon Web Services accounts for which to export Amazon
-- EBS volume recommendations.
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
exportEBSVolumeRecommendations_accountIds :: Lens.Lens' ExportEBSVolumeRecommendations (Prelude.Maybe [Prelude.Text])
exportEBSVolumeRecommendations_accountIds = Lens.lens (\ExportEBSVolumeRecommendations' {accountIds} -> accountIds) (\s@ExportEBSVolumeRecommendations' {} a -> s {accountIds = a} :: ExportEBSVolumeRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects to specify a filter that exports a more specific set
-- of Amazon EBS volume recommendations.
exportEBSVolumeRecommendations_filters :: Lens.Lens' ExportEBSVolumeRecommendations (Prelude.Maybe [EBSFilter])
exportEBSVolumeRecommendations_filters = Lens.lens (\ExportEBSVolumeRecommendations' {filters} -> filters) (\s@ExportEBSVolumeRecommendations' {} a -> s {filters = a} :: ExportEBSVolumeRecommendations) Prelude.. Lens.mapping Lens.coerced

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
exportEBSVolumeRecommendations_includeMemberAccounts :: Lens.Lens' ExportEBSVolumeRecommendations (Prelude.Maybe Prelude.Bool)
exportEBSVolumeRecommendations_includeMemberAccounts = Lens.lens (\ExportEBSVolumeRecommendations' {includeMemberAccounts} -> includeMemberAccounts) (\s@ExportEBSVolumeRecommendations' {} a -> s {includeMemberAccounts = a} :: ExportEBSVolumeRecommendations)

-- | The format of the export file.
--
-- The only export file format currently supported is @Csv@.
exportEBSVolumeRecommendations_fileFormat :: Lens.Lens' ExportEBSVolumeRecommendations (Prelude.Maybe FileFormat)
exportEBSVolumeRecommendations_fileFormat = Lens.lens (\ExportEBSVolumeRecommendations' {fileFormat} -> fileFormat) (\s@ExportEBSVolumeRecommendations' {} a -> s {fileFormat = a} :: ExportEBSVolumeRecommendations)

-- | The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
exportEBSVolumeRecommendations_fieldsToExport :: Lens.Lens' ExportEBSVolumeRecommendations (Prelude.Maybe [ExportableVolumeField])
exportEBSVolumeRecommendations_fieldsToExport = Lens.lens (\ExportEBSVolumeRecommendations' {fieldsToExport} -> fieldsToExport) (\s@ExportEBSVolumeRecommendations' {} a -> s {fieldsToExport = a} :: ExportEBSVolumeRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
exportEBSVolumeRecommendations_s3DestinationConfig :: Lens.Lens' ExportEBSVolumeRecommendations S3DestinationConfig
exportEBSVolumeRecommendations_s3DestinationConfig = Lens.lens (\ExportEBSVolumeRecommendations' {s3DestinationConfig} -> s3DestinationConfig) (\s@ExportEBSVolumeRecommendations' {} a -> s {s3DestinationConfig = a} :: ExportEBSVolumeRecommendations)

instance
  Core.AWSRequest
    ExportEBSVolumeRecommendations
  where
  type
    AWSResponse ExportEBSVolumeRecommendations =
      ExportEBSVolumeRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportEBSVolumeRecommendationsResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "s3Destination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportEBSVolumeRecommendations
  where
  hashWithSalt
    _salt
    ExportEBSVolumeRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` includeMemberAccounts
        `Prelude.hashWithSalt` fileFormat
        `Prelude.hashWithSalt` fieldsToExport
        `Prelude.hashWithSalt` s3DestinationConfig

instance
  Prelude.NFData
    ExportEBSVolumeRecommendations
  where
  rnf ExportEBSVolumeRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeMemberAccounts
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf fieldsToExport
      `Prelude.seq` Prelude.rnf s3DestinationConfig

instance
  Data.ToHeaders
    ExportEBSVolumeRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.ExportEBSVolumeRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportEBSVolumeRecommendations where
  toJSON ExportEBSVolumeRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("filters" Data..=) Prelude.<$> filters,
            ("includeMemberAccounts" Data..=)
              Prelude.<$> includeMemberAccounts,
            ("fileFormat" Data..=) Prelude.<$> fileFormat,
            ("fieldsToExport" Data..=)
              Prelude.<$> fieldsToExport,
            Prelude.Just
              ("s3DestinationConfig" Data..= s3DestinationConfig)
          ]
      )

instance Data.ToPath ExportEBSVolumeRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportEBSVolumeRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportEBSVolumeRecommendationsResponse' smart constructor.
data ExportEBSVolumeRecommendationsResponse = ExportEBSVolumeRecommendationsResponse'
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
-- Create a value of 'ExportEBSVolumeRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'exportEBSVolumeRecommendationsResponse_jobId' - The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
--
-- 's3Destination', 'exportEBSVolumeRecommendationsResponse_s3Destination' - Undocumented member.
--
-- 'httpStatus', 'exportEBSVolumeRecommendationsResponse_httpStatus' - The response's http status code.
newExportEBSVolumeRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportEBSVolumeRecommendationsResponse
newExportEBSVolumeRecommendationsResponse
  pHttpStatus_ =
    ExportEBSVolumeRecommendationsResponse'
      { jobId =
          Prelude.Nothing,
        s3Destination = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
exportEBSVolumeRecommendationsResponse_jobId :: Lens.Lens' ExportEBSVolumeRecommendationsResponse (Prelude.Maybe Prelude.Text)
exportEBSVolumeRecommendationsResponse_jobId = Lens.lens (\ExportEBSVolumeRecommendationsResponse' {jobId} -> jobId) (\s@ExportEBSVolumeRecommendationsResponse' {} a -> s {jobId = a} :: ExportEBSVolumeRecommendationsResponse)

-- | Undocumented member.
exportEBSVolumeRecommendationsResponse_s3Destination :: Lens.Lens' ExportEBSVolumeRecommendationsResponse (Prelude.Maybe S3Destination)
exportEBSVolumeRecommendationsResponse_s3Destination = Lens.lens (\ExportEBSVolumeRecommendationsResponse' {s3Destination} -> s3Destination) (\s@ExportEBSVolumeRecommendationsResponse' {} a -> s {s3Destination = a} :: ExportEBSVolumeRecommendationsResponse)

-- | The response's http status code.
exportEBSVolumeRecommendationsResponse_httpStatus :: Lens.Lens' ExportEBSVolumeRecommendationsResponse Prelude.Int
exportEBSVolumeRecommendationsResponse_httpStatus = Lens.lens (\ExportEBSVolumeRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ExportEBSVolumeRecommendationsResponse' {} a -> s {httpStatus = a} :: ExportEBSVolumeRecommendationsResponse)

instance
  Prelude.NFData
    ExportEBSVolumeRecommendationsResponse
  where
  rnf ExportEBSVolumeRecommendationsResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf httpStatus
