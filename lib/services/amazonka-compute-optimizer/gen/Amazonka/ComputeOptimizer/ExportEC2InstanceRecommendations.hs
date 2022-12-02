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
-- Module      : Amazonka.ComputeOptimizer.ExportEC2InstanceRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports optimization recommendations for Amazon EC2 instances.
--
-- Recommendations are exported in a comma-separated values (.csv) file,
-- and its metadata in a JavaScript Object Notation (JSON) (.json) file, to
-- an existing Amazon Simple Storage Service (Amazon S3) bucket that you
-- specify. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html Exporting Recommendations>
-- in the /Compute Optimizer User Guide/.
--
-- You can have only one Amazon EC2 instance export job in progress per
-- Amazon Web Services Region.
module Amazonka.ComputeOptimizer.ExportEC2InstanceRecommendations
  ( -- * Creating a Request
    ExportEC2InstanceRecommendations (..),
    newExportEC2InstanceRecommendations,

    -- * Request Lenses
    exportEC2InstanceRecommendations_accountIds,
    exportEC2InstanceRecommendations_recommendationPreferences,
    exportEC2InstanceRecommendations_filters,
    exportEC2InstanceRecommendations_includeMemberAccounts,
    exportEC2InstanceRecommendations_fileFormat,
    exportEC2InstanceRecommendations_fieldsToExport,
    exportEC2InstanceRecommendations_s3DestinationConfig,

    -- * Destructuring the Response
    ExportEC2InstanceRecommendationsResponse (..),
    newExportEC2InstanceRecommendationsResponse,

    -- * Response Lenses
    exportEC2InstanceRecommendationsResponse_jobId,
    exportEC2InstanceRecommendationsResponse_s3Destination,
    exportEC2InstanceRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportEC2InstanceRecommendations' smart constructor.
data ExportEC2InstanceRecommendations = ExportEC2InstanceRecommendations'
  { -- | The IDs of the Amazon Web Services accounts for which to export instance
    -- recommendations.
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
    -- | An object to specify the preferences for the Amazon EC2 instance
    -- recommendations to export.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences,
    -- | An array of objects to specify a filter that exports a more specific set
    -- of instance recommendations.
    filters :: Prelude.Maybe [Filter],
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
    fieldsToExport :: Prelude.Maybe [ExportableInstanceField],
    -- | An object to specify the destination Amazon Simple Storage Service
    -- (Amazon S3) bucket name and key prefix for the export job.
    --
    -- You must create the destination Amazon S3 bucket for your
    -- recommendations export before you create the export job. Compute
    -- Optimizer does not create the S3 bucket for you. After you create the S3
    -- bucket, ensure that it has the required permissions policy to allow
    -- Compute Optimizer to write the export file to it. If you plan to specify
    -- an object prefix when you create the export job, you must include the
    -- object prefix in the policy that you add to the S3 bucket. For more
    -- information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/create-s3-bucket-policy-for-compute-optimizer.html Amazon S3 Bucket Policy for Compute Optimizer>
    -- in the /Compute Optimizer User Guide/.
    s3DestinationConfig :: S3DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportEC2InstanceRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'exportEC2InstanceRecommendations_accountIds' - The IDs of the Amazon Web Services accounts for which to export instance
-- recommendations.
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
-- 'recommendationPreferences', 'exportEC2InstanceRecommendations_recommendationPreferences' - An object to specify the preferences for the Amazon EC2 instance
-- recommendations to export.
--
-- 'filters', 'exportEC2InstanceRecommendations_filters' - An array of objects to specify a filter that exports a more specific set
-- of instance recommendations.
--
-- 'includeMemberAccounts', 'exportEC2InstanceRecommendations_includeMemberAccounts' - Indicates whether to include recommendations for resources in all member
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
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the account IDs parameter, is omitted.
--
-- 'fileFormat', 'exportEC2InstanceRecommendations_fileFormat' - The format of the export file.
--
-- The only export file format currently supported is @Csv@.
--
-- 'fieldsToExport', 'exportEC2InstanceRecommendations_fieldsToExport' - The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
--
-- 's3DestinationConfig', 'exportEC2InstanceRecommendations_s3DestinationConfig' - An object to specify the destination Amazon Simple Storage Service
-- (Amazon S3) bucket name and key prefix for the export job.
--
-- You must create the destination Amazon S3 bucket for your
-- recommendations export before you create the export job. Compute
-- Optimizer does not create the S3 bucket for you. After you create the S3
-- bucket, ensure that it has the required permissions policy to allow
-- Compute Optimizer to write the export file to it. If you plan to specify
-- an object prefix when you create the export job, you must include the
-- object prefix in the policy that you add to the S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/create-s3-bucket-policy-for-compute-optimizer.html Amazon S3 Bucket Policy for Compute Optimizer>
-- in the /Compute Optimizer User Guide/.
newExportEC2InstanceRecommendations ::
  -- | 's3DestinationConfig'
  S3DestinationConfig ->
  ExportEC2InstanceRecommendations
newExportEC2InstanceRecommendations
  pS3DestinationConfig_ =
    ExportEC2InstanceRecommendations'
      { accountIds =
          Prelude.Nothing,
        recommendationPreferences =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        includeMemberAccounts = Prelude.Nothing,
        fileFormat = Prelude.Nothing,
        fieldsToExport = Prelude.Nothing,
        s3DestinationConfig =
          pS3DestinationConfig_
      }

-- | The IDs of the Amazon Web Services accounts for which to export instance
-- recommendations.
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
exportEC2InstanceRecommendations_accountIds :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe [Prelude.Text])
exportEC2InstanceRecommendations_accountIds = Lens.lens (\ExportEC2InstanceRecommendations' {accountIds} -> accountIds) (\s@ExportEC2InstanceRecommendations' {} a -> s {accountIds = a} :: ExportEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An object to specify the preferences for the Amazon EC2 instance
-- recommendations to export.
exportEC2InstanceRecommendations_recommendationPreferences :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe RecommendationPreferences)
exportEC2InstanceRecommendations_recommendationPreferences = Lens.lens (\ExportEC2InstanceRecommendations' {recommendationPreferences} -> recommendationPreferences) (\s@ExportEC2InstanceRecommendations' {} a -> s {recommendationPreferences = a} :: ExportEC2InstanceRecommendations)

-- | An array of objects to specify a filter that exports a more specific set
-- of instance recommendations.
exportEC2InstanceRecommendations_filters :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe [Filter])
exportEC2InstanceRecommendations_filters = Lens.lens (\ExportEC2InstanceRecommendations' {filters} -> filters) (\s@ExportEC2InstanceRecommendations' {} a -> s {filters = a} :: ExportEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

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
-- Recommendations for member accounts are not included in the export if
-- this parameter, or the account IDs parameter, is omitted.
exportEC2InstanceRecommendations_includeMemberAccounts :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe Prelude.Bool)
exportEC2InstanceRecommendations_includeMemberAccounts = Lens.lens (\ExportEC2InstanceRecommendations' {includeMemberAccounts} -> includeMemberAccounts) (\s@ExportEC2InstanceRecommendations' {} a -> s {includeMemberAccounts = a} :: ExportEC2InstanceRecommendations)

-- | The format of the export file.
--
-- The only export file format currently supported is @Csv@.
exportEC2InstanceRecommendations_fileFormat :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe FileFormat)
exportEC2InstanceRecommendations_fileFormat = Lens.lens (\ExportEC2InstanceRecommendations' {fileFormat} -> fileFormat) (\s@ExportEC2InstanceRecommendations' {} a -> s {fileFormat = a} :: ExportEC2InstanceRecommendations)

-- | The recommendations data to include in the export file. For more
-- information about the fields that can be exported, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/exporting-recommendations.html#exported-files Exported files>
-- in the /Compute Optimizer User Guide/.
exportEC2InstanceRecommendations_fieldsToExport :: Lens.Lens' ExportEC2InstanceRecommendations (Prelude.Maybe [ExportableInstanceField])
exportEC2InstanceRecommendations_fieldsToExport = Lens.lens (\ExportEC2InstanceRecommendations' {fieldsToExport} -> fieldsToExport) (\s@ExportEC2InstanceRecommendations' {} a -> s {fieldsToExport = a} :: ExportEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An object to specify the destination Amazon Simple Storage Service
-- (Amazon S3) bucket name and key prefix for the export job.
--
-- You must create the destination Amazon S3 bucket for your
-- recommendations export before you create the export job. Compute
-- Optimizer does not create the S3 bucket for you. After you create the S3
-- bucket, ensure that it has the required permissions policy to allow
-- Compute Optimizer to write the export file to it. If you plan to specify
-- an object prefix when you create the export job, you must include the
-- object prefix in the policy that you add to the S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/create-s3-bucket-policy-for-compute-optimizer.html Amazon S3 Bucket Policy for Compute Optimizer>
-- in the /Compute Optimizer User Guide/.
exportEC2InstanceRecommendations_s3DestinationConfig :: Lens.Lens' ExportEC2InstanceRecommendations S3DestinationConfig
exportEC2InstanceRecommendations_s3DestinationConfig = Lens.lens (\ExportEC2InstanceRecommendations' {s3DestinationConfig} -> s3DestinationConfig) (\s@ExportEC2InstanceRecommendations' {} a -> s {s3DestinationConfig = a} :: ExportEC2InstanceRecommendations)

instance
  Core.AWSRequest
    ExportEC2InstanceRecommendations
  where
  type
    AWSResponse ExportEC2InstanceRecommendations =
      ExportEC2InstanceRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportEC2InstanceRecommendationsResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "s3Destination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportEC2InstanceRecommendations
  where
  hashWithSalt
    _salt
    ExportEC2InstanceRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` recommendationPreferences
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` includeMemberAccounts
        `Prelude.hashWithSalt` fileFormat
        `Prelude.hashWithSalt` fieldsToExport
        `Prelude.hashWithSalt` s3DestinationConfig

instance
  Prelude.NFData
    ExportEC2InstanceRecommendations
  where
  rnf ExportEC2InstanceRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf recommendationPreferences
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeMemberAccounts
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf fieldsToExport
      `Prelude.seq` Prelude.rnf s3DestinationConfig

instance
  Data.ToHeaders
    ExportEC2InstanceRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.ExportEC2InstanceRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportEC2InstanceRecommendations where
  toJSON ExportEC2InstanceRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("recommendationPreferences" Data..=)
              Prelude.<$> recommendationPreferences,
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

instance Data.ToPath ExportEC2InstanceRecommendations where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ExportEC2InstanceRecommendations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportEC2InstanceRecommendationsResponse' smart constructor.
data ExportEC2InstanceRecommendationsResponse = ExportEC2InstanceRecommendationsResponse'
  { -- | The identification number of the export job.
    --
    -- Use the DescribeRecommendationExportJobs action, and specify the job ID
    -- to view the status of an export job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the destination Amazon S3 bucket of a
    -- recommendations export file.
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportEC2InstanceRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'exportEC2InstanceRecommendationsResponse_jobId' - The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
--
-- 's3Destination', 'exportEC2InstanceRecommendationsResponse_s3Destination' - An object that describes the destination Amazon S3 bucket of a
-- recommendations export file.
--
-- 'httpStatus', 'exportEC2InstanceRecommendationsResponse_httpStatus' - The response's http status code.
newExportEC2InstanceRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportEC2InstanceRecommendationsResponse
newExportEC2InstanceRecommendationsResponse
  pHttpStatus_ =
    ExportEC2InstanceRecommendationsResponse'
      { jobId =
          Prelude.Nothing,
        s3Destination = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identification number of the export job.
--
-- Use the DescribeRecommendationExportJobs action, and specify the job ID
-- to view the status of an export job.
exportEC2InstanceRecommendationsResponse_jobId :: Lens.Lens' ExportEC2InstanceRecommendationsResponse (Prelude.Maybe Prelude.Text)
exportEC2InstanceRecommendationsResponse_jobId = Lens.lens (\ExportEC2InstanceRecommendationsResponse' {jobId} -> jobId) (\s@ExportEC2InstanceRecommendationsResponse' {} a -> s {jobId = a} :: ExportEC2InstanceRecommendationsResponse)

-- | An object that describes the destination Amazon S3 bucket of a
-- recommendations export file.
exportEC2InstanceRecommendationsResponse_s3Destination :: Lens.Lens' ExportEC2InstanceRecommendationsResponse (Prelude.Maybe S3Destination)
exportEC2InstanceRecommendationsResponse_s3Destination = Lens.lens (\ExportEC2InstanceRecommendationsResponse' {s3Destination} -> s3Destination) (\s@ExportEC2InstanceRecommendationsResponse' {} a -> s {s3Destination = a} :: ExportEC2InstanceRecommendationsResponse)

-- | The response's http status code.
exportEC2InstanceRecommendationsResponse_httpStatus :: Lens.Lens' ExportEC2InstanceRecommendationsResponse Prelude.Int
exportEC2InstanceRecommendationsResponse_httpStatus = Lens.lens (\ExportEC2InstanceRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ExportEC2InstanceRecommendationsResponse' {} a -> s {httpStatus = a} :: ExportEC2InstanceRecommendationsResponse)

instance
  Prelude.NFData
    ExportEC2InstanceRecommendationsResponse
  where
  rnf ExportEC2InstanceRecommendationsResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf httpStatus
