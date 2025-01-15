{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostAndUsageReport.Types.ReportDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Types.ReportDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostAndUsageReport.Types.AWSRegion
import Amazonka.CostAndUsageReport.Types.AdditionalArtifact
import Amazonka.CostAndUsageReport.Types.CompressionFormat
import Amazonka.CostAndUsageReport.Types.ReportFormat
import Amazonka.CostAndUsageReport.Types.ReportVersioning
import Amazonka.CostAndUsageReport.Types.SchemaElement
import Amazonka.CostAndUsageReport.Types.TimeUnit
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The definition of AWS Cost and Usage Report. You can specify the report
-- name, time unit, report format, compression format, S3 bucket,
-- additional artifacts, and schema elements in the definition.
--
-- /See:/ 'newReportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { -- | A list of manifests that you want Amazon Web Services to create for this
    -- report.
    additionalArtifacts :: Prelude.Maybe [AdditionalArtifact],
    -- | The Amazon resource name of the billing view. You can get this value by
    -- using the billing view service public APIs.
    billingViewArn :: Prelude.Maybe Prelude.Text,
    -- | Whether you want Amazon Web Services to update your reports after they
    -- have been finalized if Amazon Web Services detects charges related to
    -- previous months. These charges can include refunds, credits, or support
    -- fees.
    refreshClosedReports :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want Amazon Web Services to overwrite the previous version
    -- of each report or to deliver the report in addition to the previous
    -- versions.
    reportVersioning :: Prelude.Maybe ReportVersioning,
    reportName :: Prelude.Text,
    timeUnit :: TimeUnit,
    format :: ReportFormat,
    compression :: CompressionFormat,
    -- | A list of strings that indicate additional content that Amazon Web
    -- Services includes in the report, such as individual resource IDs.
    additionalSchemaElements :: [SchemaElement],
    s3Bucket :: Prelude.Text,
    s3Prefix :: Prelude.Text,
    s3Region :: AWSRegion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalArtifacts', 'reportDefinition_additionalArtifacts' - A list of manifests that you want Amazon Web Services to create for this
-- report.
--
-- 'billingViewArn', 'reportDefinition_billingViewArn' - The Amazon resource name of the billing view. You can get this value by
-- using the billing view service public APIs.
--
-- 'refreshClosedReports', 'reportDefinition_refreshClosedReports' - Whether you want Amazon Web Services to update your reports after they
-- have been finalized if Amazon Web Services detects charges related to
-- previous months. These charges can include refunds, credits, or support
-- fees.
--
-- 'reportVersioning', 'reportDefinition_reportVersioning' - Whether you want Amazon Web Services to overwrite the previous version
-- of each report or to deliver the report in addition to the previous
-- versions.
--
-- 'reportName', 'reportDefinition_reportName' - Undocumented member.
--
-- 'timeUnit', 'reportDefinition_timeUnit' - Undocumented member.
--
-- 'format', 'reportDefinition_format' - Undocumented member.
--
-- 'compression', 'reportDefinition_compression' - Undocumented member.
--
-- 'additionalSchemaElements', 'reportDefinition_additionalSchemaElements' - A list of strings that indicate additional content that Amazon Web
-- Services includes in the report, such as individual resource IDs.
--
-- 's3Bucket', 'reportDefinition_s3Bucket' - Undocumented member.
--
-- 's3Prefix', 'reportDefinition_s3Prefix' - Undocumented member.
--
-- 's3Region', 'reportDefinition_s3Region' - Undocumented member.
newReportDefinition ::
  -- | 'reportName'
  Prelude.Text ->
  -- | 'timeUnit'
  TimeUnit ->
  -- | 'format'
  ReportFormat ->
  -- | 'compression'
  CompressionFormat ->
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Prefix'
  Prelude.Text ->
  -- | 's3Region'
  AWSRegion ->
  ReportDefinition
newReportDefinition
  pReportName_
  pTimeUnit_
  pFormat_
  pCompression_
  pS3Bucket_
  pS3Prefix_
  pS3Region_ =
    ReportDefinition'
      { additionalArtifacts =
          Prelude.Nothing,
        billingViewArn = Prelude.Nothing,
        refreshClosedReports = Prelude.Nothing,
        reportVersioning = Prelude.Nothing,
        reportName = pReportName_,
        timeUnit = pTimeUnit_,
        format = pFormat_,
        compression = pCompression_,
        additionalSchemaElements = Prelude.mempty,
        s3Bucket = pS3Bucket_,
        s3Prefix = pS3Prefix_,
        s3Region = pS3Region_
      }

-- | A list of manifests that you want Amazon Web Services to create for this
-- report.
reportDefinition_additionalArtifacts :: Lens.Lens' ReportDefinition (Prelude.Maybe [AdditionalArtifact])
reportDefinition_additionalArtifacts = Lens.lens (\ReportDefinition' {additionalArtifacts} -> additionalArtifacts) (\s@ReportDefinition' {} a -> s {additionalArtifacts = a} :: ReportDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon resource name of the billing view. You can get this value by
-- using the billing view service public APIs.
reportDefinition_billingViewArn :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.Text)
reportDefinition_billingViewArn = Lens.lens (\ReportDefinition' {billingViewArn} -> billingViewArn) (\s@ReportDefinition' {} a -> s {billingViewArn = a} :: ReportDefinition)

-- | Whether you want Amazon Web Services to update your reports after they
-- have been finalized if Amazon Web Services detects charges related to
-- previous months. These charges can include refunds, credits, or support
-- fees.
reportDefinition_refreshClosedReports :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.Bool)
reportDefinition_refreshClosedReports = Lens.lens (\ReportDefinition' {refreshClosedReports} -> refreshClosedReports) (\s@ReportDefinition' {} a -> s {refreshClosedReports = a} :: ReportDefinition)

-- | Whether you want Amazon Web Services to overwrite the previous version
-- of each report or to deliver the report in addition to the previous
-- versions.
reportDefinition_reportVersioning :: Lens.Lens' ReportDefinition (Prelude.Maybe ReportVersioning)
reportDefinition_reportVersioning = Lens.lens (\ReportDefinition' {reportVersioning} -> reportVersioning) (\s@ReportDefinition' {} a -> s {reportVersioning = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_reportName :: Lens.Lens' ReportDefinition Prelude.Text
reportDefinition_reportName = Lens.lens (\ReportDefinition' {reportName} -> reportName) (\s@ReportDefinition' {} a -> s {reportName = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_timeUnit :: Lens.Lens' ReportDefinition TimeUnit
reportDefinition_timeUnit = Lens.lens (\ReportDefinition' {timeUnit} -> timeUnit) (\s@ReportDefinition' {} a -> s {timeUnit = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_format :: Lens.Lens' ReportDefinition ReportFormat
reportDefinition_format = Lens.lens (\ReportDefinition' {format} -> format) (\s@ReportDefinition' {} a -> s {format = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_compression :: Lens.Lens' ReportDefinition CompressionFormat
reportDefinition_compression = Lens.lens (\ReportDefinition' {compression} -> compression) (\s@ReportDefinition' {} a -> s {compression = a} :: ReportDefinition)

-- | A list of strings that indicate additional content that Amazon Web
-- Services includes in the report, such as individual resource IDs.
reportDefinition_additionalSchemaElements :: Lens.Lens' ReportDefinition [SchemaElement]
reportDefinition_additionalSchemaElements = Lens.lens (\ReportDefinition' {additionalSchemaElements} -> additionalSchemaElements) (\s@ReportDefinition' {} a -> s {additionalSchemaElements = a} :: ReportDefinition) Prelude.. Lens.coerced

-- | Undocumented member.
reportDefinition_s3Bucket :: Lens.Lens' ReportDefinition Prelude.Text
reportDefinition_s3Bucket = Lens.lens (\ReportDefinition' {s3Bucket} -> s3Bucket) (\s@ReportDefinition' {} a -> s {s3Bucket = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_s3Prefix :: Lens.Lens' ReportDefinition Prelude.Text
reportDefinition_s3Prefix = Lens.lens (\ReportDefinition' {s3Prefix} -> s3Prefix) (\s@ReportDefinition' {} a -> s {s3Prefix = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_s3Region :: Lens.Lens' ReportDefinition AWSRegion
reportDefinition_s3Region = Lens.lens (\ReportDefinition' {s3Region} -> s3Region) (\s@ReportDefinition' {} a -> s {s3Region = a} :: ReportDefinition)

instance Data.FromJSON ReportDefinition where
  parseJSON =
    Data.withObject
      "ReportDefinition"
      ( \x ->
          ReportDefinition'
            Prelude.<$> ( x
                            Data..:? "AdditionalArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BillingViewArn")
            Prelude.<*> (x Data..:? "RefreshClosedReports")
            Prelude.<*> (x Data..:? "ReportVersioning")
            Prelude.<*> (x Data..: "ReportName")
            Prelude.<*> (x Data..: "TimeUnit")
            Prelude.<*> (x Data..: "Format")
            Prelude.<*> (x Data..: "Compression")
            Prelude.<*> ( x
                            Data..:? "AdditionalSchemaElements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "S3Bucket")
            Prelude.<*> (x Data..: "S3Prefix")
            Prelude.<*> (x Data..: "S3Region")
      )

instance Prelude.Hashable ReportDefinition where
  hashWithSalt _salt ReportDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` additionalArtifacts
      `Prelude.hashWithSalt` billingViewArn
      `Prelude.hashWithSalt` refreshClosedReports
      `Prelude.hashWithSalt` reportVersioning
      `Prelude.hashWithSalt` reportName
      `Prelude.hashWithSalt` timeUnit
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` additionalSchemaElements
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` s3Region

instance Prelude.NFData ReportDefinition where
  rnf ReportDefinition' {..} =
    Prelude.rnf additionalArtifacts `Prelude.seq`
      Prelude.rnf billingViewArn `Prelude.seq`
        Prelude.rnf refreshClosedReports `Prelude.seq`
          Prelude.rnf reportVersioning `Prelude.seq`
            Prelude.rnf reportName `Prelude.seq`
              Prelude.rnf timeUnit `Prelude.seq`
                Prelude.rnf format `Prelude.seq`
                  Prelude.rnf compression `Prelude.seq`
                    Prelude.rnf additionalSchemaElements `Prelude.seq`
                      Prelude.rnf s3Bucket `Prelude.seq`
                        Prelude.rnf s3Prefix `Prelude.seq`
                          Prelude.rnf s3Region

instance Data.ToJSON ReportDefinition where
  toJSON ReportDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalArtifacts" Data..=)
              Prelude.<$> additionalArtifacts,
            ("BillingViewArn" Data..=)
              Prelude.<$> billingViewArn,
            ("RefreshClosedReports" Data..=)
              Prelude.<$> refreshClosedReports,
            ("ReportVersioning" Data..=)
              Prelude.<$> reportVersioning,
            Prelude.Just ("ReportName" Data..= reportName),
            Prelude.Just ("TimeUnit" Data..= timeUnit),
            Prelude.Just ("Format" Data..= format),
            Prelude.Just ("Compression" Data..= compression),
            Prelude.Just
              ( "AdditionalSchemaElements"
                  Data..= additionalSchemaElements
              ),
            Prelude.Just ("S3Bucket" Data..= s3Bucket),
            Prelude.Just ("S3Prefix" Data..= s3Prefix),
            Prelude.Just ("S3Region" Data..= s3Region)
          ]
      )
