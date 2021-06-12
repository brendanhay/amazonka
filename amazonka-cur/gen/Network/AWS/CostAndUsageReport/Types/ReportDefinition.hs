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
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportDefinition where

import qualified Network.AWS.Core as Core
import Network.AWS.CostAndUsageReport.Types.AWSRegion
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
import Network.AWS.CostAndUsageReport.Types.ReportFormat
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
import Network.AWS.CostAndUsageReport.Types.SchemaElement
import Network.AWS.CostAndUsageReport.Types.TimeUnit
import qualified Network.AWS.Lens as Lens

-- | The definition of AWS Cost and Usage Report. You can specify the report
-- name, time unit, report format, compression format, S3 bucket,
-- additional artifacts, and schema elements in the definition.
--
-- /See:/ 'newReportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { -- | A list of manifests that you want Amazon Web Services to create for this
    -- report.
    additionalArtifacts :: Core.Maybe [AdditionalArtifact],
    -- | Whether you want Amazon Web Services to overwrite the previous version
    -- of each report or to deliver the report in addition to the previous
    -- versions.
    reportVersioning :: Core.Maybe ReportVersioning,
    -- | Whether you want Amazon Web Services to update your reports after they
    -- have been finalized if Amazon Web Services detects charges related to
    -- previous months. These charges can include refunds, credits, or support
    -- fees.
    refreshClosedReports :: Core.Maybe Core.Bool,
    reportName :: Core.Text,
    timeUnit :: TimeUnit,
    format :: ReportFormat,
    compression :: CompressionFormat,
    -- | A list of strings that indicate additional content that Amazon Web
    -- Services includes in the report, such as individual resource IDs.
    additionalSchemaElements :: [SchemaElement],
    s3Bucket :: Core.Text,
    s3Prefix :: Core.Text,
    s3Region :: AWSRegion
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'reportVersioning', 'reportDefinition_reportVersioning' - Whether you want Amazon Web Services to overwrite the previous version
-- of each report or to deliver the report in addition to the previous
-- versions.
--
-- 'refreshClosedReports', 'reportDefinition_refreshClosedReports' - Whether you want Amazon Web Services to update your reports after they
-- have been finalized if Amazon Web Services detects charges related to
-- previous months. These charges can include refunds, credits, or support
-- fees.
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
  Core.Text ->
  -- | 'timeUnit'
  TimeUnit ->
  -- | 'format'
  ReportFormat ->
  -- | 'compression'
  CompressionFormat ->
  -- | 's3Bucket'
  Core.Text ->
  -- | 's3Prefix'
  Core.Text ->
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
          Core.Nothing,
        reportVersioning = Core.Nothing,
        refreshClosedReports = Core.Nothing,
        reportName = pReportName_,
        timeUnit = pTimeUnit_,
        format = pFormat_,
        compression = pCompression_,
        additionalSchemaElements = Core.mempty,
        s3Bucket = pS3Bucket_,
        s3Prefix = pS3Prefix_,
        s3Region = pS3Region_
      }

-- | A list of manifests that you want Amazon Web Services to create for this
-- report.
reportDefinition_additionalArtifacts :: Lens.Lens' ReportDefinition (Core.Maybe [AdditionalArtifact])
reportDefinition_additionalArtifacts = Lens.lens (\ReportDefinition' {additionalArtifacts} -> additionalArtifacts) (\s@ReportDefinition' {} a -> s {additionalArtifacts = a} :: ReportDefinition) Core.. Lens.mapping Lens._Coerce

-- | Whether you want Amazon Web Services to overwrite the previous version
-- of each report or to deliver the report in addition to the previous
-- versions.
reportDefinition_reportVersioning :: Lens.Lens' ReportDefinition (Core.Maybe ReportVersioning)
reportDefinition_reportVersioning = Lens.lens (\ReportDefinition' {reportVersioning} -> reportVersioning) (\s@ReportDefinition' {} a -> s {reportVersioning = a} :: ReportDefinition)

-- | Whether you want Amazon Web Services to update your reports after they
-- have been finalized if Amazon Web Services detects charges related to
-- previous months. These charges can include refunds, credits, or support
-- fees.
reportDefinition_refreshClosedReports :: Lens.Lens' ReportDefinition (Core.Maybe Core.Bool)
reportDefinition_refreshClosedReports = Lens.lens (\ReportDefinition' {refreshClosedReports} -> refreshClosedReports) (\s@ReportDefinition' {} a -> s {refreshClosedReports = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_reportName :: Lens.Lens' ReportDefinition Core.Text
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
reportDefinition_additionalSchemaElements = Lens.lens (\ReportDefinition' {additionalSchemaElements} -> additionalSchemaElements) (\s@ReportDefinition' {} a -> s {additionalSchemaElements = a} :: ReportDefinition) Core.. Lens._Coerce

-- | Undocumented member.
reportDefinition_s3Bucket :: Lens.Lens' ReportDefinition Core.Text
reportDefinition_s3Bucket = Lens.lens (\ReportDefinition' {s3Bucket} -> s3Bucket) (\s@ReportDefinition' {} a -> s {s3Bucket = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_s3Prefix :: Lens.Lens' ReportDefinition Core.Text
reportDefinition_s3Prefix = Lens.lens (\ReportDefinition' {s3Prefix} -> s3Prefix) (\s@ReportDefinition' {} a -> s {s3Prefix = a} :: ReportDefinition)

-- | Undocumented member.
reportDefinition_s3Region :: Lens.Lens' ReportDefinition AWSRegion
reportDefinition_s3Region = Lens.lens (\ReportDefinition' {s3Region} -> s3Region) (\s@ReportDefinition' {} a -> s {s3Region = a} :: ReportDefinition)

instance Core.FromJSON ReportDefinition where
  parseJSON =
    Core.withObject
      "ReportDefinition"
      ( \x ->
          ReportDefinition'
            Core.<$> ( x Core..:? "AdditionalArtifacts"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ReportVersioning")
            Core.<*> (x Core..:? "RefreshClosedReports")
            Core.<*> (x Core..: "ReportName")
            Core.<*> (x Core..: "TimeUnit")
            Core.<*> (x Core..: "Format")
            Core.<*> (x Core..: "Compression")
            Core.<*> ( x Core..:? "AdditionalSchemaElements"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..: "S3Bucket")
            Core.<*> (x Core..: "S3Prefix")
            Core.<*> (x Core..: "S3Region")
      )

instance Core.Hashable ReportDefinition

instance Core.NFData ReportDefinition

instance Core.ToJSON ReportDefinition where
  toJSON ReportDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AdditionalArtifacts" Core..=)
              Core.<$> additionalArtifacts,
            ("ReportVersioning" Core..=)
              Core.<$> reportVersioning,
            ("RefreshClosedReports" Core..=)
              Core.<$> refreshClosedReports,
            Core.Just ("ReportName" Core..= reportName),
            Core.Just ("TimeUnit" Core..= timeUnit),
            Core.Just ("Format" Core..= format),
            Core.Just ("Compression" Core..= compression),
            Core.Just
              ( "AdditionalSchemaElements"
                  Core..= additionalSchemaElements
              ),
            Core.Just ("S3Bucket" Core..= s3Bucket),
            Core.Just ("S3Prefix" Core..= s3Prefix),
            Core.Just ("S3Region" Core..= s3Region)
          ]
      )
