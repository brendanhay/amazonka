{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportDefinition
  ( ReportDefinition (..),

    -- * Smart constructor
    mkReportDefinition,

    -- * Lenses
    rdReportName,
    rdTimeUnit,
    rdFormat,
    rdCompression,
    rdAdditionalSchemaElements,
    rdS3Bucket,
    rdS3Prefix,
    rdS3Region,
    rdAdditionalArtifacts,
    rdRefreshClosedReports,
    rdReportVersioning,
  )
where

import qualified Network.AWS.CostAndUsageReport.Types.AWSRegion as Types
import qualified Network.AWS.CostAndUsageReport.Types.AdditionalArtifact as Types
import qualified Network.AWS.CostAndUsageReport.Types.CompressionFormat as Types
import qualified Network.AWS.CostAndUsageReport.Types.ReportFormat as Types
import qualified Network.AWS.CostAndUsageReport.Types.ReportName as Types
import qualified Network.AWS.CostAndUsageReport.Types.ReportVersioning as Types
import qualified Network.AWS.CostAndUsageReport.Types.S3Bucket as Types
import qualified Network.AWS.CostAndUsageReport.Types.S3Prefix as Types
import qualified Network.AWS.CostAndUsageReport.Types.SchemaElement as Types
import qualified Network.AWS.CostAndUsageReport.Types.TimeUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition of AWS Cost and Usage Report. You can specify the report name, time unit, report format, compression format, S3 bucket, additional artifacts, and schema elements in the definition.
--
-- /See:/ 'mkReportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { reportName :: Types.ReportName,
    timeUnit :: Types.TimeUnit,
    format :: Types.ReportFormat,
    compression :: Types.CompressionFormat,
    -- | A list of strings that indicate additional content that Amazon Web Services includes in the report, such as individual resource IDs.
    additionalSchemaElements :: [Types.SchemaElement],
    s3Bucket :: Types.S3Bucket,
    s3Prefix :: Types.S3Prefix,
    s3Region :: Types.AWSRegion,
    -- | A list of manifests that you want Amazon Web Services to create for this report.
    additionalArtifacts :: Core.Maybe [Types.AdditionalArtifact],
    -- | Whether you want Amazon Web Services to update your reports after they have been finalized if Amazon Web Services detects charges related to previous months. These charges can include refunds, credits, or support fees.
    refreshClosedReports :: Core.Maybe Core.Bool,
    -- | Whether you want Amazon Web Services to overwrite the previous version of each report or to deliver the report in addition to the previous versions.
    reportVersioning :: Core.Maybe Types.ReportVersioning
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportDefinition' value with any optional fields omitted.
mkReportDefinition ::
  -- | 'reportName'
  Types.ReportName ->
  -- | 'timeUnit'
  Types.TimeUnit ->
  -- | 'format'
  Types.ReportFormat ->
  -- | 'compression'
  Types.CompressionFormat ->
  -- | 's3Bucket'
  Types.S3Bucket ->
  -- | 's3Prefix'
  Types.S3Prefix ->
  -- | 's3Region'
  Types.AWSRegion ->
  ReportDefinition
mkReportDefinition
  reportName
  timeUnit
  format
  compression
  s3Bucket
  s3Prefix
  s3Region =
    ReportDefinition'
      { reportName,
        timeUnit,
        format,
        compression,
        additionalSchemaElements = Core.mempty,
        s3Bucket,
        s3Prefix,
        s3Region,
        additionalArtifacts = Core.Nothing,
        refreshClosedReports = Core.Nothing,
        reportVersioning = Core.Nothing
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReportName :: Lens.Lens' ReportDefinition Types.ReportName
rdReportName = Lens.field @"reportName"
{-# DEPRECATED rdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTimeUnit :: Lens.Lens' ReportDefinition Types.TimeUnit
rdTimeUnit = Lens.field @"timeUnit"
{-# DEPRECATED rdTimeUnit "Use generic-lens or generic-optics with 'timeUnit' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdFormat :: Lens.Lens' ReportDefinition Types.ReportFormat
rdFormat = Lens.field @"format"
{-# DEPRECATED rdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCompression :: Lens.Lens' ReportDefinition Types.CompressionFormat
rdCompression = Lens.field @"compression"
{-# DEPRECATED rdCompression "Use generic-lens or generic-optics with 'compression' instead." #-}

-- | A list of strings that indicate additional content that Amazon Web Services includes in the report, such as individual resource IDs.
--
-- /Note:/ Consider using 'additionalSchemaElements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAdditionalSchemaElements :: Lens.Lens' ReportDefinition [Types.SchemaElement]
rdAdditionalSchemaElements = Lens.field @"additionalSchemaElements"
{-# DEPRECATED rdAdditionalSchemaElements "Use generic-lens or generic-optics with 'additionalSchemaElements' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Bucket :: Lens.Lens' ReportDefinition Types.S3Bucket
rdS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED rdS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Prefix :: Lens.Lens' ReportDefinition Types.S3Prefix
rdS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED rdS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Region :: Lens.Lens' ReportDefinition Types.AWSRegion
rdS3Region = Lens.field @"s3Region"
{-# DEPRECATED rdS3Region "Use generic-lens or generic-optics with 's3Region' instead." #-}

-- | A list of manifests that you want Amazon Web Services to create for this report.
--
-- /Note:/ Consider using 'additionalArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAdditionalArtifacts :: Lens.Lens' ReportDefinition (Core.Maybe [Types.AdditionalArtifact])
rdAdditionalArtifacts = Lens.field @"additionalArtifacts"
{-# DEPRECATED rdAdditionalArtifacts "Use generic-lens or generic-optics with 'additionalArtifacts' instead." #-}

-- | Whether you want Amazon Web Services to update your reports after they have been finalized if Amazon Web Services detects charges related to previous months. These charges can include refunds, credits, or support fees.
--
-- /Note:/ Consider using 'refreshClosedReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRefreshClosedReports :: Lens.Lens' ReportDefinition (Core.Maybe Core.Bool)
rdRefreshClosedReports = Lens.field @"refreshClosedReports"
{-# DEPRECATED rdRefreshClosedReports "Use generic-lens or generic-optics with 'refreshClosedReports' instead." #-}

-- | Whether you want Amazon Web Services to overwrite the previous version of each report or to deliver the report in addition to the previous versions.
--
-- /Note:/ Consider using 'reportVersioning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReportVersioning :: Lens.Lens' ReportDefinition (Core.Maybe Types.ReportVersioning)
rdReportVersioning = Lens.field @"reportVersioning"
{-# DEPRECATED rdReportVersioning "Use generic-lens or generic-optics with 'reportVersioning' instead." #-}

instance Core.FromJSON ReportDefinition where
  toJSON ReportDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReportName" Core..= reportName),
            Core.Just ("TimeUnit" Core..= timeUnit),
            Core.Just ("Format" Core..= format),
            Core.Just ("Compression" Core..= compression),
            Core.Just
              ("AdditionalSchemaElements" Core..= additionalSchemaElements),
            Core.Just ("S3Bucket" Core..= s3Bucket),
            Core.Just ("S3Prefix" Core..= s3Prefix),
            Core.Just ("S3Region" Core..= s3Region),
            ("AdditionalArtifacts" Core..=) Core.<$> additionalArtifacts,
            ("RefreshClosedReports" Core..=) Core.<$> refreshClosedReports,
            ("ReportVersioning" Core..=) Core.<$> reportVersioning
          ]
      )

instance Core.FromJSON ReportDefinition where
  parseJSON =
    Core.withObject "ReportDefinition" Core.$
      \x ->
        ReportDefinition'
          Core.<$> (x Core..: "ReportName")
          Core.<*> (x Core..: "TimeUnit")
          Core.<*> (x Core..: "Format")
          Core.<*> (x Core..: "Compression")
          Core.<*> (x Core..:? "AdditionalSchemaElements" Core..!= Core.mempty)
          Core.<*> (x Core..: "S3Bucket")
          Core.<*> (x Core..: "S3Prefix")
          Core.<*> (x Core..: "S3Region")
          Core.<*> (x Core..:? "AdditionalArtifacts")
          Core.<*> (x Core..:? "RefreshClosedReports")
          Core.<*> (x Core..:? "ReportVersioning")
