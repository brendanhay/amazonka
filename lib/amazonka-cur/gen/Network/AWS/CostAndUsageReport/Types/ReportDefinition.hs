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
    rdReportVersioning,
    rdS3Region,
    rdFormat,
    rdTimeUnit,
    rdCompression,
    rdReportName,
    rdAdditionalArtifacts,
    rdRefreshClosedReports,
    rdAdditionalSchemaElements,
    rdS3Prefix,
    rdS3Bucket,
  )
where

import Network.AWS.CostAndUsageReport.Types.AWSRegion
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
import Network.AWS.CostAndUsageReport.Types.ReportFormat
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
import Network.AWS.CostAndUsageReport.Types.SchemaElement
import Network.AWS.CostAndUsageReport.Types.TimeUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The definition of AWS Cost and Usage Report. You can specify the report name, time unit, report format, compression format, S3 bucket, additional artifacts, and schema elements in the definition.
--
-- /See:/ 'mkReportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { -- | Whether you want Amazon Web Services to overwrite the previous version of each report or to deliver the report in addition to the previous versions.
    reportVersioning :: Lude.Maybe ReportVersioning,
    s3Region :: AWSRegion,
    format :: ReportFormat,
    timeUnit :: TimeUnit,
    compression :: CompressionFormat,
    reportName :: Lude.Text,
    -- | A list of manifests that you want Amazon Web Services to create for this report.
    additionalArtifacts :: Lude.Maybe [AdditionalArtifact],
    -- | Whether you want Amazon Web Services to update your reports after they have been finalized if Amazon Web Services detects charges related to previous months. These charges can include refunds, credits, or support fees.
    refreshClosedReports :: Lude.Maybe Lude.Bool,
    -- | A list of strings that indicate additional content that Amazon Web Services includes in the report, such as individual resource IDs.
    additionalSchemaElements :: [SchemaElement],
    s3Prefix :: Lude.Text,
    s3Bucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportDefinition' with the minimum fields required to make a request.
--
-- * 'reportVersioning' - Whether you want Amazon Web Services to overwrite the previous version of each report or to deliver the report in addition to the previous versions.
-- * 's3Region' -
-- * 'format' -
-- * 'timeUnit' -
-- * 'compression' -
-- * 'reportName' -
-- * 'additionalArtifacts' - A list of manifests that you want Amazon Web Services to create for this report.
-- * 'refreshClosedReports' - Whether you want Amazon Web Services to update your reports after they have been finalized if Amazon Web Services detects charges related to previous months. These charges can include refunds, credits, or support fees.
-- * 'additionalSchemaElements' - A list of strings that indicate additional content that Amazon Web Services includes in the report, such as individual resource IDs.
-- * 's3Prefix' -
-- * 's3Bucket' -
mkReportDefinition ::
  -- | 's3Region'
  AWSRegion ->
  -- | 'format'
  ReportFormat ->
  -- | 'timeUnit'
  TimeUnit ->
  -- | 'compression'
  CompressionFormat ->
  -- | 'reportName'
  Lude.Text ->
  -- | 's3Prefix'
  Lude.Text ->
  -- | 's3Bucket'
  Lude.Text ->
  ReportDefinition
mkReportDefinition
  pS3Region_
  pFormat_
  pTimeUnit_
  pCompression_
  pReportName_
  pS3Prefix_
  pS3Bucket_ =
    ReportDefinition'
      { reportVersioning = Lude.Nothing,
        s3Region = pS3Region_,
        format = pFormat_,
        timeUnit = pTimeUnit_,
        compression = pCompression_,
        reportName = pReportName_,
        additionalArtifacts = Lude.Nothing,
        refreshClosedReports = Lude.Nothing,
        additionalSchemaElements = Lude.mempty,
        s3Prefix = pS3Prefix_,
        s3Bucket = pS3Bucket_
      }

-- | Whether you want Amazon Web Services to overwrite the previous version of each report or to deliver the report in addition to the previous versions.
--
-- /Note:/ Consider using 'reportVersioning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReportVersioning :: Lens.Lens' ReportDefinition (Lude.Maybe ReportVersioning)
rdReportVersioning = Lens.lens (reportVersioning :: ReportDefinition -> Lude.Maybe ReportVersioning) (\s a -> s {reportVersioning = a} :: ReportDefinition)
{-# DEPRECATED rdReportVersioning "Use generic-lens or generic-optics with 'reportVersioning' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Region :: Lens.Lens' ReportDefinition AWSRegion
rdS3Region = Lens.lens (s3Region :: ReportDefinition -> AWSRegion) (\s a -> s {s3Region = a} :: ReportDefinition)
{-# DEPRECATED rdS3Region "Use generic-lens or generic-optics with 's3Region' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdFormat :: Lens.Lens' ReportDefinition ReportFormat
rdFormat = Lens.lens (format :: ReportDefinition -> ReportFormat) (\s a -> s {format = a} :: ReportDefinition)
{-# DEPRECATED rdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTimeUnit :: Lens.Lens' ReportDefinition TimeUnit
rdTimeUnit = Lens.lens (timeUnit :: ReportDefinition -> TimeUnit) (\s a -> s {timeUnit = a} :: ReportDefinition)
{-# DEPRECATED rdTimeUnit "Use generic-lens or generic-optics with 'timeUnit' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCompression :: Lens.Lens' ReportDefinition CompressionFormat
rdCompression = Lens.lens (compression :: ReportDefinition -> CompressionFormat) (\s a -> s {compression = a} :: ReportDefinition)
{-# DEPRECATED rdCompression "Use generic-lens or generic-optics with 'compression' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReportName :: Lens.Lens' ReportDefinition Lude.Text
rdReportName = Lens.lens (reportName :: ReportDefinition -> Lude.Text) (\s a -> s {reportName = a} :: ReportDefinition)
{-# DEPRECATED rdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

-- | A list of manifests that you want Amazon Web Services to create for this report.
--
-- /Note:/ Consider using 'additionalArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAdditionalArtifacts :: Lens.Lens' ReportDefinition (Lude.Maybe [AdditionalArtifact])
rdAdditionalArtifacts = Lens.lens (additionalArtifacts :: ReportDefinition -> Lude.Maybe [AdditionalArtifact]) (\s a -> s {additionalArtifacts = a} :: ReportDefinition)
{-# DEPRECATED rdAdditionalArtifacts "Use generic-lens or generic-optics with 'additionalArtifacts' instead." #-}

-- | Whether you want Amazon Web Services to update your reports after they have been finalized if Amazon Web Services detects charges related to previous months. These charges can include refunds, credits, or support fees.
--
-- /Note:/ Consider using 'refreshClosedReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRefreshClosedReports :: Lens.Lens' ReportDefinition (Lude.Maybe Lude.Bool)
rdRefreshClosedReports = Lens.lens (refreshClosedReports :: ReportDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {refreshClosedReports = a} :: ReportDefinition)
{-# DEPRECATED rdRefreshClosedReports "Use generic-lens or generic-optics with 'refreshClosedReports' instead." #-}

-- | A list of strings that indicate additional content that Amazon Web Services includes in the report, such as individual resource IDs.
--
-- /Note:/ Consider using 'additionalSchemaElements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAdditionalSchemaElements :: Lens.Lens' ReportDefinition [SchemaElement]
rdAdditionalSchemaElements = Lens.lens (additionalSchemaElements :: ReportDefinition -> [SchemaElement]) (\s a -> s {additionalSchemaElements = a} :: ReportDefinition)
{-# DEPRECATED rdAdditionalSchemaElements "Use generic-lens or generic-optics with 'additionalSchemaElements' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Prefix :: Lens.Lens' ReportDefinition Lude.Text
rdS3Prefix = Lens.lens (s3Prefix :: ReportDefinition -> Lude.Text) (\s a -> s {s3Prefix = a} :: ReportDefinition)
{-# DEPRECATED rdS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdS3Bucket :: Lens.Lens' ReportDefinition Lude.Text
rdS3Bucket = Lens.lens (s3Bucket :: ReportDefinition -> Lude.Text) (\s a -> s {s3Bucket = a} :: ReportDefinition)
{-# DEPRECATED rdS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromJSON ReportDefinition where
  parseJSON =
    Lude.withObject
      "ReportDefinition"
      ( \x ->
          ReportDefinition'
            Lude.<$> (x Lude..:? "ReportVersioning")
            Lude.<*> (x Lude..: "S3Region")
            Lude.<*> (x Lude..: "Format")
            Lude.<*> (x Lude..: "TimeUnit")
            Lude.<*> (x Lude..: "Compression")
            Lude.<*> (x Lude..: "ReportName")
            Lude.<*> (x Lude..:? "AdditionalArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RefreshClosedReports")
            Lude.<*> (x Lude..:? "AdditionalSchemaElements" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "S3Prefix")
            Lude.<*> (x Lude..: "S3Bucket")
      )

instance Lude.ToJSON ReportDefinition where
  toJSON ReportDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReportVersioning" Lude..=) Lude.<$> reportVersioning,
            Lude.Just ("S3Region" Lude..= s3Region),
            Lude.Just ("Format" Lude..= format),
            Lude.Just ("TimeUnit" Lude..= timeUnit),
            Lude.Just ("Compression" Lude..= compression),
            Lude.Just ("ReportName" Lude..= reportName),
            ("AdditionalArtifacts" Lude..=) Lude.<$> additionalArtifacts,
            ("RefreshClosedReports" Lude..=) Lude.<$> refreshClosedReports,
            Lude.Just
              ("AdditionalSchemaElements" Lude..= additionalSchemaElements),
            Lude.Just ("S3Prefix" Lude..= s3Prefix),
            Lude.Just ("S3Bucket" Lude..= s3Bucket)
          ]
      )
