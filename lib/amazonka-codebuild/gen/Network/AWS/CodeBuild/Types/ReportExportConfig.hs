-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportExportConfig
  ( ReportExportConfig (..),

    -- * Smart constructor
    mkReportExportConfig,

    -- * Lenses
    recExportConfigType,
    recS3Destination,
  )
where

import Network.AWS.CodeBuild.Types.ReportExportConfigType
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the location where the run of a report is exported.
--
-- /See:/ 'mkReportExportConfig' smart constructor.
data ReportExportConfig = ReportExportConfig'
  { exportConfigType ::
      Lude.Maybe ReportExportConfigType,
    s3Destination :: Lude.Maybe S3ReportExportConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportExportConfig' with the minimum fields required to make a request.
--
-- * 'exportConfigType' - The export configuration type. Valid values are:
--
--
--     * @S3@ : The report results are exported to an S3 bucket.
--
--
--     * @NO_EXPORT@ : The report results are not exported.
--
--
-- * 's3Destination' - A @S3ReportExportConfig@ object that contains information about the S3 bucket where the run of a report is exported.
mkReportExportConfig ::
  ReportExportConfig
mkReportExportConfig =
  ReportExportConfig'
    { exportConfigType = Lude.Nothing,
      s3Destination = Lude.Nothing
    }

-- | The export configuration type. Valid values are:
--
--
--     * @S3@ : The report results are exported to an S3 bucket.
--
--
--     * @NO_EXPORT@ : The report results are not exported.
--
--
--
-- /Note:/ Consider using 'exportConfigType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recExportConfigType :: Lens.Lens' ReportExportConfig (Lude.Maybe ReportExportConfigType)
recExportConfigType = Lens.lens (exportConfigType :: ReportExportConfig -> Lude.Maybe ReportExportConfigType) (\s a -> s {exportConfigType = a} :: ReportExportConfig)
{-# DEPRECATED recExportConfigType "Use generic-lens or generic-optics with 'exportConfigType' instead." #-}

-- | A @S3ReportExportConfig@ object that contains information about the S3 bucket where the run of a report is exported.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recS3Destination :: Lens.Lens' ReportExportConfig (Lude.Maybe S3ReportExportConfig)
recS3Destination = Lens.lens (s3Destination :: ReportExportConfig -> Lude.Maybe S3ReportExportConfig) (\s a -> s {s3Destination = a} :: ReportExportConfig)
{-# DEPRECATED recS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

instance Lude.FromJSON ReportExportConfig where
  parseJSON =
    Lude.withObject
      "ReportExportConfig"
      ( \x ->
          ReportExportConfig'
            Lude.<$> (x Lude..:? "exportConfigType")
            Lude.<*> (x Lude..:? "s3Destination")
      )

instance Lude.ToJSON ReportExportConfig where
  toJSON ReportExportConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("exportConfigType" Lude..=) Lude.<$> exportConfigType,
            ("s3Destination" Lude..=) Lude.<$> s3Destination
          ]
      )
