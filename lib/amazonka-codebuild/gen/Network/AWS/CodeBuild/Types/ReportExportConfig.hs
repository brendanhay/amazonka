{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportExportConfig
  ( ReportExportConfig (..)
  -- * Smart constructor
  , mkReportExportConfig
  -- * Lenses
  , recExportConfigType
  , recS3Destination
  ) where

import qualified Network.AWS.CodeBuild.Types.ReportExportConfigType as Types
import qualified Network.AWS.CodeBuild.Types.S3ReportExportConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the location where the run of a report is exported. 
--
-- /See:/ 'mkReportExportConfig' smart constructor.
data ReportExportConfig = ReportExportConfig'
  { exportConfigType :: Core.Maybe Types.ReportExportConfigType
    -- ^ The export configuration type. Valid values are: 
--
--
--     * @S3@ : The report results are exported to an S3 bucket. 
--
--
--     * @NO_EXPORT@ : The report results are not exported. 
--
--
  , s3Destination :: Core.Maybe Types.S3ReportExportConfig
    -- ^ A @S3ReportExportConfig@ object that contains information about the S3 bucket where the run of a report is exported. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportExportConfig' value with any optional fields omitted.
mkReportExportConfig
    :: ReportExportConfig
mkReportExportConfig
  = ReportExportConfig'{exportConfigType = Core.Nothing,
                        s3Destination = Core.Nothing}

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
recExportConfigType :: Lens.Lens' ReportExportConfig (Core.Maybe Types.ReportExportConfigType)
recExportConfigType = Lens.field @"exportConfigType"
{-# INLINEABLE recExportConfigType #-}
{-# DEPRECATED exportConfigType "Use generic-lens or generic-optics with 'exportConfigType' instead"  #-}

-- | A @S3ReportExportConfig@ object that contains information about the S3 bucket where the run of a report is exported. 
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recS3Destination :: Lens.Lens' ReportExportConfig (Core.Maybe Types.S3ReportExportConfig)
recS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE recS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

instance Core.FromJSON ReportExportConfig where
        toJSON ReportExportConfig{..}
          = Core.object
              (Core.catMaybes
                 [("exportConfigType" Core..=) Core.<$> exportConfigType,
                  ("s3Destination" Core..=) Core.<$> s3Destination])

instance Core.FromJSON ReportExportConfig where
        parseJSON
          = Core.withObject "ReportExportConfig" Core.$
              \ x ->
                ReportExportConfig' Core.<$>
                  (x Core..:? "exportConfigType") Core.<*> x Core..:? "s3Destination"
