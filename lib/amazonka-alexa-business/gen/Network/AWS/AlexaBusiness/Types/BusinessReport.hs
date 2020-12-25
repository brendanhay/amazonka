{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReport
  ( BusinessReport (..),

    -- * Smart constructor
    mkBusinessReport,

    -- * Lenses
    brDeliveryTime,
    brDownloadUrl,
    brFailureCode,
    brS3Location,
    brStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.BusinessReportDownloadUrl as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportS3Location as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Usage report with specified parameters.
--
-- /See:/ 'mkBusinessReport' smart constructor.
data BusinessReport = BusinessReport'
  { -- | The time of report delivery.
    deliveryTime :: Core.Maybe Core.NominalDiffTime,
    -- | The download link where a user can download the report.
    downloadUrl :: Core.Maybe Types.BusinessReportDownloadUrl,
    -- | The failure code.
    failureCode :: Core.Maybe Types.BusinessReportFailureCode,
    -- | The S3 location of the output reports.
    s3Location :: Core.Maybe Types.BusinessReportS3Location,
    -- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
    status :: Core.Maybe Types.BusinessReportStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BusinessReport' value with any optional fields omitted.
mkBusinessReport ::
  BusinessReport
mkBusinessReport =
  BusinessReport'
    { deliveryTime = Core.Nothing,
      downloadUrl = Core.Nothing,
      failureCode = Core.Nothing,
      s3Location = Core.Nothing,
      status = Core.Nothing
    }

-- | The time of report delivery.
--
-- /Note:/ Consider using 'deliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDeliveryTime :: Lens.Lens' BusinessReport (Core.Maybe Core.NominalDiffTime)
brDeliveryTime = Lens.field @"deliveryTime"
{-# DEPRECATED brDeliveryTime "Use generic-lens or generic-optics with 'deliveryTime' instead." #-}

-- | The download link where a user can download the report.
--
-- /Note:/ Consider using 'downloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDownloadUrl :: Lens.Lens' BusinessReport (Core.Maybe Types.BusinessReportDownloadUrl)
brDownloadUrl = Lens.field @"downloadUrl"
{-# DEPRECATED brDownloadUrl "Use generic-lens or generic-optics with 'downloadUrl' instead." #-}

-- | The failure code.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brFailureCode :: Lens.Lens' BusinessReport (Core.Maybe Types.BusinessReportFailureCode)
brFailureCode = Lens.field @"failureCode"
{-# DEPRECATED brFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The S3 location of the output reports.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brS3Location :: Lens.Lens' BusinessReport (Core.Maybe Types.BusinessReportS3Location)
brS3Location = Lens.field @"s3Location"
{-# DEPRECATED brS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brStatus :: Lens.Lens' BusinessReport (Core.Maybe Types.BusinessReportStatus)
brStatus = Lens.field @"status"
{-# DEPRECATED brStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON BusinessReport where
  parseJSON =
    Core.withObject "BusinessReport" Core.$
      \x ->
        BusinessReport'
          Core.<$> (x Core..:? "DeliveryTime")
          Core.<*> (x Core..:? "DownloadUrl")
          Core.<*> (x Core..:? "FailureCode")
          Core.<*> (x Core..:? "S3Location")
          Core.<*> (x Core..:? "Status")
