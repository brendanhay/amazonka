{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ExportInfo
  ( ExportInfo (..)
  -- * Smart constructor
  , mkExportInfo
  -- * Lenses
  , eiExportId
  , eiExportStatus
  , eiStatusMessage
  , eiExportRequestTime
  , eiConfigurationsDownloadUrl
  , eiIsTruncated
  , eiRequestedEndTime
  , eiRequestedStartTime
  ) where

import qualified Network.AWS.Discovery.Types.ConfigurationsDownloadUrl as Types
import qualified Network.AWS.Discovery.Types.ExportId as Types
import qualified Network.AWS.Discovery.Types.ExportStatus as Types
import qualified Network.AWS.Discovery.Types.StatusMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information regarding the export status of discovered data. The value is an array of objects.
--
-- /See:/ 'mkExportInfo' smart constructor.
data ExportInfo = ExportInfo'
  { exportId :: Types.ExportId
    -- ^ A unique identifier used to query an export.
  , exportStatus :: Types.ExportStatus
    -- ^ The status of the data export job.
  , statusMessage :: Types.StatusMessage
    -- ^ A status message provided for API callers.
  , exportRequestTime :: Core.NominalDiffTime
    -- ^ The time that the data export was initiated.
  , configurationsDownloadUrl :: Core.Maybe Types.ConfigurationsDownloadUrl
    -- ^ A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
  , requestedEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
  , requestedStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExportInfo' value with any optional fields omitted.
mkExportInfo
    :: Types.ExportId -- ^ 'exportId'
    -> Types.ExportStatus -- ^ 'exportStatus'
    -> Types.StatusMessage -- ^ 'statusMessage'
    -> Core.NominalDiffTime -- ^ 'exportRequestTime'
    -> ExportInfo
mkExportInfo exportId exportStatus statusMessage exportRequestTime
  = ExportInfo'{exportId, exportStatus, statusMessage,
                exportRequestTime, configurationsDownloadUrl = Core.Nothing,
                isTruncated = Core.Nothing, requestedEndTime = Core.Nothing,
                requestedStartTime = Core.Nothing}

-- | A unique identifier used to query an export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiExportId :: Lens.Lens' ExportInfo Types.ExportId
eiExportId = Lens.field @"exportId"
{-# INLINEABLE eiExportId #-}
{-# DEPRECATED exportId "Use generic-lens or generic-optics with 'exportId' instead"  #-}

-- | The status of the data export job.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiExportStatus :: Lens.Lens' ExportInfo Types.ExportStatus
eiExportStatus = Lens.field @"exportStatus"
{-# INLINEABLE eiExportStatus #-}
{-# DEPRECATED exportStatus "Use generic-lens or generic-optics with 'exportStatus' instead"  #-}

-- | A status message provided for API callers.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiStatusMessage :: Lens.Lens' ExportInfo Types.StatusMessage
eiStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE eiStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The time that the data export was initiated.
--
-- /Note:/ Consider using 'exportRequestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiExportRequestTime :: Lens.Lens' ExportInfo Core.NominalDiffTime
eiExportRequestTime = Lens.field @"exportRequestTime"
{-# INLINEABLE eiExportRequestTime #-}
{-# DEPRECATED exportRequestTime "Use generic-lens or generic-optics with 'exportRequestTime' instead"  #-}

-- | A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
--
-- /Note:/ Consider using 'configurationsDownloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiConfigurationsDownloadUrl :: Lens.Lens' ExportInfo (Core.Maybe Types.ConfigurationsDownloadUrl)
eiConfigurationsDownloadUrl = Lens.field @"configurationsDownloadUrl"
{-# INLINEABLE eiConfigurationsDownloadUrl #-}
{-# DEPRECATED configurationsDownloadUrl "Use generic-lens or generic-optics with 'configurationsDownloadUrl' instead"  #-}

-- | If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiIsTruncated :: Lens.Lens' ExportInfo (Core.Maybe Core.Bool)
eiIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE eiIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- /Note:/ Consider using 'requestedEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRequestedEndTime :: Lens.Lens' ExportInfo (Core.Maybe Core.NominalDiffTime)
eiRequestedEndTime = Lens.field @"requestedEndTime"
{-# INLINEABLE eiRequestedEndTime #-}
{-# DEPRECATED requestedEndTime "Use generic-lens or generic-optics with 'requestedEndTime' instead"  #-}

-- | The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- /Note:/ Consider using 'requestedStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRequestedStartTime :: Lens.Lens' ExportInfo (Core.Maybe Core.NominalDiffTime)
eiRequestedStartTime = Lens.field @"requestedStartTime"
{-# INLINEABLE eiRequestedStartTime #-}
{-# DEPRECATED requestedStartTime "Use generic-lens or generic-optics with 'requestedStartTime' instead"  #-}

instance Core.FromJSON ExportInfo where
        parseJSON
          = Core.withObject "ExportInfo" Core.$
              \ x ->
                ExportInfo' Core.<$>
                  (x Core..: "exportId") Core.<*> x Core..: "exportStatus" Core.<*>
                    x Core..: "statusMessage"
                    Core.<*> x Core..: "exportRequestTime"
                    Core.<*> x Core..:? "configurationsDownloadUrl"
                    Core.<*> x Core..:? "isTruncated"
                    Core.<*> x Core..:? "requestedEndTime"
                    Core.<*> x Core..:? "requestedStartTime"
