{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DataTransferProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DataTransferProgress
  ( DataTransferProgress (..),

    -- * Smart constructor
    mkDataTransferProgress,

    -- * Lenses
    dtpCurrentRateInMegaBytesPerSecond,
    dtpDataTransferredInMegaBytes,
    dtpElapsedTimeInSeconds,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpStatus,
    dtpTotalDataInMegaBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes the status of a cluster while it is in the process of resizing with an incremental resize.
--
-- /See:/ 'mkDataTransferProgress' smart constructor.
data DataTransferProgress = DataTransferProgress'
  { -- | Describes the data transfer rate in MB's per second.
    currentRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | Describes the total amount of data that has been transfered in MB's.
    dataTransferredInMegaBytes :: Core.Maybe Core.Integer,
    -- | Describes the number of seconds that have elapsed during the data transfer.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | Describes the estimated number of seconds remaining to complete the transfer.
    estimatedTimeToCompletionInSeconds :: Core.Maybe Core.Integer,
    -- | Describes the status of the cluster. While the transfer is in progress the status is @transferringdata@ .
    status :: Core.Maybe Types.String,
    -- | Describes the total amount of data to be transfered in megabytes.
    totalDataInMegaBytes :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataTransferProgress' value with any optional fields omitted.
mkDataTransferProgress ::
  DataTransferProgress
mkDataTransferProgress =
  DataTransferProgress'
    { currentRateInMegaBytesPerSecond =
        Core.Nothing,
      dataTransferredInMegaBytes = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      estimatedTimeToCompletionInSeconds = Core.Nothing,
      status = Core.Nothing,
      totalDataInMegaBytes = Core.Nothing
    }

-- | Describes the data transfer rate in MB's per second.
--
-- /Note:/ Consider using 'currentRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpCurrentRateInMegaBytesPerSecond :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Double)
dtpCurrentRateInMegaBytesPerSecond = Lens.field @"currentRateInMegaBytesPerSecond"
{-# DEPRECATED dtpCurrentRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentRateInMegaBytesPerSecond' instead." #-}

-- | Describes the total amount of data that has been transfered in MB's.
--
-- /Note:/ Consider using 'dataTransferredInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpDataTransferredInMegaBytes :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dtpDataTransferredInMegaBytes = Lens.field @"dataTransferredInMegaBytes"
{-# DEPRECATED dtpDataTransferredInMegaBytes "Use generic-lens or generic-optics with 'dataTransferredInMegaBytes' instead." #-}

-- | Describes the number of seconds that have elapsed during the data transfer.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpElapsedTimeInSeconds :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dtpElapsedTimeInSeconds = Lens.field @"elapsedTimeInSeconds"
{-# DEPRECATED dtpElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

-- | Describes the estimated number of seconds remaining to complete the transfer.
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpEstimatedTimeToCompletionInSeconds :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dtpEstimatedTimeToCompletionInSeconds = Lens.field @"estimatedTimeToCompletionInSeconds"
{-# DEPRECATED dtpEstimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead." #-}

-- | Describes the status of the cluster. While the transfer is in progress the status is @transferringdata@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpStatus :: Lens.Lens' DataTransferProgress (Core.Maybe Types.String)
dtpStatus = Lens.field @"status"
{-# DEPRECATED dtpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Describes the total amount of data to be transfered in megabytes.
--
-- /Note:/ Consider using 'totalDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpTotalDataInMegaBytes :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dtpTotalDataInMegaBytes = Lens.field @"totalDataInMegaBytes"
{-# DEPRECATED dtpTotalDataInMegaBytes "Use generic-lens or generic-optics with 'totalDataInMegaBytes' instead." #-}

instance Core.FromXML DataTransferProgress where
  parseXML x =
    DataTransferProgress'
      Core.<$> (x Core..@? "CurrentRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "DataTransferredInMegaBytes")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "EstimatedTimeToCompletionInSeconds")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TotalDataInMegaBytes")
