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
    dtpStatus,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpDataTransferredInMegaBytes,
    dtpTotalDataInMegaBytes,
    dtpElapsedTimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of a cluster while it is in the process of resizing with an incremental resize.
--
-- /See:/ 'mkDataTransferProgress' smart constructor.
data DataTransferProgress = DataTransferProgress'
  { currentRateInMegaBytesPerSecond ::
      Lude.Maybe Lude.Double,
    status :: Lude.Maybe Lude.Text,
    estimatedTimeToCompletionInSeconds ::
      Lude.Maybe Lude.Integer,
    dataTransferredInMegaBytes ::
      Lude.Maybe Lude.Integer,
    totalDataInMegaBytes :: Lude.Maybe Lude.Integer,
    elapsedTimeInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataTransferProgress' with the minimum fields required to make a request.
--
-- * 'currentRateInMegaBytesPerSecond' - Describes the data transfer rate in MB's per second.
-- * 'dataTransferredInMegaBytes' - Describes the total amount of data that has been transfered in MB's.
-- * 'elapsedTimeInSeconds' - Describes the number of seconds that have elapsed during the data transfer.
-- * 'estimatedTimeToCompletionInSeconds' - Describes the estimated number of seconds remaining to complete the transfer.
-- * 'status' - Describes the status of the cluster. While the transfer is in progress the status is @transferringdata@ .
-- * 'totalDataInMegaBytes' - Describes the total amount of data to be transfered in megabytes.
mkDataTransferProgress ::
  DataTransferProgress
mkDataTransferProgress =
  DataTransferProgress'
    { currentRateInMegaBytesPerSecond =
        Lude.Nothing,
      status = Lude.Nothing,
      estimatedTimeToCompletionInSeconds = Lude.Nothing,
      dataTransferredInMegaBytes = Lude.Nothing,
      totalDataInMegaBytes = Lude.Nothing,
      elapsedTimeInSeconds = Lude.Nothing
    }

-- | Describes the data transfer rate in MB's per second.
--
-- /Note:/ Consider using 'currentRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpCurrentRateInMegaBytesPerSecond :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Double)
dtpCurrentRateInMegaBytesPerSecond = Lens.lens (currentRateInMegaBytesPerSecond :: DataTransferProgress -> Lude.Maybe Lude.Double) (\s a -> s {currentRateInMegaBytesPerSecond = a} :: DataTransferProgress)
{-# DEPRECATED dtpCurrentRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentRateInMegaBytesPerSecond' instead." #-}

-- | Describes the status of the cluster. While the transfer is in progress the status is @transferringdata@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpStatus :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Text)
dtpStatus = Lens.lens (status :: DataTransferProgress -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DataTransferProgress)
{-# DEPRECATED dtpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Describes the estimated number of seconds remaining to complete the transfer.
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpEstimatedTimeToCompletionInSeconds :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Integer)
dtpEstimatedTimeToCompletionInSeconds = Lens.lens (estimatedTimeToCompletionInSeconds :: DataTransferProgress -> Lude.Maybe Lude.Integer) (\s a -> s {estimatedTimeToCompletionInSeconds = a} :: DataTransferProgress)
{-# DEPRECATED dtpEstimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead." #-}

-- | Describes the total amount of data that has been transfered in MB's.
--
-- /Note:/ Consider using 'dataTransferredInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpDataTransferredInMegaBytes :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Integer)
dtpDataTransferredInMegaBytes = Lens.lens (dataTransferredInMegaBytes :: DataTransferProgress -> Lude.Maybe Lude.Integer) (\s a -> s {dataTransferredInMegaBytes = a} :: DataTransferProgress)
{-# DEPRECATED dtpDataTransferredInMegaBytes "Use generic-lens or generic-optics with 'dataTransferredInMegaBytes' instead." #-}

-- | Describes the total amount of data to be transfered in megabytes.
--
-- /Note:/ Consider using 'totalDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpTotalDataInMegaBytes :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Integer)
dtpTotalDataInMegaBytes = Lens.lens (totalDataInMegaBytes :: DataTransferProgress -> Lude.Maybe Lude.Integer) (\s a -> s {totalDataInMegaBytes = a} :: DataTransferProgress)
{-# DEPRECATED dtpTotalDataInMegaBytes "Use generic-lens or generic-optics with 'totalDataInMegaBytes' instead." #-}

-- | Describes the number of seconds that have elapsed during the data transfer.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpElapsedTimeInSeconds :: Lens.Lens' DataTransferProgress (Lude.Maybe Lude.Integer)
dtpElapsedTimeInSeconds = Lens.lens (elapsedTimeInSeconds :: DataTransferProgress -> Lude.Maybe Lude.Integer) (\s a -> s {elapsedTimeInSeconds = a} :: DataTransferProgress)
{-# DEPRECATED dtpElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

instance Lude.FromXML DataTransferProgress where
  parseXML x =
    DataTransferProgress'
      Lude.<$> (x Lude..@? "CurrentRateInMegaBytesPerSecond")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "EstimatedTimeToCompletionInSeconds")
      Lude.<*> (x Lude..@? "DataTransferredInMegaBytes")
      Lude.<*> (x Lude..@? "TotalDataInMegaBytes")
      Lude.<*> (x Lude..@? "ElapsedTimeInSeconds")
