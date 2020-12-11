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
    brStatus,
    brFailureCode,
    brDeliveryTime,
    brDownloadURL,
    brS3Location,
  )
where

import Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Usage report with specified parameters.
--
-- /See:/ 'mkBusinessReport' smart constructor.
data BusinessReport = BusinessReport'
  { status ::
      Lude.Maybe BusinessReportStatus,
    failureCode :: Lude.Maybe BusinessReportFailureCode,
    deliveryTime :: Lude.Maybe Lude.Timestamp,
    downloadURL :: Lude.Maybe Lude.Text,
    s3Location :: Lude.Maybe BusinessReportS3Location
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BusinessReport' with the minimum fields required to make a request.
--
-- * 'deliveryTime' - The time of report delivery.
-- * 'downloadURL' - The download link where a user can download the report.
-- * 'failureCode' - The failure code.
-- * 's3Location' - The S3 location of the output reports.
-- * 'status' - The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
mkBusinessReport ::
  BusinessReport
mkBusinessReport =
  BusinessReport'
    { status = Lude.Nothing,
      failureCode = Lude.Nothing,
      deliveryTime = Lude.Nothing,
      downloadURL = Lude.Nothing,
      s3Location = Lude.Nothing
    }

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brStatus :: Lens.Lens' BusinessReport (Lude.Maybe BusinessReportStatus)
brStatus = Lens.lens (status :: BusinessReport -> Lude.Maybe BusinessReportStatus) (\s a -> s {status = a} :: BusinessReport)
{-# DEPRECATED brStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The failure code.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brFailureCode :: Lens.Lens' BusinessReport (Lude.Maybe BusinessReportFailureCode)
brFailureCode = Lens.lens (failureCode :: BusinessReport -> Lude.Maybe BusinessReportFailureCode) (\s a -> s {failureCode = a} :: BusinessReport)
{-# DEPRECATED brFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The time of report delivery.
--
-- /Note:/ Consider using 'deliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDeliveryTime :: Lens.Lens' BusinessReport (Lude.Maybe Lude.Timestamp)
brDeliveryTime = Lens.lens (deliveryTime :: BusinessReport -> Lude.Maybe Lude.Timestamp) (\s a -> s {deliveryTime = a} :: BusinessReport)
{-# DEPRECATED brDeliveryTime "Use generic-lens or generic-optics with 'deliveryTime' instead." #-}

-- | The download link where a user can download the report.
--
-- /Note:/ Consider using 'downloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDownloadURL :: Lens.Lens' BusinessReport (Lude.Maybe Lude.Text)
brDownloadURL = Lens.lens (downloadURL :: BusinessReport -> Lude.Maybe Lude.Text) (\s a -> s {downloadURL = a} :: BusinessReport)
{-# DEPRECATED brDownloadURL "Use generic-lens or generic-optics with 'downloadURL' instead." #-}

-- | The S3 location of the output reports.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brS3Location :: Lens.Lens' BusinessReport (Lude.Maybe BusinessReportS3Location)
brS3Location = Lens.lens (s3Location :: BusinessReport -> Lude.Maybe BusinessReportS3Location) (\s a -> s {s3Location = a} :: BusinessReport)
{-# DEPRECATED brS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON BusinessReport where
  parseJSON =
    Lude.withObject
      "BusinessReport"
      ( \x ->
          BusinessReport'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FailureCode")
            Lude.<*> (x Lude..:? "DeliveryTime")
            Lude.<*> (x Lude..:? "DownloadUrl")
            Lude.<*> (x Lude..:? "S3Location")
      )
