{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfigSummary
  ( DataCaptureConfigSummary (..),

    -- * Smart constructor
    mkDataCaptureConfigSummary,

    -- * Lenses
    dccsDestinationS3URI,
    dccsCurrentSamplingPercentage,
    dccsCaptureStatus,
    dccsKMSKeyId,
    dccsEnableCapture,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CaptureStatus

-- |
--
-- /See:/ 'mkDataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { -- |
    destinationS3URI :: Lude.Text,
    -- |
    currentSamplingPercentage :: Lude.Natural,
    -- |
    captureStatus :: CaptureStatus,
    -- |
    kmsKeyId :: Lude.Text,
    -- |
    enableCapture :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataCaptureConfigSummary' with the minimum fields required to make a request.
--
-- * 'destinationS3URI' -
-- * 'currentSamplingPercentage' -
-- * 'captureStatus' -
-- * 'kmsKeyId' -
-- * 'enableCapture' -
mkDataCaptureConfigSummary ::
  -- | 'destinationS3URI'
  Lude.Text ->
  -- | 'currentSamplingPercentage'
  Lude.Natural ->
  -- | 'captureStatus'
  CaptureStatus ->
  -- | 'kmsKeyId'
  Lude.Text ->
  -- | 'enableCapture'
  Lude.Bool ->
  DataCaptureConfigSummary
mkDataCaptureConfigSummary
  pDestinationS3URI_
  pCurrentSamplingPercentage_
  pCaptureStatus_
  pKMSKeyId_
  pEnableCapture_ =
    DataCaptureConfigSummary'
      { destinationS3URI = pDestinationS3URI_,
        currentSamplingPercentage = pCurrentSamplingPercentage_,
        captureStatus = pCaptureStatus_,
        kmsKeyId = pKMSKeyId_,
        enableCapture = pEnableCapture_
      }

-- |
--
-- /Note:/ Consider using 'destinationS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsDestinationS3URI :: Lens.Lens' DataCaptureConfigSummary Lude.Text
dccsDestinationS3URI = Lens.lens (destinationS3URI :: DataCaptureConfigSummary -> Lude.Text) (\s a -> s {destinationS3URI = a} :: DataCaptureConfigSummary)
{-# DEPRECATED dccsDestinationS3URI "Use generic-lens or generic-optics with 'destinationS3URI' instead." #-}

-- |
--
-- /Note:/ Consider using 'currentSamplingPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsCurrentSamplingPercentage :: Lens.Lens' DataCaptureConfigSummary Lude.Natural
dccsCurrentSamplingPercentage = Lens.lens (currentSamplingPercentage :: DataCaptureConfigSummary -> Lude.Natural) (\s a -> s {currentSamplingPercentage = a} :: DataCaptureConfigSummary)
{-# DEPRECATED dccsCurrentSamplingPercentage "Use generic-lens or generic-optics with 'currentSamplingPercentage' instead." #-}

-- |
--
-- /Note:/ Consider using 'captureStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsCaptureStatus :: Lens.Lens' DataCaptureConfigSummary CaptureStatus
dccsCaptureStatus = Lens.lens (captureStatus :: DataCaptureConfigSummary -> CaptureStatus) (\s a -> s {captureStatus = a} :: DataCaptureConfigSummary)
{-# DEPRECATED dccsCaptureStatus "Use generic-lens or generic-optics with 'captureStatus' instead." #-}

-- |
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsKMSKeyId :: Lens.Lens' DataCaptureConfigSummary Lude.Text
dccsKMSKeyId = Lens.lens (kmsKeyId :: DataCaptureConfigSummary -> Lude.Text) (\s a -> s {kmsKeyId = a} :: DataCaptureConfigSummary)
{-# DEPRECATED dccsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- |
--
-- /Note:/ Consider using 'enableCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsEnableCapture :: Lens.Lens' DataCaptureConfigSummary Lude.Bool
dccsEnableCapture = Lens.lens (enableCapture :: DataCaptureConfigSummary -> Lude.Bool) (\s a -> s {enableCapture = a} :: DataCaptureConfigSummary)
{-# DEPRECATED dccsEnableCapture "Use generic-lens or generic-optics with 'enableCapture' instead." #-}

instance Lude.FromJSON DataCaptureConfigSummary where
  parseJSON =
    Lude.withObject
      "DataCaptureConfigSummary"
      ( \x ->
          DataCaptureConfigSummary'
            Lude.<$> (x Lude..: "DestinationS3Uri")
            Lude.<*> (x Lude..: "CurrentSamplingPercentage")
            Lude.<*> (x Lude..: "CaptureStatus")
            Lude.<*> (x Lude..: "KmsKeyId")
            Lude.<*> (x Lude..: "EnableCapture")
      )
