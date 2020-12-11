-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfig
  ( DataCaptureConfig (..),

    -- * Smart constructor
    mkDataCaptureConfig,

    -- * Lenses
    dccCaptureContentTypeHeader,
    dccKMSKeyId,
    dccEnableCapture,
    dccInitialSamplingPercentage,
    dccDestinationS3URI,
    dccCaptureOptions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
import Network.AWS.SageMaker.Types.CaptureOption

-- |
--
-- /See:/ 'mkDataCaptureConfig' smart constructor.
data DataCaptureConfig = DataCaptureConfig'
  { captureContentTypeHeader ::
      Lude.Maybe CaptureContentTypeHeader,
    kmsKeyId :: Lude.Maybe Lude.Text,
    enableCapture :: Lude.Maybe Lude.Bool,
    initialSamplingPercentage :: Lude.Natural,
    destinationS3URI :: Lude.Text,
    captureOptions :: Lude.NonEmpty CaptureOption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataCaptureConfig' with the minimum fields required to make a request.
--
-- * 'captureContentTypeHeader' -
-- * 'captureOptions' -
-- * 'destinationS3URI' -
-- * 'enableCapture' -
-- * 'initialSamplingPercentage' -
-- * 'kmsKeyId' -
mkDataCaptureConfig ::
  -- | 'initialSamplingPercentage'
  Lude.Natural ->
  -- | 'destinationS3URI'
  Lude.Text ->
  -- | 'captureOptions'
  Lude.NonEmpty CaptureOption ->
  DataCaptureConfig
mkDataCaptureConfig
  pInitialSamplingPercentage_
  pDestinationS3URI_
  pCaptureOptions_ =
    DataCaptureConfig'
      { captureContentTypeHeader = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        enableCapture = Lude.Nothing,
        initialSamplingPercentage = pInitialSamplingPercentage_,
        destinationS3URI = pDestinationS3URI_,
        captureOptions = pCaptureOptions_
      }

-- |
--
-- /Note:/ Consider using 'captureContentTypeHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCaptureContentTypeHeader :: Lens.Lens' DataCaptureConfig (Lude.Maybe CaptureContentTypeHeader)
dccCaptureContentTypeHeader = Lens.lens (captureContentTypeHeader :: DataCaptureConfig -> Lude.Maybe CaptureContentTypeHeader) (\s a -> s {captureContentTypeHeader = a} :: DataCaptureConfig)
{-# DEPRECATED dccCaptureContentTypeHeader "Use generic-lens or generic-optics with 'captureContentTypeHeader' instead." #-}

-- |
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccKMSKeyId :: Lens.Lens' DataCaptureConfig (Lude.Maybe Lude.Text)
dccKMSKeyId = Lens.lens (kmsKeyId :: DataCaptureConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DataCaptureConfig)
{-# DEPRECATED dccKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- |
--
-- /Note:/ Consider using 'enableCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccEnableCapture :: Lens.Lens' DataCaptureConfig (Lude.Maybe Lude.Bool)
dccEnableCapture = Lens.lens (enableCapture :: DataCaptureConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enableCapture = a} :: DataCaptureConfig)
{-# DEPRECATED dccEnableCapture "Use generic-lens or generic-optics with 'enableCapture' instead." #-}

-- |
--
-- /Note:/ Consider using 'initialSamplingPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccInitialSamplingPercentage :: Lens.Lens' DataCaptureConfig Lude.Natural
dccInitialSamplingPercentage = Lens.lens (initialSamplingPercentage :: DataCaptureConfig -> Lude.Natural) (\s a -> s {initialSamplingPercentage = a} :: DataCaptureConfig)
{-# DEPRECATED dccInitialSamplingPercentage "Use generic-lens or generic-optics with 'initialSamplingPercentage' instead." #-}

-- |
--
-- /Note:/ Consider using 'destinationS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccDestinationS3URI :: Lens.Lens' DataCaptureConfig Lude.Text
dccDestinationS3URI = Lens.lens (destinationS3URI :: DataCaptureConfig -> Lude.Text) (\s a -> s {destinationS3URI = a} :: DataCaptureConfig)
{-# DEPRECATED dccDestinationS3URI "Use generic-lens or generic-optics with 'destinationS3URI' instead." #-}

-- |
--
-- /Note:/ Consider using 'captureOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCaptureOptions :: Lens.Lens' DataCaptureConfig (Lude.NonEmpty CaptureOption)
dccCaptureOptions = Lens.lens (captureOptions :: DataCaptureConfig -> Lude.NonEmpty CaptureOption) (\s a -> s {captureOptions = a} :: DataCaptureConfig)
{-# DEPRECATED dccCaptureOptions "Use generic-lens or generic-optics with 'captureOptions' instead." #-}

instance Lude.FromJSON DataCaptureConfig where
  parseJSON =
    Lude.withObject
      "DataCaptureConfig"
      ( \x ->
          DataCaptureConfig'
            Lude.<$> (x Lude..:? "CaptureContentTypeHeader")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "EnableCapture")
            Lude.<*> (x Lude..: "InitialSamplingPercentage")
            Lude.<*> (x Lude..: "DestinationS3Uri")
            Lude.<*> (x Lude..: "CaptureOptions")
      )

instance Lude.ToJSON DataCaptureConfig where
  toJSON DataCaptureConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CaptureContentTypeHeader" Lude..=)
              Lude.<$> captureContentTypeHeader,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("EnableCapture" Lude..=) Lude.<$> enableCapture,
            Lude.Just
              ("InitialSamplingPercentage" Lude..= initialSamplingPercentage),
            Lude.Just ("DestinationS3Uri" Lude..= destinationS3URI),
            Lude.Just ("CaptureOptions" Lude..= captureOptions)
          ]
      )
