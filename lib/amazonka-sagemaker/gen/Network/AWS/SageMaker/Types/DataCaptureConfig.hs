{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dccInitialSamplingPercentage,
    dccDestinationS3Uri,
    dccCaptureOptions,
    dccCaptureContentTypeHeader,
    dccEnableCapture,
    dccKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CaptureContentTypeHeader as Types
import qualified Network.AWS.SageMaker.Types.CaptureOption as Types
import qualified Network.AWS.SageMaker.Types.DestinationS3Uri as Types
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types

-- |
--
-- /See:/ 'mkDataCaptureConfig' smart constructor.
data DataCaptureConfig = DataCaptureConfig'
  { -- |
    initialSamplingPercentage :: Core.Natural,
    -- |
    destinationS3Uri :: Types.DestinationS3Uri,
    -- |
    captureOptions :: Core.NonEmpty Types.CaptureOption,
    -- |
    captureContentTypeHeader :: Core.Maybe Types.CaptureContentTypeHeader,
    -- |
    enableCapture :: Core.Maybe Core.Bool,
    -- |
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataCaptureConfig' value with any optional fields omitted.
mkDataCaptureConfig ::
  -- | 'initialSamplingPercentage'
  Core.Natural ->
  -- | 'destinationS3Uri'
  Types.DestinationS3Uri ->
  -- | 'captureOptions'
  Core.NonEmpty Types.CaptureOption ->
  DataCaptureConfig
mkDataCaptureConfig
  initialSamplingPercentage
  destinationS3Uri
  captureOptions =
    DataCaptureConfig'
      { initialSamplingPercentage,
        destinationS3Uri,
        captureOptions,
        captureContentTypeHeader = Core.Nothing,
        enableCapture = Core.Nothing,
        kmsKeyId = Core.Nothing
      }

-- |
--
-- /Note:/ Consider using 'initialSamplingPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccInitialSamplingPercentage :: Lens.Lens' DataCaptureConfig Core.Natural
dccInitialSamplingPercentage = Lens.field @"initialSamplingPercentage"
{-# DEPRECATED dccInitialSamplingPercentage "Use generic-lens or generic-optics with 'initialSamplingPercentage' instead." #-}

-- |
--
-- /Note:/ Consider using 'destinationS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccDestinationS3Uri :: Lens.Lens' DataCaptureConfig Types.DestinationS3Uri
dccDestinationS3Uri = Lens.field @"destinationS3Uri"
{-# DEPRECATED dccDestinationS3Uri "Use generic-lens or generic-optics with 'destinationS3Uri' instead." #-}

-- |
--
-- /Note:/ Consider using 'captureOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCaptureOptions :: Lens.Lens' DataCaptureConfig (Core.NonEmpty Types.CaptureOption)
dccCaptureOptions = Lens.field @"captureOptions"
{-# DEPRECATED dccCaptureOptions "Use generic-lens or generic-optics with 'captureOptions' instead." #-}

-- |
--
-- /Note:/ Consider using 'captureContentTypeHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCaptureContentTypeHeader :: Lens.Lens' DataCaptureConfig (Core.Maybe Types.CaptureContentTypeHeader)
dccCaptureContentTypeHeader = Lens.field @"captureContentTypeHeader"
{-# DEPRECATED dccCaptureContentTypeHeader "Use generic-lens or generic-optics with 'captureContentTypeHeader' instead." #-}

-- |
--
-- /Note:/ Consider using 'enableCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccEnableCapture :: Lens.Lens' DataCaptureConfig (Core.Maybe Core.Bool)
dccEnableCapture = Lens.field @"enableCapture"
{-# DEPRECATED dccEnableCapture "Use generic-lens or generic-optics with 'enableCapture' instead." #-}

-- |
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccKmsKeyId :: Lens.Lens' DataCaptureConfig (Core.Maybe Types.KmsKeyId)
dccKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dccKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON DataCaptureConfig where
  toJSON DataCaptureConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("InitialSamplingPercentage" Core..= initialSamplingPercentage),
            Core.Just ("DestinationS3Uri" Core..= destinationS3Uri),
            Core.Just ("CaptureOptions" Core..= captureOptions),
            ("CaptureContentTypeHeader" Core..=)
              Core.<$> captureContentTypeHeader,
            ("EnableCapture" Core..=) Core.<$> enableCapture,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON DataCaptureConfig where
  parseJSON =
    Core.withObject "DataCaptureConfig" Core.$
      \x ->
        DataCaptureConfig'
          Core.<$> (x Core..: "InitialSamplingPercentage")
          Core.<*> (x Core..: "DestinationS3Uri")
          Core.<*> (x Core..: "CaptureOptions")
          Core.<*> (x Core..:? "CaptureContentTypeHeader")
          Core.<*> (x Core..:? "EnableCapture")
          Core.<*> (x Core..:? "KmsKeyId")
