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
    dccsEnableCapture,
    dccsCaptureStatus,
    dccsCurrentSamplingPercentage,
    dccsDestinationS3Uri,
    dccsKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CaptureStatus as Types
import qualified Network.AWS.SageMaker.Types.DestinationS3Uri as Types
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types

-- |
--
-- /See:/ 'mkDataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { -- |
    enableCapture :: Core.Bool,
    -- |
    captureStatus :: Types.CaptureStatus,
    -- |
    currentSamplingPercentage :: Core.Natural,
    -- |
    destinationS3Uri :: Types.DestinationS3Uri,
    -- |
    kmsKeyId :: Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataCaptureConfigSummary' value with any optional fields omitted.
mkDataCaptureConfigSummary ::
  -- | 'enableCapture'
  Core.Bool ->
  -- | 'captureStatus'
  Types.CaptureStatus ->
  -- | 'currentSamplingPercentage'
  Core.Natural ->
  -- | 'destinationS3Uri'
  Types.DestinationS3Uri ->
  -- | 'kmsKeyId'
  Types.KmsKeyId ->
  DataCaptureConfigSummary
mkDataCaptureConfigSummary
  enableCapture
  captureStatus
  currentSamplingPercentage
  destinationS3Uri
  kmsKeyId =
    DataCaptureConfigSummary'
      { enableCapture,
        captureStatus,
        currentSamplingPercentage,
        destinationS3Uri,
        kmsKeyId
      }

-- |
--
-- /Note:/ Consider using 'enableCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsEnableCapture :: Lens.Lens' DataCaptureConfigSummary Core.Bool
dccsEnableCapture = Lens.field @"enableCapture"
{-# DEPRECATED dccsEnableCapture "Use generic-lens or generic-optics with 'enableCapture' instead." #-}

-- |
--
-- /Note:/ Consider using 'captureStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsCaptureStatus :: Lens.Lens' DataCaptureConfigSummary Types.CaptureStatus
dccsCaptureStatus = Lens.field @"captureStatus"
{-# DEPRECATED dccsCaptureStatus "Use generic-lens or generic-optics with 'captureStatus' instead." #-}

-- |
--
-- /Note:/ Consider using 'currentSamplingPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsCurrentSamplingPercentage :: Lens.Lens' DataCaptureConfigSummary Core.Natural
dccsCurrentSamplingPercentage = Lens.field @"currentSamplingPercentage"
{-# DEPRECATED dccsCurrentSamplingPercentage "Use generic-lens or generic-optics with 'currentSamplingPercentage' instead." #-}

-- |
--
-- /Note:/ Consider using 'destinationS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsDestinationS3Uri :: Lens.Lens' DataCaptureConfigSummary Types.DestinationS3Uri
dccsDestinationS3Uri = Lens.field @"destinationS3Uri"
{-# DEPRECATED dccsDestinationS3Uri "Use generic-lens or generic-optics with 'destinationS3Uri' instead." #-}

-- |
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsKmsKeyId :: Lens.Lens' DataCaptureConfigSummary Types.KmsKeyId
dccsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dccsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON DataCaptureConfigSummary where
  parseJSON =
    Core.withObject "DataCaptureConfigSummary" Core.$
      \x ->
        DataCaptureConfigSummary'
          Core.<$> (x Core..: "EnableCapture")
          Core.<*> (x Core..: "CaptureStatus")
          Core.<*> (x Core..: "CurrentSamplingPercentage")
          Core.<*> (x Core..: "DestinationS3Uri")
          Core.<*> (x Core..: "KmsKeyId")
