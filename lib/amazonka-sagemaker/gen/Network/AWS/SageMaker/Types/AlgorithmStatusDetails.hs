{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmStatusDetails
  ( AlgorithmStatusDetails (..)
  -- * Smart constructor
  , mkAlgorithmStatusDetails
  -- * Lenses
  , asdImageScanStatuses
  , asdValidationStatuses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AlgorithmStatusItem as Types

-- | Specifies the validation and image scan statuses of the algorithm.
--
-- /See:/ 'mkAlgorithmStatusDetails' smart constructor.
data AlgorithmStatusDetails = AlgorithmStatusDetails'
  { imageScanStatuses :: Core.Maybe [Types.AlgorithmStatusItem]
    -- ^ The status of the scan of the algorithm's Docker image container.
  , validationStatuses :: Core.Maybe [Types.AlgorithmStatusItem]
    -- ^ The status of algorithm validation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmStatusDetails' value with any optional fields omitted.
mkAlgorithmStatusDetails
    :: AlgorithmStatusDetails
mkAlgorithmStatusDetails
  = AlgorithmStatusDetails'{imageScanStatuses = Core.Nothing,
                            validationStatuses = Core.Nothing}

-- | The status of the scan of the algorithm's Docker image container.
--
-- /Note:/ Consider using 'imageScanStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdImageScanStatuses :: Lens.Lens' AlgorithmStatusDetails (Core.Maybe [Types.AlgorithmStatusItem])
asdImageScanStatuses = Lens.field @"imageScanStatuses"
{-# INLINEABLE asdImageScanStatuses #-}
{-# DEPRECATED imageScanStatuses "Use generic-lens or generic-optics with 'imageScanStatuses' instead"  #-}

-- | The status of algorithm validation.
--
-- /Note:/ Consider using 'validationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdValidationStatuses :: Lens.Lens' AlgorithmStatusDetails (Core.Maybe [Types.AlgorithmStatusItem])
asdValidationStatuses = Lens.field @"validationStatuses"
{-# INLINEABLE asdValidationStatuses #-}
{-# DEPRECATED validationStatuses "Use generic-lens or generic-optics with 'validationStatuses' instead"  #-}

instance Core.FromJSON AlgorithmStatusDetails where
        parseJSON
          = Core.withObject "AlgorithmStatusDetails" Core.$
              \ x ->
                AlgorithmStatusDetails' Core.<$>
                  (x Core..:? "ImageScanStatuses") Core.<*>
                    x Core..:? "ValidationStatuses"
