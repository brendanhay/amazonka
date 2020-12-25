{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusDetails
  ( ModelPackageStatusDetails (..),

    -- * Smart constructor
    mkModelPackageStatusDetails,

    -- * Lenses
    mpsdValidationStatuses,
    mpsdImageScanStatuses,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ModelPackageStatusItem as Types

-- | Specifies the validation and image scan statuses of the model package.
--
-- /See:/ 'mkModelPackageStatusDetails' smart constructor.
data ModelPackageStatusDetails = ModelPackageStatusDetails'
  { -- | The validation status of the model package.
    validationStatuses :: [Types.ModelPackageStatusItem],
    -- | The status of the scan of the Docker image container for the model package.
    imageScanStatuses :: Core.Maybe [Types.ModelPackageStatusItem]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelPackageStatusDetails' value with any optional fields omitted.
mkModelPackageStatusDetails ::
  ModelPackageStatusDetails
mkModelPackageStatusDetails =
  ModelPackageStatusDetails'
    { validationStatuses = Core.mempty,
      imageScanStatuses = Core.Nothing
    }

-- | The validation status of the model package.
--
-- /Note:/ Consider using 'validationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdValidationStatuses :: Lens.Lens' ModelPackageStatusDetails [Types.ModelPackageStatusItem]
mpsdValidationStatuses = Lens.field @"validationStatuses"
{-# DEPRECATED mpsdValidationStatuses "Use generic-lens or generic-optics with 'validationStatuses' instead." #-}

-- | The status of the scan of the Docker image container for the model package.
--
-- /Note:/ Consider using 'imageScanStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdImageScanStatuses :: Lens.Lens' ModelPackageStatusDetails (Core.Maybe [Types.ModelPackageStatusItem])
mpsdImageScanStatuses = Lens.field @"imageScanStatuses"
{-# DEPRECATED mpsdImageScanStatuses "Use generic-lens or generic-optics with 'imageScanStatuses' instead." #-}

instance Core.FromJSON ModelPackageStatusDetails where
  parseJSON =
    Core.withObject "ModelPackageStatusDetails" Core.$
      \x ->
        ModelPackageStatusDetails'
          Core.<$> (x Core..:? "ValidationStatuses" Core..!= Core.mempty)
          Core.<*> (x Core..:? "ImageScanStatuses")
