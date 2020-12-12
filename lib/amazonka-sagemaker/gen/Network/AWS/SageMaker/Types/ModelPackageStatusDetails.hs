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
    mpsdImageScanStatuses,
    mpsdValidationStatuses,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ModelPackageStatusItem

-- | Specifies the validation and image scan statuses of the model package.
--
-- /See:/ 'mkModelPackageStatusDetails' smart constructor.
data ModelPackageStatusDetails = ModelPackageStatusDetails'
  { imageScanStatuses ::
      Lude.Maybe [ModelPackageStatusItem],
    validationStatuses ::
      [ModelPackageStatusItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageStatusDetails' with the minimum fields required to make a request.
--
-- * 'imageScanStatuses' - The status of the scan of the Docker image container for the model package.
-- * 'validationStatuses' - The validation status of the model package.
mkModelPackageStatusDetails ::
  ModelPackageStatusDetails
mkModelPackageStatusDetails =
  ModelPackageStatusDetails'
    { imageScanStatuses = Lude.Nothing,
      validationStatuses = Lude.mempty
    }

-- | The status of the scan of the Docker image container for the model package.
--
-- /Note:/ Consider using 'imageScanStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdImageScanStatuses :: Lens.Lens' ModelPackageStatusDetails (Lude.Maybe [ModelPackageStatusItem])
mpsdImageScanStatuses = Lens.lens (imageScanStatuses :: ModelPackageStatusDetails -> Lude.Maybe [ModelPackageStatusItem]) (\s a -> s {imageScanStatuses = a} :: ModelPackageStatusDetails)
{-# DEPRECATED mpsdImageScanStatuses "Use generic-lens or generic-optics with 'imageScanStatuses' instead." #-}

-- | The validation status of the model package.
--
-- /Note:/ Consider using 'validationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsdValidationStatuses :: Lens.Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
mpsdValidationStatuses = Lens.lens (validationStatuses :: ModelPackageStatusDetails -> [ModelPackageStatusItem]) (\s a -> s {validationStatuses = a} :: ModelPackageStatusDetails)
{-# DEPRECATED mpsdValidationStatuses "Use generic-lens or generic-optics with 'validationStatuses' instead." #-}

instance Lude.FromJSON ModelPackageStatusDetails where
  parseJSON =
    Lude.withObject
      "ModelPackageStatusDetails"
      ( \x ->
          ModelPackageStatusDetails'
            Lude.<$> (x Lude..:? "ImageScanStatuses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ValidationStatuses" Lude..!= Lude.mempty)
      )
