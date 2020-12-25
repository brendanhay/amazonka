{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
  ( ModelPackageValidationSpecification (..),

    -- * Smart constructor
    mkModelPackageValidationSpecification,

    -- * Lenses
    mpvsValidationRole,
    mpvsValidationProfiles,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ModelPackageValidationProfile as Types
import qualified Network.AWS.SageMaker.Types.ValidationRole as Types

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate your model package.
--
-- /See:/ 'mkModelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { -- | The IAM roles to be used for the validation of the model package.
    validationRole :: Types.ValidationRole,
    -- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
    validationProfiles :: Core.NonEmpty Types.ModelPackageValidationProfile
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelPackageValidationSpecification' value with any optional fields omitted.
mkModelPackageValidationSpecification ::
  -- | 'validationRole'
  Types.ValidationRole ->
  -- | 'validationProfiles'
  Core.NonEmpty Types.ModelPackageValidationProfile ->
  ModelPackageValidationSpecification
mkModelPackageValidationSpecification
  validationRole
  validationProfiles =
    ModelPackageValidationSpecification'
      { validationRole,
        validationProfiles
      }

-- | The IAM roles to be used for the validation of the model package.
--
-- /Note:/ Consider using 'validationRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvsValidationRole :: Lens.Lens' ModelPackageValidationSpecification Types.ValidationRole
mpvsValidationRole = Lens.field @"validationRole"
{-# DEPRECATED mpvsValidationRole "Use generic-lens or generic-optics with 'validationRole' instead." #-}

-- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
--
-- /Note:/ Consider using 'validationProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvsValidationProfiles :: Lens.Lens' ModelPackageValidationSpecification (Core.NonEmpty Types.ModelPackageValidationProfile)
mpvsValidationProfiles = Lens.field @"validationProfiles"
{-# DEPRECATED mpvsValidationProfiles "Use generic-lens or generic-optics with 'validationProfiles' instead." #-}

instance Core.FromJSON ModelPackageValidationSpecification where
  toJSON ModelPackageValidationSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ValidationRole" Core..= validationRole),
            Core.Just ("ValidationProfiles" Core..= validationProfiles)
          ]
      )

instance Core.FromJSON ModelPackageValidationSpecification where
  parseJSON =
    Core.withObject "ModelPackageValidationSpecification" Core.$
      \x ->
        ModelPackageValidationSpecification'
          Core.<$> (x Core..: "ValidationRole")
          Core.<*> (x Core..: "ValidationProfiles")
