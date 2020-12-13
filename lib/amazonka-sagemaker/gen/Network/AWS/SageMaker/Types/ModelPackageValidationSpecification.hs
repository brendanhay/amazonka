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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate your model package.
--
-- /See:/ 'mkModelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { -- | The IAM roles to be used for the validation of the model package.
    validationRole :: Lude.Text,
    -- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
    validationProfiles :: Lude.NonEmpty ModelPackageValidationProfile
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageValidationSpecification' with the minimum fields required to make a request.
--
-- * 'validationRole' - The IAM roles to be used for the validation of the model package.
-- * 'validationProfiles' - An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
mkModelPackageValidationSpecification ::
  -- | 'validationRole'
  Lude.Text ->
  -- | 'validationProfiles'
  Lude.NonEmpty ModelPackageValidationProfile ->
  ModelPackageValidationSpecification
mkModelPackageValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    ModelPackageValidationSpecification'
      { validationRole =
          pValidationRole_,
        validationProfiles = pValidationProfiles_
      }

-- | The IAM roles to be used for the validation of the model package.
--
-- /Note:/ Consider using 'validationRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvsValidationRole :: Lens.Lens' ModelPackageValidationSpecification Lude.Text
mpvsValidationRole = Lens.lens (validationRole :: ModelPackageValidationSpecification -> Lude.Text) (\s a -> s {validationRole = a} :: ModelPackageValidationSpecification)
{-# DEPRECATED mpvsValidationRole "Use generic-lens or generic-optics with 'validationRole' instead." #-}

-- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
--
-- /Note:/ Consider using 'validationProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvsValidationProfiles :: Lens.Lens' ModelPackageValidationSpecification (Lude.NonEmpty ModelPackageValidationProfile)
mpvsValidationProfiles = Lens.lens (validationProfiles :: ModelPackageValidationSpecification -> Lude.NonEmpty ModelPackageValidationProfile) (\s a -> s {validationProfiles = a} :: ModelPackageValidationSpecification)
{-# DEPRECATED mpvsValidationProfiles "Use generic-lens or generic-optics with 'validationProfiles' instead." #-}

instance Lude.FromJSON ModelPackageValidationSpecification where
  parseJSON =
    Lude.withObject
      "ModelPackageValidationSpecification"
      ( \x ->
          ModelPackageValidationSpecification'
            Lude.<$> (x Lude..: "ValidationRole")
            Lude.<*> (x Lude..: "ValidationProfiles")
      )

instance Lude.ToJSON ModelPackageValidationSpecification where
  toJSON ModelPackageValidationSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ValidationRole" Lude..= validationRole),
            Lude.Just ("ValidationProfiles" Lude..= validationProfiles)
          ]
      )
