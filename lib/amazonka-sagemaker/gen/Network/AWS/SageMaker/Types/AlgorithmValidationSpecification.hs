-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
  ( AlgorithmValidationSpecification (..),

    -- * Smart constructor
    mkAlgorithmValidationSpecification,

    -- * Lenses
    avsValidationRole,
    avsValidationProfiles,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AlgorithmValidationProfile

-- | Specifies configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- /See:/ 'mkAlgorithmValidationSpecification' smart constructor.
data AlgorithmValidationSpecification = AlgorithmValidationSpecification'
  { validationRole ::
      Lude.Text,
    validationProfiles ::
      Lude.NonEmpty
        AlgorithmValidationProfile
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlgorithmValidationSpecification' with the minimum fields required to make a request.
--
-- * 'validationProfiles' - An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
-- * 'validationRole' - The IAM roles that Amazon SageMaker uses to run the training jobs.
mkAlgorithmValidationSpecification ::
  -- | 'validationRole'
  Lude.Text ->
  -- | 'validationProfiles'
  Lude.NonEmpty AlgorithmValidationProfile ->
  AlgorithmValidationSpecification
mkAlgorithmValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    AlgorithmValidationSpecification'
      { validationRole =
          pValidationRole_,
        validationProfiles = pValidationProfiles_
      }

-- | The IAM roles that Amazon SageMaker uses to run the training jobs.
--
-- /Note:/ Consider using 'validationRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsValidationRole :: Lens.Lens' AlgorithmValidationSpecification Lude.Text
avsValidationRole = Lens.lens (validationRole :: AlgorithmValidationSpecification -> Lude.Text) (\s a -> s {validationRole = a} :: AlgorithmValidationSpecification)
{-# DEPRECATED avsValidationRole "Use generic-lens or generic-optics with 'validationRole' instead." #-}

-- | An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
--
-- /Note:/ Consider using 'validationProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsValidationProfiles :: Lens.Lens' AlgorithmValidationSpecification (Lude.NonEmpty AlgorithmValidationProfile)
avsValidationProfiles = Lens.lens (validationProfiles :: AlgorithmValidationSpecification -> Lude.NonEmpty AlgorithmValidationProfile) (\s a -> s {validationProfiles = a} :: AlgorithmValidationSpecification)
{-# DEPRECATED avsValidationProfiles "Use generic-lens or generic-optics with 'validationProfiles' instead." #-}

instance Lude.FromJSON AlgorithmValidationSpecification where
  parseJSON =
    Lude.withObject
      "AlgorithmValidationSpecification"
      ( \x ->
          AlgorithmValidationSpecification'
            Lude.<$> (x Lude..: "ValidationRole")
            Lude.<*> (x Lude..: "ValidationProfiles")
      )

instance Lude.ToJSON AlgorithmValidationSpecification where
  toJSON AlgorithmValidationSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ValidationRole" Lude..= validationRole),
            Lude.Just ("ValidationProfiles" Lude..= validationProfiles)
          ]
      )
