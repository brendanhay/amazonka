{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
  ( AlgorithmValidationSpecification (..)
  -- * Smart constructor
  , mkAlgorithmValidationSpecification
  -- * Lenses
  , avsValidationRole
  , avsValidationProfiles
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AlgorithmValidationProfile as Types
import qualified Network.AWS.SageMaker.Types.ValidationRole as Types

-- | Specifies configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- /See:/ 'mkAlgorithmValidationSpecification' smart constructor.
data AlgorithmValidationSpecification = AlgorithmValidationSpecification'
  { validationRole :: Types.ValidationRole
    -- ^ The IAM roles that Amazon SageMaker uses to run the training jobs.
  , validationProfiles :: Core.NonEmpty Types.AlgorithmValidationProfile
    -- ^ An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmValidationSpecification' value with any optional fields omitted.
mkAlgorithmValidationSpecification
    :: Types.ValidationRole -- ^ 'validationRole'
    -> Core.NonEmpty Types.AlgorithmValidationProfile -- ^ 'validationProfiles'
    -> AlgorithmValidationSpecification
mkAlgorithmValidationSpecification validationRole
  validationProfiles
  = AlgorithmValidationSpecification'{validationRole,
                                      validationProfiles}

-- | The IAM roles that Amazon SageMaker uses to run the training jobs.
--
-- /Note:/ Consider using 'validationRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsValidationRole :: Lens.Lens' AlgorithmValidationSpecification Types.ValidationRole
avsValidationRole = Lens.field @"validationRole"
{-# INLINEABLE avsValidationRole #-}
{-# DEPRECATED validationRole "Use generic-lens or generic-optics with 'validationRole' instead"  #-}

-- | An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
--
-- /Note:/ Consider using 'validationProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsValidationProfiles :: Lens.Lens' AlgorithmValidationSpecification (Core.NonEmpty Types.AlgorithmValidationProfile)
avsValidationProfiles = Lens.field @"validationProfiles"
{-# INLINEABLE avsValidationProfiles #-}
{-# DEPRECATED validationProfiles "Use generic-lens or generic-optics with 'validationProfiles' instead"  #-}

instance Core.FromJSON AlgorithmValidationSpecification where
        toJSON AlgorithmValidationSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ValidationRole" Core..= validationRole),
                  Core.Just ("ValidationProfiles" Core..= validationProfiles)])

instance Core.FromJSON AlgorithmValidationSpecification where
        parseJSON
          = Core.withObject "AlgorithmValidationSpecification" Core.$
              \ x ->
                AlgorithmValidationSpecification' Core.<$>
                  (x Core..: "ValidationRole") Core.<*>
                    x Core..: "ValidationProfiles"
