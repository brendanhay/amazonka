{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmValidationProfile
  ( AlgorithmValidationProfile (..),

    -- * Smart constructor
    mkAlgorithmValidationProfile,

    -- * Lenses
    avpProfileName,
    avpTrainingJobDefinition,
    avpTransformJobDefinition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EntityName as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobDefinition as Types
import qualified Network.AWS.SageMaker.Types.TransformJobDefinition as Types

-- | Defines a training job and a batch transform job that Amazon SageMaker runs to validate your algorithm.
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
-- /See:/ 'mkAlgorithmValidationProfile' smart constructor.
data AlgorithmValidationProfile = AlgorithmValidationProfile'
  { -- | The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    profileName :: Types.EntityName,
    -- | The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
    trainingJobDefinition :: Types.TrainingJobDefinition,
    -- | The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
    transformJobDefinition :: Core.Maybe Types.TransformJobDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmValidationProfile' value with any optional fields omitted.
mkAlgorithmValidationProfile ::
  -- | 'profileName'
  Types.EntityName ->
  -- | 'trainingJobDefinition'
  Types.TrainingJobDefinition ->
  AlgorithmValidationProfile
mkAlgorithmValidationProfile profileName trainingJobDefinition =
  AlgorithmValidationProfile'
    { profileName,
      trainingJobDefinition,
      transformJobDefinition = Core.Nothing
    }

-- | The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpProfileName :: Lens.Lens' AlgorithmValidationProfile Types.EntityName
avpProfileName = Lens.field @"profileName"
{-# DEPRECATED avpProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
--
-- /Note:/ Consider using 'trainingJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpTrainingJobDefinition :: Lens.Lens' AlgorithmValidationProfile Types.TrainingJobDefinition
avpTrainingJobDefinition = Lens.field @"trainingJobDefinition"
{-# DEPRECATED avpTrainingJobDefinition "Use generic-lens or generic-optics with 'trainingJobDefinition' instead." #-}

-- | The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
--
-- /Note:/ Consider using 'transformJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpTransformJobDefinition :: Lens.Lens' AlgorithmValidationProfile (Core.Maybe Types.TransformJobDefinition)
avpTransformJobDefinition = Lens.field @"transformJobDefinition"
{-# DEPRECATED avpTransformJobDefinition "Use generic-lens or generic-optics with 'transformJobDefinition' instead." #-}

instance Core.FromJSON AlgorithmValidationProfile where
  toJSON AlgorithmValidationProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProfileName" Core..= profileName),
            Core.Just ("TrainingJobDefinition" Core..= trainingJobDefinition),
            ("TransformJobDefinition" Core..=)
              Core.<$> transformJobDefinition
          ]
      )

instance Core.FromJSON AlgorithmValidationProfile where
  parseJSON =
    Core.withObject "AlgorithmValidationProfile" Core.$
      \x ->
        AlgorithmValidationProfile'
          Core.<$> (x Core..: "ProfileName")
          Core.<*> (x Core..: "TrainingJobDefinition")
          Core.<*> (x Core..:? "TransformJobDefinition")
