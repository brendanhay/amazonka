{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageValidationProfile
  ( ModelPackageValidationProfile (..)
  -- * Smart constructor
  , mkModelPackageValidationProfile
  -- * Lenses
  , mpvpProfileName
  , mpvpTransformJobDefinition
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EntityName as Types
import qualified Network.AWS.SageMaker.Types.TransformJobDefinition as Types

-- | Contains data, such as the inputs and targeted instance types that are used in the process of validating the model package.
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
-- /See:/ 'mkModelPackageValidationProfile' smart constructor.
data ModelPackageValidationProfile = ModelPackageValidationProfile'
  { profileName :: Types.EntityName
    -- ^ The name of the profile for the model package.
  , transformJobDefinition :: Types.TransformJobDefinition
    -- ^ The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelPackageValidationProfile' value with any optional fields omitted.
mkModelPackageValidationProfile
    :: Types.EntityName -- ^ 'profileName'
    -> Types.TransformJobDefinition -- ^ 'transformJobDefinition'
    -> ModelPackageValidationProfile
mkModelPackageValidationProfile profileName transformJobDefinition
  = ModelPackageValidationProfile'{profileName,
                                   transformJobDefinition}

-- | The name of the profile for the model package.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvpProfileName :: Lens.Lens' ModelPackageValidationProfile Types.EntityName
mpvpProfileName = Lens.field @"profileName"
{-# INLINEABLE mpvpProfileName #-}
{-# DEPRECATED profileName "Use generic-lens or generic-optics with 'profileName' instead"  #-}

-- | The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
--
-- /Note:/ Consider using 'transformJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpvpTransformJobDefinition :: Lens.Lens' ModelPackageValidationProfile Types.TransformJobDefinition
mpvpTransformJobDefinition = Lens.field @"transformJobDefinition"
{-# INLINEABLE mpvpTransformJobDefinition #-}
{-# DEPRECATED transformJobDefinition "Use generic-lens or generic-optics with 'transformJobDefinition' instead"  #-}

instance Core.FromJSON ModelPackageValidationProfile where
        toJSON ModelPackageValidationProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProfileName" Core..= profileName),
                  Core.Just
                    ("TransformJobDefinition" Core..= transformJobDefinition)])

instance Core.FromJSON ModelPackageValidationProfile where
        parseJSON
          = Core.withObject "ModelPackageValidationProfile" Core.$
              \ x ->
                ModelPackageValidationProfile' Core.<$>
                  (x Core..: "ProfileName") Core.<*>
                    x Core..: "TransformJobDefinition"
