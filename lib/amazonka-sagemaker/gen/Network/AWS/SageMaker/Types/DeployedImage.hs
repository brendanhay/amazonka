{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DeployedImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeployedImage
  ( DeployedImage (..),

    -- * Smart constructor
    mkDeployedImage,

    -- * Lenses
    diResolutionTime,
    diResolvedImage,
    diSpecifiedImage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ResolvedImage as Types
import qualified Network.AWS.SageMaker.Types.SpecifiedImage as Types

-- | Gets the Amazon EC2 Container Registry path of the docker image of the model that is hosted in this 'ProductionVariant' .
--
-- If you used the @registry/repository[:tag]@ form to specify the image path of the primary container when you created the model hosted in this @ProductionVariant@ , the path resolves to a path of the form @registry/repository[@digest]@ . A digest is a hash value that identifies a specific version of an image. For information about Amazon ECR paths, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an Image> in the /Amazon ECR User Guide/ .
--
-- /See:/ 'mkDeployedImage' smart constructor.
data DeployedImage = DeployedImage'
  { -- | The date and time when the image path for the model resolved to the @ResolvedImage@
    resolutionTime :: Core.Maybe Core.NominalDiffTime,
    -- | The specific digest path of the image hosted in this @ProductionVariant@ .
    resolvedImage :: Core.Maybe Types.ResolvedImage,
    -- | The image path you specified when you created the model.
    specifiedImage :: Core.Maybe Types.SpecifiedImage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeployedImage' value with any optional fields omitted.
mkDeployedImage ::
  DeployedImage
mkDeployedImage =
  DeployedImage'
    { resolutionTime = Core.Nothing,
      resolvedImage = Core.Nothing,
      specifiedImage = Core.Nothing
    }

-- | The date and time when the image path for the model resolved to the @ResolvedImage@
--
-- /Note:/ Consider using 'resolutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResolutionTime :: Lens.Lens' DeployedImage (Core.Maybe Core.NominalDiffTime)
diResolutionTime = Lens.field @"resolutionTime"
{-# DEPRECATED diResolutionTime "Use generic-lens or generic-optics with 'resolutionTime' instead." #-}

-- | The specific digest path of the image hosted in this @ProductionVariant@ .
--
-- /Note:/ Consider using 'resolvedImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResolvedImage :: Lens.Lens' DeployedImage (Core.Maybe Types.ResolvedImage)
diResolvedImage = Lens.field @"resolvedImage"
{-# DEPRECATED diResolvedImage "Use generic-lens or generic-optics with 'resolvedImage' instead." #-}

-- | The image path you specified when you created the model.
--
-- /Note:/ Consider using 'specifiedImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSpecifiedImage :: Lens.Lens' DeployedImage (Core.Maybe Types.SpecifiedImage)
diSpecifiedImage = Lens.field @"specifiedImage"
{-# DEPRECATED diSpecifiedImage "Use generic-lens or generic-optics with 'specifiedImage' instead." #-}

instance Core.FromJSON DeployedImage where
  parseJSON =
    Core.withObject "DeployedImage" Core.$
      \x ->
        DeployedImage'
          Core.<$> (x Core..:? "ResolutionTime")
          Core.<*> (x Core..:? "ResolvedImage")
          Core.<*> (x Core..:? "SpecifiedImage")
