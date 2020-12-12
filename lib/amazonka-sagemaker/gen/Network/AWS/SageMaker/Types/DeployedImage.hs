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
    diResolvedImage,
    diSpecifiedImage,
    diResolutionTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Gets the Amazon EC2 Container Registry path of the docker image of the model that is hosted in this 'ProductionVariant' .
--
-- If you used the @registry/repository[:tag]@ form to specify the image path of the primary container when you created the model hosted in this @ProductionVariant@ , the path resolves to a path of the form @registry/repository[@digest]@ . A digest is a hash value that identifies a specific version of an image. For information about Amazon ECR paths, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an Image> in the /Amazon ECR User Guide/ .
--
-- /See:/ 'mkDeployedImage' smart constructor.
data DeployedImage = DeployedImage'
  { resolvedImage ::
      Lude.Maybe Lude.Text,
    specifiedImage :: Lude.Maybe Lude.Text,
    resolutionTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeployedImage' with the minimum fields required to make a request.
--
-- * 'resolutionTime' - The date and time when the image path for the model resolved to the @ResolvedImage@
-- * 'resolvedImage' - The specific digest path of the image hosted in this @ProductionVariant@ .
-- * 'specifiedImage' - The image path you specified when you created the model.
mkDeployedImage ::
  DeployedImage
mkDeployedImage =
  DeployedImage'
    { resolvedImage = Lude.Nothing,
      specifiedImage = Lude.Nothing,
      resolutionTime = Lude.Nothing
    }

-- | The specific digest path of the image hosted in this @ProductionVariant@ .
--
-- /Note:/ Consider using 'resolvedImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResolvedImage :: Lens.Lens' DeployedImage (Lude.Maybe Lude.Text)
diResolvedImage = Lens.lens (resolvedImage :: DeployedImage -> Lude.Maybe Lude.Text) (\s a -> s {resolvedImage = a} :: DeployedImage)
{-# DEPRECATED diResolvedImage "Use generic-lens or generic-optics with 'resolvedImage' instead." #-}

-- | The image path you specified when you created the model.
--
-- /Note:/ Consider using 'specifiedImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSpecifiedImage :: Lens.Lens' DeployedImage (Lude.Maybe Lude.Text)
diSpecifiedImage = Lens.lens (specifiedImage :: DeployedImage -> Lude.Maybe Lude.Text) (\s a -> s {specifiedImage = a} :: DeployedImage)
{-# DEPRECATED diSpecifiedImage "Use generic-lens or generic-optics with 'specifiedImage' instead." #-}

-- | The date and time when the image path for the model resolved to the @ResolvedImage@
--
-- /Note:/ Consider using 'resolutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResolutionTime :: Lens.Lens' DeployedImage (Lude.Maybe Lude.Timestamp)
diResolutionTime = Lens.lens (resolutionTime :: DeployedImage -> Lude.Maybe Lude.Timestamp) (\s a -> s {resolutionTime = a} :: DeployedImage)
{-# DEPRECATED diResolutionTime "Use generic-lens or generic-optics with 'resolutionTime' instead." #-}

instance Lude.FromJSON DeployedImage where
  parseJSON =
    Lude.withObject
      "DeployedImage"
      ( \x ->
          DeployedImage'
            Lude.<$> (x Lude..:? "ResolvedImage")
            Lude.<*> (x Lude..:? "SpecifiedImage")
            Lude.<*> (x Lude..:? "ResolutionTime")
      )
