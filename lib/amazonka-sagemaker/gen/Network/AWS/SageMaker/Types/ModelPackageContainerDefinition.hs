{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
  ( ModelPackageContainerDefinition (..)
  -- * Smart constructor
  , mkModelPackageContainerDefinition
  -- * Lenses
  , mpcdImage
  , mpcdContainerHostname
  , mpcdImageDigest
  , mpcdModelDataUrl
  , mpcdProductId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContainerHostname as Types
import qualified Network.AWS.SageMaker.Types.ContainerImage as Types
import qualified Network.AWS.SageMaker.Types.ImageDigest as Types
import qualified Network.AWS.SageMaker.Types.ProductId as Types
import qualified Network.AWS.SageMaker.Types.Url as Types

-- | Describes the Docker container for the model package.
--
-- /See:/ 'mkModelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { image :: Types.ContainerImage
    -- ^ The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored.
--
-- If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
  , containerHostname :: Core.Maybe Types.ContainerHostname
    -- ^ The DNS host name for the Docker container.
  , imageDigest :: Core.Maybe Types.ImageDigest
    -- ^ An MD5 hash of the training algorithm that identifies the Docker image used for training.
  , modelDataUrl :: Core.Maybe Types.Url
    -- ^ The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
  , productId :: Core.Maybe Types.ProductId
    -- ^ The AWS Marketplace product ID of the model package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelPackageContainerDefinition' value with any optional fields omitted.
mkModelPackageContainerDefinition
    :: Types.ContainerImage -- ^ 'image'
    -> ModelPackageContainerDefinition
mkModelPackageContainerDefinition image
  = ModelPackageContainerDefinition'{image,
                                     containerHostname = Core.Nothing, imageDigest = Core.Nothing,
                                     modelDataUrl = Core.Nothing, productId = Core.Nothing}

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored.
--
-- If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdImage :: Lens.Lens' ModelPackageContainerDefinition Types.ContainerImage
mpcdImage = Lens.field @"image"
{-# INLINEABLE mpcdImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The DNS host name for the Docker container.
--
-- /Note:/ Consider using 'containerHostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdContainerHostname :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Types.ContainerHostname)
mpcdContainerHostname = Lens.field @"containerHostname"
{-# INLINEABLE mpcdContainerHostname #-}
{-# DEPRECATED containerHostname "Use generic-lens or generic-optics with 'containerHostname' instead"  #-}

-- | An MD5 hash of the training algorithm that identifies the Docker image used for training.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdImageDigest :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Types.ImageDigest)
mpcdImageDigest = Lens.field @"imageDigest"
{-# INLINEABLE mpcdImageDigest #-}
{-# DEPRECATED imageDigest "Use generic-lens or generic-optics with 'imageDigest' instead"  #-}

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- /Note:/ Consider using 'modelDataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdModelDataUrl :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Types.Url)
mpcdModelDataUrl = Lens.field @"modelDataUrl"
{-# INLINEABLE mpcdModelDataUrl #-}
{-# DEPRECATED modelDataUrl "Use generic-lens or generic-optics with 'modelDataUrl' instead"  #-}

-- | The AWS Marketplace product ID of the model package.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdProductId :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Types.ProductId)
mpcdProductId = Lens.field @"productId"
{-# INLINEABLE mpcdProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

instance Core.FromJSON ModelPackageContainerDefinition where
        toJSON ModelPackageContainerDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Image" Core..= image),
                  ("ContainerHostname" Core..=) Core.<$> containerHostname,
                  ("ImageDigest" Core..=) Core.<$> imageDigest,
                  ("ModelDataUrl" Core..=) Core.<$> modelDataUrl,
                  ("ProductId" Core..=) Core.<$> productId])

instance Core.FromJSON ModelPackageContainerDefinition where
        parseJSON
          = Core.withObject "ModelPackageContainerDefinition" Core.$
              \ x ->
                ModelPackageContainerDefinition' Core.<$>
                  (x Core..: "Image") Core.<*> x Core..:? "ContainerHostname"
                    Core.<*> x Core..:? "ImageDigest"
                    Core.<*> x Core..:? "ModelDataUrl"
                    Core.<*> x Core..:? "ProductId"
