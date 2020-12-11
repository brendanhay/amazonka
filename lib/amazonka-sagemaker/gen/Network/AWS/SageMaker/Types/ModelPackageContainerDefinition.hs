-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
  ( ModelPackageContainerDefinition (..),

    -- * Smart constructor
    mkModelPackageContainerDefinition,

    -- * Lenses
    mpcdModelDataURL,
    mpcdImageDigest,
    mpcdContainerHostname,
    mpcdProductId,
    mpcdImage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Docker container for the model package.
--
-- /See:/ 'mkModelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { modelDataURL ::
      Lude.Maybe Lude.Text,
    imageDigest ::
      Lude.Maybe Lude.Text,
    containerHostname ::
      Lude.Maybe Lude.Text,
    productId ::
      Lude.Maybe Lude.Text,
    image :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageContainerDefinition' with the minimum fields required to make a request.
--
-- * 'containerHostname' - The DNS host name for the Docker container.
-- * 'image' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored.
--
-- If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
-- * 'imageDigest' - An MD5 hash of the training algorithm that identifies the Docker image used for training.
-- * 'modelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
-- * 'productId' - The AWS Marketplace product ID of the model package.
mkModelPackageContainerDefinition ::
  -- | 'image'
  Lude.Text ->
  ModelPackageContainerDefinition
mkModelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { modelDataURL = Lude.Nothing,
      imageDigest = Lude.Nothing,
      containerHostname = Lude.Nothing,
      productId = Lude.Nothing,
      image = pImage_
    }

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- /Note:/ Consider using 'modelDataURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdModelDataURL :: Lens.Lens' ModelPackageContainerDefinition (Lude.Maybe Lude.Text)
mpcdModelDataURL = Lens.lens (modelDataURL :: ModelPackageContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {modelDataURL = a} :: ModelPackageContainerDefinition)
{-# DEPRECATED mpcdModelDataURL "Use generic-lens or generic-optics with 'modelDataURL' instead." #-}

-- | An MD5 hash of the training algorithm that identifies the Docker image used for training.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdImageDigest :: Lens.Lens' ModelPackageContainerDefinition (Lude.Maybe Lude.Text)
mpcdImageDigest = Lens.lens (imageDigest :: ModelPackageContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: ModelPackageContainerDefinition)
{-# DEPRECATED mpcdImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The DNS host name for the Docker container.
--
-- /Note:/ Consider using 'containerHostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdContainerHostname :: Lens.Lens' ModelPackageContainerDefinition (Lude.Maybe Lude.Text)
mpcdContainerHostname = Lens.lens (containerHostname :: ModelPackageContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {containerHostname = a} :: ModelPackageContainerDefinition)
{-# DEPRECATED mpcdContainerHostname "Use generic-lens or generic-optics with 'containerHostname' instead." #-}

-- | The AWS Marketplace product ID of the model package.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdProductId :: Lens.Lens' ModelPackageContainerDefinition (Lude.Maybe Lude.Text)
mpcdProductId = Lens.lens (productId :: ModelPackageContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ModelPackageContainerDefinition)
{-# DEPRECATED mpcdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored.
--
-- If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdImage :: Lens.Lens' ModelPackageContainerDefinition Lude.Text
mpcdImage = Lens.lens (image :: ModelPackageContainerDefinition -> Lude.Text) (\s a -> s {image = a} :: ModelPackageContainerDefinition)
{-# DEPRECATED mpcdImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.FromJSON ModelPackageContainerDefinition where
  parseJSON =
    Lude.withObject
      "ModelPackageContainerDefinition"
      ( \x ->
          ModelPackageContainerDefinition'
            Lude.<$> (x Lude..:? "ModelDataUrl")
            Lude.<*> (x Lude..:? "ImageDigest")
            Lude.<*> (x Lude..:? "ContainerHostname")
            Lude.<*> (x Lude..:? "ProductId")
            Lude.<*> (x Lude..: "Image")
      )

instance Lude.ToJSON ModelPackageContainerDefinition where
  toJSON ModelPackageContainerDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ModelDataUrl" Lude..=) Lude.<$> modelDataURL,
            ("ImageDigest" Lude..=) Lude.<$> imageDigest,
            ("ContainerHostname" Lude..=) Lude.<$> containerHostname,
            ("ProductId" Lude..=) Lude.<$> productId,
            Lude.Just ("Image" Lude..= image)
          ]
      )
