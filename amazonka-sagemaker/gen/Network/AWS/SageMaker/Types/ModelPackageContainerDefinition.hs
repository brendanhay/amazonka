{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageContainerDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the Docker container for the model package.
--
-- /See:/ 'newModelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { -- | An MD5 hash of the training algorithm that identifies the Docker image
    -- used for training.
    imageDigest :: Core.Maybe Core.Text,
    -- | The Amazon S3 path where the model artifacts, which result from model
    -- training, are stored. This path must point to a single @gzip@ compressed
    -- tar archive (@.tar.gz@ suffix).
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the model package.
    modelDataUrl :: Core.Maybe Core.Text,
    -- | The DNS host name for the Docker container.
    containerHostname :: Core.Maybe Core.Text,
    -- | The AWS Marketplace product ID of the model package.
    productId :: Core.Maybe Core.Text,
    -- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code
    -- is stored.
    --
    -- If you are using your own custom algorithm instead of an algorithm
    -- provided by Amazon SageMaker, the inference code must meet Amazon
    -- SageMaker requirements. Amazon SageMaker supports both
    -- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
    -- path formats. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    image :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackageContainerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'modelPackageContainerDefinition_imageDigest' - An MD5 hash of the training algorithm that identifies the Docker image
-- used for training.
--
-- 'modelDataUrl', 'modelPackageContainerDefinition_modelDataUrl' - The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model package.
--
-- 'containerHostname', 'modelPackageContainerDefinition_containerHostname' - The DNS host name for the Docker container.
--
-- 'productId', 'modelPackageContainerDefinition_productId' - The AWS Marketplace product ID of the model package.
--
-- 'image', 'modelPackageContainerDefinition_image' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code
-- is stored.
--
-- If you are using your own custom algorithm instead of an algorithm
-- provided by Amazon SageMaker, the inference code must meet Amazon
-- SageMaker requirements. Amazon SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
newModelPackageContainerDefinition ::
  -- | 'image'
  Core.Text ->
  ModelPackageContainerDefinition
newModelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { imageDigest =
        Core.Nothing,
      modelDataUrl = Core.Nothing,
      containerHostname = Core.Nothing,
      productId = Core.Nothing,
      image = pImage_
    }

-- | An MD5 hash of the training algorithm that identifies the Docker image
-- used for training.
modelPackageContainerDefinition_imageDigest :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Core.Text)
modelPackageContainerDefinition_imageDigest = Lens.lens (\ModelPackageContainerDefinition' {imageDigest} -> imageDigest) (\s@ModelPackageContainerDefinition' {} a -> s {imageDigest = a} :: ModelPackageContainerDefinition)

-- | The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model package.
modelPackageContainerDefinition_modelDataUrl :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Core.Text)
modelPackageContainerDefinition_modelDataUrl = Lens.lens (\ModelPackageContainerDefinition' {modelDataUrl} -> modelDataUrl) (\s@ModelPackageContainerDefinition' {} a -> s {modelDataUrl = a} :: ModelPackageContainerDefinition)

-- | The DNS host name for the Docker container.
modelPackageContainerDefinition_containerHostname :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Core.Text)
modelPackageContainerDefinition_containerHostname = Lens.lens (\ModelPackageContainerDefinition' {containerHostname} -> containerHostname) (\s@ModelPackageContainerDefinition' {} a -> s {containerHostname = a} :: ModelPackageContainerDefinition)

-- | The AWS Marketplace product ID of the model package.
modelPackageContainerDefinition_productId :: Lens.Lens' ModelPackageContainerDefinition (Core.Maybe Core.Text)
modelPackageContainerDefinition_productId = Lens.lens (\ModelPackageContainerDefinition' {productId} -> productId) (\s@ModelPackageContainerDefinition' {} a -> s {productId = a} :: ModelPackageContainerDefinition)

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code
-- is stored.
--
-- If you are using your own custom algorithm instead of an algorithm
-- provided by Amazon SageMaker, the inference code must meet Amazon
-- SageMaker requirements. Amazon SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
modelPackageContainerDefinition_image :: Lens.Lens' ModelPackageContainerDefinition Core.Text
modelPackageContainerDefinition_image = Lens.lens (\ModelPackageContainerDefinition' {image} -> image) (\s@ModelPackageContainerDefinition' {} a -> s {image = a} :: ModelPackageContainerDefinition)

instance
  Core.FromJSON
    ModelPackageContainerDefinition
  where
  parseJSON =
    Core.withObject
      "ModelPackageContainerDefinition"
      ( \x ->
          ModelPackageContainerDefinition'
            Core.<$> (x Core..:? "ImageDigest")
            Core.<*> (x Core..:? "ModelDataUrl")
            Core.<*> (x Core..:? "ContainerHostname")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..: "Image")
      )

instance
  Core.Hashable
    ModelPackageContainerDefinition

instance Core.NFData ModelPackageContainerDefinition

instance Core.ToJSON ModelPackageContainerDefinition where
  toJSON ModelPackageContainerDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageDigest" Core..=) Core.<$> imageDigest,
            ("ModelDataUrl" Core..=) Core.<$> modelDataUrl,
            ("ContainerHostname" Core..=)
              Core.<$> containerHostname,
            ("ProductId" Core..=) Core.<$> productId,
            Core.Just ("Image" Core..= image)
          ]
      )
