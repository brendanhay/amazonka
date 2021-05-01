{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Docker container for the model package.
--
-- /See:/ 'newModelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { -- | An MD5 hash of the training algorithm that identifies the Docker image
    -- used for training.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path where the model artifacts, which result from model
    -- training, are stored. This path must point to a single @gzip@ compressed
    -- tar archive (@.tar.gz@ suffix).
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the model package.
    modelDataUrl :: Prelude.Maybe Prelude.Text,
    -- | The DNS host name for the Docker container.
    containerHostname :: Prelude.Maybe Prelude.Text,
    -- | The AWS Marketplace product ID of the model package.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code
    -- is stored.
    --
    -- If you are using your own custom algorithm instead of an algorithm
    -- provided by Amazon SageMaker, the inference code must meet Amazon
    -- SageMaker requirements. Amazon SageMaker supports both
    -- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
    -- path formats. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    image :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ModelPackageContainerDefinition
newModelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { imageDigest =
        Prelude.Nothing,
      modelDataUrl = Prelude.Nothing,
      containerHostname = Prelude.Nothing,
      productId = Prelude.Nothing,
      image = pImage_
    }

-- | An MD5 hash of the training algorithm that identifies the Docker image
-- used for training.
modelPackageContainerDefinition_imageDigest :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_imageDigest = Lens.lens (\ModelPackageContainerDefinition' {imageDigest} -> imageDigest) (\s@ModelPackageContainerDefinition' {} a -> s {imageDigest = a} :: ModelPackageContainerDefinition)

-- | The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model package.
modelPackageContainerDefinition_modelDataUrl :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_modelDataUrl = Lens.lens (\ModelPackageContainerDefinition' {modelDataUrl} -> modelDataUrl) (\s@ModelPackageContainerDefinition' {} a -> s {modelDataUrl = a} :: ModelPackageContainerDefinition)

-- | The DNS host name for the Docker container.
modelPackageContainerDefinition_containerHostname :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_containerHostname = Lens.lens (\ModelPackageContainerDefinition' {containerHostname} -> containerHostname) (\s@ModelPackageContainerDefinition' {} a -> s {containerHostname = a} :: ModelPackageContainerDefinition)

-- | The AWS Marketplace product ID of the model package.
modelPackageContainerDefinition_productId :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
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
modelPackageContainerDefinition_image :: Lens.Lens' ModelPackageContainerDefinition Prelude.Text
modelPackageContainerDefinition_image = Lens.lens (\ModelPackageContainerDefinition' {image} -> image) (\s@ModelPackageContainerDefinition' {} a -> s {image = a} :: ModelPackageContainerDefinition)

instance
  Prelude.FromJSON
    ModelPackageContainerDefinition
  where
  parseJSON =
    Prelude.withObject
      "ModelPackageContainerDefinition"
      ( \x ->
          ModelPackageContainerDefinition'
            Prelude.<$> (x Prelude..:? "ImageDigest")
            Prelude.<*> (x Prelude..:? "ModelDataUrl")
            Prelude.<*> (x Prelude..:? "ContainerHostname")
            Prelude.<*> (x Prelude..:? "ProductId")
            Prelude.<*> (x Prelude..: "Image")
      )

instance
  Prelude.Hashable
    ModelPackageContainerDefinition

instance
  Prelude.NFData
    ModelPackageContainerDefinition

instance
  Prelude.ToJSON
    ModelPackageContainerDefinition
  where
  toJSON ModelPackageContainerDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ImageDigest" Prelude..=) Prelude.<$> imageDigest,
            ("ModelDataUrl" Prelude..=) Prelude.<$> modelDataUrl,
            ("ContainerHostname" Prelude..=)
              Prelude.<$> containerHostname,
            ("ProductId" Prelude..=) Prelude.<$> productId,
            Prelude.Just ("Image" Prelude..= image)
          ]
      )
