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
-- Module      : Amazonka.SageMaker.Types.ModelPackageContainerDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageContainerDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelInput

-- | Describes the Docker container for the model package.
--
-- /See:/ 'newModelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { -- | The environment variables to set in the Docker container. Each key and
    -- value in the @Environment@ string to string map can have length of up to
    -- 1024. We support up to 16 entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The DNS host name for the Docker container.
    containerHostname :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path where the model artifacts, which result from model
    -- training, are stored. This path must point to a single @gzip@ compressed
    -- tar archive (@.tar.gz@ suffix).
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the model package.
    modelDataUrl :: Prelude.Maybe Prelude.Text,
    -- | A structure with Model Input details.
    modelInput :: Prelude.Maybe ModelInput,
    -- | The Amazon Web Services Marketplace product ID of the model package.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The name of a pre-trained machine learning benchmarked by Amazon
    -- SageMaker Inference Recommender model that matches your model. You can
    -- find a list of benchmarked models by calling @ListModelMetadata@.
    nearestModelName :: Prelude.Maybe Prelude.Text,
    -- | The framework version of the Model Package Container Image.
    frameworkVersion :: Prelude.Maybe Prelude.Text,
    -- | An MD5 hash of the training algorithm that identifies the Docker image
    -- used for training.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The machine learning framework of the model package container image.
    framework :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code
    -- is stored.
    --
    -- If you are using your own custom algorithm instead of an algorithm
    -- provided by SageMaker, the inference code must meet SageMaker
    -- requirements. SageMaker supports both @registry\/repository[:tag]@ and
    -- @registry\/repository[\@digest]@ image path formats. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    image :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageContainerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'modelPackageContainerDefinition_environment' - The environment variables to set in the Docker container. Each key and
-- value in the @Environment@ string to string map can have length of up to
-- 1024. We support up to 16 entries in the map.
--
-- 'containerHostname', 'modelPackageContainerDefinition_containerHostname' - The DNS host name for the Docker container.
--
-- 'modelDataUrl', 'modelPackageContainerDefinition_modelDataUrl' - The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model package.
--
-- 'modelInput', 'modelPackageContainerDefinition_modelInput' - A structure with Model Input details.
--
-- 'productId', 'modelPackageContainerDefinition_productId' - The Amazon Web Services Marketplace product ID of the model package.
--
-- 'nearestModelName', 'modelPackageContainerDefinition_nearestModelName' - The name of a pre-trained machine learning benchmarked by Amazon
-- SageMaker Inference Recommender model that matches your model. You can
-- find a list of benchmarked models by calling @ListModelMetadata@.
--
-- 'frameworkVersion', 'modelPackageContainerDefinition_frameworkVersion' - The framework version of the Model Package Container Image.
--
-- 'imageDigest', 'modelPackageContainerDefinition_imageDigest' - An MD5 hash of the training algorithm that identifies the Docker image
-- used for training.
--
-- 'framework', 'modelPackageContainerDefinition_framework' - The machine learning framework of the model package container image.
--
-- 'image', 'modelPackageContainerDefinition_image' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code
-- is stored.
--
-- If you are using your own custom algorithm instead of an algorithm
-- provided by SageMaker, the inference code must meet SageMaker
-- requirements. SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
newModelPackageContainerDefinition ::
  -- | 'image'
  Prelude.Text ->
  ModelPackageContainerDefinition
newModelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { environment =
        Prelude.Nothing,
      containerHostname = Prelude.Nothing,
      modelDataUrl = Prelude.Nothing,
      modelInput = Prelude.Nothing,
      productId = Prelude.Nothing,
      nearestModelName = Prelude.Nothing,
      frameworkVersion = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      framework = Prelude.Nothing,
      image = pImage_
    }

-- | The environment variables to set in the Docker container. Each key and
-- value in the @Environment@ string to string map can have length of up to
-- 1024. We support up to 16 entries in the map.
modelPackageContainerDefinition_environment :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelPackageContainerDefinition_environment = Lens.lens (\ModelPackageContainerDefinition' {environment} -> environment) (\s@ModelPackageContainerDefinition' {} a -> s {environment = a} :: ModelPackageContainerDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The DNS host name for the Docker container.
modelPackageContainerDefinition_containerHostname :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_containerHostname = Lens.lens (\ModelPackageContainerDefinition' {containerHostname} -> containerHostname) (\s@ModelPackageContainerDefinition' {} a -> s {containerHostname = a} :: ModelPackageContainerDefinition)

-- | The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model package.
modelPackageContainerDefinition_modelDataUrl :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_modelDataUrl = Lens.lens (\ModelPackageContainerDefinition' {modelDataUrl} -> modelDataUrl) (\s@ModelPackageContainerDefinition' {} a -> s {modelDataUrl = a} :: ModelPackageContainerDefinition)

-- | A structure with Model Input details.
modelPackageContainerDefinition_modelInput :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe ModelInput)
modelPackageContainerDefinition_modelInput = Lens.lens (\ModelPackageContainerDefinition' {modelInput} -> modelInput) (\s@ModelPackageContainerDefinition' {} a -> s {modelInput = a} :: ModelPackageContainerDefinition)

-- | The Amazon Web Services Marketplace product ID of the model package.
modelPackageContainerDefinition_productId :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_productId = Lens.lens (\ModelPackageContainerDefinition' {productId} -> productId) (\s@ModelPackageContainerDefinition' {} a -> s {productId = a} :: ModelPackageContainerDefinition)

-- | The name of a pre-trained machine learning benchmarked by Amazon
-- SageMaker Inference Recommender model that matches your model. You can
-- find a list of benchmarked models by calling @ListModelMetadata@.
modelPackageContainerDefinition_nearestModelName :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_nearestModelName = Lens.lens (\ModelPackageContainerDefinition' {nearestModelName} -> nearestModelName) (\s@ModelPackageContainerDefinition' {} a -> s {nearestModelName = a} :: ModelPackageContainerDefinition)

-- | The framework version of the Model Package Container Image.
modelPackageContainerDefinition_frameworkVersion :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_frameworkVersion = Lens.lens (\ModelPackageContainerDefinition' {frameworkVersion} -> frameworkVersion) (\s@ModelPackageContainerDefinition' {} a -> s {frameworkVersion = a} :: ModelPackageContainerDefinition)

-- | An MD5 hash of the training algorithm that identifies the Docker image
-- used for training.
modelPackageContainerDefinition_imageDigest :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_imageDigest = Lens.lens (\ModelPackageContainerDefinition' {imageDigest} -> imageDigest) (\s@ModelPackageContainerDefinition' {} a -> s {imageDigest = a} :: ModelPackageContainerDefinition)

-- | The machine learning framework of the model package container image.
modelPackageContainerDefinition_framework :: Lens.Lens' ModelPackageContainerDefinition (Prelude.Maybe Prelude.Text)
modelPackageContainerDefinition_framework = Lens.lens (\ModelPackageContainerDefinition' {framework} -> framework) (\s@ModelPackageContainerDefinition' {} a -> s {framework = a} :: ModelPackageContainerDefinition)

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code
-- is stored.
--
-- If you are using your own custom algorithm instead of an algorithm
-- provided by SageMaker, the inference code must meet SageMaker
-- requirements. SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
modelPackageContainerDefinition_image :: Lens.Lens' ModelPackageContainerDefinition Prelude.Text
modelPackageContainerDefinition_image = Lens.lens (\ModelPackageContainerDefinition' {image} -> image) (\s@ModelPackageContainerDefinition' {} a -> s {image = a} :: ModelPackageContainerDefinition)

instance
  Data.FromJSON
    ModelPackageContainerDefinition
  where
  parseJSON =
    Data.withObject
      "ModelPackageContainerDefinition"
      ( \x ->
          ModelPackageContainerDefinition'
            Prelude.<$> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ContainerHostname")
            Prelude.<*> (x Data..:? "ModelDataUrl")
            Prelude.<*> (x Data..:? "ModelInput")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "NearestModelName")
            Prelude.<*> (x Data..:? "FrameworkVersion")
            Prelude.<*> (x Data..:? "ImageDigest")
            Prelude.<*> (x Data..:? "Framework")
            Prelude.<*> (x Data..: "Image")
      )

instance
  Prelude.Hashable
    ModelPackageContainerDefinition
  where
  hashWithSalt
    _salt
    ModelPackageContainerDefinition' {..} =
      _salt `Prelude.hashWithSalt` environment
        `Prelude.hashWithSalt` containerHostname
        `Prelude.hashWithSalt` modelDataUrl
        `Prelude.hashWithSalt` modelInput
        `Prelude.hashWithSalt` productId
        `Prelude.hashWithSalt` nearestModelName
        `Prelude.hashWithSalt` frameworkVersion
        `Prelude.hashWithSalt` imageDigest
        `Prelude.hashWithSalt` framework
        `Prelude.hashWithSalt` image

instance
  Prelude.NFData
    ModelPackageContainerDefinition
  where
  rnf ModelPackageContainerDefinition' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf containerHostname
      `Prelude.seq` Prelude.rnf modelDataUrl
      `Prelude.seq` Prelude.rnf modelInput
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf nearestModelName
      `Prelude.seq` Prelude.rnf frameworkVersion
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf image

instance Data.ToJSON ModelPackageContainerDefinition where
  toJSON ModelPackageContainerDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Environment" Data..=) Prelude.<$> environment,
            ("ContainerHostname" Data..=)
              Prelude.<$> containerHostname,
            ("ModelDataUrl" Data..=) Prelude.<$> modelDataUrl,
            ("ModelInput" Data..=) Prelude.<$> modelInput,
            ("ProductId" Data..=) Prelude.<$> productId,
            ("NearestModelName" Data..=)
              Prelude.<$> nearestModelName,
            ("FrameworkVersion" Data..=)
              Prelude.<$> frameworkVersion,
            ("ImageDigest" Data..=) Prelude.<$> imageDigest,
            ("Framework" Data..=) Prelude.<$> framework,
            Prelude.Just ("Image" Data..= image)
          ]
      )
