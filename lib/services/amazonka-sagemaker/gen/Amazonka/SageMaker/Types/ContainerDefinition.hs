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
-- Module      : Amazonka.SageMaker.Types.ContainerDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContainerDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ContainerMode
import Amazonka.SageMaker.Types.ImageConfig
import Amazonka.SageMaker.Types.MultiModelConfig

-- | Describes the container, as part of model definition.
--
-- /See:/ 'newContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { -- | Specifies whether the model container is in Amazon ECR or a private
    -- Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
    -- For information about storing containers in a private Docker registry,
    -- see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | The environment variables to set in the Docker container. Each key and
    -- value in the @Environment@ string to string map can have length of up to
    -- 1024. We support up to 16 entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This parameter is ignored for models that contain only a
    -- @PrimaryContainer@.
    --
    -- When a @ContainerDefinition@ is part of an inference pipeline, the value
    -- of the parameter uniquely identifies the container for the purposes of
    -- logging and metrics. For information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline>.
    -- If you don\'t specify a value for this parameter for a
    -- @ContainerDefinition@ that is part of an inference pipeline, a unique
    -- name is automatically assigned based on the position of the
    -- @ContainerDefinition@ in the pipeline. If you specify a value for the
    -- @ContainerHostName@ for any @ContainerDefinition@ that is part of an
    -- inference pipeline, you must specify a value for the @ContainerHostName@
    -- parameter of every @ContainerDefinition@ in that pipeline.
    containerHostname :: Prelude.Maybe Prelude.Text,
    -- | The S3 path where the model artifacts, which result from model training,
    -- are stored. This path must point to a single gzip compressed tar archive
    -- (.tar.gz suffix). The S3 path is required for SageMaker built-in
    -- algorithms, but not if you use your own algorithms. For more information
    -- on built-in algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the model or endpoint you are creating.
    --
    -- If you provide a value for this parameter, SageMaker uses Amazon Web
    -- Services Security Token Service to download model artifacts from the S3
    -- path you provide. Amazon Web Services STS is activated in your IAM user
    -- account by default. If you previously deactivated Amazon Web Services
    -- STS for a region, you need to reactivate Amazon Web Services STS for
    -- that region. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating Amazon Web Services STS in an Amazon Web Services Region>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    --
    -- If you use a built-in algorithm to create a model, SageMaker requires
    -- that you provide a S3 path to the model artifacts in @ModelDataUrl@.
    modelDataUrl :: Prelude.Maybe Prelude.Text,
    -- | Specifies additional configuration for multi-model endpoints.
    multiModelConfig :: Prelude.Maybe MultiModelConfig,
    -- | The inference specification name in the model package version.
    inferenceSpecificationName :: Prelude.Maybe Prelude.Text,
    -- | Whether the container hosts a single model or multiple models.
    mode :: Prelude.Maybe ContainerMode,
    -- | The path where inference code is stored. This can be either in Amazon
    -- EC2 Container Registry or in a Docker registry that is accessible from
    -- the same VPC that you configure for your endpoint. If you are using your
    -- own custom algorithm instead of an algorithm provided by SageMaker, the
    -- inference code must meet SageMaker requirements. SageMaker supports both
    -- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
    -- path formats. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
    image :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the model package to use to
    -- create the model.
    modelPackageName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfig', 'containerDefinition_imageConfig' - Specifies whether the model container is in Amazon ECR or a private
-- Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
-- For information about storing containers in a private Docker registry,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
--
-- 'environment', 'containerDefinition_environment' - The environment variables to set in the Docker container. Each key and
-- value in the @Environment@ string to string map can have length of up to
-- 1024. We support up to 16 entries in the map.
--
-- 'containerHostname', 'containerDefinition_containerHostname' - This parameter is ignored for models that contain only a
-- @PrimaryContainer@.
--
-- When a @ContainerDefinition@ is part of an inference pipeline, the value
-- of the parameter uniquely identifies the container for the purposes of
-- logging and metrics. For information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline>.
-- If you don\'t specify a value for this parameter for a
-- @ContainerDefinition@ that is part of an inference pipeline, a unique
-- name is automatically assigned based on the position of the
-- @ContainerDefinition@ in the pipeline. If you specify a value for the
-- @ContainerHostName@ for any @ContainerDefinition@ that is part of an
-- inference pipeline, you must specify a value for the @ContainerHostName@
-- parameter of every @ContainerDefinition@ in that pipeline.
--
-- 'modelDataUrl', 'containerDefinition_modelDataUrl' - The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix). The S3 path is required for SageMaker built-in
-- algorithms, but not if you use your own algorithms. For more information
-- on built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model or endpoint you are creating.
--
-- If you provide a value for this parameter, SageMaker uses Amazon Web
-- Services Security Token Service to download model artifacts from the S3
-- path you provide. Amazon Web Services STS is activated in your IAM user
-- account by default. If you previously deactivated Amazon Web Services
-- STS for a region, you need to reactivate Amazon Web Services STS for
-- that region. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- If you use a built-in algorithm to create a model, SageMaker requires
-- that you provide a S3 path to the model artifacts in @ModelDataUrl@.
--
-- 'multiModelConfig', 'containerDefinition_multiModelConfig' - Specifies additional configuration for multi-model endpoints.
--
-- 'inferenceSpecificationName', 'containerDefinition_inferenceSpecificationName' - The inference specification name in the model package version.
--
-- 'mode', 'containerDefinition_mode' - Whether the container hosts a single model or multiple models.
--
-- 'image', 'containerDefinition_image' - The path where inference code is stored. This can be either in Amazon
-- EC2 Container Registry or in a Docker registry that is accessible from
-- the same VPC that you configure for your endpoint. If you are using your
-- own custom algorithm instead of an algorithm provided by SageMaker, the
-- inference code must meet SageMaker requirements. SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
--
-- 'modelPackageName', 'containerDefinition_modelPackageName' - The name or Amazon Resource Name (ARN) of the model package to use to
-- create the model.
newContainerDefinition ::
  ContainerDefinition
newContainerDefinition =
  ContainerDefinition'
    { imageConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      containerHostname = Prelude.Nothing,
      modelDataUrl = Prelude.Nothing,
      multiModelConfig = Prelude.Nothing,
      inferenceSpecificationName = Prelude.Nothing,
      mode = Prelude.Nothing,
      image = Prelude.Nothing,
      modelPackageName = Prelude.Nothing
    }

-- | Specifies whether the model container is in Amazon ECR or a private
-- Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
-- For information about storing containers in a private Docker registry,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
containerDefinition_imageConfig :: Lens.Lens' ContainerDefinition (Prelude.Maybe ImageConfig)
containerDefinition_imageConfig = Lens.lens (\ContainerDefinition' {imageConfig} -> imageConfig) (\s@ContainerDefinition' {} a -> s {imageConfig = a} :: ContainerDefinition)

-- | The environment variables to set in the Docker container. Each key and
-- value in the @Environment@ string to string map can have length of up to
-- 1024. We support up to 16 entries in the map.
containerDefinition_environment :: Lens.Lens' ContainerDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
containerDefinition_environment = Lens.lens (\ContainerDefinition' {environment} -> environment) (\s@ContainerDefinition' {} a -> s {environment = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens.coerced

-- | This parameter is ignored for models that contain only a
-- @PrimaryContainer@.
--
-- When a @ContainerDefinition@ is part of an inference pipeline, the value
-- of the parameter uniquely identifies the container for the purposes of
-- logging and metrics. For information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline>.
-- If you don\'t specify a value for this parameter for a
-- @ContainerDefinition@ that is part of an inference pipeline, a unique
-- name is automatically assigned based on the position of the
-- @ContainerDefinition@ in the pipeline. If you specify a value for the
-- @ContainerHostName@ for any @ContainerDefinition@ that is part of an
-- inference pipeline, you must specify a value for the @ContainerHostName@
-- parameter of every @ContainerDefinition@ in that pipeline.
containerDefinition_containerHostname :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_containerHostname = Lens.lens (\ContainerDefinition' {containerHostname} -> containerHostname) (\s@ContainerDefinition' {} a -> s {containerHostname = a} :: ContainerDefinition)

-- | The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix). The S3 path is required for SageMaker built-in
-- algorithms, but not if you use your own algorithms. For more information
-- on built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model or endpoint you are creating.
--
-- If you provide a value for this parameter, SageMaker uses Amazon Web
-- Services Security Token Service to download model artifacts from the S3
-- path you provide. Amazon Web Services STS is activated in your IAM user
-- account by default. If you previously deactivated Amazon Web Services
-- STS for a region, you need to reactivate Amazon Web Services STS for
-- that region. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- If you use a built-in algorithm to create a model, SageMaker requires
-- that you provide a S3 path to the model artifacts in @ModelDataUrl@.
containerDefinition_modelDataUrl :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_modelDataUrl = Lens.lens (\ContainerDefinition' {modelDataUrl} -> modelDataUrl) (\s@ContainerDefinition' {} a -> s {modelDataUrl = a} :: ContainerDefinition)

-- | Specifies additional configuration for multi-model endpoints.
containerDefinition_multiModelConfig :: Lens.Lens' ContainerDefinition (Prelude.Maybe MultiModelConfig)
containerDefinition_multiModelConfig = Lens.lens (\ContainerDefinition' {multiModelConfig} -> multiModelConfig) (\s@ContainerDefinition' {} a -> s {multiModelConfig = a} :: ContainerDefinition)

-- | The inference specification name in the model package version.
containerDefinition_inferenceSpecificationName :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_inferenceSpecificationName = Lens.lens (\ContainerDefinition' {inferenceSpecificationName} -> inferenceSpecificationName) (\s@ContainerDefinition' {} a -> s {inferenceSpecificationName = a} :: ContainerDefinition)

-- | Whether the container hosts a single model or multiple models.
containerDefinition_mode :: Lens.Lens' ContainerDefinition (Prelude.Maybe ContainerMode)
containerDefinition_mode = Lens.lens (\ContainerDefinition' {mode} -> mode) (\s@ContainerDefinition' {} a -> s {mode = a} :: ContainerDefinition)

-- | The path where inference code is stored. This can be either in Amazon
-- EC2 Container Registry or in a Docker registry that is accessible from
-- the same VPC that you configure for your endpoint. If you are using your
-- own custom algorithm instead of an algorithm provided by SageMaker, the
-- inference code must meet SageMaker requirements. SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
containerDefinition_image :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_image = Lens.lens (\ContainerDefinition' {image} -> image) (\s@ContainerDefinition' {} a -> s {image = a} :: ContainerDefinition)

-- | The name or Amazon Resource Name (ARN) of the model package to use to
-- create the model.
containerDefinition_modelPackageName :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_modelPackageName = Lens.lens (\ContainerDefinition' {modelPackageName} -> modelPackageName) (\s@ContainerDefinition' {} a -> s {modelPackageName = a} :: ContainerDefinition)

instance Data.FromJSON ContainerDefinition where
  parseJSON =
    Data.withObject
      "ContainerDefinition"
      ( \x ->
          ContainerDefinition'
            Prelude.<$> (x Data..:? "ImageConfig")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ContainerHostname")
            Prelude.<*> (x Data..:? "ModelDataUrl")
            Prelude.<*> (x Data..:? "MultiModelConfig")
            Prelude.<*> (x Data..:? "InferenceSpecificationName")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "Image")
            Prelude.<*> (x Data..:? "ModelPackageName")
      )

instance Prelude.Hashable ContainerDefinition where
  hashWithSalt _salt ContainerDefinition' {..} =
    _salt `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` containerHostname
      `Prelude.hashWithSalt` modelDataUrl
      `Prelude.hashWithSalt` multiModelConfig
      `Prelude.hashWithSalt` inferenceSpecificationName
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` modelPackageName

instance Prelude.NFData ContainerDefinition where
  rnf ContainerDefinition' {..} =
    Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf containerHostname
      `Prelude.seq` Prelude.rnf modelDataUrl
      `Prelude.seq` Prelude.rnf multiModelConfig
      `Prelude.seq` Prelude.rnf inferenceSpecificationName
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf modelPackageName

instance Data.ToJSON ContainerDefinition where
  toJSON ContainerDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImageConfig" Data..=) Prelude.<$> imageConfig,
            ("Environment" Data..=) Prelude.<$> environment,
            ("ContainerHostname" Data..=)
              Prelude.<$> containerHostname,
            ("ModelDataUrl" Data..=) Prelude.<$> modelDataUrl,
            ("MultiModelConfig" Data..=)
              Prelude.<$> multiModelConfig,
            ("InferenceSpecificationName" Data..=)
              Prelude.<$> inferenceSpecificationName,
            ("Mode" Data..=) Prelude.<$> mode,
            ("Image" Data..=) Prelude.<$> image,
            ("ModelPackageName" Data..=)
              Prelude.<$> modelPackageName
          ]
      )
