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
-- Module      : Network.AWS.SageMaker.Types.ContainerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContainerDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ContainerMode
import Network.AWS.SageMaker.Types.ImageConfig
import Network.AWS.SageMaker.Types.MultiModelConfig

-- | Describes the container, as part of model definition.
--
-- /See:/ 'newContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { -- | Specifies additional configuration for multi-model endpoints.
    multiModelConfig :: Prelude.Maybe MultiModelConfig,
    -- | The S3 path where the model artifacts, which result from model training,
    -- are stored. This path must point to a single gzip compressed tar archive
    -- (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in
    -- algorithms, but not if you use your own algorithms. For more information
    -- on built-in algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the model or endpoint you are creating.
    --
    -- If you provide a value for this parameter, Amazon SageMaker uses AWS
    -- Security Token Service to download model artifacts from the S3 path you
    -- provide. AWS STS is activated in your IAM user account by default. If
    -- you previously deactivated AWS STS for a region, you need to reactivate
    -- AWS STS for that region. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region>
    -- in the /AWS Identity and Access Management User Guide/.
    --
    -- If you use a built-in algorithm to create a model, Amazon SageMaker
    -- requires that you provide a S3 path to the model artifacts in
    -- @ModelDataUrl@.
    modelDataUrl :: Prelude.Maybe Prelude.Text,
    -- | Whether the container hosts a single model or multiple models.
    mode :: Prelude.Maybe ContainerMode,
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
    -- | Specifies whether the model container is in Amazon ECR or a private
    -- Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
    -- For information about storing containers in a private Docker registry,
    -- see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | The environment variables to set in the Docker container. Each key and
    -- value in the @Environment@ string to string map can have length of up to
    -- 1024. We support up to 16 entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name or Amazon Resource Name (ARN) of the model package to use to
    -- create the model.
    modelPackageName :: Prelude.Maybe Prelude.Text,
    -- | The path where inference code is stored. This can be either in Amazon
    -- EC2 Container Registry or in a Docker registry that is accessible from
    -- the same VPC that you configure for your endpoint. If you are using your
    -- own custom algorithm instead of an algorithm provided by Amazon
    -- SageMaker, the inference code must meet Amazon SageMaker requirements.
    -- Amazon SageMaker supports both @registry\/repository[:tag]@ and
    -- @registry\/repository[\@digest]@ image path formats. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
    image :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiModelConfig', 'containerDefinition_multiModelConfig' - Specifies additional configuration for multi-model endpoints.
--
-- 'modelDataUrl', 'containerDefinition_modelDataUrl' - The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in
-- algorithms, but not if you use your own algorithms. For more information
-- on built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model or endpoint you are creating.
--
-- If you provide a value for this parameter, Amazon SageMaker uses AWS
-- Security Token Service to download model artifacts from the S3 path you
-- provide. AWS STS is activated in your IAM user account by default. If
-- you previously deactivated AWS STS for a region, you need to reactivate
-- AWS STS for that region. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region>
-- in the /AWS Identity and Access Management User Guide/.
--
-- If you use a built-in algorithm to create a model, Amazon SageMaker
-- requires that you provide a S3 path to the model artifacts in
-- @ModelDataUrl@.
--
-- 'mode', 'containerDefinition_mode' - Whether the container hosts a single model or multiple models.
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
-- 'modelPackageName', 'containerDefinition_modelPackageName' - The name or Amazon Resource Name (ARN) of the model package to use to
-- create the model.
--
-- 'image', 'containerDefinition_image' - The path where inference code is stored. This can be either in Amazon
-- EC2 Container Registry or in a Docker registry that is accessible from
-- the same VPC that you configure for your endpoint. If you are using your
-- own custom algorithm instead of an algorithm provided by Amazon
-- SageMaker, the inference code must meet Amazon SageMaker requirements.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
newContainerDefinition ::
  ContainerDefinition
newContainerDefinition =
  ContainerDefinition'
    { multiModelConfig =
        Prelude.Nothing,
      modelDataUrl = Prelude.Nothing,
      mode = Prelude.Nothing,
      containerHostname = Prelude.Nothing,
      imageConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      image = Prelude.Nothing
    }

-- | Specifies additional configuration for multi-model endpoints.
containerDefinition_multiModelConfig :: Lens.Lens' ContainerDefinition (Prelude.Maybe MultiModelConfig)
containerDefinition_multiModelConfig = Lens.lens (\ContainerDefinition' {multiModelConfig} -> multiModelConfig) (\s@ContainerDefinition' {} a -> s {multiModelConfig = a} :: ContainerDefinition)

-- | The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in
-- algorithms, but not if you use your own algorithms. For more information
-- on built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters>.
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the model or endpoint you are creating.
--
-- If you provide a value for this parameter, Amazon SageMaker uses AWS
-- Security Token Service to download model artifacts from the S3 path you
-- provide. AWS STS is activated in your IAM user account by default. If
-- you previously deactivated AWS STS for a region, you need to reactivate
-- AWS STS for that region. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region>
-- in the /AWS Identity and Access Management User Guide/.
--
-- If you use a built-in algorithm to create a model, Amazon SageMaker
-- requires that you provide a S3 path to the model artifacts in
-- @ModelDataUrl@.
containerDefinition_modelDataUrl :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_modelDataUrl = Lens.lens (\ContainerDefinition' {modelDataUrl} -> modelDataUrl) (\s@ContainerDefinition' {} a -> s {modelDataUrl = a} :: ContainerDefinition)

-- | Whether the container hosts a single model or multiple models.
containerDefinition_mode :: Lens.Lens' ContainerDefinition (Prelude.Maybe ContainerMode)
containerDefinition_mode = Lens.lens (\ContainerDefinition' {mode} -> mode) (\s@ContainerDefinition' {} a -> s {mode = a} :: ContainerDefinition)

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
containerDefinition_environment = Lens.lens (\ContainerDefinition' {environment} -> environment) (\s@ContainerDefinition' {} a -> s {environment = a} :: ContainerDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or Amazon Resource Name (ARN) of the model package to use to
-- create the model.
containerDefinition_modelPackageName :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_modelPackageName = Lens.lens (\ContainerDefinition' {modelPackageName} -> modelPackageName) (\s@ContainerDefinition' {} a -> s {modelPackageName = a} :: ContainerDefinition)

-- | The path where inference code is stored. This can be either in Amazon
-- EC2 Container Registry or in a Docker registry that is accessible from
-- the same VPC that you configure for your endpoint. If you are using your
-- own custom algorithm instead of an algorithm provided by Amazon
-- SageMaker, the inference code must meet Amazon SageMaker requirements.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
containerDefinition_image :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_image = Lens.lens (\ContainerDefinition' {image} -> image) (\s@ContainerDefinition' {} a -> s {image = a} :: ContainerDefinition)

instance Prelude.FromJSON ContainerDefinition where
  parseJSON =
    Prelude.withObject
      "ContainerDefinition"
      ( \x ->
          ContainerDefinition'
            Prelude.<$> (x Prelude..:? "MultiModelConfig")
            Prelude.<*> (x Prelude..:? "ModelDataUrl")
            Prelude.<*> (x Prelude..:? "Mode")
            Prelude.<*> (x Prelude..:? "ContainerHostname")
            Prelude.<*> (x Prelude..:? "ImageConfig")
            Prelude.<*> ( x Prelude..:? "Environment"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ModelPackageName")
            Prelude.<*> (x Prelude..:? "Image")
      )

instance Prelude.Hashable ContainerDefinition

instance Prelude.NFData ContainerDefinition

instance Prelude.ToJSON ContainerDefinition where
  toJSON ContainerDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MultiModelConfig" Prelude..=)
              Prelude.<$> multiModelConfig,
            ("ModelDataUrl" Prelude..=) Prelude.<$> modelDataUrl,
            ("Mode" Prelude..=) Prelude.<$> mode,
            ("ContainerHostname" Prelude..=)
              Prelude.<$> containerHostname,
            ("ImageConfig" Prelude..=) Prelude.<$> imageConfig,
            ("Environment" Prelude..=) Prelude.<$> environment,
            ("ModelPackageName" Prelude..=)
              Prelude.<$> modelPackageName,
            ("Image" Prelude..=) Prelude.<$> image
          ]
      )
