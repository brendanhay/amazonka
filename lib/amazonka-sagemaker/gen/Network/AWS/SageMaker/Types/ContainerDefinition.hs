{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContainerDefinition
  ( ContainerDefinition (..),

    -- * Smart constructor
    mkContainerDefinition,

    -- * Lenses
    cdModelDataURL,
    cdImage,
    cdModelPackageName,
    cdEnvironment,
    cdImageConfig,
    cdMode,
    cdContainerHostname,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ContainerMode
import Network.AWS.SageMaker.Types.ImageConfig

-- | Describes the container, as part of model definition.
--
-- /See:/ 'mkContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { -- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in algorithms, but not if you use your own algorithms. For more information on built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters> .
    --
    -- If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
    -- /Important:/ If you use a built-in algorithm to create a model, Amazon SageMaker requires that you provide a S3 path to the model artifacts in @ModelDataUrl@ .
    modelDataURL :: Lude.Maybe Lude.Text,
    -- | The path where inference code is stored. This can be either in Amazon EC2 Container Registry or in a Docker registry that is accessible from the same VPC that you configure for your endpoint. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
    image :: Lude.Maybe Lude.Text,
    -- | The name or Amazon Resource Name (ARN) of the model package to use to create the model.
    modelPackageName :: Lude.Maybe Lude.Text,
    -- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC). For information about storing containers in a private Docker registry, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
    imageConfig :: Lude.Maybe ImageConfig,
    -- | Whether the container hosts a single model or multiple models.
    mode :: Lude.Maybe ContainerMode,
    -- | This parameter is ignored for models that contain only a @PrimaryContainer@ .
    --
    -- When a @ContainerDefinition@ is part of an inference pipeline, the value of the parameter uniquely identifies the container for the purposes of logging and metrics. For information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline> . If you don't specify a value for this parameter for a @ContainerDefinition@ that is part of an inference pipeline, a unique name is automatically assigned based on the position of the @ContainerDefinition@ in the pipeline. If you specify a value for the @ContainerHostName@ for any @ContainerDefinition@ that is part of an inference pipeline, you must specify a value for the @ContainerHostName@ parameter of every @ContainerDefinition@ in that pipeline.
    containerHostname :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- * 'modelDataURL' - The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in algorithms, but not if you use your own algorithms. For more information on built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters> .
--
-- If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- /Important:/ If you use a built-in algorithm to create a model, Amazon SageMaker requires that you provide a S3 path to the model artifacts in @ModelDataUrl@ .
-- * 'image' - The path where inference code is stored. This can be either in Amazon EC2 Container Registry or in a Docker registry that is accessible from the same VPC that you configure for your endpoint. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
-- * 'modelPackageName' - The name or Amazon Resource Name (ARN) of the model package to use to create the model.
-- * 'environment' - The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
-- * 'imageConfig' - Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC). For information about storing containers in a private Docker registry, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
-- * 'mode' - Whether the container hosts a single model or multiple models.
-- * 'containerHostname' - This parameter is ignored for models that contain only a @PrimaryContainer@ .
--
-- When a @ContainerDefinition@ is part of an inference pipeline, the value of the parameter uniquely identifies the container for the purposes of logging and metrics. For information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline> . If you don't specify a value for this parameter for a @ContainerDefinition@ that is part of an inference pipeline, a unique name is automatically assigned based on the position of the @ContainerDefinition@ in the pipeline. If you specify a value for the @ContainerHostName@ for any @ContainerDefinition@ that is part of an inference pipeline, you must specify a value for the @ContainerHostName@ parameter of every @ContainerDefinition@ in that pipeline.
mkContainerDefinition ::
  ContainerDefinition
mkContainerDefinition =
  ContainerDefinition'
    { modelDataURL = Lude.Nothing,
      image = Lude.Nothing,
      modelPackageName = Lude.Nothing,
      environment = Lude.Nothing,
      imageConfig = Lude.Nothing,
      mode = Lude.Nothing,
      containerHostname = Lude.Nothing
    }

-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in algorithms, but not if you use your own algorithms. For more information on built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters> .
--
-- If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- /Important:/ If you use a built-in algorithm to create a model, Amazon SageMaker requires that you provide a S3 path to the model artifacts in @ModelDataUrl@ .
--
-- /Note:/ Consider using 'modelDataURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdModelDataURL :: Lens.Lens' ContainerDefinition (Lude.Maybe Lude.Text)
cdModelDataURL = Lens.lens (modelDataURL :: ContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {modelDataURL = a} :: ContainerDefinition)
{-# DEPRECATED cdModelDataURL "Use generic-lens or generic-optics with 'modelDataURL' instead." #-}

-- | The path where inference code is stored. This can be either in Amazon EC2 Container Registry or in a Docker registry that is accessible from the same VPC that you configure for your endpoint. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImage :: Lens.Lens' ContainerDefinition (Lude.Maybe Lude.Text)
cdImage = Lens.lens (image :: ContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: ContainerDefinition)
{-# DEPRECATED cdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the model package to use to create the model.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdModelPackageName :: Lens.Lens' ContainerDefinition (Lude.Maybe Lude.Text)
cdModelPackageName = Lens.lens (modelPackageName :: ContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {modelPackageName = a} :: ContainerDefinition)
{-# DEPRECATED cdModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironment :: Lens.Lens' ContainerDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdEnvironment = Lens.lens (environment :: ContainerDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: ContainerDefinition)
{-# DEPRECATED cdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC). For information about storing containers in a private Docker registry, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
--
-- /Note:/ Consider using 'imageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImageConfig :: Lens.Lens' ContainerDefinition (Lude.Maybe ImageConfig)
cdImageConfig = Lens.lens (imageConfig :: ContainerDefinition -> Lude.Maybe ImageConfig) (\s a -> s {imageConfig = a} :: ContainerDefinition)
{-# DEPRECATED cdImageConfig "Use generic-lens or generic-optics with 'imageConfig' instead." #-}

-- | Whether the container hosts a single model or multiple models.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMode :: Lens.Lens' ContainerDefinition (Lude.Maybe ContainerMode)
cdMode = Lens.lens (mode :: ContainerDefinition -> Lude.Maybe ContainerMode) (\s a -> s {mode = a} :: ContainerDefinition)
{-# DEPRECATED cdMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | This parameter is ignored for models that contain only a @PrimaryContainer@ .
--
-- When a @ContainerDefinition@ is part of an inference pipeline, the value of the parameter uniquely identifies the container for the purposes of logging and metrics. For information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline> . If you don't specify a value for this parameter for a @ContainerDefinition@ that is part of an inference pipeline, a unique name is automatically assigned based on the position of the @ContainerDefinition@ in the pipeline. If you specify a value for the @ContainerHostName@ for any @ContainerDefinition@ that is part of an inference pipeline, you must specify a value for the @ContainerHostName@ parameter of every @ContainerDefinition@ in that pipeline.
--
-- /Note:/ Consider using 'containerHostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerHostname :: Lens.Lens' ContainerDefinition (Lude.Maybe Lude.Text)
cdContainerHostname = Lens.lens (containerHostname :: ContainerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {containerHostname = a} :: ContainerDefinition)
{-# DEPRECATED cdContainerHostname "Use generic-lens or generic-optics with 'containerHostname' instead." #-}

instance Lude.FromJSON ContainerDefinition where
  parseJSON =
    Lude.withObject
      "ContainerDefinition"
      ( \x ->
          ContainerDefinition'
            Lude.<$> (x Lude..:? "ModelDataUrl")
            Lude.<*> (x Lude..:? "Image")
            Lude.<*> (x Lude..:? "ModelPackageName")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ImageConfig")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "ContainerHostname")
      )

instance Lude.ToJSON ContainerDefinition where
  toJSON ContainerDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ModelDataUrl" Lude..=) Lude.<$> modelDataURL,
            ("Image" Lude..=) Lude.<$> image,
            ("ModelPackageName" Lude..=) Lude.<$> modelPackageName,
            ("Environment" Lude..=) Lude.<$> environment,
            ("ImageConfig" Lude..=) Lude.<$> imageConfig,
            ("Mode" Lude..=) Lude.<$> mode,
            ("ContainerHostname" Lude..=) Lude.<$> containerHostname
          ]
      )
