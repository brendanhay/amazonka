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
    cdContainerHostname,
    cdEnvironment,
    cdImage,
    cdImageConfig,
    cdMode,
    cdModelDataUrl,
    cdModelPackageName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContainerHostname as Types
import qualified Network.AWS.SageMaker.Types.ContainerImage as Types
import qualified Network.AWS.SageMaker.Types.ContainerMode as Types
import qualified Network.AWS.SageMaker.Types.EnvironmentKey as Types
import qualified Network.AWS.SageMaker.Types.EnvironmentValue as Types
import qualified Network.AWS.SageMaker.Types.ImageConfig as Types
import qualified Network.AWS.SageMaker.Types.Url as Types
import qualified Network.AWS.SageMaker.Types.VersionedArnOrName as Types

-- | Describes the container, as part of model definition.
--
-- /See:/ 'mkContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { -- | This parameter is ignored for models that contain only a @PrimaryContainer@ .
    --
    -- When a @ContainerDefinition@ is part of an inference pipeline, the value of the parameter uniquely identifies the container for the purposes of logging and metrics. For information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline> . If you don't specify a value for this parameter for a @ContainerDefinition@ that is part of an inference pipeline, a unique name is automatically assigned based on the position of the @ContainerDefinition@ in the pipeline. If you specify a value for the @ContainerHostName@ for any @ContainerDefinition@ that is part of an inference pipeline, you must specify a value for the @ContainerHostName@ parameter of every @ContainerDefinition@ in that pipeline.
    containerHostname :: Core.Maybe Types.ContainerHostname,
    -- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
    environment :: Core.Maybe (Core.HashMap Types.EnvironmentKey Types.EnvironmentValue),
    -- | The path where inference code is stored. This can be either in Amazon EC2 Container Registry or in a Docker registry that is accessible from the same VPC that you configure for your endpoint. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
    image :: Core.Maybe Types.ContainerImage,
    -- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC). For information about storing containers in a private Docker registry, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
    imageConfig :: Core.Maybe Types.ImageConfig,
    -- | Whether the container hosts a single model or multiple models.
    mode :: Core.Maybe Types.ContainerMode,
    -- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in algorithms, but not if you use your own algorithms. For more information on built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters> .
    --
    -- If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
    -- /Important:/ If you use a built-in algorithm to create a model, Amazon SageMaker requires that you provide a S3 path to the model artifacts in @ModelDataUrl@ .
    modelDataUrl :: Core.Maybe Types.Url,
    -- | The name or Amazon Resource Name (ARN) of the model package to use to create the model.
    modelPackageName :: Core.Maybe Types.VersionedArnOrName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerDefinition' value with any optional fields omitted.
mkContainerDefinition ::
  ContainerDefinition
mkContainerDefinition =
  ContainerDefinition'
    { containerHostname = Core.Nothing,
      environment = Core.Nothing,
      image = Core.Nothing,
      imageConfig = Core.Nothing,
      mode = Core.Nothing,
      modelDataUrl = Core.Nothing,
      modelPackageName = Core.Nothing
    }

-- | This parameter is ignored for models that contain only a @PrimaryContainer@ .
--
-- When a @ContainerDefinition@ is part of an inference pipeline, the value of the parameter uniquely identifies the container for the purposes of logging and metrics. For information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/inference-pipeline-logs-metrics.html Use Logs and Metrics to Monitor an Inference Pipeline> . If you don't specify a value for this parameter for a @ContainerDefinition@ that is part of an inference pipeline, a unique name is automatically assigned based on the position of the @ContainerDefinition@ in the pipeline. If you specify a value for the @ContainerHostName@ for any @ContainerDefinition@ that is part of an inference pipeline, you must specify a value for the @ContainerHostName@ parameter of every @ContainerDefinition@ in that pipeline.
--
-- /Note:/ Consider using 'containerHostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerHostname :: Lens.Lens' ContainerDefinition (Core.Maybe Types.ContainerHostname)
cdContainerHostname = Lens.field @"containerHostname"
{-# DEPRECATED cdContainerHostname "Use generic-lens or generic-optics with 'containerHostname' instead." #-}

-- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironment :: Lens.Lens' ContainerDefinition (Core.Maybe (Core.HashMap Types.EnvironmentKey Types.EnvironmentValue))
cdEnvironment = Lens.field @"environment"
{-# DEPRECATED cdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The path where inference code is stored. This can be either in Amazon EC2 Container Registry or in a Docker registry that is accessible from the same VPC that you configure for your endpoint. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImage :: Lens.Lens' ContainerDefinition (Core.Maybe Types.ContainerImage)
cdImage = Lens.field @"image"
{-# DEPRECATED cdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC). For information about storing containers in a private Docker registry, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-containers-inference-private.html Use a Private Docker Registry for Real-Time Inference Containers>
--
-- /Note:/ Consider using 'imageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImageConfig :: Lens.Lens' ContainerDefinition (Core.Maybe Types.ImageConfig)
cdImageConfig = Lens.field @"imageConfig"
{-# DEPRECATED cdImageConfig "Use generic-lens or generic-optics with 'imageConfig' instead." #-}

-- | Whether the container hosts a single model or multiple models.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMode :: Lens.Lens' ContainerDefinition (Core.Maybe Types.ContainerMode)
cdMode = Lens.field @"mode"
{-# DEPRECATED cdMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix). The S3 path is required for Amazon SageMaker built-in algorithms, but not if you use your own algorithms. For more information on built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Common Parameters> .
--
-- If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- /Important:/ If you use a built-in algorithm to create a model, Amazon SageMaker requires that you provide a S3 path to the model artifacts in @ModelDataUrl@ .
--
-- /Note:/ Consider using 'modelDataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdModelDataUrl :: Lens.Lens' ContainerDefinition (Core.Maybe Types.Url)
cdModelDataUrl = Lens.field @"modelDataUrl"
{-# DEPRECATED cdModelDataUrl "Use generic-lens or generic-optics with 'modelDataUrl' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the model package to use to create the model.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdModelPackageName :: Lens.Lens' ContainerDefinition (Core.Maybe Types.VersionedArnOrName)
cdModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED cdModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

instance Core.FromJSON ContainerDefinition where
  toJSON ContainerDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContainerHostname" Core..=) Core.<$> containerHostname,
            ("Environment" Core..=) Core.<$> environment,
            ("Image" Core..=) Core.<$> image,
            ("ImageConfig" Core..=) Core.<$> imageConfig,
            ("Mode" Core..=) Core.<$> mode,
            ("ModelDataUrl" Core..=) Core.<$> modelDataUrl,
            ("ModelPackageName" Core..=) Core.<$> modelPackageName
          ]
      )

instance Core.FromJSON ContainerDefinition where
  parseJSON =
    Core.withObject "ContainerDefinition" Core.$
      \x ->
        ContainerDefinition'
          Core.<$> (x Core..:? "ContainerHostname")
          Core.<*> (x Core..:? "Environment")
          Core.<*> (x Core..:? "Image")
          Core.<*> (x Core..:? "ImageConfig")
          Core.<*> (x Core..:? "Mode")
          Core.<*> (x Core..:? "ModelDataUrl")
          Core.<*> (x Core..:? "ModelPackageName")
