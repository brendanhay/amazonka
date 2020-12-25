{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDataContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDataContainer
  ( ResourceDataContainer (..),

    -- * Smart constructor
    mkResourceDataContainer,

    -- * Lenses
    rdcLocalDeviceResourceData,
    rdcLocalVolumeResourceData,
    rdcS3MachineLearningModelResourceData,
    rdcSageMakerMachineLearningModelResourceData,
    rdcSecretsManagerSecretResourceData,
  )
where

import qualified Network.AWS.Greengrass.Types.LocalDeviceResourceData as Types
import qualified Network.AWS.Greengrass.Types.LocalVolumeResourceData as Types
import qualified Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData as Types
import qualified Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData as Types
import qualified Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for resource data. The container takes only one of the following supported resource data types: ''LocalDeviceResourceData'', ''LocalVolumeResourceData'', ''SageMakerMachineLearningModelResourceData'', ''S3MachineLearningModelResourceData'', ''SecretsManagerSecretResourceData''.
--
-- /See:/ 'mkResourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { -- | Attributes that define the local device resource.
    localDeviceResourceData :: Core.Maybe Types.LocalDeviceResourceData,
    -- | Attributes that define the local volume resource.
    localVolumeResourceData :: Core.Maybe Types.LocalVolumeResourceData,
    -- | Attributes that define an Amazon S3 machine learning resource.
    s3MachineLearningModelResourceData :: Core.Maybe Types.S3MachineLearningModelResourceData,
    -- | Attributes that define an Amazon SageMaker machine learning resource.
    sageMakerMachineLearningModelResourceData :: Core.Maybe Types.SageMakerMachineLearningModelResourceData,
    -- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
    secretsManagerSecretResourceData :: Core.Maybe Types.SecretsManagerSecretResourceData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataContainer' value with any optional fields omitted.
mkResourceDataContainer ::
  ResourceDataContainer
mkResourceDataContainer =
  ResourceDataContainer'
    { localDeviceResourceData = Core.Nothing,
      localVolumeResourceData = Core.Nothing,
      s3MachineLearningModelResourceData = Core.Nothing,
      sageMakerMachineLearningModelResourceData = Core.Nothing,
      secretsManagerSecretResourceData = Core.Nothing
    }

-- | Attributes that define the local device resource.
--
-- /Note:/ Consider using 'localDeviceResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcLocalDeviceResourceData :: Lens.Lens' ResourceDataContainer (Core.Maybe Types.LocalDeviceResourceData)
rdcLocalDeviceResourceData = Lens.field @"localDeviceResourceData"
{-# DEPRECATED rdcLocalDeviceResourceData "Use generic-lens or generic-optics with 'localDeviceResourceData' instead." #-}

-- | Attributes that define the local volume resource.
--
-- /Note:/ Consider using 'localVolumeResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcLocalVolumeResourceData :: Lens.Lens' ResourceDataContainer (Core.Maybe Types.LocalVolumeResourceData)
rdcLocalVolumeResourceData = Lens.field @"localVolumeResourceData"
{-# DEPRECATED rdcLocalVolumeResourceData "Use generic-lens or generic-optics with 'localVolumeResourceData' instead." #-}

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /Note:/ Consider using 's3MachineLearningModelResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3MachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Core.Maybe Types.S3MachineLearningModelResourceData)
rdcS3MachineLearningModelResourceData = Lens.field @"s3MachineLearningModelResourceData"
{-# DEPRECATED rdcS3MachineLearningModelResourceData "Use generic-lens or generic-optics with 's3MachineLearningModelResourceData' instead." #-}

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /Note:/ Consider using 'sageMakerMachineLearningModelResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcSageMakerMachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Core.Maybe Types.SageMakerMachineLearningModelResourceData)
rdcSageMakerMachineLearningModelResourceData = Lens.field @"sageMakerMachineLearningModelResourceData"
{-# DEPRECATED rdcSageMakerMachineLearningModelResourceData "Use generic-lens or generic-optics with 'sageMakerMachineLearningModelResourceData' instead." #-}

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
--
-- /Note:/ Consider using 'secretsManagerSecretResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcSecretsManagerSecretResourceData :: Lens.Lens' ResourceDataContainer (Core.Maybe Types.SecretsManagerSecretResourceData)
rdcSecretsManagerSecretResourceData = Lens.field @"secretsManagerSecretResourceData"
{-# DEPRECATED rdcSecretsManagerSecretResourceData "Use generic-lens or generic-optics with 'secretsManagerSecretResourceData' instead." #-}

instance Core.FromJSON ResourceDataContainer where
  toJSON ResourceDataContainer {..} =
    Core.object
      ( Core.catMaybes
          [ ("LocalDeviceResourceData" Core..=)
              Core.<$> localDeviceResourceData,
            ("LocalVolumeResourceData" Core..=)
              Core.<$> localVolumeResourceData,
            ("S3MachineLearningModelResourceData" Core..=)
              Core.<$> s3MachineLearningModelResourceData,
            ("SageMakerMachineLearningModelResourceData" Core..=)
              Core.<$> sageMakerMachineLearningModelResourceData,
            ("SecretsManagerSecretResourceData" Core..=)
              Core.<$> secretsManagerSecretResourceData
          ]
      )

instance Core.FromJSON ResourceDataContainer where
  parseJSON =
    Core.withObject "ResourceDataContainer" Core.$
      \x ->
        ResourceDataContainer'
          Core.<$> (x Core..:? "LocalDeviceResourceData")
          Core.<*> (x Core..:? "LocalVolumeResourceData")
          Core.<*> (x Core..:? "S3MachineLearningModelResourceData")
          Core.<*> (x Core..:? "SageMakerMachineLearningModelResourceData")
          Core.<*> (x Core..:? "SecretsManagerSecretResourceData")
