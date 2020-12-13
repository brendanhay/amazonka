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
    rdcS3MachineLearningModelResourceData,
    rdcSageMakerMachineLearningModelResourceData,
    rdcLocalVolumeResourceData,
    rdcLocalDeviceResourceData,
    rdcSecretsManagerSecretResourceData,
  )
where

import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for resource data. The container takes only one of the following supported resource data types: ''LocalDeviceResourceData'', ''LocalVolumeResourceData'', ''SageMakerMachineLearningModelResourceData'', ''S3MachineLearningModelResourceData'', ''SecretsManagerSecretResourceData''.
--
-- /See:/ 'mkResourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { -- | Attributes that define an Amazon S3 machine learning resource.
    s3MachineLearningModelResourceData :: Lude.Maybe S3MachineLearningModelResourceData,
    -- | Attributes that define an Amazon SageMaker machine learning resource.
    sageMakerMachineLearningModelResourceData :: Lude.Maybe SageMakerMachineLearningModelResourceData,
    -- | Attributes that define the local volume resource.
    localVolumeResourceData :: Lude.Maybe LocalVolumeResourceData,
    -- | Attributes that define the local device resource.
    localDeviceResourceData :: Lude.Maybe LocalDeviceResourceData,
    -- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
    secretsManagerSecretResourceData :: Lude.Maybe SecretsManagerSecretResourceData
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataContainer' with the minimum fields required to make a request.
--
-- * 's3MachineLearningModelResourceData' - Attributes that define an Amazon S3 machine learning resource.
-- * 'sageMakerMachineLearningModelResourceData' - Attributes that define an Amazon SageMaker machine learning resource.
-- * 'localVolumeResourceData' - Attributes that define the local volume resource.
-- * 'localDeviceResourceData' - Attributes that define the local device resource.
-- * 'secretsManagerSecretResourceData' - Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
mkResourceDataContainer ::
  ResourceDataContainer
mkResourceDataContainer =
  ResourceDataContainer'
    { s3MachineLearningModelResourceData =
        Lude.Nothing,
      sageMakerMachineLearningModelResourceData = Lude.Nothing,
      localVolumeResourceData = Lude.Nothing,
      localDeviceResourceData = Lude.Nothing,
      secretsManagerSecretResourceData = Lude.Nothing
    }

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /Note:/ Consider using 's3MachineLearningModelResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3MachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Lude.Maybe S3MachineLearningModelResourceData)
rdcS3MachineLearningModelResourceData = Lens.lens (s3MachineLearningModelResourceData :: ResourceDataContainer -> Lude.Maybe S3MachineLearningModelResourceData) (\s a -> s {s3MachineLearningModelResourceData = a} :: ResourceDataContainer)
{-# DEPRECATED rdcS3MachineLearningModelResourceData "Use generic-lens or generic-optics with 's3MachineLearningModelResourceData' instead." #-}

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /Note:/ Consider using 'sageMakerMachineLearningModelResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcSageMakerMachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Lude.Maybe SageMakerMachineLearningModelResourceData)
rdcSageMakerMachineLearningModelResourceData = Lens.lens (sageMakerMachineLearningModelResourceData :: ResourceDataContainer -> Lude.Maybe SageMakerMachineLearningModelResourceData) (\s a -> s {sageMakerMachineLearningModelResourceData = a} :: ResourceDataContainer)
{-# DEPRECATED rdcSageMakerMachineLearningModelResourceData "Use generic-lens or generic-optics with 'sageMakerMachineLearningModelResourceData' instead." #-}

-- | Attributes that define the local volume resource.
--
-- /Note:/ Consider using 'localVolumeResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcLocalVolumeResourceData :: Lens.Lens' ResourceDataContainer (Lude.Maybe LocalVolumeResourceData)
rdcLocalVolumeResourceData = Lens.lens (localVolumeResourceData :: ResourceDataContainer -> Lude.Maybe LocalVolumeResourceData) (\s a -> s {localVolumeResourceData = a} :: ResourceDataContainer)
{-# DEPRECATED rdcLocalVolumeResourceData "Use generic-lens or generic-optics with 'localVolumeResourceData' instead." #-}

-- | Attributes that define the local device resource.
--
-- /Note:/ Consider using 'localDeviceResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcLocalDeviceResourceData :: Lens.Lens' ResourceDataContainer (Lude.Maybe LocalDeviceResourceData)
rdcLocalDeviceResourceData = Lens.lens (localDeviceResourceData :: ResourceDataContainer -> Lude.Maybe LocalDeviceResourceData) (\s a -> s {localDeviceResourceData = a} :: ResourceDataContainer)
{-# DEPRECATED rdcLocalDeviceResourceData "Use generic-lens or generic-optics with 'localDeviceResourceData' instead." #-}

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager.
--
-- /Note:/ Consider using 'secretsManagerSecretResourceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcSecretsManagerSecretResourceData :: Lens.Lens' ResourceDataContainer (Lude.Maybe SecretsManagerSecretResourceData)
rdcSecretsManagerSecretResourceData = Lens.lens (secretsManagerSecretResourceData :: ResourceDataContainer -> Lude.Maybe SecretsManagerSecretResourceData) (\s a -> s {secretsManagerSecretResourceData = a} :: ResourceDataContainer)
{-# DEPRECATED rdcSecretsManagerSecretResourceData "Use generic-lens or generic-optics with 'secretsManagerSecretResourceData' instead." #-}

instance Lude.FromJSON ResourceDataContainer where
  parseJSON =
    Lude.withObject
      "ResourceDataContainer"
      ( \x ->
          ResourceDataContainer'
            Lude.<$> (x Lude..:? "S3MachineLearningModelResourceData")
            Lude.<*> (x Lude..:? "SageMakerMachineLearningModelResourceData")
            Lude.<*> (x Lude..:? "LocalVolumeResourceData")
            Lude.<*> (x Lude..:? "LocalDeviceResourceData")
            Lude.<*> (x Lude..:? "SecretsManagerSecretResourceData")
      )

instance Lude.ToJSON ResourceDataContainer where
  toJSON ResourceDataContainer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3MachineLearningModelResourceData" Lude..=)
              Lude.<$> s3MachineLearningModelResourceData,
            ("SageMakerMachineLearningModelResourceData" Lude..=)
              Lude.<$> sageMakerMachineLearningModelResourceData,
            ("LocalVolumeResourceData" Lude..=)
              Lude.<$> localVolumeResourceData,
            ("LocalDeviceResourceData" Lude..=)
              Lude.<$> localDeviceResourceData,
            ("SecretsManagerSecretResourceData" Lude..=)
              Lude.<$> secretsManagerSecretResourceData
          ]
      )
