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
-- Module      : Network.AWS.Greengrass.Types.ResourceDataContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDataContainer where

import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for resource data. The container takes only one of the
-- following supported resource data types:
-- \'\'LocalDeviceResourceData\'\', \'\'LocalVolumeResourceData\'\',
-- \'\'SageMakerMachineLearningModelResourceData\'\',
-- \'\'S3MachineLearningModelResourceData\'\',
-- \'\'SecretsManagerSecretResourceData\'\'.
--
-- /See:/ 'newResourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { -- | Attributes that define the local volume resource.
    localVolumeResourceData :: Prelude.Maybe LocalVolumeResourceData,
    -- | Attributes that define the local device resource.
    localDeviceResourceData :: Prelude.Maybe LocalDeviceResourceData,
    -- | Attributes that define an Amazon SageMaker machine learning resource.
    sageMakerMachineLearningModelResourceData :: Prelude.Maybe SageMakerMachineLearningModelResourceData,
    -- | Attributes that define an Amazon S3 machine learning resource.
    s3MachineLearningModelResourceData :: Prelude.Maybe S3MachineLearningModelResourceData,
    -- | Attributes that define a secret resource, which references a secret from
    -- AWS Secrets Manager.
    secretsManagerSecretResourceData :: Prelude.Maybe SecretsManagerSecretResourceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localVolumeResourceData', 'resourceDataContainer_localVolumeResourceData' - Attributes that define the local volume resource.
--
-- 'localDeviceResourceData', 'resourceDataContainer_localDeviceResourceData' - Attributes that define the local device resource.
--
-- 'sageMakerMachineLearningModelResourceData', 'resourceDataContainer_sageMakerMachineLearningModelResourceData' - Attributes that define an Amazon SageMaker machine learning resource.
--
-- 's3MachineLearningModelResourceData', 'resourceDataContainer_s3MachineLearningModelResourceData' - Attributes that define an Amazon S3 machine learning resource.
--
-- 'secretsManagerSecretResourceData', 'resourceDataContainer_secretsManagerSecretResourceData' - Attributes that define a secret resource, which references a secret from
-- AWS Secrets Manager.
newResourceDataContainer ::
  ResourceDataContainer
newResourceDataContainer =
  ResourceDataContainer'
    { localVolumeResourceData =
        Prelude.Nothing,
      localDeviceResourceData = Prelude.Nothing,
      sageMakerMachineLearningModelResourceData =
        Prelude.Nothing,
      s3MachineLearningModelResourceData = Prelude.Nothing,
      secretsManagerSecretResourceData = Prelude.Nothing
    }

-- | Attributes that define the local volume resource.
resourceDataContainer_localVolumeResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe LocalVolumeResourceData)
resourceDataContainer_localVolumeResourceData = Lens.lens (\ResourceDataContainer' {localVolumeResourceData} -> localVolumeResourceData) (\s@ResourceDataContainer' {} a -> s {localVolumeResourceData = a} :: ResourceDataContainer)

-- | Attributes that define the local device resource.
resourceDataContainer_localDeviceResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe LocalDeviceResourceData)
resourceDataContainer_localDeviceResourceData = Lens.lens (\ResourceDataContainer' {localDeviceResourceData} -> localDeviceResourceData) (\s@ResourceDataContainer' {} a -> s {localDeviceResourceData = a} :: ResourceDataContainer)

-- | Attributes that define an Amazon SageMaker machine learning resource.
resourceDataContainer_sageMakerMachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe SageMakerMachineLearningModelResourceData)
resourceDataContainer_sageMakerMachineLearningModelResourceData = Lens.lens (\ResourceDataContainer' {sageMakerMachineLearningModelResourceData} -> sageMakerMachineLearningModelResourceData) (\s@ResourceDataContainer' {} a -> s {sageMakerMachineLearningModelResourceData = a} :: ResourceDataContainer)

-- | Attributes that define an Amazon S3 machine learning resource.
resourceDataContainer_s3MachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe S3MachineLearningModelResourceData)
resourceDataContainer_s3MachineLearningModelResourceData = Lens.lens (\ResourceDataContainer' {s3MachineLearningModelResourceData} -> s3MachineLearningModelResourceData) (\s@ResourceDataContainer' {} a -> s {s3MachineLearningModelResourceData = a} :: ResourceDataContainer)

-- | Attributes that define a secret resource, which references a secret from
-- AWS Secrets Manager.
resourceDataContainer_secretsManagerSecretResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe SecretsManagerSecretResourceData)
resourceDataContainer_secretsManagerSecretResourceData = Lens.lens (\ResourceDataContainer' {secretsManagerSecretResourceData} -> secretsManagerSecretResourceData) (\s@ResourceDataContainer' {} a -> s {secretsManagerSecretResourceData = a} :: ResourceDataContainer)

instance Prelude.FromJSON ResourceDataContainer where
  parseJSON =
    Prelude.withObject
      "ResourceDataContainer"
      ( \x ->
          ResourceDataContainer'
            Prelude.<$> (x Prelude..:? "LocalVolumeResourceData")
            Prelude.<*> (x Prelude..:? "LocalDeviceResourceData")
            Prelude.<*> ( x
                            Prelude..:? "SageMakerMachineLearningModelResourceData"
                        )
            Prelude.<*> (x Prelude..:? "S3MachineLearningModelResourceData")
            Prelude.<*> (x Prelude..:? "SecretsManagerSecretResourceData")
      )

instance Prelude.Hashable ResourceDataContainer

instance Prelude.NFData ResourceDataContainer

instance Prelude.ToJSON ResourceDataContainer where
  toJSON ResourceDataContainer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LocalVolumeResourceData" Prelude..=)
              Prelude.<$> localVolumeResourceData,
            ("LocalDeviceResourceData" Prelude..=)
              Prelude.<$> localDeviceResourceData,
            ( "SageMakerMachineLearningModelResourceData"
                Prelude..=
            )
              Prelude.<$> sageMakerMachineLearningModelResourceData,
            ("S3MachineLearningModelResourceData" Prelude..=)
              Prelude.<$> s3MachineLearningModelResourceData,
            ("SecretsManagerSecretResourceData" Prelude..=)
              Prelude.<$> secretsManagerSecretResourceData
          ]
      )
