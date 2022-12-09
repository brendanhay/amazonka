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
-- Module      : Amazonka.Greengrass.Types.ResourceDataContainer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.ResourceDataContainer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.LocalDeviceResourceData
import Amazonka.Greengrass.Types.LocalVolumeResourceData
import Amazonka.Greengrass.Types.S3MachineLearningModelResourceData
import Amazonka.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Amazonka.Greengrass.Types.SecretsManagerSecretResourceData
import qualified Amazonka.Prelude as Prelude

-- | A container for resource data. The container takes only one of the
-- following supported resource data types:
-- \'\'LocalDeviceResourceData\'\', \'\'LocalVolumeResourceData\'\',
-- \'\'SageMakerMachineLearningModelResourceData\'\',
-- \'\'S3MachineLearningModelResourceData\'\',
-- \'\'SecretsManagerSecretResourceData\'\'.
--
-- /See:/ 'newResourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { -- | Attributes that define the local device resource.
    localDeviceResourceData :: Prelude.Maybe LocalDeviceResourceData,
    -- | Attributes that define the local volume resource.
    localVolumeResourceData :: Prelude.Maybe LocalVolumeResourceData,
    -- | Attributes that define an Amazon S3 machine learning resource.
    s3MachineLearningModelResourceData :: Prelude.Maybe S3MachineLearningModelResourceData,
    -- | Attributes that define an Amazon SageMaker machine learning resource.
    sageMakerMachineLearningModelResourceData :: Prelude.Maybe SageMakerMachineLearningModelResourceData,
    -- | Attributes that define a secret resource, which references a secret from
    -- AWS Secrets Manager.
    secretsManagerSecretResourceData :: Prelude.Maybe SecretsManagerSecretResourceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localDeviceResourceData', 'resourceDataContainer_localDeviceResourceData' - Attributes that define the local device resource.
--
-- 'localVolumeResourceData', 'resourceDataContainer_localVolumeResourceData' - Attributes that define the local volume resource.
--
-- 's3MachineLearningModelResourceData', 'resourceDataContainer_s3MachineLearningModelResourceData' - Attributes that define an Amazon S3 machine learning resource.
--
-- 'sageMakerMachineLearningModelResourceData', 'resourceDataContainer_sageMakerMachineLearningModelResourceData' - Attributes that define an Amazon SageMaker machine learning resource.
--
-- 'secretsManagerSecretResourceData', 'resourceDataContainer_secretsManagerSecretResourceData' - Attributes that define a secret resource, which references a secret from
-- AWS Secrets Manager.
newResourceDataContainer ::
  ResourceDataContainer
newResourceDataContainer =
  ResourceDataContainer'
    { localDeviceResourceData =
        Prelude.Nothing,
      localVolumeResourceData = Prelude.Nothing,
      s3MachineLearningModelResourceData = Prelude.Nothing,
      sageMakerMachineLearningModelResourceData =
        Prelude.Nothing,
      secretsManagerSecretResourceData = Prelude.Nothing
    }

-- | Attributes that define the local device resource.
resourceDataContainer_localDeviceResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe LocalDeviceResourceData)
resourceDataContainer_localDeviceResourceData = Lens.lens (\ResourceDataContainer' {localDeviceResourceData} -> localDeviceResourceData) (\s@ResourceDataContainer' {} a -> s {localDeviceResourceData = a} :: ResourceDataContainer)

-- | Attributes that define the local volume resource.
resourceDataContainer_localVolumeResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe LocalVolumeResourceData)
resourceDataContainer_localVolumeResourceData = Lens.lens (\ResourceDataContainer' {localVolumeResourceData} -> localVolumeResourceData) (\s@ResourceDataContainer' {} a -> s {localVolumeResourceData = a} :: ResourceDataContainer)

-- | Attributes that define an Amazon S3 machine learning resource.
resourceDataContainer_s3MachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe S3MachineLearningModelResourceData)
resourceDataContainer_s3MachineLearningModelResourceData = Lens.lens (\ResourceDataContainer' {s3MachineLearningModelResourceData} -> s3MachineLearningModelResourceData) (\s@ResourceDataContainer' {} a -> s {s3MachineLearningModelResourceData = a} :: ResourceDataContainer)

-- | Attributes that define an Amazon SageMaker machine learning resource.
resourceDataContainer_sageMakerMachineLearningModelResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe SageMakerMachineLearningModelResourceData)
resourceDataContainer_sageMakerMachineLearningModelResourceData = Lens.lens (\ResourceDataContainer' {sageMakerMachineLearningModelResourceData} -> sageMakerMachineLearningModelResourceData) (\s@ResourceDataContainer' {} a -> s {sageMakerMachineLearningModelResourceData = a} :: ResourceDataContainer)

-- | Attributes that define a secret resource, which references a secret from
-- AWS Secrets Manager.
resourceDataContainer_secretsManagerSecretResourceData :: Lens.Lens' ResourceDataContainer (Prelude.Maybe SecretsManagerSecretResourceData)
resourceDataContainer_secretsManagerSecretResourceData = Lens.lens (\ResourceDataContainer' {secretsManagerSecretResourceData} -> secretsManagerSecretResourceData) (\s@ResourceDataContainer' {} a -> s {secretsManagerSecretResourceData = a} :: ResourceDataContainer)

instance Data.FromJSON ResourceDataContainer where
  parseJSON =
    Data.withObject
      "ResourceDataContainer"
      ( \x ->
          ResourceDataContainer'
            Prelude.<$> (x Data..:? "LocalDeviceResourceData")
            Prelude.<*> (x Data..:? "LocalVolumeResourceData")
            Prelude.<*> (x Data..:? "S3MachineLearningModelResourceData")
            Prelude.<*> ( x
                            Data..:? "SageMakerMachineLearningModelResourceData"
                        )
            Prelude.<*> (x Data..:? "SecretsManagerSecretResourceData")
      )

instance Prelude.Hashable ResourceDataContainer where
  hashWithSalt _salt ResourceDataContainer' {..} =
    _salt
      `Prelude.hashWithSalt` localDeviceResourceData
      `Prelude.hashWithSalt` localVolumeResourceData
      `Prelude.hashWithSalt` s3MachineLearningModelResourceData
      `Prelude.hashWithSalt` sageMakerMachineLearningModelResourceData
      `Prelude.hashWithSalt` secretsManagerSecretResourceData

instance Prelude.NFData ResourceDataContainer where
  rnf ResourceDataContainer' {..} =
    Prelude.rnf localDeviceResourceData
      `Prelude.seq` Prelude.rnf localVolumeResourceData
      `Prelude.seq` Prelude.rnf s3MachineLearningModelResourceData
      `Prelude.seq` Prelude.rnf sageMakerMachineLearningModelResourceData
      `Prelude.seq` Prelude.rnf secretsManagerSecretResourceData

instance Data.ToJSON ResourceDataContainer where
  toJSON ResourceDataContainer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LocalDeviceResourceData" Data..=)
              Prelude.<$> localDeviceResourceData,
            ("LocalVolumeResourceData" Data..=)
              Prelude.<$> localVolumeResourceData,
            ("S3MachineLearningModelResourceData" Data..=)
              Prelude.<$> s3MachineLearningModelResourceData,
            ("SageMakerMachineLearningModelResourceData" Data..=)
              Prelude.<$> sageMakerMachineLearningModelResourceData,
            ("SecretsManagerSecretResourceData" Data..=)
              Prelude.<$> secretsManagerSecretResourceData
          ]
      )
