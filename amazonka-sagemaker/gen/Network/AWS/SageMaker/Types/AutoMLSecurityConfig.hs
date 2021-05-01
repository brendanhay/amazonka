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
-- Module      : Network.AWS.SageMaker.Types.AutoMLSecurityConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSecurityConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.VpcConfig

-- | Security options.
--
-- /See:/ 'newAutoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { -- | VPC configuration.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Whether to use traffic encryption between the container layers.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | The key used to encrypt stored data.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoMLSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'autoMLSecurityConfig_vpcConfig' - VPC configuration.
--
-- 'enableInterContainerTrafficEncryption', 'autoMLSecurityConfig_enableInterContainerTrafficEncryption' - Whether to use traffic encryption between the container layers.
--
-- 'volumeKmsKeyId', 'autoMLSecurityConfig_volumeKmsKeyId' - The key used to encrypt stored data.
newAutoMLSecurityConfig ::
  AutoMLSecurityConfig
newAutoMLSecurityConfig =
  AutoMLSecurityConfig'
    { vpcConfig = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing
    }

-- | VPC configuration.
autoMLSecurityConfig_vpcConfig :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe VpcConfig)
autoMLSecurityConfig_vpcConfig = Lens.lens (\AutoMLSecurityConfig' {vpcConfig} -> vpcConfig) (\s@AutoMLSecurityConfig' {} a -> s {vpcConfig = a} :: AutoMLSecurityConfig)

-- | Whether to use traffic encryption between the container layers.
autoMLSecurityConfig_enableInterContainerTrafficEncryption :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe Prelude.Bool)
autoMLSecurityConfig_enableInterContainerTrafficEncryption = Lens.lens (\AutoMLSecurityConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@AutoMLSecurityConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: AutoMLSecurityConfig)

-- | The key used to encrypt stored data.
autoMLSecurityConfig_volumeKmsKeyId :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe Prelude.Text)
autoMLSecurityConfig_volumeKmsKeyId = Lens.lens (\AutoMLSecurityConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@AutoMLSecurityConfig' {} a -> s {volumeKmsKeyId = a} :: AutoMLSecurityConfig)

instance Prelude.FromJSON AutoMLSecurityConfig where
  parseJSON =
    Prelude.withObject
      "AutoMLSecurityConfig"
      ( \x ->
          AutoMLSecurityConfig'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> ( x
                            Prelude..:? "EnableInterContainerTrafficEncryption"
                        )
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
      )

instance Prelude.Hashable AutoMLSecurityConfig

instance Prelude.NFData AutoMLSecurityConfig

instance Prelude.ToJSON AutoMLSecurityConfig where
  toJSON AutoMLSecurityConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Prelude..=) Prelude.<$> vpcConfig,
            ("EnableInterContainerTrafficEncryption" Prelude..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("VolumeKmsKeyId" Prelude..=)
              Prelude.<$> volumeKmsKeyId
          ]
      )
