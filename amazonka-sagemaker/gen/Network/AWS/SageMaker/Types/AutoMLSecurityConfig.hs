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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.VpcConfig

-- | Security options.
--
-- /See:/ 'newAutoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { -- | VPC configuration.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | Whether to use traffic encryption between the container layers.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    -- | The key used to encrypt stored data.
    volumeKmsKeyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { vpcConfig = Core.Nothing,
      enableInterContainerTrafficEncryption = Core.Nothing,
      volumeKmsKeyId = Core.Nothing
    }

-- | VPC configuration.
autoMLSecurityConfig_vpcConfig :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe VpcConfig)
autoMLSecurityConfig_vpcConfig = Lens.lens (\AutoMLSecurityConfig' {vpcConfig} -> vpcConfig) (\s@AutoMLSecurityConfig' {} a -> s {vpcConfig = a} :: AutoMLSecurityConfig)

-- | Whether to use traffic encryption between the container layers.
autoMLSecurityConfig_enableInterContainerTrafficEncryption :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe Core.Bool)
autoMLSecurityConfig_enableInterContainerTrafficEncryption = Lens.lens (\AutoMLSecurityConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@AutoMLSecurityConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: AutoMLSecurityConfig)

-- | The key used to encrypt stored data.
autoMLSecurityConfig_volumeKmsKeyId :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe Core.Text)
autoMLSecurityConfig_volumeKmsKeyId = Lens.lens (\AutoMLSecurityConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@AutoMLSecurityConfig' {} a -> s {volumeKmsKeyId = a} :: AutoMLSecurityConfig)

instance Core.FromJSON AutoMLSecurityConfig where
  parseJSON =
    Core.withObject
      "AutoMLSecurityConfig"
      ( \x ->
          AutoMLSecurityConfig'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
      )

instance Core.Hashable AutoMLSecurityConfig

instance Core.NFData AutoMLSecurityConfig

instance Core.ToJSON AutoMLSecurityConfig where
  toJSON AutoMLSecurityConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Core.<$> enableInterContainerTrafficEncryption,
            ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId
          ]
      )
