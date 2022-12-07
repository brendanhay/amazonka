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
-- Module      : Amazonka.SageMaker.Types.AutoMLSecurityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLSecurityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VpcConfig

-- | Security options.
--
-- /See:/ 'newAutoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { -- | The VPC configuration.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The key used to encrypt stored data.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Whether to use traffic encryption between the container layers.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'autoMLSecurityConfig_vpcConfig' - The VPC configuration.
--
-- 'volumeKmsKeyId', 'autoMLSecurityConfig_volumeKmsKeyId' - The key used to encrypt stored data.
--
-- 'enableInterContainerTrafficEncryption', 'autoMLSecurityConfig_enableInterContainerTrafficEncryption' - Whether to use traffic encryption between the container layers.
newAutoMLSecurityConfig ::
  AutoMLSecurityConfig
newAutoMLSecurityConfig =
  AutoMLSecurityConfig'
    { vpcConfig = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing
    }

-- | The VPC configuration.
autoMLSecurityConfig_vpcConfig :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe VpcConfig)
autoMLSecurityConfig_vpcConfig = Lens.lens (\AutoMLSecurityConfig' {vpcConfig} -> vpcConfig) (\s@AutoMLSecurityConfig' {} a -> s {vpcConfig = a} :: AutoMLSecurityConfig)

-- | The key used to encrypt stored data.
autoMLSecurityConfig_volumeKmsKeyId :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe Prelude.Text)
autoMLSecurityConfig_volumeKmsKeyId = Lens.lens (\AutoMLSecurityConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@AutoMLSecurityConfig' {} a -> s {volumeKmsKeyId = a} :: AutoMLSecurityConfig)

-- | Whether to use traffic encryption between the container layers.
autoMLSecurityConfig_enableInterContainerTrafficEncryption :: Lens.Lens' AutoMLSecurityConfig (Prelude.Maybe Prelude.Bool)
autoMLSecurityConfig_enableInterContainerTrafficEncryption = Lens.lens (\AutoMLSecurityConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@AutoMLSecurityConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: AutoMLSecurityConfig)

instance Data.FromJSON AutoMLSecurityConfig where
  parseJSON =
    Data.withObject
      "AutoMLSecurityConfig"
      ( \x ->
          AutoMLSecurityConfig'
            Prelude.<$> (x Data..:? "VpcConfig")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "EnableInterContainerTrafficEncryption")
      )

instance Prelude.Hashable AutoMLSecurityConfig where
  hashWithSalt _salt AutoMLSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` enableInterContainerTrafficEncryption

instance Prelude.NFData AutoMLSecurityConfig where
  rnf AutoMLSecurityConfig' {..} =
    Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf enableInterContainerTrafficEncryption

instance Data.ToJSON AutoMLSecurityConfig where
  toJSON AutoMLSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("EnableInterContainerTrafficEncryption" Data..=)
              Prelude.<$> enableInterContainerTrafficEncryption
          ]
      )
