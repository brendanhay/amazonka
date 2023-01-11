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
-- Module      : Amazonka.SageMaker.Types.NetworkConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NetworkConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VpcConfig

-- | Networking options for a job, such as network traffic encryption between
-- containers, whether to allow inbound and outbound network calls to and
-- from containers, and the VPC subnets and security groups to use for
-- VPC-enabled jobs.
--
-- /See:/ 'newNetworkConfig' smart constructor.
data NetworkConfig = NetworkConfig'
  { -- | Whether to encrypt all communications between distributed processing
    -- jobs. Choose @True@ to encrypt communications. Encryption provides
    -- greater security for distributed processing jobs, but the processing
    -- might take longer.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Whether to allow inbound and outbound network calls to and from the
    -- containers used for the processing job.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableInterContainerTrafficEncryption', 'networkConfig_enableInterContainerTrafficEncryption' - Whether to encrypt all communications between distributed processing
-- jobs. Choose @True@ to encrypt communications. Encryption provides
-- greater security for distributed processing jobs, but the processing
-- might take longer.
--
-- 'enableNetworkIsolation', 'networkConfig_enableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the
-- containers used for the processing job.
--
-- 'vpcConfig', 'networkConfig_vpcConfig' - Undocumented member.
newNetworkConfig ::
  NetworkConfig
newNetworkConfig =
  NetworkConfig'
    { enableInterContainerTrafficEncryption =
        Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | Whether to encrypt all communications between distributed processing
-- jobs. Choose @True@ to encrypt communications. Encryption provides
-- greater security for distributed processing jobs, but the processing
-- might take longer.
networkConfig_enableInterContainerTrafficEncryption :: Lens.Lens' NetworkConfig (Prelude.Maybe Prelude.Bool)
networkConfig_enableInterContainerTrafficEncryption = Lens.lens (\NetworkConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@NetworkConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: NetworkConfig)

-- | Whether to allow inbound and outbound network calls to and from the
-- containers used for the processing job.
networkConfig_enableNetworkIsolation :: Lens.Lens' NetworkConfig (Prelude.Maybe Prelude.Bool)
networkConfig_enableNetworkIsolation = Lens.lens (\NetworkConfig' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@NetworkConfig' {} a -> s {enableNetworkIsolation = a} :: NetworkConfig)

-- | Undocumented member.
networkConfig_vpcConfig :: Lens.Lens' NetworkConfig (Prelude.Maybe VpcConfig)
networkConfig_vpcConfig = Lens.lens (\NetworkConfig' {vpcConfig} -> vpcConfig) (\s@NetworkConfig' {} a -> s {vpcConfig = a} :: NetworkConfig)

instance Data.FromJSON NetworkConfig where
  parseJSON =
    Data.withObject
      "NetworkConfig"
      ( \x ->
          NetworkConfig'
            Prelude.<$> (x Data..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Data..:? "EnableNetworkIsolation")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable NetworkConfig where
  hashWithSalt _salt NetworkConfig' {..} =
    _salt
      `Prelude.hashWithSalt` enableInterContainerTrafficEncryption
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData NetworkConfig where
  rnf NetworkConfig' {..} =
    Prelude.rnf enableInterContainerTrafficEncryption
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf vpcConfig

instance Data.ToJSON NetworkConfig where
  toJSON NetworkConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableInterContainerTrafficEncryption" Data..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("EnableNetworkIsolation" Data..=)
              Prelude.<$> enableNetworkIsolation,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )
