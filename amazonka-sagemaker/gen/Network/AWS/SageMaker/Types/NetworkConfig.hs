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
-- Module      : Network.AWS.SageMaker.Types.NetworkConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NetworkConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.VpcConfig

-- | Networking options for a job, such as network traffic encryption between
-- containers, whether to allow inbound and outbound network calls to and
-- from containers, and the VPC subnets and security groups to use for
-- VPC-enabled jobs.
--
-- /See:/ 'newNetworkConfig' smart constructor.
data NetworkConfig = NetworkConfig'
  { vpcConfig :: Core.Maybe VpcConfig,
    -- | Whether to allow inbound and outbound network calls to and from the
    -- containers used for the processing job.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | Whether to encrypt all communications between distributed processing
    -- jobs. Choose @True@ to encrypt communications. Encryption provides
    -- greater security for distributed processing jobs, but the processing
    -- might take longer.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'networkConfig_vpcConfig' - Undocumented member.
--
-- 'enableNetworkIsolation', 'networkConfig_enableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the
-- containers used for the processing job.
--
-- 'enableInterContainerTrafficEncryption', 'networkConfig_enableInterContainerTrafficEncryption' - Whether to encrypt all communications between distributed processing
-- jobs. Choose @True@ to encrypt communications. Encryption provides
-- greater security for distributed processing jobs, but the processing
-- might take longer.
newNetworkConfig ::
  NetworkConfig
newNetworkConfig =
  NetworkConfig'
    { vpcConfig = Core.Nothing,
      enableNetworkIsolation = Core.Nothing,
      enableInterContainerTrafficEncryption = Core.Nothing
    }

-- | Undocumented member.
networkConfig_vpcConfig :: Lens.Lens' NetworkConfig (Core.Maybe VpcConfig)
networkConfig_vpcConfig = Lens.lens (\NetworkConfig' {vpcConfig} -> vpcConfig) (\s@NetworkConfig' {} a -> s {vpcConfig = a} :: NetworkConfig)

-- | Whether to allow inbound and outbound network calls to and from the
-- containers used for the processing job.
networkConfig_enableNetworkIsolation :: Lens.Lens' NetworkConfig (Core.Maybe Core.Bool)
networkConfig_enableNetworkIsolation = Lens.lens (\NetworkConfig' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@NetworkConfig' {} a -> s {enableNetworkIsolation = a} :: NetworkConfig)

-- | Whether to encrypt all communications between distributed processing
-- jobs. Choose @True@ to encrypt communications. Encryption provides
-- greater security for distributed processing jobs, but the processing
-- might take longer.
networkConfig_enableInterContainerTrafficEncryption :: Lens.Lens' NetworkConfig (Core.Maybe Core.Bool)
networkConfig_enableInterContainerTrafficEncryption = Lens.lens (\NetworkConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@NetworkConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: NetworkConfig)

instance Core.FromJSON NetworkConfig where
  parseJSON =
    Core.withObject
      "NetworkConfig"
      ( \x ->
          NetworkConfig'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "EnableNetworkIsolation")
            Core.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
      )

instance Core.Hashable NetworkConfig

instance Core.NFData NetworkConfig

instance Core.ToJSON NetworkConfig where
  toJSON NetworkConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("EnableNetworkIsolation" Core..=)
              Core.<$> enableNetworkIsolation,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Core.<$> enableInterContainerTrafficEncryption
          ]
      )
