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
-- Module      : Amazonka.SageMaker.Types.MonitoringNetworkConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringNetworkConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VpcConfig

-- | The networking configuration for the monitoring job.
--
-- /See:/ 'newMonitoringNetworkConfig' smart constructor.
data MonitoringNetworkConfig = MonitoringNetworkConfig'
  { -- | Whether to allow inbound and outbound network calls to and from the
    -- containers used for the monitoring job.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Whether to encrypt all communications between the instances used for the
    -- monitoring jobs. Choose @True@ to encrypt communications. Encryption
    -- provides greater security for distributed jobs, but the processing might
    -- take longer.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringNetworkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableNetworkIsolation', 'monitoringNetworkConfig_enableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the
-- containers used for the monitoring job.
--
-- 'vpcConfig', 'monitoringNetworkConfig_vpcConfig' - Undocumented member.
--
-- 'enableInterContainerTrafficEncryption', 'monitoringNetworkConfig_enableInterContainerTrafficEncryption' - Whether to encrypt all communications between the instances used for the
-- monitoring jobs. Choose @True@ to encrypt communications. Encryption
-- provides greater security for distributed jobs, but the processing might
-- take longer.
newMonitoringNetworkConfig ::
  MonitoringNetworkConfig
newMonitoringNetworkConfig =
  MonitoringNetworkConfig'
    { enableNetworkIsolation =
        Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing
    }

-- | Whether to allow inbound and outbound network calls to and from the
-- containers used for the monitoring job.
monitoringNetworkConfig_enableNetworkIsolation :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe Prelude.Bool)
monitoringNetworkConfig_enableNetworkIsolation = Lens.lens (\MonitoringNetworkConfig' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@MonitoringNetworkConfig' {} a -> s {enableNetworkIsolation = a} :: MonitoringNetworkConfig)

-- | Undocumented member.
monitoringNetworkConfig_vpcConfig :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe VpcConfig)
monitoringNetworkConfig_vpcConfig = Lens.lens (\MonitoringNetworkConfig' {vpcConfig} -> vpcConfig) (\s@MonitoringNetworkConfig' {} a -> s {vpcConfig = a} :: MonitoringNetworkConfig)

-- | Whether to encrypt all communications between the instances used for the
-- monitoring jobs. Choose @True@ to encrypt communications. Encryption
-- provides greater security for distributed jobs, but the processing might
-- take longer.
monitoringNetworkConfig_enableInterContainerTrafficEncryption :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe Prelude.Bool)
monitoringNetworkConfig_enableInterContainerTrafficEncryption = Lens.lens (\MonitoringNetworkConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@MonitoringNetworkConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: MonitoringNetworkConfig)

instance Core.FromJSON MonitoringNetworkConfig where
  parseJSON =
    Core.withObject
      "MonitoringNetworkConfig"
      ( \x ->
          MonitoringNetworkConfig'
            Prelude.<$> (x Core..:? "EnableNetworkIsolation")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
      )

instance Prelude.Hashable MonitoringNetworkConfig

instance Prelude.NFData MonitoringNetworkConfig

instance Core.ToJSON MonitoringNetworkConfig where
  toJSON MonitoringNetworkConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableNetworkIsolation" Core..=)
              Prelude.<$> enableNetworkIsolation,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Prelude.<$> enableInterContainerTrafficEncryption
          ]
      )
