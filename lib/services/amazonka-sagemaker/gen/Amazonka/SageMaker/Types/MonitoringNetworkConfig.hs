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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringNetworkConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VpcConfig

-- | The networking configuration for the monitoring job.
--
-- /See:/ 'newMonitoringNetworkConfig' smart constructor.
data MonitoringNetworkConfig = MonitoringNetworkConfig'
  { vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Whether to allow inbound and outbound network calls to and from the
    -- containers used for the monitoring job.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
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
-- 'vpcConfig', 'monitoringNetworkConfig_vpcConfig' - Undocumented member.
--
-- 'enableNetworkIsolation', 'monitoringNetworkConfig_enableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the
-- containers used for the monitoring job.
--
-- 'enableInterContainerTrafficEncryption', 'monitoringNetworkConfig_enableInterContainerTrafficEncryption' - Whether to encrypt all communications between the instances used for the
-- monitoring jobs. Choose @True@ to encrypt communications. Encryption
-- provides greater security for distributed jobs, but the processing might
-- take longer.
newMonitoringNetworkConfig ::
  MonitoringNetworkConfig
newMonitoringNetworkConfig =
  MonitoringNetworkConfig'
    { vpcConfig =
        Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing
    }

-- | Undocumented member.
monitoringNetworkConfig_vpcConfig :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe VpcConfig)
monitoringNetworkConfig_vpcConfig = Lens.lens (\MonitoringNetworkConfig' {vpcConfig} -> vpcConfig) (\s@MonitoringNetworkConfig' {} a -> s {vpcConfig = a} :: MonitoringNetworkConfig)

-- | Whether to allow inbound and outbound network calls to and from the
-- containers used for the monitoring job.
monitoringNetworkConfig_enableNetworkIsolation :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe Prelude.Bool)
monitoringNetworkConfig_enableNetworkIsolation = Lens.lens (\MonitoringNetworkConfig' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@MonitoringNetworkConfig' {} a -> s {enableNetworkIsolation = a} :: MonitoringNetworkConfig)

-- | Whether to encrypt all communications between the instances used for the
-- monitoring jobs. Choose @True@ to encrypt communications. Encryption
-- provides greater security for distributed jobs, but the processing might
-- take longer.
monitoringNetworkConfig_enableInterContainerTrafficEncryption :: Lens.Lens' MonitoringNetworkConfig (Prelude.Maybe Prelude.Bool)
monitoringNetworkConfig_enableInterContainerTrafficEncryption = Lens.lens (\MonitoringNetworkConfig' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@MonitoringNetworkConfig' {} a -> s {enableInterContainerTrafficEncryption = a} :: MonitoringNetworkConfig)

instance Data.FromJSON MonitoringNetworkConfig where
  parseJSON =
    Data.withObject
      "MonitoringNetworkConfig"
      ( \x ->
          MonitoringNetworkConfig'
            Prelude.<$> (x Data..:? "VpcConfig")
            Prelude.<*> (x Data..:? "EnableNetworkIsolation")
            Prelude.<*> (x Data..:? "EnableInterContainerTrafficEncryption")
      )

instance Prelude.Hashable MonitoringNetworkConfig where
  hashWithSalt _salt MonitoringNetworkConfig' {..} =
    _salt `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` enableInterContainerTrafficEncryption

instance Prelude.NFData MonitoringNetworkConfig where
  rnf MonitoringNetworkConfig' {..} =
    Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf enableInterContainerTrafficEncryption

instance Data.ToJSON MonitoringNetworkConfig where
  toJSON MonitoringNetworkConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("EnableNetworkIsolation" Data..=)
              Prelude.<$> enableNetworkIsolation,
            ("EnableInterContainerTrafficEncryption" Data..=)
              Prelude.<$> enableInterContainerTrafficEncryption
          ]
      )
