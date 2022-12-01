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
-- Module      : Amazonka.SageMaker.Types.MonitoringResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringClusterConfig

-- | Identifies the resources to deploy for a monitoring job.
--
-- /See:/ 'newMonitoringResources' smart constructor.
data MonitoringResources = MonitoringResources'
  { -- | The configuration for the cluster resources used to run the processing
    -- job.
    clusterConfig :: MonitoringClusterConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterConfig', 'monitoringResources_clusterConfig' - The configuration for the cluster resources used to run the processing
-- job.
newMonitoringResources ::
  -- | 'clusterConfig'
  MonitoringClusterConfig ->
  MonitoringResources
newMonitoringResources pClusterConfig_ =
  MonitoringResources'
    { clusterConfig =
        pClusterConfig_
    }

-- | The configuration for the cluster resources used to run the processing
-- job.
monitoringResources_clusterConfig :: Lens.Lens' MonitoringResources MonitoringClusterConfig
monitoringResources_clusterConfig = Lens.lens (\MonitoringResources' {clusterConfig} -> clusterConfig) (\s@MonitoringResources' {} a -> s {clusterConfig = a} :: MonitoringResources)

instance Core.FromJSON MonitoringResources where
  parseJSON =
    Core.withObject
      "MonitoringResources"
      ( \x ->
          MonitoringResources'
            Prelude.<$> (x Core..: "ClusterConfig")
      )

instance Prelude.Hashable MonitoringResources where
  hashWithSalt _salt MonitoringResources' {..} =
    _salt `Prelude.hashWithSalt` clusterConfig

instance Prelude.NFData MonitoringResources where
  rnf MonitoringResources' {..} =
    Prelude.rnf clusterConfig

instance Core.ToJSON MonitoringResources where
  toJSON MonitoringResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClusterConfig" Core..= clusterConfig)
          ]
      )
