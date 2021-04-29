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
-- Module      : Network.AWS.SageMaker.Types.MonitoringResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringResources where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringClusterConfig

-- | Identifies the resources to deploy for a monitoring job.
--
-- /See:/ 'newMonitoringResources' smart constructor.
data MonitoringResources = MonitoringResources'
  { -- | The configuration for the cluster resources used to run the processing
    -- job.
    clusterConfig :: MonitoringClusterConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON MonitoringResources where
  parseJSON =
    Prelude.withObject
      "MonitoringResources"
      ( \x ->
          MonitoringResources'
            Prelude.<$> (x Prelude..: "ClusterConfig")
      )

instance Prelude.Hashable MonitoringResources

instance Prelude.NFData MonitoringResources

instance Prelude.ToJSON MonitoringResources where
  toJSON MonitoringResources' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClusterConfig" Prelude..= clusterConfig)
          ]
      )
