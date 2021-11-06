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
-- Module      : Amazonka.OpenSearch.Types.ClusterConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ClusterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.ColdStorageOptions
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
import Amazonka.OpenSearch.Types.ZoneAwarenessConfig
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the domain cluster, such as the type and number of
-- instances.
--
-- /See:/ 'newClusterConfig' smart constructor.
data ClusterConfig = ClusterConfig'
  { -- | Total number of dedicated master nodes, active and on standby, for the
    -- cluster.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int,
    -- | The instance type for a dedicated master node.
    dedicatedMasterType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | A boolean value to indicate whether a dedicated master node is enabled.
    -- See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
    -- for more information.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of instances in the specified domain cluster.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the @ColdStorageOptions@ config for a Domain
    coldStorageOptions :: Prelude.Maybe ColdStorageOptions,
    -- | A boolean value to indicate whether zone awareness is enabled. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
    -- for more information.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for an OpenSearch cluster. UltraWarm instance types
    -- are not supported for data instances.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | True to enable UltraWarm storage.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The zone awareness configuration for a domain when zone awareness is
    -- enabled.
    zoneAwarenessConfig :: Prelude.Maybe ZoneAwarenessConfig,
    -- | The number of UltraWarm nodes in the cluster.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | The instance type for the OpenSearch cluster\'s warm nodes.
    warmType :: Prelude.Maybe OpenSearchWarmPartitionInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedMasterCount', 'clusterConfig_dedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the
-- cluster.
--
-- 'dedicatedMasterType', 'clusterConfig_dedicatedMasterType' - The instance type for a dedicated master node.
--
-- 'dedicatedMasterEnabled', 'clusterConfig_dedicatedMasterEnabled' - A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
-- for more information.
--
-- 'instanceCount', 'clusterConfig_instanceCount' - The number of instances in the specified domain cluster.
--
-- 'coldStorageOptions', 'clusterConfig_coldStorageOptions' - Specifies the @ColdStorageOptions@ config for a Domain
--
-- 'zoneAwarenessEnabled', 'clusterConfig_zoneAwarenessEnabled' - A boolean value to indicate whether zone awareness is enabled. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
-- for more information.
--
-- 'instanceType', 'clusterConfig_instanceType' - The instance type for an OpenSearch cluster. UltraWarm instance types
-- are not supported for data instances.
--
-- 'warmEnabled', 'clusterConfig_warmEnabled' - True to enable UltraWarm storage.
--
-- 'zoneAwarenessConfig', 'clusterConfig_zoneAwarenessConfig' - The zone awareness configuration for a domain when zone awareness is
-- enabled.
--
-- 'warmCount', 'clusterConfig_warmCount' - The number of UltraWarm nodes in the cluster.
--
-- 'warmType', 'clusterConfig_warmType' - The instance type for the OpenSearch cluster\'s warm nodes.
newClusterConfig ::
  ClusterConfig
newClusterConfig =
  ClusterConfig'
    { dedicatedMasterCount =
        Prelude.Nothing,
      dedicatedMasterType = Prelude.Nothing,
      dedicatedMasterEnabled = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      coldStorageOptions = Prelude.Nothing,
      zoneAwarenessEnabled = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      warmEnabled = Prelude.Nothing,
      zoneAwarenessConfig = Prelude.Nothing,
      warmCount = Prelude.Nothing,
      warmType = Prelude.Nothing
    }

-- | Total number of dedicated master nodes, active and on standby, for the
-- cluster.
clusterConfig_dedicatedMasterCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_dedicatedMasterCount = Lens.lens (\ClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ClusterConfig)

-- | The instance type for a dedicated master node.
clusterConfig_dedicatedMasterType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_dedicatedMasterType = Lens.lens (\ClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ClusterConfig' {} a -> s {dedicatedMasterType = a} :: ClusterConfig)

-- | A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
-- for more information.
clusterConfig_dedicatedMasterEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_dedicatedMasterEnabled = Lens.lens (\ClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ClusterConfig)

-- | The number of instances in the specified domain cluster.
clusterConfig_instanceCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_instanceCount = Lens.lens (\ClusterConfig' {instanceCount} -> instanceCount) (\s@ClusterConfig' {} a -> s {instanceCount = a} :: ClusterConfig)

-- | Specifies the @ColdStorageOptions@ config for a Domain
clusterConfig_coldStorageOptions :: Lens.Lens' ClusterConfig (Prelude.Maybe ColdStorageOptions)
clusterConfig_coldStorageOptions = Lens.lens (\ClusterConfig' {coldStorageOptions} -> coldStorageOptions) (\s@ClusterConfig' {} a -> s {coldStorageOptions = a} :: ClusterConfig)

-- | A boolean value to indicate whether zone awareness is enabled. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
-- for more information.
clusterConfig_zoneAwarenessEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_zoneAwarenessEnabled = Lens.lens (\ClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ClusterConfig)

-- | The instance type for an OpenSearch cluster. UltraWarm instance types
-- are not supported for data instances.
clusterConfig_instanceType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_instanceType = Lens.lens (\ClusterConfig' {instanceType} -> instanceType) (\s@ClusterConfig' {} a -> s {instanceType = a} :: ClusterConfig)

-- | True to enable UltraWarm storage.
clusterConfig_warmEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_warmEnabled = Lens.lens (\ClusterConfig' {warmEnabled} -> warmEnabled) (\s@ClusterConfig' {} a -> s {warmEnabled = a} :: ClusterConfig)

-- | The zone awareness configuration for a domain when zone awareness is
-- enabled.
clusterConfig_zoneAwarenessConfig :: Lens.Lens' ClusterConfig (Prelude.Maybe ZoneAwarenessConfig)
clusterConfig_zoneAwarenessConfig = Lens.lens (\ClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ClusterConfig)

-- | The number of UltraWarm nodes in the cluster.
clusterConfig_warmCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_warmCount = Lens.lens (\ClusterConfig' {warmCount} -> warmCount) (\s@ClusterConfig' {} a -> s {warmCount = a} :: ClusterConfig)

-- | The instance type for the OpenSearch cluster\'s warm nodes.
clusterConfig_warmType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchWarmPartitionInstanceType)
clusterConfig_warmType = Lens.lens (\ClusterConfig' {warmType} -> warmType) (\s@ClusterConfig' {} a -> s {warmType = a} :: ClusterConfig)

instance Core.FromJSON ClusterConfig where
  parseJSON =
    Core.withObject
      "ClusterConfig"
      ( \x ->
          ClusterConfig'
            Prelude.<$> (x Core..:? "DedicatedMasterCount")
            Prelude.<*> (x Core..:? "DedicatedMasterType")
            Prelude.<*> (x Core..:? "DedicatedMasterEnabled")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "ColdStorageOptions")
            Prelude.<*> (x Core..:? "ZoneAwarenessEnabled")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "WarmEnabled")
            Prelude.<*> (x Core..:? "ZoneAwarenessConfig")
            Prelude.<*> (x Core..:? "WarmCount")
            Prelude.<*> (x Core..:? "WarmType")
      )

instance Prelude.Hashable ClusterConfig

instance Prelude.NFData ClusterConfig

instance Core.ToJSON ClusterConfig where
  toJSON ClusterConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DedicatedMasterCount" Core..=)
              Prelude.<$> dedicatedMasterCount,
            ("DedicatedMasterType" Core..=)
              Prelude.<$> dedicatedMasterType,
            ("DedicatedMasterEnabled" Core..=)
              Prelude.<$> dedicatedMasterEnabled,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("ColdStorageOptions" Core..=)
              Prelude.<$> coldStorageOptions,
            ("ZoneAwarenessEnabled" Core..=)
              Prelude.<$> zoneAwarenessEnabled,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("WarmEnabled" Core..=) Prelude.<$> warmEnabled,
            ("ZoneAwarenessConfig" Core..=)
              Prelude.<$> zoneAwarenessConfig,
            ("WarmCount" Core..=) Prelude.<$> warmCount,
            ("WarmType" Core..=) Prelude.<$> warmType
          ]
      )
