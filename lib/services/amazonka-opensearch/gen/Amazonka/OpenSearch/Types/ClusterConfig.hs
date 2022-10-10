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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The number of UltraWarm nodes in the cluster.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the @ColdStorageOptions@ config for a Domain
    coldStorageOptions :: Prelude.Maybe ColdStorageOptions,
    -- | The instance type for a dedicated master node.
    dedicatedMasterType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | A boolean value to indicate whether zone awareness is enabled. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
    -- for more information.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value to indicate whether a dedicated master node is enabled.
    -- See
    -- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
    -- for more information.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for the OpenSearch cluster\'s warm nodes.
    warmType :: Prelude.Maybe OpenSearchWarmPartitionInstanceType,
    -- | The instance type for an OpenSearch cluster. UltraWarm instance types
    -- are not supported for data instances.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The zone awareness configuration for a domain when zone awareness is
    -- enabled.
    zoneAwarenessConfig :: Prelude.Maybe ZoneAwarenessConfig,
    -- | The number of instances in the specified domain cluster.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | True to enable UltraWarm storage.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Total number of dedicated master nodes, active and on standby, for the
    -- cluster.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int
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
-- 'warmCount', 'clusterConfig_warmCount' - The number of UltraWarm nodes in the cluster.
--
-- 'coldStorageOptions', 'clusterConfig_coldStorageOptions' - Specifies the @ColdStorageOptions@ config for a Domain
--
-- 'dedicatedMasterType', 'clusterConfig_dedicatedMasterType' - The instance type for a dedicated master node.
--
-- 'zoneAwarenessEnabled', 'clusterConfig_zoneAwarenessEnabled' - A boolean value to indicate whether zone awareness is enabled. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
-- for more information.
--
-- 'dedicatedMasterEnabled', 'clusterConfig_dedicatedMasterEnabled' - A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
-- for more information.
--
-- 'warmType', 'clusterConfig_warmType' - The instance type for the OpenSearch cluster\'s warm nodes.
--
-- 'instanceType', 'clusterConfig_instanceType' - The instance type for an OpenSearch cluster. UltraWarm instance types
-- are not supported for data instances.
--
-- 'zoneAwarenessConfig', 'clusterConfig_zoneAwarenessConfig' - The zone awareness configuration for a domain when zone awareness is
-- enabled.
--
-- 'instanceCount', 'clusterConfig_instanceCount' - The number of instances in the specified domain cluster.
--
-- 'warmEnabled', 'clusterConfig_warmEnabled' - True to enable UltraWarm storage.
--
-- 'dedicatedMasterCount', 'clusterConfig_dedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the
-- cluster.
newClusterConfig ::
  ClusterConfig
newClusterConfig =
  ClusterConfig'
    { warmCount = Prelude.Nothing,
      coldStorageOptions = Prelude.Nothing,
      dedicatedMasterType = Prelude.Nothing,
      zoneAwarenessEnabled = Prelude.Nothing,
      dedicatedMasterEnabled = Prelude.Nothing,
      warmType = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      zoneAwarenessConfig = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      warmEnabled = Prelude.Nothing,
      dedicatedMasterCount = Prelude.Nothing
    }

-- | The number of UltraWarm nodes in the cluster.
clusterConfig_warmCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_warmCount = Lens.lens (\ClusterConfig' {warmCount} -> warmCount) (\s@ClusterConfig' {} a -> s {warmCount = a} :: ClusterConfig)

-- | Specifies the @ColdStorageOptions@ config for a Domain
clusterConfig_coldStorageOptions :: Lens.Lens' ClusterConfig (Prelude.Maybe ColdStorageOptions)
clusterConfig_coldStorageOptions = Lens.lens (\ClusterConfig' {coldStorageOptions} -> coldStorageOptions) (\s@ClusterConfig' {} a -> s {coldStorageOptions = a} :: ClusterConfig)

-- | The instance type for a dedicated master node.
clusterConfig_dedicatedMasterType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_dedicatedMasterType = Lens.lens (\ClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ClusterConfig' {} a -> s {dedicatedMasterType = a} :: ClusterConfig)

-- | A boolean value to indicate whether zone awareness is enabled. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-multiaz.html Configuring a multi-AZ domain in Amazon OpenSearch Service>
-- for more information.
clusterConfig_zoneAwarenessEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_zoneAwarenessEnabled = Lens.lens (\ClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ClusterConfig)

-- | A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-dedicatedmasternodes Dedicated master nodes in Amazon OpenSearch Service>
-- for more information.
clusterConfig_dedicatedMasterEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_dedicatedMasterEnabled = Lens.lens (\ClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ClusterConfig)

-- | The instance type for the OpenSearch cluster\'s warm nodes.
clusterConfig_warmType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchWarmPartitionInstanceType)
clusterConfig_warmType = Lens.lens (\ClusterConfig' {warmType} -> warmType) (\s@ClusterConfig' {} a -> s {warmType = a} :: ClusterConfig)

-- | The instance type for an OpenSearch cluster. UltraWarm instance types
-- are not supported for data instances.
clusterConfig_instanceType :: Lens.Lens' ClusterConfig (Prelude.Maybe OpenSearchPartitionInstanceType)
clusterConfig_instanceType = Lens.lens (\ClusterConfig' {instanceType} -> instanceType) (\s@ClusterConfig' {} a -> s {instanceType = a} :: ClusterConfig)

-- | The zone awareness configuration for a domain when zone awareness is
-- enabled.
clusterConfig_zoneAwarenessConfig :: Lens.Lens' ClusterConfig (Prelude.Maybe ZoneAwarenessConfig)
clusterConfig_zoneAwarenessConfig = Lens.lens (\ClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ClusterConfig)

-- | The number of instances in the specified domain cluster.
clusterConfig_instanceCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_instanceCount = Lens.lens (\ClusterConfig' {instanceCount} -> instanceCount) (\s@ClusterConfig' {} a -> s {instanceCount = a} :: ClusterConfig)

-- | True to enable UltraWarm storage.
clusterConfig_warmEnabled :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Bool)
clusterConfig_warmEnabled = Lens.lens (\ClusterConfig' {warmEnabled} -> warmEnabled) (\s@ClusterConfig' {} a -> s {warmEnabled = a} :: ClusterConfig)

-- | Total number of dedicated master nodes, active and on standby, for the
-- cluster.
clusterConfig_dedicatedMasterCount :: Lens.Lens' ClusterConfig (Prelude.Maybe Prelude.Int)
clusterConfig_dedicatedMasterCount = Lens.lens (\ClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ClusterConfig)

instance Core.FromJSON ClusterConfig where
  parseJSON =
    Core.withObject
      "ClusterConfig"
      ( \x ->
          ClusterConfig'
            Prelude.<$> (x Core..:? "WarmCount")
            Prelude.<*> (x Core..:? "ColdStorageOptions")
            Prelude.<*> (x Core..:? "DedicatedMasterType")
            Prelude.<*> (x Core..:? "ZoneAwarenessEnabled")
            Prelude.<*> (x Core..:? "DedicatedMasterEnabled")
            Prelude.<*> (x Core..:? "WarmType")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "ZoneAwarenessConfig")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "WarmEnabled")
            Prelude.<*> (x Core..:? "DedicatedMasterCount")
      )

instance Prelude.Hashable ClusterConfig where
  hashWithSalt _salt ClusterConfig' {..} =
    _salt `Prelude.hashWithSalt` warmCount
      `Prelude.hashWithSalt` coldStorageOptions
      `Prelude.hashWithSalt` dedicatedMasterType
      `Prelude.hashWithSalt` zoneAwarenessEnabled
      `Prelude.hashWithSalt` dedicatedMasterEnabled
      `Prelude.hashWithSalt` warmType
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` zoneAwarenessConfig
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` warmEnabled
      `Prelude.hashWithSalt` dedicatedMasterCount

instance Prelude.NFData ClusterConfig where
  rnf ClusterConfig' {..} =
    Prelude.rnf warmCount
      `Prelude.seq` Prelude.rnf coldStorageOptions
      `Prelude.seq` Prelude.rnf dedicatedMasterType
      `Prelude.seq` Prelude.rnf zoneAwarenessEnabled
      `Prelude.seq` Prelude.rnf dedicatedMasterEnabled
      `Prelude.seq` Prelude.rnf warmType
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf zoneAwarenessConfig
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf warmEnabled
      `Prelude.seq` Prelude.rnf dedicatedMasterCount

instance Core.ToJSON ClusterConfig where
  toJSON ClusterConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WarmCount" Core..=) Prelude.<$> warmCount,
            ("ColdStorageOptions" Core..=)
              Prelude.<$> coldStorageOptions,
            ("DedicatedMasterType" Core..=)
              Prelude.<$> dedicatedMasterType,
            ("ZoneAwarenessEnabled" Core..=)
              Prelude.<$> zoneAwarenessEnabled,
            ("DedicatedMasterEnabled" Core..=)
              Prelude.<$> dedicatedMasterEnabled,
            ("WarmType" Core..=) Prelude.<$> warmType,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("ZoneAwarenessConfig" Core..=)
              Prelude.<$> zoneAwarenessConfig,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("WarmEnabled" Core..=) Prelude.<$> warmEnabled,
            ("DedicatedMasterCount" Core..=)
              Prelude.<$> dedicatedMasterCount
          ]
      )
