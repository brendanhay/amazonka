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
-- Module      : Amazonka.ElasticSearch.Types.ElasticsearchClusterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ElasticsearchClusterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticSearch.Types.ColdStorageOptions
import Amazonka.ElasticSearch.Types.ESPartitionInstanceType
import Amazonka.ElasticSearch.Types.ESWarmPartitionInstanceType
import Amazonka.ElasticSearch.Types.ZoneAwarenessConfig
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for the domain cluster, such as the type and
-- number of instances.
--
-- /See:/ 'newElasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
  { -- | The number of warm nodes in the cluster.
    warmCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the @ColdStorageOptions@ config for Elasticsearch Domain
    coldStorageOptions :: Prelude.Maybe ColdStorageOptions,
    -- | The instance type for a dedicated master node.
    dedicatedMasterType :: Prelude.Maybe ESPartitionInstanceType,
    -- | A boolean value to indicate whether zone awareness is enabled. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness>
    -- for more information.
    zoneAwarenessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value to indicate whether a dedicated master node is enabled.
    -- See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes>
    -- for more information.
    dedicatedMasterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The instance type for the Elasticsearch cluster\'s warm nodes.
    warmType :: Prelude.Maybe ESWarmPartitionInstanceType,
    -- | The instance type for an Elasticsearch cluster. UltraWarm instance types
    -- are not supported for data instances.
    instanceType :: Prelude.Maybe ESPartitionInstanceType,
    -- | Specifies the zone awareness configuration for a domain when zone
    -- awareness is enabled.
    zoneAwarenessConfig :: Prelude.Maybe ZoneAwarenessConfig,
    -- | The number of instances in the specified domain cluster.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | True to enable warm storage.
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Total number of dedicated master nodes, active and on standby, for the
    -- cluster.
    dedicatedMasterCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warmCount', 'elasticsearchClusterConfig_warmCount' - The number of warm nodes in the cluster.
--
-- 'coldStorageOptions', 'elasticsearchClusterConfig_coldStorageOptions' - Specifies the @ColdStorageOptions@ config for Elasticsearch Domain
--
-- 'dedicatedMasterType', 'elasticsearchClusterConfig_dedicatedMasterType' - The instance type for a dedicated master node.
--
-- 'zoneAwarenessEnabled', 'elasticsearchClusterConfig_zoneAwarenessEnabled' - A boolean value to indicate whether zone awareness is enabled. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness>
-- for more information.
--
-- 'dedicatedMasterEnabled', 'elasticsearchClusterConfig_dedicatedMasterEnabled' - A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes>
-- for more information.
--
-- 'warmType', 'elasticsearchClusterConfig_warmType' - The instance type for the Elasticsearch cluster\'s warm nodes.
--
-- 'instanceType', 'elasticsearchClusterConfig_instanceType' - The instance type for an Elasticsearch cluster. UltraWarm instance types
-- are not supported for data instances.
--
-- 'zoneAwarenessConfig', 'elasticsearchClusterConfig_zoneAwarenessConfig' - Specifies the zone awareness configuration for a domain when zone
-- awareness is enabled.
--
-- 'instanceCount', 'elasticsearchClusterConfig_instanceCount' - The number of instances in the specified domain cluster.
--
-- 'warmEnabled', 'elasticsearchClusterConfig_warmEnabled' - True to enable warm storage.
--
-- 'dedicatedMasterCount', 'elasticsearchClusterConfig_dedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the
-- cluster.
newElasticsearchClusterConfig ::
  ElasticsearchClusterConfig
newElasticsearchClusterConfig =
  ElasticsearchClusterConfig'
    { warmCount =
        Prelude.Nothing,
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

-- | The number of warm nodes in the cluster.
elasticsearchClusterConfig_warmCount :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Int)
elasticsearchClusterConfig_warmCount = Lens.lens (\ElasticsearchClusterConfig' {warmCount} -> warmCount) (\s@ElasticsearchClusterConfig' {} a -> s {warmCount = a} :: ElasticsearchClusterConfig)

-- | Specifies the @ColdStorageOptions@ config for Elasticsearch Domain
elasticsearchClusterConfig_coldStorageOptions :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe ColdStorageOptions)
elasticsearchClusterConfig_coldStorageOptions = Lens.lens (\ElasticsearchClusterConfig' {coldStorageOptions} -> coldStorageOptions) (\s@ElasticsearchClusterConfig' {} a -> s {coldStorageOptions = a} :: ElasticsearchClusterConfig)

-- | The instance type for a dedicated master node.
elasticsearchClusterConfig_dedicatedMasterType :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe ESPartitionInstanceType)
elasticsearchClusterConfig_dedicatedMasterType = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterType = a} :: ElasticsearchClusterConfig)

-- | A boolean value to indicate whether zone awareness is enabled. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness>
-- for more information.
elasticsearchClusterConfig_zoneAwarenessEnabled :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Bool)
elasticsearchClusterConfig_zoneAwarenessEnabled = Lens.lens (\ElasticsearchClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ElasticsearchClusterConfig)

-- | A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes>
-- for more information.
elasticsearchClusterConfig_dedicatedMasterEnabled :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Bool)
elasticsearchClusterConfig_dedicatedMasterEnabled = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ElasticsearchClusterConfig)

-- | The instance type for the Elasticsearch cluster\'s warm nodes.
elasticsearchClusterConfig_warmType :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe ESWarmPartitionInstanceType)
elasticsearchClusterConfig_warmType = Lens.lens (\ElasticsearchClusterConfig' {warmType} -> warmType) (\s@ElasticsearchClusterConfig' {} a -> s {warmType = a} :: ElasticsearchClusterConfig)

-- | The instance type for an Elasticsearch cluster. UltraWarm instance types
-- are not supported for data instances.
elasticsearchClusterConfig_instanceType :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe ESPartitionInstanceType)
elasticsearchClusterConfig_instanceType = Lens.lens (\ElasticsearchClusterConfig' {instanceType} -> instanceType) (\s@ElasticsearchClusterConfig' {} a -> s {instanceType = a} :: ElasticsearchClusterConfig)

-- | Specifies the zone awareness configuration for a domain when zone
-- awareness is enabled.
elasticsearchClusterConfig_zoneAwarenessConfig :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe ZoneAwarenessConfig)
elasticsearchClusterConfig_zoneAwarenessConfig = Lens.lens (\ElasticsearchClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ElasticsearchClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ElasticsearchClusterConfig)

-- | The number of instances in the specified domain cluster.
elasticsearchClusterConfig_instanceCount :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Int)
elasticsearchClusterConfig_instanceCount = Lens.lens (\ElasticsearchClusterConfig' {instanceCount} -> instanceCount) (\s@ElasticsearchClusterConfig' {} a -> s {instanceCount = a} :: ElasticsearchClusterConfig)

-- | True to enable warm storage.
elasticsearchClusterConfig_warmEnabled :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Bool)
elasticsearchClusterConfig_warmEnabled = Lens.lens (\ElasticsearchClusterConfig' {warmEnabled} -> warmEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {warmEnabled = a} :: ElasticsearchClusterConfig)

-- | Total number of dedicated master nodes, active and on standby, for the
-- cluster.
elasticsearchClusterConfig_dedicatedMasterCount :: Lens.Lens' ElasticsearchClusterConfig (Prelude.Maybe Prelude.Int)
elasticsearchClusterConfig_dedicatedMasterCount = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ElasticsearchClusterConfig)

instance Core.FromJSON ElasticsearchClusterConfig where
  parseJSON =
    Core.withObject
      "ElasticsearchClusterConfig"
      ( \x ->
          ElasticsearchClusterConfig'
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

instance Prelude.Hashable ElasticsearchClusterConfig where
  hashWithSalt _salt ElasticsearchClusterConfig' {..} =
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

instance Prelude.NFData ElasticsearchClusterConfig where
  rnf ElasticsearchClusterConfig' {..} =
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

instance Core.ToJSON ElasticsearchClusterConfig where
  toJSON ElasticsearchClusterConfig' {..} =
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
