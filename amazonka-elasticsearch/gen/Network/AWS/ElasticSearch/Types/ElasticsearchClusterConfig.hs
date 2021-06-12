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
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import qualified Network.AWS.Lens as Lens

-- | Specifies the configuration for the domain cluster, such as the type and
-- number of instances.
--
-- /See:/ 'newElasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
  { -- | Specifies the zone awareness configuration for a domain when zone
    -- awareness is enabled.
    zoneAwarenessConfig :: Core.Maybe ZoneAwarenessConfig,
    -- | Total number of dedicated master nodes, active and on standby, for the
    -- cluster.
    dedicatedMasterCount :: Core.Maybe Core.Int,
    -- | True to enable warm storage.
    warmEnabled :: Core.Maybe Core.Bool,
    -- | The instance type for an Elasticsearch cluster. UltraWarm instance types
    -- are not supported for data instances.
    instanceType :: Core.Maybe ESPartitionInstanceType,
    -- | A boolean value to indicate whether zone awareness is enabled. See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness>
    -- for more information.
    zoneAwarenessEnabled :: Core.Maybe Core.Bool,
    -- | A boolean value to indicate whether a dedicated master node is enabled.
    -- See
    -- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes>
    -- for more information.
    dedicatedMasterEnabled :: Core.Maybe Core.Bool,
    -- | The number of warm nodes in the cluster.
    warmCount :: Core.Maybe Core.Int,
    -- | The instance type for a dedicated master node.
    dedicatedMasterType :: Core.Maybe ESPartitionInstanceType,
    -- | The instance type for the Elasticsearch cluster\'s warm nodes.
    warmType :: Core.Maybe ESWarmPartitionInstanceType,
    -- | The number of instances in the specified domain cluster.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zoneAwarenessConfig', 'elasticsearchClusterConfig_zoneAwarenessConfig' - Specifies the zone awareness configuration for a domain when zone
-- awareness is enabled.
--
-- 'dedicatedMasterCount', 'elasticsearchClusterConfig_dedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the
-- cluster.
--
-- 'warmEnabled', 'elasticsearchClusterConfig_warmEnabled' - True to enable warm storage.
--
-- 'instanceType', 'elasticsearchClusterConfig_instanceType' - The instance type for an Elasticsearch cluster. UltraWarm instance types
-- are not supported for data instances.
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
-- 'warmCount', 'elasticsearchClusterConfig_warmCount' - The number of warm nodes in the cluster.
--
-- 'dedicatedMasterType', 'elasticsearchClusterConfig_dedicatedMasterType' - The instance type for a dedicated master node.
--
-- 'warmType', 'elasticsearchClusterConfig_warmType' - The instance type for the Elasticsearch cluster\'s warm nodes.
--
-- 'instanceCount', 'elasticsearchClusterConfig_instanceCount' - The number of instances in the specified domain cluster.
newElasticsearchClusterConfig ::
  ElasticsearchClusterConfig
newElasticsearchClusterConfig =
  ElasticsearchClusterConfig'
    { zoneAwarenessConfig =
        Core.Nothing,
      dedicatedMasterCount = Core.Nothing,
      warmEnabled = Core.Nothing,
      instanceType = Core.Nothing,
      zoneAwarenessEnabled = Core.Nothing,
      dedicatedMasterEnabled = Core.Nothing,
      warmCount = Core.Nothing,
      dedicatedMasterType = Core.Nothing,
      warmType = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | Specifies the zone awareness configuration for a domain when zone
-- awareness is enabled.
elasticsearchClusterConfig_zoneAwarenessConfig :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe ZoneAwarenessConfig)
elasticsearchClusterConfig_zoneAwarenessConfig = Lens.lens (\ElasticsearchClusterConfig' {zoneAwarenessConfig} -> zoneAwarenessConfig) (\s@ElasticsearchClusterConfig' {} a -> s {zoneAwarenessConfig = a} :: ElasticsearchClusterConfig)

-- | Total number of dedicated master nodes, active and on standby, for the
-- cluster.
elasticsearchClusterConfig_dedicatedMasterCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
elasticsearchClusterConfig_dedicatedMasterCount = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterCount} -> dedicatedMasterCount) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterCount = a} :: ElasticsearchClusterConfig)

-- | True to enable warm storage.
elasticsearchClusterConfig_warmEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
elasticsearchClusterConfig_warmEnabled = Lens.lens (\ElasticsearchClusterConfig' {warmEnabled} -> warmEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {warmEnabled = a} :: ElasticsearchClusterConfig)

-- | The instance type for an Elasticsearch cluster. UltraWarm instance types
-- are not supported for data instances.
elasticsearchClusterConfig_instanceType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe ESPartitionInstanceType)
elasticsearchClusterConfig_instanceType = Lens.lens (\ElasticsearchClusterConfig' {instanceType} -> instanceType) (\s@ElasticsearchClusterConfig' {} a -> s {instanceType = a} :: ElasticsearchClusterConfig)

-- | A boolean value to indicate whether zone awareness is enabled. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness>
-- for more information.
elasticsearchClusterConfig_zoneAwarenessEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
elasticsearchClusterConfig_zoneAwarenessEnabled = Lens.lens (\ElasticsearchClusterConfig' {zoneAwarenessEnabled} -> zoneAwarenessEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {zoneAwarenessEnabled = a} :: ElasticsearchClusterConfig)

-- | A boolean value to indicate whether a dedicated master node is enabled.
-- See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes>
-- for more information.
elasticsearchClusterConfig_dedicatedMasterEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
elasticsearchClusterConfig_dedicatedMasterEnabled = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterEnabled} -> dedicatedMasterEnabled) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterEnabled = a} :: ElasticsearchClusterConfig)

-- | The number of warm nodes in the cluster.
elasticsearchClusterConfig_warmCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
elasticsearchClusterConfig_warmCount = Lens.lens (\ElasticsearchClusterConfig' {warmCount} -> warmCount) (\s@ElasticsearchClusterConfig' {} a -> s {warmCount = a} :: ElasticsearchClusterConfig)

-- | The instance type for a dedicated master node.
elasticsearchClusterConfig_dedicatedMasterType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe ESPartitionInstanceType)
elasticsearchClusterConfig_dedicatedMasterType = Lens.lens (\ElasticsearchClusterConfig' {dedicatedMasterType} -> dedicatedMasterType) (\s@ElasticsearchClusterConfig' {} a -> s {dedicatedMasterType = a} :: ElasticsearchClusterConfig)

-- | The instance type for the Elasticsearch cluster\'s warm nodes.
elasticsearchClusterConfig_warmType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe ESWarmPartitionInstanceType)
elasticsearchClusterConfig_warmType = Lens.lens (\ElasticsearchClusterConfig' {warmType} -> warmType) (\s@ElasticsearchClusterConfig' {} a -> s {warmType = a} :: ElasticsearchClusterConfig)

-- | The number of instances in the specified domain cluster.
elasticsearchClusterConfig_instanceCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
elasticsearchClusterConfig_instanceCount = Lens.lens (\ElasticsearchClusterConfig' {instanceCount} -> instanceCount) (\s@ElasticsearchClusterConfig' {} a -> s {instanceCount = a} :: ElasticsearchClusterConfig)

instance Core.FromJSON ElasticsearchClusterConfig where
  parseJSON =
    Core.withObject
      "ElasticsearchClusterConfig"
      ( \x ->
          ElasticsearchClusterConfig'
            Core.<$> (x Core..:? "ZoneAwarenessConfig")
            Core.<*> (x Core..:? "DedicatedMasterCount")
            Core.<*> (x Core..:? "WarmEnabled")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "ZoneAwarenessEnabled")
            Core.<*> (x Core..:? "DedicatedMasterEnabled")
            Core.<*> (x Core..:? "WarmCount")
            Core.<*> (x Core..:? "DedicatedMasterType")
            Core.<*> (x Core..:? "WarmType")
            Core.<*> (x Core..:? "InstanceCount")
      )

instance Core.Hashable ElasticsearchClusterConfig

instance Core.NFData ElasticsearchClusterConfig

instance Core.ToJSON ElasticsearchClusterConfig where
  toJSON ElasticsearchClusterConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ZoneAwarenessConfig" Core..=)
              Core.<$> zoneAwarenessConfig,
            ("DedicatedMasterCount" Core..=)
              Core.<$> dedicatedMasterCount,
            ("WarmEnabled" Core..=) Core.<$> warmEnabled,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("ZoneAwarenessEnabled" Core..=)
              Core.<$> zoneAwarenessEnabled,
            ("DedicatedMasterEnabled" Core..=)
              Core.<$> dedicatedMasterEnabled,
            ("WarmCount" Core..=) Core.<$> warmCount,
            ("DedicatedMasterType" Core..=)
              Core.<$> dedicatedMasterType,
            ("WarmType" Core..=) Core.<$> warmType,
            ("InstanceCount" Core..=) Core.<$> instanceCount
          ]
      )
