{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
  ( ElasticsearchClusterConfig (..),

    -- * Smart constructor
    mkElasticsearchClusterConfig,

    -- * Lenses
    eccDedicatedMasterCount,
    eccDedicatedMasterEnabled,
    eccDedicatedMasterType,
    eccInstanceCount,
    eccInstanceType,
    eccWarmCount,
    eccWarmEnabled,
    eccWarmType,
    eccZoneAwarenessConfig,
    eccZoneAwarenessEnabled,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ESPartitionInstanceType as Types
import qualified Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType as Types
import qualified Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration for the domain cluster, such as the type and number of instances.
--
-- /See:/ 'mkElasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
  { -- | Total number of dedicated master nodes, active and on standby, for the cluster.
    dedicatedMasterCount :: Core.Maybe Core.Int,
    -- | A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
    dedicatedMasterEnabled :: Core.Maybe Core.Bool,
    -- | The instance type for a dedicated master node.
    dedicatedMasterType :: Core.Maybe Types.ESPartitionInstanceType,
    -- | The number of instances in the specified domain cluster.
    instanceCount :: Core.Maybe Core.Int,
    -- | The instance type for an Elasticsearch cluster. UltraWarm instance types are not supported for data instances.
    instanceType :: Core.Maybe Types.ESPartitionInstanceType,
    -- | The number of warm nodes in the cluster.
    warmCount :: Core.Maybe Core.Int,
    -- | True to enable warm storage.
    warmEnabled :: Core.Maybe Core.Bool,
    -- | The instance type for the Elasticsearch cluster's warm nodes.
    warmType :: Core.Maybe Types.ESWarmPartitionInstanceType,
    -- | Specifies the zone awareness configuration for a domain when zone awareness is enabled.
    zoneAwarenessConfig :: Core.Maybe Types.ZoneAwarenessConfig,
    -- | A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
    zoneAwarenessEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchClusterConfig' value with any optional fields omitted.
mkElasticsearchClusterConfig ::
  ElasticsearchClusterConfig
mkElasticsearchClusterConfig =
  ElasticsearchClusterConfig'
    { dedicatedMasterCount = Core.Nothing,
      dedicatedMasterEnabled = Core.Nothing,
      dedicatedMasterType = Core.Nothing,
      instanceCount = Core.Nothing,
      instanceType = Core.Nothing,
      warmCount = Core.Nothing,
      warmEnabled = Core.Nothing,
      warmType = Core.Nothing,
      zoneAwarenessConfig = Core.Nothing,
      zoneAwarenessEnabled = Core.Nothing
    }

-- | Total number of dedicated master nodes, active and on standby, for the cluster.
--
-- /Note:/ Consider using 'dedicatedMasterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
eccDedicatedMasterCount = Lens.field @"dedicatedMasterCount"
{-# DEPRECATED eccDedicatedMasterCount "Use generic-lens or generic-optics with 'dedicatedMasterCount' instead." #-}

-- | A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
--
-- /Note:/ Consider using 'dedicatedMasterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
eccDedicatedMasterEnabled = Lens.field @"dedicatedMasterEnabled"
{-# DEPRECATED eccDedicatedMasterEnabled "Use generic-lens or generic-optics with 'dedicatedMasterEnabled' instead." #-}

-- | The instance type for a dedicated master node.
--
-- /Note:/ Consider using 'dedicatedMasterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Types.ESPartitionInstanceType)
eccDedicatedMasterType = Lens.field @"dedicatedMasterType"
{-# DEPRECATED eccDedicatedMasterType "Use generic-lens or generic-optics with 'dedicatedMasterType' instead." #-}

-- | The number of instances in the specified domain cluster.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccInstanceCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
eccInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED eccInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The instance type for an Elasticsearch cluster. UltraWarm instance types are not supported for data instances.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccInstanceType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Types.ESPartitionInstanceType)
eccInstanceType = Lens.field @"instanceType"
{-# DEPRECATED eccInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of warm nodes in the cluster.
--
-- /Note:/ Consider using 'warmCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmCount :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Int)
eccWarmCount = Lens.field @"warmCount"
{-# DEPRECATED eccWarmCount "Use generic-lens or generic-optics with 'warmCount' instead." #-}

-- | True to enable warm storage.
--
-- /Note:/ Consider using 'warmEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
eccWarmEnabled = Lens.field @"warmEnabled"
{-# DEPRECATED eccWarmEnabled "Use generic-lens or generic-optics with 'warmEnabled' instead." #-}

-- | The instance type for the Elasticsearch cluster's warm nodes.
--
-- /Note:/ Consider using 'warmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmType :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Types.ESWarmPartitionInstanceType)
eccWarmType = Lens.field @"warmType"
{-# DEPRECATED eccWarmType "Use generic-lens or generic-optics with 'warmType' instead." #-}

-- | Specifies the zone awareness configuration for a domain when zone awareness is enabled.
--
-- /Note:/ Consider using 'zoneAwarenessConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccZoneAwarenessConfig :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Types.ZoneAwarenessConfig)
eccZoneAwarenessConfig = Lens.field @"zoneAwarenessConfig"
{-# DEPRECATED eccZoneAwarenessConfig "Use generic-lens or generic-optics with 'zoneAwarenessConfig' instead." #-}

-- | A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
--
-- /Note:/ Consider using 'zoneAwarenessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccZoneAwarenessEnabled :: Lens.Lens' ElasticsearchClusterConfig (Core.Maybe Core.Bool)
eccZoneAwarenessEnabled = Lens.field @"zoneAwarenessEnabled"
{-# DEPRECATED eccZoneAwarenessEnabled "Use generic-lens or generic-optics with 'zoneAwarenessEnabled' instead." #-}

instance Core.FromJSON ElasticsearchClusterConfig where
  toJSON ElasticsearchClusterConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("DedicatedMasterCount" Core..=) Core.<$> dedicatedMasterCount,
            ("DedicatedMasterEnabled" Core..=) Core.<$> dedicatedMasterEnabled,
            ("DedicatedMasterType" Core..=) Core.<$> dedicatedMasterType,
            ("InstanceCount" Core..=) Core.<$> instanceCount,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("WarmCount" Core..=) Core.<$> warmCount,
            ("WarmEnabled" Core..=) Core.<$> warmEnabled,
            ("WarmType" Core..=) Core.<$> warmType,
            ("ZoneAwarenessConfig" Core..=) Core.<$> zoneAwarenessConfig,
            ("ZoneAwarenessEnabled" Core..=) Core.<$> zoneAwarenessEnabled
          ]
      )

instance Core.FromJSON ElasticsearchClusterConfig where
  parseJSON =
    Core.withObject "ElasticsearchClusterConfig" Core.$
      \x ->
        ElasticsearchClusterConfig'
          Core.<$> (x Core..:? "DedicatedMasterCount")
          Core.<*> (x Core..:? "DedicatedMasterEnabled")
          Core.<*> (x Core..:? "DedicatedMasterType")
          Core.<*> (x Core..:? "InstanceCount")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "WarmCount")
          Core.<*> (x Core..:? "WarmEnabled")
          Core.<*> (x Core..:? "WarmType")
          Core.<*> (x Core..:? "ZoneAwarenessConfig")
          Core.<*> (x Core..:? "ZoneAwarenessEnabled")
