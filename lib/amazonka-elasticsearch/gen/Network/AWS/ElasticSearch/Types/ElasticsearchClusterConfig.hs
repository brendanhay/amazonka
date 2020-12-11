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
    eccDedicatedMasterType,
    eccDedicatedMasterEnabled,
    eccInstanceCount,
    eccZoneAwarenessEnabled,
    eccInstanceType,
    eccWarmEnabled,
    eccZoneAwarenessConfig,
    eccWarmCount,
    eccWarmType,
  )
where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration for the domain cluster, such as the type and number of instances.
--
-- /See:/ 'mkElasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
  { dedicatedMasterCount ::
      Lude.Maybe Lude.Int,
    dedicatedMasterType ::
      Lude.Maybe ESPartitionInstanceType,
    dedicatedMasterEnabled ::
      Lude.Maybe Lude.Bool,
    instanceCount :: Lude.Maybe Lude.Int,
    zoneAwarenessEnabled ::
      Lude.Maybe Lude.Bool,
    instanceType ::
      Lude.Maybe ESPartitionInstanceType,
    warmEnabled :: Lude.Maybe Lude.Bool,
    zoneAwarenessConfig ::
      Lude.Maybe ZoneAwarenessConfig,
    warmCount :: Lude.Maybe Lude.Int,
    warmType ::
      Lude.Maybe
        ESWarmPartitionInstanceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchClusterConfig' with the minimum fields required to make a request.
--
-- * 'dedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the cluster.
-- * 'dedicatedMasterEnabled' - A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
-- * 'dedicatedMasterType' - The instance type for a dedicated master node.
-- * 'instanceCount' - The number of instances in the specified domain cluster.
-- * 'instanceType' - The instance type for an Elasticsearch cluster. UltraWarm instance types are not supported for data instances.
-- * 'warmCount' - The number of warm nodes in the cluster.
-- * 'warmEnabled' - True to enable warm storage.
-- * 'warmType' - The instance type for the Elasticsearch cluster's warm nodes.
-- * 'zoneAwarenessConfig' - Specifies the zone awareness configuration for a domain when zone awareness is enabled.
-- * 'zoneAwarenessEnabled' - A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
mkElasticsearchClusterConfig ::
  ElasticsearchClusterConfig
mkElasticsearchClusterConfig =
  ElasticsearchClusterConfig'
    { dedicatedMasterCount = Lude.Nothing,
      dedicatedMasterType = Lude.Nothing,
      dedicatedMasterEnabled = Lude.Nothing,
      instanceCount = Lude.Nothing,
      zoneAwarenessEnabled = Lude.Nothing,
      instanceType = Lude.Nothing,
      warmEnabled = Lude.Nothing,
      zoneAwarenessConfig = Lude.Nothing,
      warmCount = Lude.Nothing,
      warmType = Lude.Nothing
    }

-- | Total number of dedicated master nodes, active and on standby, for the cluster.
--
-- /Note:/ Consider using 'dedicatedMasterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterCount :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Int)
eccDedicatedMasterCount = Lens.lens (dedicatedMasterCount :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Int) (\s a -> s {dedicatedMasterCount = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccDedicatedMasterCount "Use generic-lens or generic-optics with 'dedicatedMasterCount' instead." #-}

-- | The instance type for a dedicated master node.
--
-- /Note:/ Consider using 'dedicatedMasterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterType :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe ESPartitionInstanceType)
eccDedicatedMasterType = Lens.lens (dedicatedMasterType :: ElasticsearchClusterConfig -> Lude.Maybe ESPartitionInstanceType) (\s a -> s {dedicatedMasterType = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccDedicatedMasterType "Use generic-lens or generic-optics with 'dedicatedMasterType' instead." #-}

-- | A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
--
-- /Note:/ Consider using 'dedicatedMasterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccDedicatedMasterEnabled :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Bool)
eccDedicatedMasterEnabled = Lens.lens (dedicatedMasterEnabled :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Bool) (\s a -> s {dedicatedMasterEnabled = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccDedicatedMasterEnabled "Use generic-lens or generic-optics with 'dedicatedMasterEnabled' instead." #-}

-- | The number of instances in the specified domain cluster.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccInstanceCount :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Int)
eccInstanceCount = Lens.lens (instanceCount :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
--
-- /Note:/ Consider using 'zoneAwarenessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccZoneAwarenessEnabled :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Bool)
eccZoneAwarenessEnabled = Lens.lens (zoneAwarenessEnabled :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Bool) (\s a -> s {zoneAwarenessEnabled = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccZoneAwarenessEnabled "Use generic-lens or generic-optics with 'zoneAwarenessEnabled' instead." #-}

-- | The instance type for an Elasticsearch cluster. UltraWarm instance types are not supported for data instances.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccInstanceType :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe ESPartitionInstanceType)
eccInstanceType = Lens.lens (instanceType :: ElasticsearchClusterConfig -> Lude.Maybe ESPartitionInstanceType) (\s a -> s {instanceType = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | True to enable warm storage.
--
-- /Note:/ Consider using 'warmEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmEnabled :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Bool)
eccWarmEnabled = Lens.lens (warmEnabled :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Bool) (\s a -> s {warmEnabled = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccWarmEnabled "Use generic-lens or generic-optics with 'warmEnabled' instead." #-}

-- | Specifies the zone awareness configuration for a domain when zone awareness is enabled.
--
-- /Note:/ Consider using 'zoneAwarenessConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccZoneAwarenessConfig :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe ZoneAwarenessConfig)
eccZoneAwarenessConfig = Lens.lens (zoneAwarenessConfig :: ElasticsearchClusterConfig -> Lude.Maybe ZoneAwarenessConfig) (\s a -> s {zoneAwarenessConfig = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccZoneAwarenessConfig "Use generic-lens or generic-optics with 'zoneAwarenessConfig' instead." #-}

-- | The number of warm nodes in the cluster.
--
-- /Note:/ Consider using 'warmCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmCount :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe Lude.Int)
eccWarmCount = Lens.lens (warmCount :: ElasticsearchClusterConfig -> Lude.Maybe Lude.Int) (\s a -> s {warmCount = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccWarmCount "Use generic-lens or generic-optics with 'warmCount' instead." #-}

-- | The instance type for the Elasticsearch cluster's warm nodes.
--
-- /Note:/ Consider using 'warmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccWarmType :: Lens.Lens' ElasticsearchClusterConfig (Lude.Maybe ESWarmPartitionInstanceType)
eccWarmType = Lens.lens (warmType :: ElasticsearchClusterConfig -> Lude.Maybe ESWarmPartitionInstanceType) (\s a -> s {warmType = a} :: ElasticsearchClusterConfig)
{-# DEPRECATED eccWarmType "Use generic-lens or generic-optics with 'warmType' instead." #-}

instance Lude.FromJSON ElasticsearchClusterConfig where
  parseJSON =
    Lude.withObject
      "ElasticsearchClusterConfig"
      ( \x ->
          ElasticsearchClusterConfig'
            Lude.<$> (x Lude..:? "DedicatedMasterCount")
            Lude.<*> (x Lude..:? "DedicatedMasterType")
            Lude.<*> (x Lude..:? "DedicatedMasterEnabled")
            Lude.<*> (x Lude..:? "InstanceCount")
            Lude.<*> (x Lude..:? "ZoneAwarenessEnabled")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "WarmEnabled")
            Lude.<*> (x Lude..:? "ZoneAwarenessConfig")
            Lude.<*> (x Lude..:? "WarmCount")
            Lude.<*> (x Lude..:? "WarmType")
      )

instance Lude.ToJSON ElasticsearchClusterConfig where
  toJSON ElasticsearchClusterConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DedicatedMasterCount" Lude..=) Lude.<$> dedicatedMasterCount,
            ("DedicatedMasterType" Lude..=) Lude.<$> dedicatedMasterType,
            ("DedicatedMasterEnabled" Lude..=) Lude.<$> dedicatedMasterEnabled,
            ("InstanceCount" Lude..=) Lude.<$> instanceCount,
            ("ZoneAwarenessEnabled" Lude..=) Lude.<$> zoneAwarenessEnabled,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("WarmEnabled" Lude..=) Lude.<$> warmEnabled,
            ("ZoneAwarenessConfig" Lude..=) Lude.<$> zoneAwarenessConfig,
            ("WarmCount" Lude..=) Lude.<$> warmCount,
            ("WarmType" Lude..=) Lude.<$> warmType
          ]
      )
