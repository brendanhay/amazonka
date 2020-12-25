{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
  ( ElasticsearchClusterConfigStatus (..),

    -- * Smart constructor
    mkElasticsearchClusterConfigStatus,

    -- * Lenses
    eccsOptions,
    eccsStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration status for the specified Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
  { -- | Specifies the cluster configuration for the specified Elasticsearch domain.
    options :: Types.ElasticsearchClusterConfig,
    -- | Specifies the status of the configuration for the specified Elasticsearch domain.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ElasticsearchClusterConfigStatus' value with any optional fields omitted.
mkElasticsearchClusterConfigStatus ::
  -- | 'options'
  Types.ElasticsearchClusterConfig ->
  -- | 'status'
  Types.OptionStatus ->
  ElasticsearchClusterConfigStatus
mkElasticsearchClusterConfigStatus options status =
  ElasticsearchClusterConfigStatus' {options, status}

-- | Specifies the cluster configuration for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccsOptions :: Lens.Lens' ElasticsearchClusterConfigStatus Types.ElasticsearchClusterConfig
eccsOptions = Lens.field @"options"
{-# DEPRECATED eccsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of the configuration for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccsStatus :: Lens.Lens' ElasticsearchClusterConfigStatus Types.OptionStatus
eccsStatus = Lens.field @"status"
{-# DEPRECATED eccsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ElasticsearchClusterConfigStatus where
  parseJSON =
    Core.withObject "ElasticsearchClusterConfigStatus" Core.$
      \x ->
        ElasticsearchClusterConfigStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")
