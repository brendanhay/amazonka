{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
  ( ElasticsearchDataSourceConfig (..)
  -- * Smart constructor
  , mkElasticsearchDataSourceConfig
  -- * Lenses
  , edscEndpoint
  , edscAwsRegion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Elasticsearch data source configuration.
--
-- /See:/ 'mkElasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { endpoint :: Core.Text
    -- ^ The endpoint.
  , awsRegion :: Core.Text
    -- ^ The AWS Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchDataSourceConfig' value with any optional fields omitted.
mkElasticsearchDataSourceConfig
    :: Core.Text -- ^ 'endpoint'
    -> Core.Text -- ^ 'awsRegion'
    -> ElasticsearchDataSourceConfig
mkElasticsearchDataSourceConfig endpoint awsRegion
  = ElasticsearchDataSourceConfig'{endpoint, awsRegion}

-- | The endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edscEndpoint :: Lens.Lens' ElasticsearchDataSourceConfig Core.Text
edscEndpoint = Lens.field @"endpoint"
{-# INLINEABLE edscEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edscAwsRegion :: Lens.Lens' ElasticsearchDataSourceConfig Core.Text
edscAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE edscAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

instance Core.FromJSON ElasticsearchDataSourceConfig where
        toJSON ElasticsearchDataSourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("endpoint" Core..= endpoint),
                  Core.Just ("awsRegion" Core..= awsRegion)])

instance Core.FromJSON ElasticsearchDataSourceConfig where
        parseJSON
          = Core.withObject "ElasticsearchDataSourceConfig" Core.$
              \ x ->
                ElasticsearchDataSourceConfig' Core.<$>
                  (x Core..: "endpoint") Core.<*> x Core..: "awsRegion"
