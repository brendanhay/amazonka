{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CrawlerMetrics
  ( CrawlerMetrics (..)
  -- * Smart constructor
  , mkCrawlerMetrics
  -- * Lenses
  , cmCrawlerName
  , cmLastRuntimeSeconds
  , cmMedianRuntimeSeconds
  , cmStillEstimating
  , cmTablesCreated
  , cmTablesDeleted
  , cmTablesUpdated
  , cmTimeLeftSeconds
  ) where

import qualified Network.AWS.Glue.Types.CrawlerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metrics for a specified crawler.
--
-- /See:/ 'mkCrawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { crawlerName :: Core.Maybe Types.CrawlerName
    -- ^ The name of the crawler.
  , lastRuntimeSeconds :: Core.Maybe Core.Double
    -- ^ The duration of the crawler's most recent run, in seconds.
  , medianRuntimeSeconds :: Core.Maybe Core.Double
    -- ^ The median duration of this crawler's runs, in seconds.
  , stillEstimating :: Core.Maybe Core.Bool
    -- ^ True if the crawler is still estimating how long it will take to complete this run.
  , tablesCreated :: Core.Maybe Core.Natural
    -- ^ The number of tables created by this crawler.
  , tablesDeleted :: Core.Maybe Core.Natural
    -- ^ The number of tables deleted by this crawler.
  , tablesUpdated :: Core.Maybe Core.Natural
    -- ^ The number of tables updated by this crawler.
  , timeLeftSeconds :: Core.Maybe Core.Double
    -- ^ The estimated time left to complete a running crawl.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CrawlerMetrics' value with any optional fields omitted.
mkCrawlerMetrics
    :: CrawlerMetrics
mkCrawlerMetrics
  = CrawlerMetrics'{crawlerName = Core.Nothing,
                    lastRuntimeSeconds = Core.Nothing,
                    medianRuntimeSeconds = Core.Nothing,
                    stillEstimating = Core.Nothing, tablesCreated = Core.Nothing,
                    tablesDeleted = Core.Nothing, tablesUpdated = Core.Nothing,
                    timeLeftSeconds = Core.Nothing}

-- | The name of the crawler.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCrawlerName :: Lens.Lens' CrawlerMetrics (Core.Maybe Types.CrawlerName)
cmCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE cmCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

-- | The duration of the crawler's most recent run, in seconds.
--
-- /Note:/ Consider using 'lastRuntimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmLastRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
cmLastRuntimeSeconds = Lens.field @"lastRuntimeSeconds"
{-# INLINEABLE cmLastRuntimeSeconds #-}
{-# DEPRECATED lastRuntimeSeconds "Use generic-lens or generic-optics with 'lastRuntimeSeconds' instead"  #-}

-- | The median duration of this crawler's runs, in seconds.
--
-- /Note:/ Consider using 'medianRuntimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMedianRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
cmMedianRuntimeSeconds = Lens.field @"medianRuntimeSeconds"
{-# INLINEABLE cmMedianRuntimeSeconds #-}
{-# DEPRECATED medianRuntimeSeconds "Use generic-lens or generic-optics with 'medianRuntimeSeconds' instead"  #-}

-- | True if the crawler is still estimating how long it will take to complete this run.
--
-- /Note:/ Consider using 'stillEstimating' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmStillEstimating :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Bool)
cmStillEstimating = Lens.field @"stillEstimating"
{-# INLINEABLE cmStillEstimating #-}
{-# DEPRECATED stillEstimating "Use generic-lens or generic-optics with 'stillEstimating' instead"  #-}

-- | The number of tables created by this crawler.
--
-- /Note:/ Consider using 'tablesCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesCreated :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
cmTablesCreated = Lens.field @"tablesCreated"
{-# INLINEABLE cmTablesCreated #-}
{-# DEPRECATED tablesCreated "Use generic-lens or generic-optics with 'tablesCreated' instead"  #-}

-- | The number of tables deleted by this crawler.
--
-- /Note:/ Consider using 'tablesDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesDeleted :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
cmTablesDeleted = Lens.field @"tablesDeleted"
{-# INLINEABLE cmTablesDeleted #-}
{-# DEPRECATED tablesDeleted "Use generic-lens or generic-optics with 'tablesDeleted' instead"  #-}

-- | The number of tables updated by this crawler.
--
-- /Note:/ Consider using 'tablesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesUpdated :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
cmTablesUpdated = Lens.field @"tablesUpdated"
{-# INLINEABLE cmTablesUpdated #-}
{-# DEPRECATED tablesUpdated "Use generic-lens or generic-optics with 'tablesUpdated' instead"  #-}

-- | The estimated time left to complete a running crawl.
--
-- /Note:/ Consider using 'timeLeftSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTimeLeftSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
cmTimeLeftSeconds = Lens.field @"timeLeftSeconds"
{-# INLINEABLE cmTimeLeftSeconds #-}
{-# DEPRECATED timeLeftSeconds "Use generic-lens or generic-optics with 'timeLeftSeconds' instead"  #-}

instance Core.FromJSON CrawlerMetrics where
        parseJSON
          = Core.withObject "CrawlerMetrics" Core.$
              \ x ->
                CrawlerMetrics' Core.<$>
                  (x Core..:? "CrawlerName") Core.<*> x Core..:? "LastRuntimeSeconds"
                    Core.<*> x Core..:? "MedianRuntimeSeconds"
                    Core.<*> x Core..:? "StillEstimating"
                    Core.<*> x Core..:? "TablesCreated"
                    Core.<*> x Core..:? "TablesDeleted"
                    Core.<*> x Core..:? "TablesUpdated"
                    Core.<*> x Core..:? "TimeLeftSeconds"
