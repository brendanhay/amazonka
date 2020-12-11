-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerMetrics
  ( CrawlerMetrics (..),

    -- * Smart constructor
    mkCrawlerMetrics,

    -- * Lenses
    cmLastRuntimeSeconds,
    cmTablesCreated,
    cmStillEstimating,
    cmMedianRuntimeSeconds,
    cmTimeLeftSeconds,
    cmTablesDeleted,
    cmTablesUpdated,
    cmCrawlerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metrics for a specified crawler.
--
-- /See:/ 'mkCrawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { lastRuntimeSeconds ::
      Lude.Maybe Lude.Double,
    tablesCreated :: Lude.Maybe Lude.Natural,
    stillEstimating :: Lude.Maybe Lude.Bool,
    medianRuntimeSeconds :: Lude.Maybe Lude.Double,
    timeLeftSeconds :: Lude.Maybe Lude.Double,
    tablesDeleted :: Lude.Maybe Lude.Natural,
    tablesUpdated :: Lude.Maybe Lude.Natural,
    crawlerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CrawlerMetrics' with the minimum fields required to make a request.
--
-- * 'crawlerName' - The name of the crawler.
-- * 'lastRuntimeSeconds' - The duration of the crawler's most recent run, in seconds.
-- * 'medianRuntimeSeconds' - The median duration of this crawler's runs, in seconds.
-- * 'stillEstimating' - True if the crawler is still estimating how long it will take to complete this run.
-- * 'tablesCreated' - The number of tables created by this crawler.
-- * 'tablesDeleted' - The number of tables deleted by this crawler.
-- * 'tablesUpdated' - The number of tables updated by this crawler.
-- * 'timeLeftSeconds' - The estimated time left to complete a running crawl.
mkCrawlerMetrics ::
  CrawlerMetrics
mkCrawlerMetrics =
  CrawlerMetrics'
    { lastRuntimeSeconds = Lude.Nothing,
      tablesCreated = Lude.Nothing,
      stillEstimating = Lude.Nothing,
      medianRuntimeSeconds = Lude.Nothing,
      timeLeftSeconds = Lude.Nothing,
      tablesDeleted = Lude.Nothing,
      tablesUpdated = Lude.Nothing,
      crawlerName = Lude.Nothing
    }

-- | The duration of the crawler's most recent run, in seconds.
--
-- /Note:/ Consider using 'lastRuntimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmLastRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Double)
cmLastRuntimeSeconds = Lens.lens (lastRuntimeSeconds :: CrawlerMetrics -> Lude.Maybe Lude.Double) (\s a -> s {lastRuntimeSeconds = a} :: CrawlerMetrics)
{-# DEPRECATED cmLastRuntimeSeconds "Use generic-lens or generic-optics with 'lastRuntimeSeconds' instead." #-}

-- | The number of tables created by this crawler.
--
-- /Note:/ Consider using 'tablesCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesCreated :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Natural)
cmTablesCreated = Lens.lens (tablesCreated :: CrawlerMetrics -> Lude.Maybe Lude.Natural) (\s a -> s {tablesCreated = a} :: CrawlerMetrics)
{-# DEPRECATED cmTablesCreated "Use generic-lens or generic-optics with 'tablesCreated' instead." #-}

-- | True if the crawler is still estimating how long it will take to complete this run.
--
-- /Note:/ Consider using 'stillEstimating' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmStillEstimating :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Bool)
cmStillEstimating = Lens.lens (stillEstimating :: CrawlerMetrics -> Lude.Maybe Lude.Bool) (\s a -> s {stillEstimating = a} :: CrawlerMetrics)
{-# DEPRECATED cmStillEstimating "Use generic-lens or generic-optics with 'stillEstimating' instead." #-}

-- | The median duration of this crawler's runs, in seconds.
--
-- /Note:/ Consider using 'medianRuntimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMedianRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Double)
cmMedianRuntimeSeconds = Lens.lens (medianRuntimeSeconds :: CrawlerMetrics -> Lude.Maybe Lude.Double) (\s a -> s {medianRuntimeSeconds = a} :: CrawlerMetrics)
{-# DEPRECATED cmMedianRuntimeSeconds "Use generic-lens or generic-optics with 'medianRuntimeSeconds' instead." #-}

-- | The estimated time left to complete a running crawl.
--
-- /Note:/ Consider using 'timeLeftSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTimeLeftSeconds :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Double)
cmTimeLeftSeconds = Lens.lens (timeLeftSeconds :: CrawlerMetrics -> Lude.Maybe Lude.Double) (\s a -> s {timeLeftSeconds = a} :: CrawlerMetrics)
{-# DEPRECATED cmTimeLeftSeconds "Use generic-lens or generic-optics with 'timeLeftSeconds' instead." #-}

-- | The number of tables deleted by this crawler.
--
-- /Note:/ Consider using 'tablesDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesDeleted :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Natural)
cmTablesDeleted = Lens.lens (tablesDeleted :: CrawlerMetrics -> Lude.Maybe Lude.Natural) (\s a -> s {tablesDeleted = a} :: CrawlerMetrics)
{-# DEPRECATED cmTablesDeleted "Use generic-lens or generic-optics with 'tablesDeleted' instead." #-}

-- | The number of tables updated by this crawler.
--
-- /Note:/ Consider using 'tablesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTablesUpdated :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Natural)
cmTablesUpdated = Lens.lens (tablesUpdated :: CrawlerMetrics -> Lude.Maybe Lude.Natural) (\s a -> s {tablesUpdated = a} :: CrawlerMetrics)
{-# DEPRECATED cmTablesUpdated "Use generic-lens or generic-optics with 'tablesUpdated' instead." #-}

-- | The name of the crawler.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCrawlerName :: Lens.Lens' CrawlerMetrics (Lude.Maybe Lude.Text)
cmCrawlerName = Lens.lens (crawlerName :: CrawlerMetrics -> Lude.Maybe Lude.Text) (\s a -> s {crawlerName = a} :: CrawlerMetrics)
{-# DEPRECATED cmCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.FromJSON CrawlerMetrics where
  parseJSON =
    Lude.withObject
      "CrawlerMetrics"
      ( \x ->
          CrawlerMetrics'
            Lude.<$> (x Lude..:? "LastRuntimeSeconds")
            Lude.<*> (x Lude..:? "TablesCreated")
            Lude.<*> (x Lude..:? "StillEstimating")
            Lude.<*> (x Lude..:? "MedianRuntimeSeconds")
            Lude.<*> (x Lude..:? "TimeLeftSeconds")
            Lude.<*> (x Lude..:? "TablesDeleted")
            Lude.<*> (x Lude..:? "TablesUpdated")
            Lude.<*> (x Lude..:? "CrawlerName")
      )
