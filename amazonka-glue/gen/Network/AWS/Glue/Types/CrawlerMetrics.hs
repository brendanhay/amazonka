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
-- Module      : Network.AWS.Glue.Types.CrawlerMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerMetrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metrics for a specified crawler.
--
-- /See:/ 'newCrawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { -- | The name of the crawler.
    crawlerName :: Core.Maybe Core.Text,
    -- | The number of tables deleted by this crawler.
    tablesDeleted :: Core.Maybe Core.Natural,
    -- | The number of tables updated by this crawler.
    tablesUpdated :: Core.Maybe Core.Natural,
    -- | The number of tables created by this crawler.
    tablesCreated :: Core.Maybe Core.Natural,
    -- | The median duration of this crawler\'s runs, in seconds.
    medianRuntimeSeconds :: Core.Maybe Core.Double,
    -- | True if the crawler is still estimating how long it will take to
    -- complete this run.
    stillEstimating :: Core.Maybe Core.Bool,
    -- | The estimated time left to complete a running crawl.
    timeLeftSeconds :: Core.Maybe Core.Double,
    -- | The duration of the crawler\'s most recent run, in seconds.
    lastRuntimeSeconds :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CrawlerMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerName', 'crawlerMetrics_crawlerName' - The name of the crawler.
--
-- 'tablesDeleted', 'crawlerMetrics_tablesDeleted' - The number of tables deleted by this crawler.
--
-- 'tablesUpdated', 'crawlerMetrics_tablesUpdated' - The number of tables updated by this crawler.
--
-- 'tablesCreated', 'crawlerMetrics_tablesCreated' - The number of tables created by this crawler.
--
-- 'medianRuntimeSeconds', 'crawlerMetrics_medianRuntimeSeconds' - The median duration of this crawler\'s runs, in seconds.
--
-- 'stillEstimating', 'crawlerMetrics_stillEstimating' - True if the crawler is still estimating how long it will take to
-- complete this run.
--
-- 'timeLeftSeconds', 'crawlerMetrics_timeLeftSeconds' - The estimated time left to complete a running crawl.
--
-- 'lastRuntimeSeconds', 'crawlerMetrics_lastRuntimeSeconds' - The duration of the crawler\'s most recent run, in seconds.
newCrawlerMetrics ::
  CrawlerMetrics
newCrawlerMetrics =
  CrawlerMetrics'
    { crawlerName = Core.Nothing,
      tablesDeleted = Core.Nothing,
      tablesUpdated = Core.Nothing,
      tablesCreated = Core.Nothing,
      medianRuntimeSeconds = Core.Nothing,
      stillEstimating = Core.Nothing,
      timeLeftSeconds = Core.Nothing,
      lastRuntimeSeconds = Core.Nothing
    }

-- | The name of the crawler.
crawlerMetrics_crawlerName :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Text)
crawlerMetrics_crawlerName = Lens.lens (\CrawlerMetrics' {crawlerName} -> crawlerName) (\s@CrawlerMetrics' {} a -> s {crawlerName = a} :: CrawlerMetrics)

-- | The number of tables deleted by this crawler.
crawlerMetrics_tablesDeleted :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
crawlerMetrics_tablesDeleted = Lens.lens (\CrawlerMetrics' {tablesDeleted} -> tablesDeleted) (\s@CrawlerMetrics' {} a -> s {tablesDeleted = a} :: CrawlerMetrics)

-- | The number of tables updated by this crawler.
crawlerMetrics_tablesUpdated :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
crawlerMetrics_tablesUpdated = Lens.lens (\CrawlerMetrics' {tablesUpdated} -> tablesUpdated) (\s@CrawlerMetrics' {} a -> s {tablesUpdated = a} :: CrawlerMetrics)

-- | The number of tables created by this crawler.
crawlerMetrics_tablesCreated :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Natural)
crawlerMetrics_tablesCreated = Lens.lens (\CrawlerMetrics' {tablesCreated} -> tablesCreated) (\s@CrawlerMetrics' {} a -> s {tablesCreated = a} :: CrawlerMetrics)

-- | The median duration of this crawler\'s runs, in seconds.
crawlerMetrics_medianRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
crawlerMetrics_medianRuntimeSeconds = Lens.lens (\CrawlerMetrics' {medianRuntimeSeconds} -> medianRuntimeSeconds) (\s@CrawlerMetrics' {} a -> s {medianRuntimeSeconds = a} :: CrawlerMetrics)

-- | True if the crawler is still estimating how long it will take to
-- complete this run.
crawlerMetrics_stillEstimating :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Bool)
crawlerMetrics_stillEstimating = Lens.lens (\CrawlerMetrics' {stillEstimating} -> stillEstimating) (\s@CrawlerMetrics' {} a -> s {stillEstimating = a} :: CrawlerMetrics)

-- | The estimated time left to complete a running crawl.
crawlerMetrics_timeLeftSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
crawlerMetrics_timeLeftSeconds = Lens.lens (\CrawlerMetrics' {timeLeftSeconds} -> timeLeftSeconds) (\s@CrawlerMetrics' {} a -> s {timeLeftSeconds = a} :: CrawlerMetrics)

-- | The duration of the crawler\'s most recent run, in seconds.
crawlerMetrics_lastRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Core.Maybe Core.Double)
crawlerMetrics_lastRuntimeSeconds = Lens.lens (\CrawlerMetrics' {lastRuntimeSeconds} -> lastRuntimeSeconds) (\s@CrawlerMetrics' {} a -> s {lastRuntimeSeconds = a} :: CrawlerMetrics)

instance Core.FromJSON CrawlerMetrics where
  parseJSON =
    Core.withObject
      "CrawlerMetrics"
      ( \x ->
          CrawlerMetrics'
            Core.<$> (x Core..:? "CrawlerName")
            Core.<*> (x Core..:? "TablesDeleted")
            Core.<*> (x Core..:? "TablesUpdated")
            Core.<*> (x Core..:? "TablesCreated")
            Core.<*> (x Core..:? "MedianRuntimeSeconds")
            Core.<*> (x Core..:? "StillEstimating")
            Core.<*> (x Core..:? "TimeLeftSeconds")
            Core.<*> (x Core..:? "LastRuntimeSeconds")
      )

instance Core.Hashable CrawlerMetrics

instance Core.NFData CrawlerMetrics
