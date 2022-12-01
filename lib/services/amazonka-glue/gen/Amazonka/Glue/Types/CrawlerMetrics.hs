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
-- Module      : Amazonka.Glue.Types.CrawlerMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Metrics for a specified crawler.
--
-- /See:/ 'newCrawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { -- | The median duration of this crawler\'s runs, in seconds.
    medianRuntimeSeconds :: Prelude.Maybe Prelude.Double,
    -- | The duration of the crawler\'s most recent run, in seconds.
    lastRuntimeSeconds :: Prelude.Maybe Prelude.Double,
    -- | True if the crawler is still estimating how long it will take to
    -- complete this run.
    stillEstimating :: Prelude.Maybe Prelude.Bool,
    -- | The number of tables updated by this crawler.
    tablesUpdated :: Prelude.Maybe Prelude.Natural,
    -- | The name of the crawler.
    crawlerName :: Prelude.Maybe Prelude.Text,
    -- | The number of tables created by this crawler.
    tablesCreated :: Prelude.Maybe Prelude.Natural,
    -- | The number of tables deleted by this crawler.
    tablesDeleted :: Prelude.Maybe Prelude.Natural,
    -- | The estimated time left to complete a running crawl.
    timeLeftSeconds :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrawlerMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medianRuntimeSeconds', 'crawlerMetrics_medianRuntimeSeconds' - The median duration of this crawler\'s runs, in seconds.
--
-- 'lastRuntimeSeconds', 'crawlerMetrics_lastRuntimeSeconds' - The duration of the crawler\'s most recent run, in seconds.
--
-- 'stillEstimating', 'crawlerMetrics_stillEstimating' - True if the crawler is still estimating how long it will take to
-- complete this run.
--
-- 'tablesUpdated', 'crawlerMetrics_tablesUpdated' - The number of tables updated by this crawler.
--
-- 'crawlerName', 'crawlerMetrics_crawlerName' - The name of the crawler.
--
-- 'tablesCreated', 'crawlerMetrics_tablesCreated' - The number of tables created by this crawler.
--
-- 'tablesDeleted', 'crawlerMetrics_tablesDeleted' - The number of tables deleted by this crawler.
--
-- 'timeLeftSeconds', 'crawlerMetrics_timeLeftSeconds' - The estimated time left to complete a running crawl.
newCrawlerMetrics ::
  CrawlerMetrics
newCrawlerMetrics =
  CrawlerMetrics'
    { medianRuntimeSeconds =
        Prelude.Nothing,
      lastRuntimeSeconds = Prelude.Nothing,
      stillEstimating = Prelude.Nothing,
      tablesUpdated = Prelude.Nothing,
      crawlerName = Prelude.Nothing,
      tablesCreated = Prelude.Nothing,
      tablesDeleted = Prelude.Nothing,
      timeLeftSeconds = Prelude.Nothing
    }

-- | The median duration of this crawler\'s runs, in seconds.
crawlerMetrics_medianRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Double)
crawlerMetrics_medianRuntimeSeconds = Lens.lens (\CrawlerMetrics' {medianRuntimeSeconds} -> medianRuntimeSeconds) (\s@CrawlerMetrics' {} a -> s {medianRuntimeSeconds = a} :: CrawlerMetrics)

-- | The duration of the crawler\'s most recent run, in seconds.
crawlerMetrics_lastRuntimeSeconds :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Double)
crawlerMetrics_lastRuntimeSeconds = Lens.lens (\CrawlerMetrics' {lastRuntimeSeconds} -> lastRuntimeSeconds) (\s@CrawlerMetrics' {} a -> s {lastRuntimeSeconds = a} :: CrawlerMetrics)

-- | True if the crawler is still estimating how long it will take to
-- complete this run.
crawlerMetrics_stillEstimating :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Bool)
crawlerMetrics_stillEstimating = Lens.lens (\CrawlerMetrics' {stillEstimating} -> stillEstimating) (\s@CrawlerMetrics' {} a -> s {stillEstimating = a} :: CrawlerMetrics)

-- | The number of tables updated by this crawler.
crawlerMetrics_tablesUpdated :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Natural)
crawlerMetrics_tablesUpdated = Lens.lens (\CrawlerMetrics' {tablesUpdated} -> tablesUpdated) (\s@CrawlerMetrics' {} a -> s {tablesUpdated = a} :: CrawlerMetrics)

-- | The name of the crawler.
crawlerMetrics_crawlerName :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Text)
crawlerMetrics_crawlerName = Lens.lens (\CrawlerMetrics' {crawlerName} -> crawlerName) (\s@CrawlerMetrics' {} a -> s {crawlerName = a} :: CrawlerMetrics)

-- | The number of tables created by this crawler.
crawlerMetrics_tablesCreated :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Natural)
crawlerMetrics_tablesCreated = Lens.lens (\CrawlerMetrics' {tablesCreated} -> tablesCreated) (\s@CrawlerMetrics' {} a -> s {tablesCreated = a} :: CrawlerMetrics)

-- | The number of tables deleted by this crawler.
crawlerMetrics_tablesDeleted :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Natural)
crawlerMetrics_tablesDeleted = Lens.lens (\CrawlerMetrics' {tablesDeleted} -> tablesDeleted) (\s@CrawlerMetrics' {} a -> s {tablesDeleted = a} :: CrawlerMetrics)

-- | The estimated time left to complete a running crawl.
crawlerMetrics_timeLeftSeconds :: Lens.Lens' CrawlerMetrics (Prelude.Maybe Prelude.Double)
crawlerMetrics_timeLeftSeconds = Lens.lens (\CrawlerMetrics' {timeLeftSeconds} -> timeLeftSeconds) (\s@CrawlerMetrics' {} a -> s {timeLeftSeconds = a} :: CrawlerMetrics)

instance Core.FromJSON CrawlerMetrics where
  parseJSON =
    Core.withObject
      "CrawlerMetrics"
      ( \x ->
          CrawlerMetrics'
            Prelude.<$> (x Core..:? "MedianRuntimeSeconds")
            Prelude.<*> (x Core..:? "LastRuntimeSeconds")
            Prelude.<*> (x Core..:? "StillEstimating")
            Prelude.<*> (x Core..:? "TablesUpdated")
            Prelude.<*> (x Core..:? "CrawlerName")
            Prelude.<*> (x Core..:? "TablesCreated")
            Prelude.<*> (x Core..:? "TablesDeleted")
            Prelude.<*> (x Core..:? "TimeLeftSeconds")
      )

instance Prelude.Hashable CrawlerMetrics where
  hashWithSalt _salt CrawlerMetrics' {..} =
    _salt `Prelude.hashWithSalt` medianRuntimeSeconds
      `Prelude.hashWithSalt` lastRuntimeSeconds
      `Prelude.hashWithSalt` stillEstimating
      `Prelude.hashWithSalt` tablesUpdated
      `Prelude.hashWithSalt` crawlerName
      `Prelude.hashWithSalt` tablesCreated
      `Prelude.hashWithSalt` tablesDeleted
      `Prelude.hashWithSalt` timeLeftSeconds

instance Prelude.NFData CrawlerMetrics where
  rnf CrawlerMetrics' {..} =
    Prelude.rnf medianRuntimeSeconds
      `Prelude.seq` Prelude.rnf lastRuntimeSeconds
      `Prelude.seq` Prelude.rnf stillEstimating
      `Prelude.seq` Prelude.rnf tablesUpdated
      `Prelude.seq` Prelude.rnf crawlerName
      `Prelude.seq` Prelude.rnf tablesCreated
      `Prelude.seq` Prelude.rnf tablesDeleted
      `Prelude.seq` Prelude.rnf timeLeftSeconds
