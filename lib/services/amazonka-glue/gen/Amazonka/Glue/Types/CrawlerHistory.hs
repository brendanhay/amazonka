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
-- Module      : Amazonka.Glue.Types.CrawlerHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CrawlerHistoryState
import qualified Amazonka.Prelude as Prelude

-- | Contains the information for a run of a crawler.
--
-- /See:/ 'newCrawlerHistory' smart constructor.
data CrawlerHistory = CrawlerHistory'
  { -- | The log group associated with the crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The number of data processing units (DPU) used in hours for the crawl.
    dPUHour :: Prelude.Maybe Prelude.Double,
    -- | The log stream associated with the crawl.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, the error message associated with the crawl.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of the crawl.
    state :: Prelude.Maybe CrawlerHistoryState,
    -- | A run summary for the specific crawl in JSON. Contains the catalog
    -- tables and partitions that were added, updated, or deleted.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The date and time on which the crawl ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The prefix for a CloudWatch message about this crawl.
    messagePrefix :: Prelude.Maybe Prelude.Text,
    -- | A UUID identifier for each crawl.
    crawlId :: Prelude.Maybe Prelude.Text,
    -- | The date and time on which the crawl started.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrawlerHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'crawlerHistory_logGroup' - The log group associated with the crawl.
--
-- 'dPUHour', 'crawlerHistory_dPUHour' - The number of data processing units (DPU) used in hours for the crawl.
--
-- 'logStream', 'crawlerHistory_logStream' - The log stream associated with the crawl.
--
-- 'errorMessage', 'crawlerHistory_errorMessage' - If an error occurred, the error message associated with the crawl.
--
-- 'state', 'crawlerHistory_state' - The state of the crawl.
--
-- 'summary', 'crawlerHistory_summary' - A run summary for the specific crawl in JSON. Contains the catalog
-- tables and partitions that were added, updated, or deleted.
--
-- 'endTime', 'crawlerHistory_endTime' - The date and time on which the crawl ended.
--
-- 'messagePrefix', 'crawlerHistory_messagePrefix' - The prefix for a CloudWatch message about this crawl.
--
-- 'crawlId', 'crawlerHistory_crawlId' - A UUID identifier for each crawl.
--
-- 'startTime', 'crawlerHistory_startTime' - The date and time on which the crawl started.
newCrawlerHistory ::
  CrawlerHistory
newCrawlerHistory =
  CrawlerHistory'
    { logGroup = Prelude.Nothing,
      dPUHour = Prelude.Nothing,
      logStream = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      state = Prelude.Nothing,
      summary = Prelude.Nothing,
      endTime = Prelude.Nothing,
      messagePrefix = Prelude.Nothing,
      crawlId = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The log group associated with the crawl.
crawlerHistory_logGroup :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_logGroup = Lens.lens (\CrawlerHistory' {logGroup} -> logGroup) (\s@CrawlerHistory' {} a -> s {logGroup = a} :: CrawlerHistory)

-- | The number of data processing units (DPU) used in hours for the crawl.
crawlerHistory_dPUHour :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Double)
crawlerHistory_dPUHour = Lens.lens (\CrawlerHistory' {dPUHour} -> dPUHour) (\s@CrawlerHistory' {} a -> s {dPUHour = a} :: CrawlerHistory)

-- | The log stream associated with the crawl.
crawlerHistory_logStream :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_logStream = Lens.lens (\CrawlerHistory' {logStream} -> logStream) (\s@CrawlerHistory' {} a -> s {logStream = a} :: CrawlerHistory)

-- | If an error occurred, the error message associated with the crawl.
crawlerHistory_errorMessage :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_errorMessage = Lens.lens (\CrawlerHistory' {errorMessage} -> errorMessage) (\s@CrawlerHistory' {} a -> s {errorMessage = a} :: CrawlerHistory)

-- | The state of the crawl.
crawlerHistory_state :: Lens.Lens' CrawlerHistory (Prelude.Maybe CrawlerHistoryState)
crawlerHistory_state = Lens.lens (\CrawlerHistory' {state} -> state) (\s@CrawlerHistory' {} a -> s {state = a} :: CrawlerHistory)

-- | A run summary for the specific crawl in JSON. Contains the catalog
-- tables and partitions that were added, updated, or deleted.
crawlerHistory_summary :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_summary = Lens.lens (\CrawlerHistory' {summary} -> summary) (\s@CrawlerHistory' {} a -> s {summary = a} :: CrawlerHistory)

-- | The date and time on which the crawl ended.
crawlerHistory_endTime :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.UTCTime)
crawlerHistory_endTime = Lens.lens (\CrawlerHistory' {endTime} -> endTime) (\s@CrawlerHistory' {} a -> s {endTime = a} :: CrawlerHistory) Prelude.. Lens.mapping Core._Time

-- | The prefix for a CloudWatch message about this crawl.
crawlerHistory_messagePrefix :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_messagePrefix = Lens.lens (\CrawlerHistory' {messagePrefix} -> messagePrefix) (\s@CrawlerHistory' {} a -> s {messagePrefix = a} :: CrawlerHistory)

-- | A UUID identifier for each crawl.
crawlerHistory_crawlId :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_crawlId = Lens.lens (\CrawlerHistory' {crawlId} -> crawlId) (\s@CrawlerHistory' {} a -> s {crawlId = a} :: CrawlerHistory)

-- | The date and time on which the crawl started.
crawlerHistory_startTime :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.UTCTime)
crawlerHistory_startTime = Lens.lens (\CrawlerHistory' {startTime} -> startTime) (\s@CrawlerHistory' {} a -> s {startTime = a} :: CrawlerHistory) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CrawlerHistory where
  parseJSON =
    Core.withObject
      "CrawlerHistory"
      ( \x ->
          CrawlerHistory'
            Prelude.<$> (x Core..:? "LogGroup")
            Prelude.<*> (x Core..:? "DPUHour")
            Prelude.<*> (x Core..:? "LogStream")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "MessagePrefix")
            Prelude.<*> (x Core..:? "CrawlId")
            Prelude.<*> (x Core..:? "StartTime")
      )

instance Prelude.Hashable CrawlerHistory where
  hashWithSalt _salt CrawlerHistory' {..} =
    _salt `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` dPUHour
      `Prelude.hashWithSalt` logStream
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` messagePrefix
      `Prelude.hashWithSalt` crawlId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData CrawlerHistory where
  rnf CrawlerHistory' {..} =
    Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf dPUHour
      `Prelude.seq` Prelude.rnf logStream
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf messagePrefix
      `Prelude.seq` Prelude.rnf crawlId
      `Prelude.seq` Prelude.rnf startTime
