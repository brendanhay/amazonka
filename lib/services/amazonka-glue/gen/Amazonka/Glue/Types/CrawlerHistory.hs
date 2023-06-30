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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CrawlerHistoryState
import qualified Amazonka.Prelude as Prelude

-- | Contains the information for a run of a crawler.
--
-- /See:/ 'newCrawlerHistory' smart constructor.
data CrawlerHistory = CrawlerHistory'
  { -- | A UUID identifier for each crawl.
    crawlId :: Prelude.Maybe Prelude.Text,
    -- | The number of data processing units (DPU) used in hours for the crawl.
    dPUHour :: Prelude.Maybe Prelude.Double,
    -- | The date and time on which the crawl ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, the error message associated with the crawl.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The log group associated with the crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The log stream associated with the crawl.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | The prefix for a CloudWatch message about this crawl.
    messagePrefix :: Prelude.Maybe Prelude.Text,
    -- | The date and time on which the crawl started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the crawl.
    state :: Prelude.Maybe CrawlerHistoryState,
    -- | A run summary for the specific crawl in JSON. Contains the catalog
    -- tables and partitions that were added, updated, or deleted.
    summary :: Prelude.Maybe Prelude.Text
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
-- 'crawlId', 'crawlerHistory_crawlId' - A UUID identifier for each crawl.
--
-- 'dPUHour', 'crawlerHistory_dPUHour' - The number of data processing units (DPU) used in hours for the crawl.
--
-- 'endTime', 'crawlerHistory_endTime' - The date and time on which the crawl ended.
--
-- 'errorMessage', 'crawlerHistory_errorMessage' - If an error occurred, the error message associated with the crawl.
--
-- 'logGroup', 'crawlerHistory_logGroup' - The log group associated with the crawl.
--
-- 'logStream', 'crawlerHistory_logStream' - The log stream associated with the crawl.
--
-- 'messagePrefix', 'crawlerHistory_messagePrefix' - The prefix for a CloudWatch message about this crawl.
--
-- 'startTime', 'crawlerHistory_startTime' - The date and time on which the crawl started.
--
-- 'state', 'crawlerHistory_state' - The state of the crawl.
--
-- 'summary', 'crawlerHistory_summary' - A run summary for the specific crawl in JSON. Contains the catalog
-- tables and partitions that were added, updated, or deleted.
newCrawlerHistory ::
  CrawlerHistory
newCrawlerHistory =
  CrawlerHistory'
    { crawlId = Prelude.Nothing,
      dPUHour = Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      logStream = Prelude.Nothing,
      messagePrefix = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      summary = Prelude.Nothing
    }

-- | A UUID identifier for each crawl.
crawlerHistory_crawlId :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_crawlId = Lens.lens (\CrawlerHistory' {crawlId} -> crawlId) (\s@CrawlerHistory' {} a -> s {crawlId = a} :: CrawlerHistory)

-- | The number of data processing units (DPU) used in hours for the crawl.
crawlerHistory_dPUHour :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Double)
crawlerHistory_dPUHour = Lens.lens (\CrawlerHistory' {dPUHour} -> dPUHour) (\s@CrawlerHistory' {} a -> s {dPUHour = a} :: CrawlerHistory)

-- | The date and time on which the crawl ended.
crawlerHistory_endTime :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.UTCTime)
crawlerHistory_endTime = Lens.lens (\CrawlerHistory' {endTime} -> endTime) (\s@CrawlerHistory' {} a -> s {endTime = a} :: CrawlerHistory) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, the error message associated with the crawl.
crawlerHistory_errorMessage :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_errorMessage = Lens.lens (\CrawlerHistory' {errorMessage} -> errorMessage) (\s@CrawlerHistory' {} a -> s {errorMessage = a} :: CrawlerHistory)

-- | The log group associated with the crawl.
crawlerHistory_logGroup :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_logGroup = Lens.lens (\CrawlerHistory' {logGroup} -> logGroup) (\s@CrawlerHistory' {} a -> s {logGroup = a} :: CrawlerHistory)

-- | The log stream associated with the crawl.
crawlerHistory_logStream :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_logStream = Lens.lens (\CrawlerHistory' {logStream} -> logStream) (\s@CrawlerHistory' {} a -> s {logStream = a} :: CrawlerHistory)

-- | The prefix for a CloudWatch message about this crawl.
crawlerHistory_messagePrefix :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_messagePrefix = Lens.lens (\CrawlerHistory' {messagePrefix} -> messagePrefix) (\s@CrawlerHistory' {} a -> s {messagePrefix = a} :: CrawlerHistory)

-- | The date and time on which the crawl started.
crawlerHistory_startTime :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.UTCTime)
crawlerHistory_startTime = Lens.lens (\CrawlerHistory' {startTime} -> startTime) (\s@CrawlerHistory' {} a -> s {startTime = a} :: CrawlerHistory) Prelude.. Lens.mapping Data._Time

-- | The state of the crawl.
crawlerHistory_state :: Lens.Lens' CrawlerHistory (Prelude.Maybe CrawlerHistoryState)
crawlerHistory_state = Lens.lens (\CrawlerHistory' {state} -> state) (\s@CrawlerHistory' {} a -> s {state = a} :: CrawlerHistory)

-- | A run summary for the specific crawl in JSON. Contains the catalog
-- tables and partitions that were added, updated, or deleted.
crawlerHistory_summary :: Lens.Lens' CrawlerHistory (Prelude.Maybe Prelude.Text)
crawlerHistory_summary = Lens.lens (\CrawlerHistory' {summary} -> summary) (\s@CrawlerHistory' {} a -> s {summary = a} :: CrawlerHistory)

instance Data.FromJSON CrawlerHistory where
  parseJSON =
    Data.withObject
      "CrawlerHistory"
      ( \x ->
          CrawlerHistory'
            Prelude.<$> (x Data..:? "CrawlId")
            Prelude.<*> (x Data..:? "DPUHour")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LogGroup")
            Prelude.<*> (x Data..:? "LogStream")
            Prelude.<*> (x Data..:? "MessagePrefix")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Summary")
      )

instance Prelude.Hashable CrawlerHistory where
  hashWithSalt _salt CrawlerHistory' {..} =
    _salt
      `Prelude.hashWithSalt` crawlId
      `Prelude.hashWithSalt` dPUHour
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` logStream
      `Prelude.hashWithSalt` messagePrefix
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` summary

instance Prelude.NFData CrawlerHistory where
  rnf CrawlerHistory' {..} =
    Prelude.rnf crawlId
      `Prelude.seq` Prelude.rnf dPUHour
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf logStream
      `Prelude.seq` Prelude.rnf messagePrefix
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf summary
