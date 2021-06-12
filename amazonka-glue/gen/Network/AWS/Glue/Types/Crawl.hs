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
-- Module      : Network.AWS.Glue.Types.Crawl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawl where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CrawlState
import qualified Network.AWS.Lens as Lens

-- | The details of a crawl in the workflow.
--
-- /See:/ 'newCrawl' smart constructor.
data Crawl = Crawl'
  { -- | The log group associated with the crawl.
    logGroup :: Core.Maybe Core.Text,
    -- | The state of the crawler.
    state :: Core.Maybe CrawlState,
    -- | The date and time on which the crawl completed.
    completedOn :: Core.Maybe Core.POSIX,
    -- | The error message associated with the crawl.
    errorMessage :: Core.Maybe Core.Text,
    -- | The date and time on which the crawl started.
    startedOn :: Core.Maybe Core.POSIX,
    -- | The log stream associated with the crawl.
    logStream :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Crawl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'crawl_logGroup' - The log group associated with the crawl.
--
-- 'state', 'crawl_state' - The state of the crawler.
--
-- 'completedOn', 'crawl_completedOn' - The date and time on which the crawl completed.
--
-- 'errorMessage', 'crawl_errorMessage' - The error message associated with the crawl.
--
-- 'startedOn', 'crawl_startedOn' - The date and time on which the crawl started.
--
-- 'logStream', 'crawl_logStream' - The log stream associated with the crawl.
newCrawl ::
  Crawl
newCrawl =
  Crawl'
    { logGroup = Core.Nothing,
      state = Core.Nothing,
      completedOn = Core.Nothing,
      errorMessage = Core.Nothing,
      startedOn = Core.Nothing,
      logStream = Core.Nothing
    }

-- | The log group associated with the crawl.
crawl_logGroup :: Lens.Lens' Crawl (Core.Maybe Core.Text)
crawl_logGroup = Lens.lens (\Crawl' {logGroup} -> logGroup) (\s@Crawl' {} a -> s {logGroup = a} :: Crawl)

-- | The state of the crawler.
crawl_state :: Lens.Lens' Crawl (Core.Maybe CrawlState)
crawl_state = Lens.lens (\Crawl' {state} -> state) (\s@Crawl' {} a -> s {state = a} :: Crawl)

-- | The date and time on which the crawl completed.
crawl_completedOn :: Lens.Lens' Crawl (Core.Maybe Core.UTCTime)
crawl_completedOn = Lens.lens (\Crawl' {completedOn} -> completedOn) (\s@Crawl' {} a -> s {completedOn = a} :: Crawl) Core.. Lens.mapping Core._Time

-- | The error message associated with the crawl.
crawl_errorMessage :: Lens.Lens' Crawl (Core.Maybe Core.Text)
crawl_errorMessage = Lens.lens (\Crawl' {errorMessage} -> errorMessage) (\s@Crawl' {} a -> s {errorMessage = a} :: Crawl)

-- | The date and time on which the crawl started.
crawl_startedOn :: Lens.Lens' Crawl (Core.Maybe Core.UTCTime)
crawl_startedOn = Lens.lens (\Crawl' {startedOn} -> startedOn) (\s@Crawl' {} a -> s {startedOn = a} :: Crawl) Core.. Lens.mapping Core._Time

-- | The log stream associated with the crawl.
crawl_logStream :: Lens.Lens' Crawl (Core.Maybe Core.Text)
crawl_logStream = Lens.lens (\Crawl' {logStream} -> logStream) (\s@Crawl' {} a -> s {logStream = a} :: Crawl)

instance Core.FromJSON Crawl where
  parseJSON =
    Core.withObject
      "Crawl"
      ( \x ->
          Crawl'
            Core.<$> (x Core..:? "LogGroup")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "CompletedOn")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "StartedOn")
            Core.<*> (x Core..:? "LogStream")
      )

instance Core.Hashable Crawl

instance Core.NFData Crawl
