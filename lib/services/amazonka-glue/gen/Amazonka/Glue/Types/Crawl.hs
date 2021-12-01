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
-- Module      : Amazonka.Glue.Types.Crawl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Crawl where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.CrawlState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a crawl in the workflow.
--
-- /See:/ 'newCrawl' smart constructor.
data Crawl = Crawl'
  { -- | The date and time on which the crawl completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The state of the crawler.
    state :: Prelude.Maybe CrawlState,
    -- | The date and time on which the crawl started.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | The log stream associated with the crawl.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | The log group associated with the crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The error message associated with the crawl.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Crawl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'crawl_completedOn' - The date and time on which the crawl completed.
--
-- 'state', 'crawl_state' - The state of the crawler.
--
-- 'startedOn', 'crawl_startedOn' - The date and time on which the crawl started.
--
-- 'logStream', 'crawl_logStream' - The log stream associated with the crawl.
--
-- 'logGroup', 'crawl_logGroup' - The log group associated with the crawl.
--
-- 'errorMessage', 'crawl_errorMessage' - The error message associated with the crawl.
newCrawl ::
  Crawl
newCrawl =
  Crawl'
    { completedOn = Prelude.Nothing,
      state = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      logStream = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The date and time on which the crawl completed.
crawl_completedOn :: Lens.Lens' Crawl (Prelude.Maybe Prelude.UTCTime)
crawl_completedOn = Lens.lens (\Crawl' {completedOn} -> completedOn) (\s@Crawl' {} a -> s {completedOn = a} :: Crawl) Prelude.. Lens.mapping Core._Time

-- | The state of the crawler.
crawl_state :: Lens.Lens' Crawl (Prelude.Maybe CrawlState)
crawl_state = Lens.lens (\Crawl' {state} -> state) (\s@Crawl' {} a -> s {state = a} :: Crawl)

-- | The date and time on which the crawl started.
crawl_startedOn :: Lens.Lens' Crawl (Prelude.Maybe Prelude.UTCTime)
crawl_startedOn = Lens.lens (\Crawl' {startedOn} -> startedOn) (\s@Crawl' {} a -> s {startedOn = a} :: Crawl) Prelude.. Lens.mapping Core._Time

-- | The log stream associated with the crawl.
crawl_logStream :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_logStream = Lens.lens (\Crawl' {logStream} -> logStream) (\s@Crawl' {} a -> s {logStream = a} :: Crawl)

-- | The log group associated with the crawl.
crawl_logGroup :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_logGroup = Lens.lens (\Crawl' {logGroup} -> logGroup) (\s@Crawl' {} a -> s {logGroup = a} :: Crawl)

-- | The error message associated with the crawl.
crawl_errorMessage :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_errorMessage = Lens.lens (\Crawl' {errorMessage} -> errorMessage) (\s@Crawl' {} a -> s {errorMessage = a} :: Crawl)

instance Core.FromJSON Crawl where
  parseJSON =
    Core.withObject
      "Crawl"
      ( \x ->
          Crawl'
            Prelude.<$> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "LogStream")
            Prelude.<*> (x Core..:? "LogGroup")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable Crawl where
  hashWithSalt salt' Crawl' {..} =
    salt' `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` logStream
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` completedOn

instance Prelude.NFData Crawl where
  rnf Crawl' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf logStream
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf state
