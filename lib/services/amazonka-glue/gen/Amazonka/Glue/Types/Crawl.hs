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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Crawl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CrawlState
import qualified Amazonka.Prelude as Prelude

-- | The details of a crawl in the workflow.
--
-- /See:/ 'newCrawl' smart constructor.
data Crawl = Crawl'
  { -- | The date and time on which the crawl completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The error message associated with the crawl.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The log group associated with the crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The log stream associated with the crawl.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | The date and time on which the crawl started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The state of the crawler.
    state :: Prelude.Maybe CrawlState
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
-- 'errorMessage', 'crawl_errorMessage' - The error message associated with the crawl.
--
-- 'logGroup', 'crawl_logGroup' - The log group associated with the crawl.
--
-- 'logStream', 'crawl_logStream' - The log stream associated with the crawl.
--
-- 'startedOn', 'crawl_startedOn' - The date and time on which the crawl started.
--
-- 'state', 'crawl_state' - The state of the crawler.
newCrawl ::
  Crawl
newCrawl =
  Crawl'
    { completedOn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      logStream = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The date and time on which the crawl completed.
crawl_completedOn :: Lens.Lens' Crawl (Prelude.Maybe Prelude.UTCTime)
crawl_completedOn = Lens.lens (\Crawl' {completedOn} -> completedOn) (\s@Crawl' {} a -> s {completedOn = a} :: Crawl) Prelude.. Lens.mapping Data._Time

-- | The error message associated with the crawl.
crawl_errorMessage :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_errorMessage = Lens.lens (\Crawl' {errorMessage} -> errorMessage) (\s@Crawl' {} a -> s {errorMessage = a} :: Crawl)

-- | The log group associated with the crawl.
crawl_logGroup :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_logGroup = Lens.lens (\Crawl' {logGroup} -> logGroup) (\s@Crawl' {} a -> s {logGroup = a} :: Crawl)

-- | The log stream associated with the crawl.
crawl_logStream :: Lens.Lens' Crawl (Prelude.Maybe Prelude.Text)
crawl_logStream = Lens.lens (\Crawl' {logStream} -> logStream) (\s@Crawl' {} a -> s {logStream = a} :: Crawl)

-- | The date and time on which the crawl started.
crawl_startedOn :: Lens.Lens' Crawl (Prelude.Maybe Prelude.UTCTime)
crawl_startedOn = Lens.lens (\Crawl' {startedOn} -> startedOn) (\s@Crawl' {} a -> s {startedOn = a} :: Crawl) Prelude.. Lens.mapping Data._Time

-- | The state of the crawler.
crawl_state :: Lens.Lens' Crawl (Prelude.Maybe CrawlState)
crawl_state = Lens.lens (\Crawl' {state} -> state) (\s@Crawl' {} a -> s {state = a} :: Crawl)

instance Data.FromJSON Crawl where
  parseJSON =
    Data.withObject
      "Crawl"
      ( \x ->
          Crawl'
            Prelude.<$> (x Data..:? "CompletedOn")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LogGroup")
            Prelude.<*> (x Data..:? "LogStream")
            Prelude.<*> (x Data..:? "StartedOn")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Crawl where
  hashWithSalt _salt Crawl' {..} =
    _salt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` logStream
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` state

instance Prelude.NFData Crawl where
  rnf Crawl' {..} =
    Prelude.rnf completedOn `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf logGroup `Prelude.seq`
          Prelude.rnf logStream `Prelude.seq`
            Prelude.rnf startedOn `Prelude.seq`
              Prelude.rnf state
