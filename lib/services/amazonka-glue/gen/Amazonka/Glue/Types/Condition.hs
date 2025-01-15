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
-- Module      : Amazonka.Glue.Types.Condition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CrawlState
import Amazonka.Glue.Types.JobRunState
import Amazonka.Glue.Types.LogicalOperator
import qualified Amazonka.Prelude as Prelude

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The state of the crawler to which this condition applies.
    crawlState :: Prelude.Maybe CrawlState,
    -- | The name of the crawler to which this condition applies.
    crawlerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the job whose @JobRuns@ this condition applies to, and on
    -- which this trigger waits.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | A logical operator.
    logicalOperator :: Prelude.Maybe LogicalOperator,
    -- | The condition state. Currently, the only job states that a trigger can
    -- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
    -- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
    -- and @CANCELLED@.
    state :: Prelude.Maybe JobRunState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlState', 'condition_crawlState' - The state of the crawler to which this condition applies.
--
-- 'crawlerName', 'condition_crawlerName' - The name of the crawler to which this condition applies.
--
-- 'jobName', 'condition_jobName' - The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
--
-- 'logicalOperator', 'condition_logicalOperator' - A logical operator.
--
-- 'state', 'condition_state' - The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
newCondition ::
  Condition
newCondition =
  Condition'
    { crawlState = Prelude.Nothing,
      crawlerName = Prelude.Nothing,
      jobName = Prelude.Nothing,
      logicalOperator = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The state of the crawler to which this condition applies.
condition_crawlState :: Lens.Lens' Condition (Prelude.Maybe CrawlState)
condition_crawlState = Lens.lens (\Condition' {crawlState} -> crawlState) (\s@Condition' {} a -> s {crawlState = a} :: Condition)

-- | The name of the crawler to which this condition applies.
condition_crawlerName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_crawlerName = Lens.lens (\Condition' {crawlerName} -> crawlerName) (\s@Condition' {} a -> s {crawlerName = a} :: Condition)

-- | The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
condition_jobName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_jobName = Lens.lens (\Condition' {jobName} -> jobName) (\s@Condition' {} a -> s {jobName = a} :: Condition)

-- | A logical operator.
condition_logicalOperator :: Lens.Lens' Condition (Prelude.Maybe LogicalOperator)
condition_logicalOperator = Lens.lens (\Condition' {logicalOperator} -> logicalOperator) (\s@Condition' {} a -> s {logicalOperator = a} :: Condition)

-- | The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
condition_state :: Lens.Lens' Condition (Prelude.Maybe JobRunState)
condition_state = Lens.lens (\Condition' {state} -> state) (\s@Condition' {} a -> s {state = a} :: Condition)

instance Data.FromJSON Condition where
  parseJSON =
    Data.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Data..:? "CrawlState")
            Prelude.<*> (x Data..:? "CrawlerName")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "LogicalOperator")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt
      `Prelude.hashWithSalt` crawlState
      `Prelude.hashWithSalt` crawlerName
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` logicalOperator
      `Prelude.hashWithSalt` state

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf crawlState `Prelude.seq`
      Prelude.rnf crawlerName `Prelude.seq`
        Prelude.rnf jobName `Prelude.seq`
          Prelude.rnf logicalOperator `Prelude.seq`
            Prelude.rnf state

instance Data.ToJSON Condition where
  toJSON Condition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlState" Data..=) Prelude.<$> crawlState,
            ("CrawlerName" Data..=) Prelude.<$> crawlerName,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("LogicalOperator" Data..=)
              Prelude.<$> logicalOperator,
            ("State" Data..=) Prelude.<$> state
          ]
      )
