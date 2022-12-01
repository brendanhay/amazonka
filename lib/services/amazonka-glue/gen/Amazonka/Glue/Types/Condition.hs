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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CrawlState
import Amazonka.Glue.Types.JobRunState
import Amazonka.Glue.Types.LogicalOperator
import qualified Amazonka.Prelude as Prelude

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | A logical operator.
    logicalOperator :: Prelude.Maybe LogicalOperator,
    -- | The state of the crawler to which this condition applies.
    crawlState :: Prelude.Maybe CrawlState,
    -- | The name of the job whose @JobRuns@ this condition applies to, and on
    -- which this trigger waits.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The condition state. Currently, the only job states that a trigger can
    -- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
    -- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
    -- and @CANCELLED@.
    state :: Prelude.Maybe JobRunState,
    -- | The name of the crawler to which this condition applies.
    crawlerName :: Prelude.Maybe Prelude.Text
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
-- 'logicalOperator', 'condition_logicalOperator' - A logical operator.
--
-- 'crawlState', 'condition_crawlState' - The state of the crawler to which this condition applies.
--
-- 'jobName', 'condition_jobName' - The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
--
-- 'state', 'condition_state' - The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
--
-- 'crawlerName', 'condition_crawlerName' - The name of the crawler to which this condition applies.
newCondition ::
  Condition
newCondition =
  Condition'
    { logicalOperator = Prelude.Nothing,
      crawlState = Prelude.Nothing,
      jobName = Prelude.Nothing,
      state = Prelude.Nothing,
      crawlerName = Prelude.Nothing
    }

-- | A logical operator.
condition_logicalOperator :: Lens.Lens' Condition (Prelude.Maybe LogicalOperator)
condition_logicalOperator = Lens.lens (\Condition' {logicalOperator} -> logicalOperator) (\s@Condition' {} a -> s {logicalOperator = a} :: Condition)

-- | The state of the crawler to which this condition applies.
condition_crawlState :: Lens.Lens' Condition (Prelude.Maybe CrawlState)
condition_crawlState = Lens.lens (\Condition' {crawlState} -> crawlState) (\s@Condition' {} a -> s {crawlState = a} :: Condition)

-- | The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
condition_jobName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_jobName = Lens.lens (\Condition' {jobName} -> jobName) (\s@Condition' {} a -> s {jobName = a} :: Condition)

-- | The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
condition_state :: Lens.Lens' Condition (Prelude.Maybe JobRunState)
condition_state = Lens.lens (\Condition' {state} -> state) (\s@Condition' {} a -> s {state = a} :: Condition)

-- | The name of the crawler to which this condition applies.
condition_crawlerName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_crawlerName = Lens.lens (\Condition' {crawlerName} -> crawlerName) (\s@Condition' {} a -> s {crawlerName = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..:? "LogicalOperator")
            Prelude.<*> (x Core..:? "CrawlState")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CrawlerName")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` logicalOperator
      `Prelude.hashWithSalt` crawlState
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` crawlerName

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf logicalOperator
      `Prelude.seq` Prelude.rnf crawlState
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf crawlerName

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogicalOperator" Core..=)
              Prelude.<$> logicalOperator,
            ("CrawlState" Core..=) Prelude.<$> crawlState,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("State" Core..=) Prelude.<$> state,
            ("CrawlerName" Core..=) Prelude.<$> crawlerName
          ]
      )
