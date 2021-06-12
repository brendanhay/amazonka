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
-- Module      : Network.AWS.Glue.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Condition where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.LogicalOperator
import qualified Network.AWS.Lens as Lens

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The state of the crawler to which this condition applies.
    crawlState :: Core.Maybe CrawlState,
    -- | The name of the crawler to which this condition applies.
    crawlerName :: Core.Maybe Core.Text,
    -- | The condition state. Currently, the only job states that a trigger can
    -- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
    -- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
    -- and @CANCELLED@.
    state :: Core.Maybe JobRunState,
    -- | A logical operator.
    logicalOperator :: Core.Maybe LogicalOperator,
    -- | The name of the job whose @JobRuns@ this condition applies to, and on
    -- which this trigger waits.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'state', 'condition_state' - The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
--
-- 'logicalOperator', 'condition_logicalOperator' - A logical operator.
--
-- 'jobName', 'condition_jobName' - The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
newCondition ::
  Condition
newCondition =
  Condition'
    { crawlState = Core.Nothing,
      crawlerName = Core.Nothing,
      state = Core.Nothing,
      logicalOperator = Core.Nothing,
      jobName = Core.Nothing
    }

-- | The state of the crawler to which this condition applies.
condition_crawlState :: Lens.Lens' Condition (Core.Maybe CrawlState)
condition_crawlState = Lens.lens (\Condition' {crawlState} -> crawlState) (\s@Condition' {} a -> s {crawlState = a} :: Condition)

-- | The name of the crawler to which this condition applies.
condition_crawlerName :: Lens.Lens' Condition (Core.Maybe Core.Text)
condition_crawlerName = Lens.lens (\Condition' {crawlerName} -> crawlerName) (\s@Condition' {} a -> s {crawlerName = a} :: Condition)

-- | The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
condition_state :: Lens.Lens' Condition (Core.Maybe JobRunState)
condition_state = Lens.lens (\Condition' {state} -> state) (\s@Condition' {} a -> s {state = a} :: Condition)

-- | A logical operator.
condition_logicalOperator :: Lens.Lens' Condition (Core.Maybe LogicalOperator)
condition_logicalOperator = Lens.lens (\Condition' {logicalOperator} -> logicalOperator) (\s@Condition' {} a -> s {logicalOperator = a} :: Condition)

-- | The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
condition_jobName :: Lens.Lens' Condition (Core.Maybe Core.Text)
condition_jobName = Lens.lens (\Condition' {jobName} -> jobName) (\s@Condition' {} a -> s {jobName = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Core.<$> (x Core..:? "CrawlState")
            Core.<*> (x Core..:? "CrawlerName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "LogicalOperator")
            Core.<*> (x Core..:? "JobName")
      )

instance Core.Hashable Condition

instance Core.NFData Condition

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CrawlState" Core..=) Core.<$> crawlState,
            ("CrawlerName" Core..=) Core.<$> crawlerName,
            ("State" Core..=) Core.<$> state,
            ("LogicalOperator" Core..=) Core.<$> logicalOperator,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
