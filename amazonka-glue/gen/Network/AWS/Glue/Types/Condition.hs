{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.LogicalOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The state of the crawler to which this condition applies.
    crawlState :: Prelude.Maybe CrawlState,
    -- | The name of the crawler to which this condition applies.
    crawlerName :: Prelude.Maybe Prelude.Text,
    -- | The condition state. Currently, the only job states that a trigger can
    -- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
    -- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
    -- and @CANCELLED@.
    state :: Prelude.Maybe JobRunState,
    -- | A logical operator.
    logicalOperator :: Prelude.Maybe LogicalOperator,
    -- | The name of the job whose @JobRuns@ this condition applies to, and on
    -- which this trigger waits.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { crawlState = Prelude.Nothing,
      crawlerName = Prelude.Nothing,
      state = Prelude.Nothing,
      logicalOperator = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | The state of the crawler to which this condition applies.
condition_crawlState :: Lens.Lens' Condition (Prelude.Maybe CrawlState)
condition_crawlState = Lens.lens (\Condition' {crawlState} -> crawlState) (\s@Condition' {} a -> s {crawlState = a} :: Condition)

-- | The name of the crawler to which this condition applies.
condition_crawlerName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_crawlerName = Lens.lens (\Condition' {crawlerName} -> crawlerName) (\s@Condition' {} a -> s {crawlerName = a} :: Condition)

-- | The condition state. Currently, the only job states that a trigger can
-- listen for are @SUCCEEDED@, @STOPPED@, @FAILED@, and @TIMEOUT@. The only
-- crawler states that a trigger can listen for are @SUCCEEDED@, @FAILED@,
-- and @CANCELLED@.
condition_state :: Lens.Lens' Condition (Prelude.Maybe JobRunState)
condition_state = Lens.lens (\Condition' {state} -> state) (\s@Condition' {} a -> s {state = a} :: Condition)

-- | A logical operator.
condition_logicalOperator :: Lens.Lens' Condition (Prelude.Maybe LogicalOperator)
condition_logicalOperator = Lens.lens (\Condition' {logicalOperator} -> logicalOperator) (\s@Condition' {} a -> s {logicalOperator = a} :: Condition)

-- | The name of the job whose @JobRuns@ this condition applies to, and on
-- which this trigger waits.
condition_jobName :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_jobName = Lens.lens (\Condition' {jobName} -> jobName) (\s@Condition' {} a -> s {jobName = a} :: Condition)

instance Prelude.FromJSON Condition where
  parseJSON =
    Prelude.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Prelude..:? "CrawlState")
            Prelude.<*> (x Prelude..:? "CrawlerName")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "LogicalOperator")
            Prelude.<*> (x Prelude..:? "JobName")
      )

instance Prelude.Hashable Condition

instance Prelude.NFData Condition

instance Prelude.ToJSON Condition where
  toJSON Condition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CrawlState" Prelude..=) Prelude.<$> crawlState,
            ("CrawlerName" Prelude..=) Prelude.<$> crawlerName,
            ("State" Prelude..=) Prelude.<$> state,
            ("LogicalOperator" Prelude..=)
              Prelude.<$> logicalOperator,
            ("JobName" Prelude..=) Prelude.<$> jobName
          ]
      )
