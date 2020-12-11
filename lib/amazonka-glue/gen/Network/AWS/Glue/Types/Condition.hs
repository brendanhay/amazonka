-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cCrawlState,
    cState,
    cJobName,
    cLogicalOperator,
    cCrawlerName,
  )
where

import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.LogicalOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { crawlState :: Lude.Maybe CrawlState,
    state :: Lude.Maybe JobRunState,
    jobName :: Lude.Maybe Lude.Text,
    logicalOperator :: Lude.Maybe LogicalOperator,
    crawlerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'crawlState' - The state of the crawler to which this condition applies.
-- * 'crawlerName' - The name of the crawler to which this condition applies.
-- * 'jobName' - The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
-- * 'logicalOperator' - A logical operator.
-- * 'state' - The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
mkCondition ::
  Condition
mkCondition =
  Condition'
    { crawlState = Lude.Nothing,
      state = Lude.Nothing,
      jobName = Lude.Nothing,
      logicalOperator = Lude.Nothing,
      crawlerName = Lude.Nothing
    }

-- | The state of the crawler to which this condition applies.
--
-- /Note:/ Consider using 'crawlState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCrawlState :: Lens.Lens' Condition (Lude.Maybe CrawlState)
cCrawlState = Lens.lens (crawlState :: Condition -> Lude.Maybe CrawlState) (\s a -> s {crawlState = a} :: Condition)
{-# DEPRECATED cCrawlState "Use generic-lens or generic-optics with 'crawlState' instead." #-}

-- | The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Condition (Lude.Maybe JobRunState)
cState = Lens.lens (state :: Condition -> Lude.Maybe JobRunState) (\s a -> s {state = a} :: Condition)
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJobName :: Lens.Lens' Condition (Lude.Maybe Lude.Text)
cJobName = Lens.lens (jobName :: Condition -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: Condition)
{-# DEPRECATED cJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | A logical operator.
--
-- /Note:/ Consider using 'logicalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogicalOperator :: Lens.Lens' Condition (Lude.Maybe LogicalOperator)
cLogicalOperator = Lens.lens (logicalOperator :: Condition -> Lude.Maybe LogicalOperator) (\s a -> s {logicalOperator = a} :: Condition)
{-# DEPRECATED cLogicalOperator "Use generic-lens or generic-optics with 'logicalOperator' instead." #-}

-- | The name of the crawler to which this condition applies.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCrawlerName :: Lens.Lens' Condition (Lude.Maybe Lude.Text)
cCrawlerName = Lens.lens (crawlerName :: Condition -> Lude.Maybe Lude.Text) (\s a -> s {crawlerName = a} :: Condition)
{-# DEPRECATED cCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.FromJSON Condition where
  parseJSON =
    Lude.withObject
      "Condition"
      ( \x ->
          Condition'
            Lude.<$> (x Lude..:? "CrawlState")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "LogicalOperator")
            Lude.<*> (x Lude..:? "CrawlerName")
      )

instance Lude.ToJSON Condition where
  toJSON Condition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CrawlState" Lude..=) Lude.<$> crawlState,
            ("State" Lude..=) Lude.<$> state,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("LogicalOperator" Lude..=) Lude.<$> logicalOperator,
            ("CrawlerName" Lude..=) Lude.<$> crawlerName
          ]
      )
