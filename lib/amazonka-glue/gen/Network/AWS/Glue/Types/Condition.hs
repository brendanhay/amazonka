{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Condition
  ( Condition (..)
  -- * Smart constructor
  , mkCondition
  -- * Lenses
  , cfCrawlState
  , cfCrawlerName
  , cfJobName
  , cfLogicalOperator
  , cfState
  ) where

import qualified Network.AWS.Glue.Types.CrawlState as Types
import qualified Network.AWS.Glue.Types.JobRunState as Types
import qualified Network.AWS.Glue.Types.LogicalOperator as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a condition under which a trigger fires.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { crawlState :: Core.Maybe Types.CrawlState
    -- ^ The state of the crawler to which this condition applies.
  , crawlerName :: Core.Maybe Types.NameString
    -- ^ The name of the crawler to which this condition applies.
  , jobName :: Core.Maybe Types.NameString
    -- ^ The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
  , logicalOperator :: Core.Maybe Types.LogicalOperator
    -- ^ A logical operator.
  , state :: Core.Maybe Types.JobRunState
    -- ^ The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Condition' value with any optional fields omitted.
mkCondition
    :: Condition
mkCondition
  = Condition'{crawlState = Core.Nothing, crawlerName = Core.Nothing,
               jobName = Core.Nothing, logicalOperator = Core.Nothing,
               state = Core.Nothing}

-- | The state of the crawler to which this condition applies.
--
-- /Note:/ Consider using 'crawlState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCrawlState :: Lens.Lens' Condition (Core.Maybe Types.CrawlState)
cfCrawlState = Lens.field @"crawlState"
{-# INLINEABLE cfCrawlState #-}
{-# DEPRECATED crawlState "Use generic-lens or generic-optics with 'crawlState' instead"  #-}

-- | The name of the crawler to which this condition applies.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCrawlerName :: Lens.Lens' Condition (Core.Maybe Types.NameString)
cfCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE cfCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

-- | The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfJobName :: Lens.Lens' Condition (Core.Maybe Types.NameString)
cfJobName = Lens.field @"jobName"
{-# INLINEABLE cfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | A logical operator.
--
-- /Note:/ Consider using 'logicalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogicalOperator :: Lens.Lens' Condition (Core.Maybe Types.LogicalOperator)
cfLogicalOperator = Lens.field @"logicalOperator"
{-# INLINEABLE cfLogicalOperator #-}
{-# DEPRECATED logicalOperator "Use generic-lens or generic-optics with 'logicalOperator' instead"  #-}

-- | The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfState :: Lens.Lens' Condition (Core.Maybe Types.JobRunState)
cfState = Lens.field @"state"
{-# INLINEABLE cfState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON Condition where
        toJSON Condition{..}
          = Core.object
              (Core.catMaybes
                 [("CrawlState" Core..=) Core.<$> crawlState,
                  ("CrawlerName" Core..=) Core.<$> crawlerName,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("LogicalOperator" Core..=) Core.<$> logicalOperator,
                  ("State" Core..=) Core.<$> state])

instance Core.FromJSON Condition where
        parseJSON
          = Core.withObject "Condition" Core.$
              \ x ->
                Condition' Core.<$>
                  (x Core..:? "CrawlState") Core.<*> x Core..:? "CrawlerName"
                    Core.<*> x Core..:? "JobName"
                    Core.<*> x Core..:? "LogicalOperator"
                    Core.<*> x Core..:? "State"
