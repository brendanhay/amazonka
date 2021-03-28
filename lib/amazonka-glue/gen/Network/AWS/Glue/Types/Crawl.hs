{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Crawl
  ( Crawl (..)
  -- * Smart constructor
  , mkCrawl
  -- * Lenses
  , cCompletedOn
  , cErrorMessage
  , cLogGroup
  , cLogStream
  , cStartedOn
  , cState
  ) where

import qualified Network.AWS.Glue.Types.CrawlState as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.LogGroup as Types
import qualified Network.AWS.Glue.Types.LogStream as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a crawl in the workflow.
--
-- /See:/ 'mkCrawl' smart constructor.
data Crawl = Crawl'
  { completedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time on which the crawl completed.
  , errorMessage :: Core.Maybe Types.DescriptionString
    -- ^ The error message associated with the crawl.
  , logGroup :: Core.Maybe Types.LogGroup
    -- ^ The log group associated with the crawl.
  , logStream :: Core.Maybe Types.LogStream
    -- ^ The log stream associated with the crawl.
  , startedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time on which the crawl started.
  , state :: Core.Maybe Types.CrawlState
    -- ^ The state of the crawler.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Crawl' value with any optional fields omitted.
mkCrawl
    :: Crawl
mkCrawl
  = Crawl'{completedOn = Core.Nothing, errorMessage = Core.Nothing,
           logGroup = Core.Nothing, logStream = Core.Nothing,
           startedOn = Core.Nothing, state = Core.Nothing}

-- | The date and time on which the crawl completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedOn :: Lens.Lens' Crawl (Core.Maybe Core.NominalDiffTime)
cCompletedOn = Lens.field @"completedOn"
{-# INLINEABLE cCompletedOn #-}
{-# DEPRECATED completedOn "Use generic-lens or generic-optics with 'completedOn' instead"  #-}

-- | The error message associated with the crawl.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cErrorMessage :: Lens.Lens' Crawl (Core.Maybe Types.DescriptionString)
cErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE cErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The log group associated with the crawl.
--
-- /Note:/ Consider using 'logGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogGroup :: Lens.Lens' Crawl (Core.Maybe Types.LogGroup)
cLogGroup = Lens.field @"logGroup"
{-# INLINEABLE cLogGroup #-}
{-# DEPRECATED logGroup "Use generic-lens or generic-optics with 'logGroup' instead"  #-}

-- | The log stream associated with the crawl.
--
-- /Note:/ Consider using 'logStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogStream :: Lens.Lens' Crawl (Core.Maybe Types.LogStream)
cLogStream = Lens.field @"logStream"
{-# INLINEABLE cLogStream #-}
{-# DEPRECATED logStream "Use generic-lens or generic-optics with 'logStream' instead"  #-}

-- | The date and time on which the crawl started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStartedOn :: Lens.Lens' Crawl (Core.Maybe Core.NominalDiffTime)
cStartedOn = Lens.field @"startedOn"
{-# INLINEABLE cStartedOn #-}
{-# DEPRECATED startedOn "Use generic-lens or generic-optics with 'startedOn' instead"  #-}

-- | The state of the crawler.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Crawl (Core.Maybe Types.CrawlState)
cState = Lens.field @"state"
{-# INLINEABLE cState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON Crawl where
        parseJSON
          = Core.withObject "Crawl" Core.$
              \ x ->
                Crawl' Core.<$>
                  (x Core..:? "CompletedOn") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "LogGroup"
                    Core.<*> x Core..:? "LogStream"
                    Core.<*> x Core..:? "StartedOn"
                    Core.<*> x Core..:? "State"
