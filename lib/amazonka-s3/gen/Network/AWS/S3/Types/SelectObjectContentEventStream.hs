{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectObjectContentEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SelectObjectContentEventStream
  ( SelectObjectContentEventStream (..)
  -- * Smart constructor
  , mkSelectObjectContentEventStream
  -- * Lenses
  , socesCont
  , socesEnd
  , socesProgress
  , socesRecords
  , socesStats
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ContinuationEvent as Types
import qualified Network.AWS.S3.Types.EndEvent as Types
import qualified Network.AWS.S3.Types.ProgressEvent as Types
import qualified Network.AWS.S3.Types.RecordsEvent as Types
import qualified Network.AWS.S3.Types.StatsEvent as Types

-- | The container for selecting objects from a content event stream.
--
-- /See:/ 'mkSelectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { cont :: Core.Maybe Types.ContinuationEvent
    -- ^ The Continuation Event.
  , end :: Core.Maybe Types.EndEvent
    -- ^ The End Event.
  , progress :: Core.Maybe Types.ProgressEvent
    -- ^ The Progress Event.
  , records :: Core.Maybe Types.RecordsEvent
    -- ^ The Records Event.
  , stats :: Core.Maybe Types.StatsEvent
    -- ^ The Stats Event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectObjectContentEventStream' value with any optional fields omitted.
mkSelectObjectContentEventStream
    :: SelectObjectContentEventStream
mkSelectObjectContentEventStream
  = SelectObjectContentEventStream'{cont = Core.Nothing,
                                    end = Core.Nothing, progress = Core.Nothing,
                                    records = Core.Nothing, stats = Core.Nothing}

-- | The Continuation Event.
--
-- /Note:/ Consider using 'cont' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesCont :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe Types.ContinuationEvent)
socesCont = Lens.field @"cont"
{-# INLINEABLE socesCont #-}
{-# DEPRECATED cont "Use generic-lens or generic-optics with 'cont' instead"  #-}

-- | The End Event.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesEnd :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe Types.EndEvent)
socesEnd = Lens.field @"end"
{-# INLINEABLE socesEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | The Progress Event.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesProgress :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe Types.ProgressEvent)
socesProgress = Lens.field @"progress"
{-# INLINEABLE socesProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The Records Event.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesRecords :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe Types.RecordsEvent)
socesRecords = Lens.field @"records"
{-# INLINEABLE socesRecords #-}
{-# DEPRECATED records "Use generic-lens or generic-optics with 'records' instead"  #-}

-- | The Stats Event.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesStats :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe Types.StatsEvent)
socesStats = Lens.field @"stats"
{-# INLINEABLE socesStats #-}
{-# DEPRECATED stats "Use generic-lens or generic-optics with 'stats' instead"  #-}

instance Core.FromXML SelectObjectContentEventStream where
        parseXML x
          = SelectObjectContentEventStream' Core.<$>
              (x Core..@? "Cont") Core.<*> x Core..@? "End" Core.<*>
                x Core..@? "Progress"
                Core.<*> x Core..@? "Records"
                Core.<*> x Core..@? "Stats"
