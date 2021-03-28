{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.QueueTransition
  ( QueueTransition (..)
  -- * Smart constructor
  , mkQueueTransition
  -- * Lenses
  , qtDestinationQueue
  , qtSourceQueue
  , qtTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Description of the source and destination queues between which the job has moved, along with the timestamp of the move
--
-- /See:/ 'mkQueueTransition' smart constructor.
data QueueTransition = QueueTransition'
  { destinationQueue :: Core.Maybe Core.Text
    -- ^ The queue that the job was on after the transition.
  , sourceQueue :: Core.Maybe Core.Text
    -- ^ The queue that the job was on before the transition.
  , timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'QueueTransition' value with any optional fields omitted.
mkQueueTransition
    :: QueueTransition
mkQueueTransition
  = QueueTransition'{destinationQueue = Core.Nothing,
                     sourceQueue = Core.Nothing, timestamp = Core.Nothing}

-- | The queue that the job was on after the transition.
--
-- /Note:/ Consider using 'destinationQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtDestinationQueue :: Lens.Lens' QueueTransition (Core.Maybe Core.Text)
qtDestinationQueue = Lens.field @"destinationQueue"
{-# INLINEABLE qtDestinationQueue #-}
{-# DEPRECATED destinationQueue "Use generic-lens or generic-optics with 'destinationQueue' instead"  #-}

-- | The queue that the job was on before the transition.
--
-- /Note:/ Consider using 'sourceQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtSourceQueue :: Lens.Lens' QueueTransition (Core.Maybe Core.Text)
qtSourceQueue = Lens.field @"sourceQueue"
{-# INLINEABLE qtSourceQueue #-}
{-# DEPRECATED sourceQueue "Use generic-lens or generic-optics with 'sourceQueue' instead"  #-}

-- | The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTimestamp :: Lens.Lens' QueueTransition (Core.Maybe Core.NominalDiffTime)
qtTimestamp = Lens.field @"timestamp"
{-# INLINEABLE qtTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON QueueTransition where
        parseJSON
          = Core.withObject "QueueTransition" Core.$
              \ x ->
                QueueTransition' Core.<$>
                  (x Core..:? "destinationQueue") Core.<*> x Core..:? "sourceQueue"
                    Core.<*> x Core..:? "timestamp"
