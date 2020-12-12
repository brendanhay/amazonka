{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.QueueTransition
  ( QueueTransition (..),

    -- * Smart constructor
    mkQueueTransition,

    -- * Lenses
    qtSourceQueue,
    qtDestinationQueue,
    qtTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Description of the source and destination queues between which the job has moved, along with the timestamp of the move
--
-- /See:/ 'mkQueueTransition' smart constructor.
data QueueTransition = QueueTransition'
  { sourceQueue ::
      Lude.Maybe Lude.Text,
    destinationQueue :: Lude.Maybe Lude.Text,
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueueTransition' with the minimum fields required to make a request.
--
-- * 'destinationQueue' - The queue that the job was on after the transition.
-- * 'sourceQueue' - The queue that the job was on before the transition.
-- * 'timestamp' - The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
mkQueueTransition ::
  QueueTransition
mkQueueTransition =
  QueueTransition'
    { sourceQueue = Lude.Nothing,
      destinationQueue = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The queue that the job was on before the transition.
--
-- /Note:/ Consider using 'sourceQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtSourceQueue :: Lens.Lens' QueueTransition (Lude.Maybe Lude.Text)
qtSourceQueue = Lens.lens (sourceQueue :: QueueTransition -> Lude.Maybe Lude.Text) (\s a -> s {sourceQueue = a} :: QueueTransition)
{-# DEPRECATED qtSourceQueue "Use generic-lens or generic-optics with 'sourceQueue' instead." #-}

-- | The queue that the job was on after the transition.
--
-- /Note:/ Consider using 'destinationQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtDestinationQueue :: Lens.Lens' QueueTransition (Lude.Maybe Lude.Text)
qtDestinationQueue = Lens.lens (destinationQueue :: QueueTransition -> Lude.Maybe Lude.Text) (\s a -> s {destinationQueue = a} :: QueueTransition)
{-# DEPRECATED qtDestinationQueue "Use generic-lens or generic-optics with 'destinationQueue' instead." #-}

-- | The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTimestamp :: Lens.Lens' QueueTransition (Lude.Maybe Lude.Timestamp)
qtTimestamp = Lens.lens (timestamp :: QueueTransition -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: QueueTransition)
{-# DEPRECATED qtTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON QueueTransition where
  parseJSON =
    Lude.withObject
      "QueueTransition"
      ( \x ->
          QueueTransition'
            Lude.<$> (x Lude..:? "sourceQueue")
            Lude.<*> (x Lude..:? "destinationQueue")
            Lude.<*> (x Lude..:? "timestamp")
      )
