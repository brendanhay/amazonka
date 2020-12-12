{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectObjectContentEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectObjectContentEventStream
  ( SelectObjectContentEventStream (..),

    -- * Smart constructor
    mkSelectObjectContentEventStream,

    -- * Lenses
    socesProgress,
    socesRecords,
    socesCont,
    socesStats,
    socesEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.StatsEvent

-- | The container for selecting objects from a content event stream.
--
-- /See:/ 'mkSelectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { progress ::
      Lude.Maybe ProgressEvent,
    records ::
      Lude.Maybe RecordsEvent,
    cont ::
      Lude.Maybe ContinuationEvent,
    stats ::
      Lude.Maybe StatsEvent,
    end :: Lude.Maybe EndEvent
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectObjectContentEventStream' with the minimum fields required to make a request.
--
-- * 'cont' - The Continuation Event.
-- * 'end' - The End Event.
-- * 'progress' - The Progress Event.
-- * 'records' - The Records Event.
-- * 'stats' - The Stats Event.
mkSelectObjectContentEventStream ::
  SelectObjectContentEventStream
mkSelectObjectContentEventStream =
  SelectObjectContentEventStream'
    { progress = Lude.Nothing,
      records = Lude.Nothing,
      cont = Lude.Nothing,
      stats = Lude.Nothing,
      end = Lude.Nothing
    }

-- | The Progress Event.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesProgress :: Lens.Lens' SelectObjectContentEventStream (Lude.Maybe ProgressEvent)
socesProgress = Lens.lens (progress :: SelectObjectContentEventStream -> Lude.Maybe ProgressEvent) (\s a -> s {progress = a} :: SelectObjectContentEventStream)
{-# DEPRECATED socesProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The Records Event.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesRecords :: Lens.Lens' SelectObjectContentEventStream (Lude.Maybe RecordsEvent)
socesRecords = Lens.lens (records :: SelectObjectContentEventStream -> Lude.Maybe RecordsEvent) (\s a -> s {records = a} :: SelectObjectContentEventStream)
{-# DEPRECATED socesRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | The Continuation Event.
--
-- /Note:/ Consider using 'cont' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesCont :: Lens.Lens' SelectObjectContentEventStream (Lude.Maybe ContinuationEvent)
socesCont = Lens.lens (cont :: SelectObjectContentEventStream -> Lude.Maybe ContinuationEvent) (\s a -> s {cont = a} :: SelectObjectContentEventStream)
{-# DEPRECATED socesCont "Use generic-lens or generic-optics with 'cont' instead." #-}

-- | The Stats Event.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesStats :: Lens.Lens' SelectObjectContentEventStream (Lude.Maybe StatsEvent)
socesStats = Lens.lens (stats :: SelectObjectContentEventStream -> Lude.Maybe StatsEvent) (\s a -> s {stats = a} :: SelectObjectContentEventStream)
{-# DEPRECATED socesStats "Use generic-lens or generic-optics with 'stats' instead." #-}

-- | The End Event.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socesEnd :: Lens.Lens' SelectObjectContentEventStream (Lude.Maybe EndEvent)
socesEnd = Lens.lens (end :: SelectObjectContentEventStream -> Lude.Maybe EndEvent) (\s a -> s {end = a} :: SelectObjectContentEventStream)
{-# DEPRECATED socesEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.FromXML SelectObjectContentEventStream where
  parseXML x =
    SelectObjectContentEventStream'
      Lude.<$> (x Lude..@? "Progress")
      Lude.<*> (x Lude..@? "Records")
      Lude.<*> (x Lude..@? "Cont")
      Lude.<*> (x Lude..@? "Stats")
      Lude.<*> (x Lude..@? "End")
