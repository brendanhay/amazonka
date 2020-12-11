-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
  ( RejectedLogEventsInfo (..),

    -- * Smart constructor
    mkRejectedLogEventsInfo,

    -- * Lenses
    rleiTooOldLogEventEndIndex,
    rleiTooNewLogEventStartIndex,
    rleiExpiredLogEventEndIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the rejected events.
--
-- /See:/ 'mkRejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
  { tooOldLogEventEndIndex ::
      Lude.Maybe Lude.Int,
    tooNewLogEventStartIndex :: Lude.Maybe Lude.Int,
    expiredLogEventEndIndex :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectedLogEventsInfo' with the minimum fields required to make a request.
--
-- * 'expiredLogEventEndIndex' - The expired log events.
-- * 'tooNewLogEventStartIndex' - The log events that are too new.
-- * 'tooOldLogEventEndIndex' - The log events that are too old.
mkRejectedLogEventsInfo ::
  RejectedLogEventsInfo
mkRejectedLogEventsInfo =
  RejectedLogEventsInfo'
    { tooOldLogEventEndIndex = Lude.Nothing,
      tooNewLogEventStartIndex = Lude.Nothing,
      expiredLogEventEndIndex = Lude.Nothing
    }

-- | The log events that are too old.
--
-- /Note:/ Consider using 'tooOldLogEventEndIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiTooOldLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Lude.Maybe Lude.Int)
rleiTooOldLogEventEndIndex = Lens.lens (tooOldLogEventEndIndex :: RejectedLogEventsInfo -> Lude.Maybe Lude.Int) (\s a -> s {tooOldLogEventEndIndex = a} :: RejectedLogEventsInfo)
{-# DEPRECATED rleiTooOldLogEventEndIndex "Use generic-lens or generic-optics with 'tooOldLogEventEndIndex' instead." #-}

-- | The log events that are too new.
--
-- /Note:/ Consider using 'tooNewLogEventStartIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiTooNewLogEventStartIndex :: Lens.Lens' RejectedLogEventsInfo (Lude.Maybe Lude.Int)
rleiTooNewLogEventStartIndex = Lens.lens (tooNewLogEventStartIndex :: RejectedLogEventsInfo -> Lude.Maybe Lude.Int) (\s a -> s {tooNewLogEventStartIndex = a} :: RejectedLogEventsInfo)
{-# DEPRECATED rleiTooNewLogEventStartIndex "Use generic-lens or generic-optics with 'tooNewLogEventStartIndex' instead." #-}

-- | The expired log events.
--
-- /Note:/ Consider using 'expiredLogEventEndIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rleiExpiredLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Lude.Maybe Lude.Int)
rleiExpiredLogEventEndIndex = Lens.lens (expiredLogEventEndIndex :: RejectedLogEventsInfo -> Lude.Maybe Lude.Int) (\s a -> s {expiredLogEventEndIndex = a} :: RejectedLogEventsInfo)
{-# DEPRECATED rleiExpiredLogEventEndIndex "Use generic-lens or generic-optics with 'expiredLogEventEndIndex' instead." #-}

instance Lude.FromJSON RejectedLogEventsInfo where
  parseJSON =
    Lude.withObject
      "RejectedLogEventsInfo"
      ( \x ->
          RejectedLogEventsInfo'
            Lude.<$> (x Lude..:? "tooOldLogEventEndIndex")
            Lude.<*> (x Lude..:? "tooNewLogEventStartIndex")
            Lude.<*> (x Lude..:? "expiredLogEventEndIndex")
      )
