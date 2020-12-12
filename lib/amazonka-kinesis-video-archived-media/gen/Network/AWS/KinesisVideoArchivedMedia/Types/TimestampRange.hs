{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
  ( TimestampRange (..),

    -- * Smart constructor
    mkTimestampRange,

    -- * Lenses
    trStartTimestamp,
    trEndTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The range of timestamps for which to return fragments.
--
-- /See:/ 'mkTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { startTimestamp ::
      Lude.Timestamp,
    endTimestamp :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- * 'endTimestamp' - The ending timestamp in the range of timestamps for which to return fragments.
-- * 'startTimestamp' - The starting timestamp in the range of timestamps for which to return fragments.
mkTimestampRange ::
  -- | 'startTimestamp'
  Lude.Timestamp ->
  -- | 'endTimestamp'
  Lude.Timestamp ->
  TimestampRange
mkTimestampRange pStartTimestamp_ pEndTimestamp_ =
  TimestampRange'
    { startTimestamp = pStartTimestamp_,
      endTimestamp = pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return fragments.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStartTimestamp :: Lens.Lens' TimestampRange Lude.Timestamp
trStartTimestamp = Lens.lens (startTimestamp :: TimestampRange -> Lude.Timestamp) (\s a -> s {startTimestamp = a} :: TimestampRange)
{-# DEPRECATED trStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The ending timestamp in the range of timestamps for which to return fragments.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEndTimestamp :: Lens.Lens' TimestampRange Lude.Timestamp
trEndTimestamp = Lens.lens (endTimestamp :: TimestampRange -> Lude.Timestamp) (\s a -> s {endTimestamp = a} :: TimestampRange)
{-# DEPRECATED trEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

instance Lude.ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartTimestamp" Lude..= startTimestamp),
            Lude.Just ("EndTimestamp" Lude..= endTimestamp)
          ]
      )
