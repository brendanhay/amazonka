-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
  ( ClipTimestampRange (..),

    -- * Smart constructor
    mkClipTimestampRange,

    -- * Lenses
    ctrStartTimestamp,
    ctrEndTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The range of timestamps for which to return fragments.
--
-- The values in the ClipTimestampRange are @inclusive@ . Fragments that begin before the start time but continue past it, or fragments that begin before the end time but continue past it, are included in the session.
--
-- /See:/ 'mkClipTimestampRange' smart constructor.
data ClipTimestampRange = ClipTimestampRange'
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

-- | Creates a value of 'ClipTimestampRange' with the minimum fields required to make a request.
--
-- * 'endTimestamp' - The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session.
-- * 'startTimestamp' - The starting timestamp in the range of timestamps for which to return fragments.
--
-- This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head.
mkClipTimestampRange ::
  -- | 'startTimestamp'
  Lude.Timestamp ->
  -- | 'endTimestamp'
  Lude.Timestamp ->
  ClipTimestampRange
mkClipTimestampRange pStartTimestamp_ pEndTimestamp_ =
  ClipTimestampRange'
    { startTimestamp = pStartTimestamp_,
      endTimestamp = pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return fragments.
--
-- This value is inclusive. Fragments that start before the @StartTimestamp@ and continue past it are included in the session. If @FragmentSelectorType@ is @SERVER_TIMESTAMP@ , the @StartTimestamp@ must be later than the stream head.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrStartTimestamp :: Lens.Lens' ClipTimestampRange Lude.Timestamp
ctrStartTimestamp = Lens.lens (startTimestamp :: ClipTimestampRange -> Lude.Timestamp) (\s a -> s {startTimestamp = a} :: ClipTimestampRange)
{-# DEPRECATED ctrStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value. If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- This value is inclusive. The @EndTimestamp@ is compared to the (starting) timestamp of the fragment. Fragments that start before the @EndTimestamp@ value and continue past it are included in the session.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrEndTimestamp :: Lens.Lens' ClipTimestampRange Lude.Timestamp
ctrEndTimestamp = Lens.lens (endTimestamp :: ClipTimestampRange -> Lude.Timestamp) (\s a -> s {endTimestamp = a} :: ClipTimestampRange)
{-# DEPRECATED ctrEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

instance Lude.ToJSON ClipTimestampRange where
  toJSON ClipTimestampRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartTimestamp" Lude..= startTimestamp),
            Lude.Just ("EndTimestamp" Lude..= endTimestamp)
          ]
      )
