-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
  ( HLSTimestampRange (..),

    -- * Smart constructor
    mkHLSTimestampRange,

    -- * Lenses
    hlstrEndTimestamp,
    hlstrStartTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
-- /See:/ 'mkHLSTimestampRange' smart constructor.
data HLSTimestampRange = HLSTimestampRange'
  { endTimestamp ::
      Lude.Maybe Lude.Timestamp,
    startTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HLSTimestampRange' with the minimum fields required to make a request.
--
-- * 'endTimestamp' - The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
-- * 'startTimestamp' - The start of the timestamp range for the requested media.
--
-- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
mkHLSTimestampRange ::
  HLSTimestampRange
mkHLSTimestampRange =
  HLSTimestampRange'
    { endTimestamp = Lude.Nothing,
      startTimestamp = Lude.Nothing
    }

-- | The end of the timestamp range for the requested media. This value must be within 3 hours of the specified @StartTimestamp@ , and it must be later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@ , this value must be in the past.
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for @LIVE_REPLAY@ mode then the session will continue to include newly ingested fragments until the session expires.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlstrEndTimestamp :: Lens.Lens' HLSTimestampRange (Lude.Maybe Lude.Timestamp)
hlstrEndTimestamp = Lens.lens (endTimestamp :: HLSTimestampRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTimestamp = a} :: HLSTimestampRange)
{-# DEPRECATED hlstrEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

-- | The start of the timestamp range for the requested media.
--
-- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@ value is required.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlstrStartTimestamp :: Lens.Lens' HLSTimestampRange (Lude.Maybe Lude.Timestamp)
hlstrStartTimestamp = Lens.lens (startTimestamp :: HLSTimestampRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTimestamp = a} :: HLSTimestampRange)
{-# DEPRECATED hlstrStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

instance Lude.ToJSON HLSTimestampRange where
  toJSON HLSTimestampRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndTimestamp" Lude..=) Lude.<$> endTimestamp,
            ("StartTimestamp" Lude..=) Lude.<$> startTimestamp
          ]
      )
