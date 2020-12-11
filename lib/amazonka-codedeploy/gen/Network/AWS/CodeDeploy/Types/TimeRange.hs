-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeRange
  ( TimeRange (..),

    -- * Smart constructor
    mkTimeRange,

    -- * Lenses
    trStart,
    trEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a time range.
--
-- /See:/ 'mkTimeRange' smart constructor.
data TimeRange = TimeRange'
  { start :: Lude.Maybe Lude.Timestamp,
    end :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeRange' with the minimum fields required to make a request.
--
-- * 'end' - The end time of the time range.
-- * 'start' - The start time of the time range.
mkTimeRange ::
  TimeRange
mkTimeRange = TimeRange' {start = Lude.Nothing, end = Lude.Nothing}

-- | The start time of the time range.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStart :: Lens.Lens' TimeRange (Lude.Maybe Lude.Timestamp)
trStart = Lens.lens (start :: TimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {start = a} :: TimeRange)
{-# DEPRECATED trStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The end time of the time range.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEnd :: Lens.Lens' TimeRange (Lude.Maybe Lude.Timestamp)
trEnd = Lens.lens (end :: TimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {end = a} :: TimeRange)
{-# DEPRECATED trEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [("start" Lude..=) Lude.<$> start, ("end" Lude..=) Lude.<$> end]
      )
