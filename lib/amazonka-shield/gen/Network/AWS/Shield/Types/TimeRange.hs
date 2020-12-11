-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.TimeRange
  ( TimeRange (..),

    -- * Smart constructor
    mkTimeRange,

    -- * Lenses
    trFromInclusive,
    trToExclusive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The time range.
--
-- /See:/ 'mkTimeRange' smart constructor.
data TimeRange = TimeRange'
  { fromInclusive ::
      Lude.Maybe Lude.Timestamp,
    toExclusive :: Lude.Maybe Lude.Timestamp
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
-- * 'fromInclusive' - The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
-- * 'toExclusive' - The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
mkTimeRange ::
  TimeRange
mkTimeRange =
  TimeRange'
    { fromInclusive = Lude.Nothing,
      toExclusive = Lude.Nothing
    }

-- | The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'fromInclusive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trFromInclusive :: Lens.Lens' TimeRange (Lude.Maybe Lude.Timestamp)
trFromInclusive = Lens.lens (fromInclusive :: TimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {fromInclusive = a} :: TimeRange)
{-# DEPRECATED trFromInclusive "Use generic-lens or generic-optics with 'fromInclusive' instead." #-}

-- | The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'toExclusive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trToExclusive :: Lens.Lens' TimeRange (Lude.Maybe Lude.Timestamp)
trToExclusive = Lens.lens (toExclusive :: TimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {toExclusive = a} :: TimeRange)
{-# DEPRECATED trToExclusive "Use generic-lens or generic-optics with 'toExclusive' instead." #-}

instance Lude.FromJSON TimeRange where
  parseJSON =
    Lude.withObject
      "TimeRange"
      ( \x ->
          TimeRange'
            Lude.<$> (x Lude..:? "FromInclusive") Lude.<*> (x Lude..:? "ToExclusive")
      )

instance Lude.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FromInclusive" Lude..=) Lude.<$> fromInclusive,
            ("ToExclusive" Lude..=) Lude.<$> toExclusive
          ]
      )
