{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.DateTimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.DateTimeRange
  ( DateTimeRange (..),

    -- * Smart constructor
    mkDateTimeRange,

    -- * Lenses
    dtrTo,
    dtrFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of dates and times that is used by the <https://docs.aws.amazon.com/health/latest/APIReference/API_EventFilter.html EventFilter> and <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> objects. If @from@ is set and @to@ is set: match items where the timestamp (@startTime@ , @endTime@ , or @lastUpdatedTime@ ) is between @from@ and @to@ inclusive. If @from@ is set and @to@ is not set: match items where the timestamp value is equal to or after @from@ . If @from@ is not set and @to@ is set: match items where the timestamp value is equal to or before @to@ .
--
-- /See:/ 'mkDateTimeRange' smart constructor.
data DateTimeRange = DateTimeRange'
  { -- | The ending date and time of a time range.
    to :: Lude.Maybe Lude.Timestamp,
    -- | The starting date and time of a time range.
    from :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DateTimeRange' with the minimum fields required to make a request.
--
-- * 'to' - The ending date and time of a time range.
-- * 'from' - The starting date and time of a time range.
mkDateTimeRange ::
  DateTimeRange
mkDateTimeRange =
  DateTimeRange' {to = Lude.Nothing, from = Lude.Nothing}

-- | The ending date and time of a time range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrTo :: Lens.Lens' DateTimeRange (Lude.Maybe Lude.Timestamp)
dtrTo = Lens.lens (to :: DateTimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {to = a} :: DateTimeRange)
{-# DEPRECATED dtrTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The starting date and time of a time range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrFrom :: Lens.Lens' DateTimeRange (Lude.Maybe Lude.Timestamp)
dtrFrom = Lens.lens (from :: DateTimeRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {from = a} :: DateTimeRange)
{-# DEPRECATED dtrFrom "Use generic-lens or generic-optics with 'from' instead." #-}

instance Lude.ToJSON DateTimeRange where
  toJSON DateTimeRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [("to" Lude..=) Lude.<$> to, ("from" Lude..=) Lude.<$> from]
      )
