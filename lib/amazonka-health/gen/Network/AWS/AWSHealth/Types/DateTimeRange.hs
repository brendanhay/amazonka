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
    dtrFrom,
    dtrTo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of dates and times that is used by the <https://docs.aws.amazon.com/health/latest/APIReference/API_EventFilter.html EventFilter> and <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> objects. If @from@ is set and @to@ is set: match items where the timestamp (@startTime@ , @endTime@ , or @lastUpdatedTime@ ) is between @from@ and @to@ inclusive. If @from@ is set and @to@ is not set: match items where the timestamp value is equal to or after @from@ . If @from@ is not set and @to@ is set: match items where the timestamp value is equal to or before @to@ .
--
-- /See:/ 'mkDateTimeRange' smart constructor.
data DateTimeRange = DateTimeRange'
  { -- | The starting date and time of a time range.
    from :: Core.Maybe Core.NominalDiffTime,
    -- | The ending date and time of a time range.
    to :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DateTimeRange' value with any optional fields omitted.
mkDateTimeRange ::
  DateTimeRange
mkDateTimeRange =
  DateTimeRange' {from = Core.Nothing, to = Core.Nothing}

-- | The starting date and time of a time range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrFrom :: Lens.Lens' DateTimeRange (Core.Maybe Core.NominalDiffTime)
dtrFrom = Lens.field @"from"
{-# DEPRECATED dtrFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The ending date and time of a time range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrTo :: Lens.Lens' DateTimeRange (Core.Maybe Core.NominalDiffTime)
dtrTo = Lens.field @"to"
{-# DEPRECATED dtrTo "Use generic-lens or generic-optics with 'to' instead." #-}

instance Core.FromJSON DateTimeRange where
  toJSON DateTimeRange {..} =
    Core.object
      ( Core.catMaybes
          [("from" Core..=) Core.<$> from, ("to" Core..=) Core.<$> to]
      )
