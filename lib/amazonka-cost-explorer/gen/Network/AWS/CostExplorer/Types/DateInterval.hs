-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DateInterval
  ( DateInterval (..),

    -- * Smart constructor
    mkDateInterval,

    -- * Lenses
    diStart,
    diEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The time period that you want the usage and costs for.
--
-- /See:/ 'mkDateInterval' smart constructor.
data DateInterval = DateInterval'
  { start :: Lude.Text,
    end :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DateInterval' with the minimum fields required to make a request.
--
-- * 'end' - The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
-- * 'start' - The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
mkDateInterval ::
  -- | 'start'
  Lude.Text ->
  -- | 'end'
  Lude.Text ->
  DateInterval
mkDateInterval pStart_ pEnd_ =
  DateInterval' {start = pStart_, end = pEnd_}

-- | The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStart :: Lens.Lens' DateInterval Lude.Text
diStart = Lens.lens (start :: DateInterval -> Lude.Text) (\s a -> s {start = a} :: DateInterval)
{-# DEPRECATED diStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEnd :: Lens.Lens' DateInterval Lude.Text
diEnd = Lens.lens (end :: DateInterval -> Lude.Text) (\s a -> s {end = a} :: DateInterval)
{-# DEPRECATED diEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.FromJSON DateInterval where
  parseJSON =
    Lude.withObject
      "DateInterval"
      ( \x ->
          DateInterval'
            Lude.<$> (x Lude..: "Start") Lude.<*> (x Lude..: "End")
      )

instance Lude.ToJSON DateInterval where
  toJSON DateInterval' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Start" Lude..= start), Lude.Just ("End" Lude..= end)]
      )
