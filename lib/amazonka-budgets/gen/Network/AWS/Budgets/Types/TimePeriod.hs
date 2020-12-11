-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimePeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimePeriod
  ( TimePeriod (..),

    -- * Smart constructor
    mkTimePeriod,

    -- * Lenses
    tpStart,
    tpEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date.
--
-- /See:/ 'mkTimePeriod' smart constructor.
data TimePeriod = TimePeriod'
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

-- | Creates a value of 'TimePeriod' with the minimum fields required to make a request.
--
-- * 'end' - The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
-- * 'start' - The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
mkTimePeriod ::
  TimePeriod
mkTimePeriod =
  TimePeriod' {start = Lude.Nothing, end = Lude.Nothing}

-- | The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpStart :: Lens.Lens' TimePeriod (Lude.Maybe Lude.Timestamp)
tpStart = Lens.lens (start :: TimePeriod -> Lude.Maybe Lude.Timestamp) (\s a -> s {start = a} :: TimePeriod)
{-# DEPRECATED tpStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpEnd :: Lens.Lens' TimePeriod (Lude.Maybe Lude.Timestamp)
tpEnd = Lens.lens (end :: TimePeriod -> Lude.Maybe Lude.Timestamp) (\s a -> s {end = a} :: TimePeriod)
{-# DEPRECATED tpEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.FromJSON TimePeriod where
  parseJSON =
    Lude.withObject
      "TimePeriod"
      ( \x ->
          TimePeriod'
            Lude.<$> (x Lude..:? "Start") Lude.<*> (x Lude..:? "End")
      )

instance Lude.ToJSON TimePeriod where
  toJSON TimePeriod' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Start" Lude..=) Lude.<$> start, ("End" Lude..=) Lude.<$> end]
      )
