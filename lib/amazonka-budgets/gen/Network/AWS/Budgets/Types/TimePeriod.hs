{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimePeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.TimePeriod
  ( TimePeriod (..)
  -- * Smart constructor
  , mkTimePeriod
  -- * Lenses
  , tpEnd
  , tpStart
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date. 
--
-- /See:/ 'mkTimePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { end :: Core.Maybe Core.NominalDiffTime
    -- ^ The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
  , start :: Core.Maybe Core.NominalDiffTime
    -- ^ The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TimePeriod' value with any optional fields omitted.
mkTimePeriod
    :: TimePeriod
mkTimePeriod
  = TimePeriod'{end = Core.Nothing, start = Core.Nothing}

-- | The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpEnd :: Lens.Lens' TimePeriod (Core.Maybe Core.NominalDiffTime)
tpEnd = Lens.field @"end"
{-# INLINEABLE tpEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpStart :: Lens.Lens' TimePeriod (Core.Maybe Core.NominalDiffTime)
tpStart = Lens.field @"start"
{-# INLINEABLE tpStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

instance Core.FromJSON TimePeriod where
        toJSON TimePeriod{..}
          = Core.object
              (Core.catMaybes
                 [("End" Core..=) Core.<$> end, ("Start" Core..=) Core.<$> start])

instance Core.FromJSON TimePeriod where
        parseJSON
          = Core.withObject "TimePeriod" Core.$
              \ x ->
                TimePeriod' Core.<$> (x Core..:? "End") Core.<*> x Core..:? "Start"
