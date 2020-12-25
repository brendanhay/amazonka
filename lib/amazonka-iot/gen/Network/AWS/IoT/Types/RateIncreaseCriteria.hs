{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RateIncreaseCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RateIncreaseCriteria
  ( RateIncreaseCriteria (..),

    -- * Smart constructor
    mkRateIncreaseCriteria,

    -- * Lenses
    ricNumberOfNotifiedThings,
    ricNumberOfSucceededThings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Allows you to define a criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'mkRateIncreaseCriteria' smart constructor.
data RateIncreaseCriteria = RateIncreaseCriteria'
  { -- | The threshold for number of notified things that will initiate the increase in rate of rollout.
    numberOfNotifiedThings :: Core.Maybe Core.Natural,
    -- | The threshold for number of succeeded things that will initiate the increase in rate of rollout.
    numberOfSucceededThings :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RateIncreaseCriteria' value with any optional fields omitted.
mkRateIncreaseCriteria ::
  RateIncreaseCriteria
mkRateIncreaseCriteria =
  RateIncreaseCriteria'
    { numberOfNotifiedThings = Core.Nothing,
      numberOfSucceededThings = Core.Nothing
    }

-- | The threshold for number of notified things that will initiate the increase in rate of rollout.
--
-- /Note:/ Consider using 'numberOfNotifiedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricNumberOfNotifiedThings :: Lens.Lens' RateIncreaseCriteria (Core.Maybe Core.Natural)
ricNumberOfNotifiedThings = Lens.field @"numberOfNotifiedThings"
{-# DEPRECATED ricNumberOfNotifiedThings "Use generic-lens or generic-optics with 'numberOfNotifiedThings' instead." #-}

-- | The threshold for number of succeeded things that will initiate the increase in rate of rollout.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricNumberOfSucceededThings :: Lens.Lens' RateIncreaseCriteria (Core.Maybe Core.Natural)
ricNumberOfSucceededThings = Lens.field @"numberOfSucceededThings"
{-# DEPRECATED ricNumberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead." #-}

instance Core.FromJSON RateIncreaseCriteria where
  toJSON RateIncreaseCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ ("numberOfNotifiedThings" Core..=)
              Core.<$> numberOfNotifiedThings,
            ("numberOfSucceededThings" Core..=)
              Core.<$> numberOfSucceededThings
          ]
      )

instance Core.FromJSON RateIncreaseCriteria where
  parseJSON =
    Core.withObject "RateIncreaseCriteria" Core.$
      \x ->
        RateIncreaseCriteria'
          Core.<$> (x Core..:? "numberOfNotifiedThings")
          Core.<*> (x Core..:? "numberOfSucceededThings")
