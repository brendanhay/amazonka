{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ExponentialRolloutRate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExponentialRolloutRate
  ( ExponentialRolloutRate (..),

    -- * Smart constructor
    mkExponentialRolloutRate,

    -- * Lenses
    errBaseRatePerMinute,
    errIncrementFactor,
    errRateIncreaseCriteria,
  )
where

import qualified Network.AWS.IoT.Types.RateIncreaseCriteria as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Allows you to create an exponential rate of rollout for a job.
--
-- /See:/ 'mkExponentialRolloutRate' smart constructor.
data ExponentialRolloutRate = ExponentialRolloutRate'
  { -- | The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
    baseRatePerMinute :: Core.Natural,
    -- | The exponential factor to increase the rate of rollout for a job.
    --
    -- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
    incrementFactor :: Core.Double,
    -- | The criteria to initiate the increase in rate of rollout for a job.
    rateIncreaseCriteria :: Types.RateIncreaseCriteria
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExponentialRolloutRate' value with any optional fields omitted.
mkExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Core.Natural ->
  -- | 'incrementFactor'
  Core.Double ->
  -- | 'rateIncreaseCriteria'
  Types.RateIncreaseCriteria ->
  ExponentialRolloutRate
mkExponentialRolloutRate
  baseRatePerMinute
  incrementFactor
  rateIncreaseCriteria =
    ExponentialRolloutRate'
      { baseRatePerMinute,
        incrementFactor,
        rateIncreaseCriteria
      }

-- | The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
--
-- /Note:/ Consider using 'baseRatePerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errBaseRatePerMinute :: Lens.Lens' ExponentialRolloutRate Core.Natural
errBaseRatePerMinute = Lens.field @"baseRatePerMinute"
{-# DEPRECATED errBaseRatePerMinute "Use generic-lens or generic-optics with 'baseRatePerMinute' instead." #-}

-- | The exponential factor to increase the rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
--
-- /Note:/ Consider using 'incrementFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errIncrementFactor :: Lens.Lens' ExponentialRolloutRate Core.Double
errIncrementFactor = Lens.field @"incrementFactor"
{-# DEPRECATED errIncrementFactor "Use generic-lens or generic-optics with 'incrementFactor' instead." #-}

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /Note:/ Consider using 'rateIncreaseCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errRateIncreaseCriteria :: Lens.Lens' ExponentialRolloutRate Types.RateIncreaseCriteria
errRateIncreaseCriteria = Lens.field @"rateIncreaseCriteria"
{-# DEPRECATED errRateIncreaseCriteria "Use generic-lens or generic-optics with 'rateIncreaseCriteria' instead." #-}

instance Core.FromJSON ExponentialRolloutRate where
  toJSON ExponentialRolloutRate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("baseRatePerMinute" Core..= baseRatePerMinute),
            Core.Just ("incrementFactor" Core..= incrementFactor),
            Core.Just ("rateIncreaseCriteria" Core..= rateIncreaseCriteria)
          ]
      )

instance Core.FromJSON ExponentialRolloutRate where
  parseJSON =
    Core.withObject "ExponentialRolloutRate" Core.$
      \x ->
        ExponentialRolloutRate'
          Core.<$> (x Core..: "baseRatePerMinute")
          Core.<*> (x Core..: "incrementFactor")
          Core.<*> (x Core..: "rateIncreaseCriteria")
