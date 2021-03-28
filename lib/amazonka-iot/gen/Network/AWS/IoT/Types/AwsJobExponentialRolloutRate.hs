{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
  ( AwsJobExponentialRolloutRate (..)
  -- * Smart constructor
  , mkAwsJobExponentialRolloutRate
  -- * Lenses
  , ajerrBaseRatePerMinute
  , ajerrIncrementFactor
  , ajerrRateIncreaseCriteria
  ) where

import qualified Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
-- /See:/ 'mkAwsJobExponentialRolloutRate' smart constructor.
data AwsJobExponentialRolloutRate = AwsJobExponentialRolloutRate'
  { baseRatePerMinute :: Core.Natural
    -- ^ The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
  , incrementFactor :: Core.Double
    -- ^ The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
  , rateIncreaseCriteria :: Types.AwsJobRateIncreaseCriteria
    -- ^ The criteria to initiate the increase in rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobExponentialRolloutRate' value with any optional fields omitted.
mkAwsJobExponentialRolloutRate
    :: Core.Natural -- ^ 'baseRatePerMinute'
    -> Core.Double -- ^ 'incrementFactor'
    -> Types.AwsJobRateIncreaseCriteria -- ^ 'rateIncreaseCriteria'
    -> AwsJobExponentialRolloutRate
mkAwsJobExponentialRolloutRate baseRatePerMinute incrementFactor
  rateIncreaseCriteria
  = AwsJobExponentialRolloutRate'{baseRatePerMinute, incrementFactor,
                                  rateIncreaseCriteria}

-- | The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
--
-- /Note:/ Consider using 'baseRatePerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrBaseRatePerMinute :: Lens.Lens' AwsJobExponentialRolloutRate Core.Natural
ajerrBaseRatePerMinute = Lens.field @"baseRatePerMinute"
{-# INLINEABLE ajerrBaseRatePerMinute #-}
{-# DEPRECATED baseRatePerMinute "Use generic-lens or generic-optics with 'baseRatePerMinute' instead"  #-}

-- | The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
--
-- /Note:/ Consider using 'incrementFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrIncrementFactor :: Lens.Lens' AwsJobExponentialRolloutRate Core.Double
ajerrIncrementFactor = Lens.field @"incrementFactor"
{-# INLINEABLE ajerrIncrementFactor #-}
{-# DEPRECATED incrementFactor "Use generic-lens or generic-optics with 'incrementFactor' instead"  #-}

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
--
-- /Note:/ Consider using 'rateIncreaseCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrRateIncreaseCriteria :: Lens.Lens' AwsJobExponentialRolloutRate Types.AwsJobRateIncreaseCriteria
ajerrRateIncreaseCriteria = Lens.field @"rateIncreaseCriteria"
{-# INLINEABLE ajerrRateIncreaseCriteria #-}
{-# DEPRECATED rateIncreaseCriteria "Use generic-lens or generic-optics with 'rateIncreaseCriteria' instead"  #-}

instance Core.FromJSON AwsJobExponentialRolloutRate where
        toJSON AwsJobExponentialRolloutRate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("baseRatePerMinute" Core..= baseRatePerMinute),
                  Core.Just ("incrementFactor" Core..= incrementFactor),
                  Core.Just ("rateIncreaseCriteria" Core..= rateIncreaseCriteria)])

instance Core.FromJSON AwsJobExponentialRolloutRate where
        parseJSON
          = Core.withObject "AwsJobExponentialRolloutRate" Core.$
              \ x ->
                AwsJobExponentialRolloutRate' Core.<$>
                  (x Core..: "baseRatePerMinute") Core.<*>
                    x Core..: "incrementFactor"
                    Core.<*> x Core..: "rateIncreaseCriteria"
