{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
  ( AwsJobRateIncreaseCriteria (..)
  -- * Smart constructor
  , mkAwsJobRateIncreaseCriteria
  -- * Lenses
  , ajricNumberOfNotifiedThings
  , ajricNumberOfSucceededThings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'mkAwsJobRateIncreaseCriteria' smart constructor.
data AwsJobRateIncreaseCriteria = AwsJobRateIncreaseCriteria'
  { numberOfNotifiedThings :: Core.Maybe Core.Natural
    -- ^ When this number of things have been notified, it will initiate an increase in the rollout rate.
  , numberOfSucceededThings :: Core.Maybe Core.Natural
    -- ^ When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobRateIncreaseCriteria' value with any optional fields omitted.
mkAwsJobRateIncreaseCriteria
    :: AwsJobRateIncreaseCriteria
mkAwsJobRateIncreaseCriteria
  = AwsJobRateIncreaseCriteria'{numberOfNotifiedThings =
                                  Core.Nothing,
                                numberOfSucceededThings = Core.Nothing}

-- | When this number of things have been notified, it will initiate an increase in the rollout rate.
--
-- /Note:/ Consider using 'numberOfNotifiedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajricNumberOfNotifiedThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Core.Maybe Core.Natural)
ajricNumberOfNotifiedThings = Lens.field @"numberOfNotifiedThings"
{-# INLINEABLE ajricNumberOfNotifiedThings #-}
{-# DEPRECATED numberOfNotifiedThings "Use generic-lens or generic-optics with 'numberOfNotifiedThings' instead"  #-}

-- | When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajricNumberOfSucceededThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Core.Maybe Core.Natural)
ajricNumberOfSucceededThings = Lens.field @"numberOfSucceededThings"
{-# INLINEABLE ajricNumberOfSucceededThings #-}
{-# DEPRECATED numberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead"  #-}

instance Core.FromJSON AwsJobRateIncreaseCriteria where
        toJSON AwsJobRateIncreaseCriteria{..}
          = Core.object
              (Core.catMaybes
                 [("numberOfNotifiedThings" Core..=) Core.<$>
                    numberOfNotifiedThings,
                  ("numberOfSucceededThings" Core..=) Core.<$>
                    numberOfSucceededThings])

instance Core.FromJSON AwsJobRateIncreaseCriteria where
        parseJSON
          = Core.withObject "AwsJobRateIncreaseCriteria" Core.$
              \ x ->
                AwsJobRateIncreaseCriteria' Core.<$>
                  (x Core..:? "numberOfNotifiedThings") Core.<*>
                    x Core..:? "numberOfSucceededThings"
