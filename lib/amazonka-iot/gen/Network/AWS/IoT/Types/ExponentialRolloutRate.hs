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

import Network.AWS.IoT.Types.RateIncreaseCriteria
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Allows you to create an exponential rate of rollout for a job.
--
-- /See:/ 'mkExponentialRolloutRate' smart constructor.
data ExponentialRolloutRate = ExponentialRolloutRate'
  { -- | The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
    baseRatePerMinute :: Lude.Natural,
    -- | The exponential factor to increase the rate of rollout for a job.
    --
    -- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
    incrementFactor :: Lude.Double,
    -- | The criteria to initiate the increase in rate of rollout for a job.
    rateIncreaseCriteria :: RateIncreaseCriteria
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExponentialRolloutRate' with the minimum fields required to make a request.
--
-- * 'baseRatePerMinute' - The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
-- * 'incrementFactor' - The exponential factor to increase the rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
-- * 'rateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job.
mkExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Lude.Natural ->
  -- | 'incrementFactor'
  Lude.Double ->
  -- | 'rateIncreaseCriteria'
  RateIncreaseCriteria ->
  ExponentialRolloutRate
mkExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    ExponentialRolloutRate'
      { baseRatePerMinute = pBaseRatePerMinute_,
        incrementFactor = pIncrementFactor_,
        rateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per minute at the start of job rollout. This parameter allows you to define the initial rate of rollout.
--
-- /Note:/ Consider using 'baseRatePerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errBaseRatePerMinute :: Lens.Lens' ExponentialRolloutRate Lude.Natural
errBaseRatePerMinute = Lens.lens (baseRatePerMinute :: ExponentialRolloutRate -> Lude.Natural) (\s a -> s {baseRatePerMinute = a} :: ExponentialRolloutRate)
{-# DEPRECATED errBaseRatePerMinute "Use generic-lens or generic-optics with 'baseRatePerMinute' instead." #-}

-- | The exponential factor to increase the rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
--
-- /Note:/ Consider using 'incrementFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errIncrementFactor :: Lens.Lens' ExponentialRolloutRate Lude.Double
errIncrementFactor = Lens.lens (incrementFactor :: ExponentialRolloutRate -> Lude.Double) (\s a -> s {incrementFactor = a} :: ExponentialRolloutRate)
{-# DEPRECATED errIncrementFactor "Use generic-lens or generic-optics with 'incrementFactor' instead." #-}

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /Note:/ Consider using 'rateIncreaseCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errRateIncreaseCriteria :: Lens.Lens' ExponentialRolloutRate RateIncreaseCriteria
errRateIncreaseCriteria = Lens.lens (rateIncreaseCriteria :: ExponentialRolloutRate -> RateIncreaseCriteria) (\s a -> s {rateIncreaseCriteria = a} :: ExponentialRolloutRate)
{-# DEPRECATED errRateIncreaseCriteria "Use generic-lens or generic-optics with 'rateIncreaseCriteria' instead." #-}

instance Lude.FromJSON ExponentialRolloutRate where
  parseJSON =
    Lude.withObject
      "ExponentialRolloutRate"
      ( \x ->
          ExponentialRolloutRate'
            Lude.<$> (x Lude..: "baseRatePerMinute")
            Lude.<*> (x Lude..: "incrementFactor")
            Lude.<*> (x Lude..: "rateIncreaseCriteria")
      )

instance Lude.ToJSON ExponentialRolloutRate where
  toJSON ExponentialRolloutRate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("baseRatePerMinute" Lude..= baseRatePerMinute),
            Lude.Just ("incrementFactor" Lude..= incrementFactor),
            Lude.Just ("rateIncreaseCriteria" Lude..= rateIncreaseCriteria)
          ]
      )
