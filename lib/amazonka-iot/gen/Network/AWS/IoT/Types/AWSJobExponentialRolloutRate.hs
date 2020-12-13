{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
  ( AWSJobExponentialRolloutRate (..),

    -- * Smart constructor
    mkAWSJobExponentialRolloutRate,

    -- * Lenses
    ajerrBaseRatePerMinute,
    ajerrIncrementFactor,
    ajerrRateIncreaseCriteria,
  )
where

import Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
-- /See:/ 'mkAWSJobExponentialRolloutRate' smart constructor.
data AWSJobExponentialRolloutRate = AWSJobExponentialRolloutRate'
  { -- | The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
    baseRatePerMinute :: Lude.Natural,
    -- | The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
    incrementFactor :: Lude.Double,
    -- | The criteria to initiate the increase in rate of rollout for a job.
    --
    -- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
    rateIncreaseCriteria :: AWSJobRateIncreaseCriteria
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSJobExponentialRolloutRate' with the minimum fields required to make a request.
--
-- * 'baseRatePerMinute' - The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
-- * 'incrementFactor' - The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
-- * 'rateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
mkAWSJobExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Lude.Natural ->
  -- | 'incrementFactor'
  Lude.Double ->
  -- | 'rateIncreaseCriteria'
  AWSJobRateIncreaseCriteria ->
  AWSJobExponentialRolloutRate
mkAWSJobExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    AWSJobExponentialRolloutRate'
      { baseRatePerMinute =
          pBaseRatePerMinute_,
        incrementFactor = pIncrementFactor_,
        rateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per minute, at the start of the job rollout. This is the initial rate of the rollout.
--
-- /Note:/ Consider using 'baseRatePerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrBaseRatePerMinute :: Lens.Lens' AWSJobExponentialRolloutRate Lude.Natural
ajerrBaseRatePerMinute = Lens.lens (baseRatePerMinute :: AWSJobExponentialRolloutRate -> Lude.Natural) (\s a -> s {baseRatePerMinute = a} :: AWSJobExponentialRolloutRate)
{-# DEPRECATED ajerrBaseRatePerMinute "Use generic-lens or generic-optics with 'baseRatePerMinute' instead." #-}

-- | The rate of increase for a job rollout. The number of things notified is multiplied by this factor.
--
-- /Note:/ Consider using 'incrementFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrIncrementFactor :: Lens.Lens' AWSJobExponentialRolloutRate Lude.Double
ajerrIncrementFactor = Lens.lens (incrementFactor :: AWSJobExponentialRolloutRate -> Lude.Double) (\s a -> s {incrementFactor = a} :: AWSJobExponentialRolloutRate)
{-# DEPRECATED ajerrIncrementFactor "Use generic-lens or generic-optics with 'incrementFactor' instead." #-}

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5, but not 1.55).
--
-- /Note:/ Consider using 'rateIncreaseCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajerrRateIncreaseCriteria :: Lens.Lens' AWSJobExponentialRolloutRate AWSJobRateIncreaseCriteria
ajerrRateIncreaseCriteria = Lens.lens (rateIncreaseCriteria :: AWSJobExponentialRolloutRate -> AWSJobRateIncreaseCriteria) (\s a -> s {rateIncreaseCriteria = a} :: AWSJobExponentialRolloutRate)
{-# DEPRECATED ajerrRateIncreaseCriteria "Use generic-lens or generic-optics with 'rateIncreaseCriteria' instead." #-}

instance Lude.FromJSON AWSJobExponentialRolloutRate where
  parseJSON =
    Lude.withObject
      "AWSJobExponentialRolloutRate"
      ( \x ->
          AWSJobExponentialRolloutRate'
            Lude.<$> (x Lude..: "baseRatePerMinute")
            Lude.<*> (x Lude..: "incrementFactor")
            Lude.<*> (x Lude..: "rateIncreaseCriteria")
      )

instance Lude.ToJSON AWSJobExponentialRolloutRate where
  toJSON AWSJobExponentialRolloutRate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("baseRatePerMinute" Lude..= baseRatePerMinute),
            Lude.Just ("incrementFactor" Lude..= incrementFactor),
            Lude.Just ("rateIncreaseCriteria" Lude..= rateIncreaseCriteria)
          ]
      )
