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
import qualified Network.AWS.Prelude as Lude

-- | Allows you to define a criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'mkRateIncreaseCriteria' smart constructor.
data RateIncreaseCriteria = RateIncreaseCriteria'
  { -- | The threshold for number of notified things that will initiate the increase in rate of rollout.
    numberOfNotifiedThings :: Lude.Maybe Lude.Natural,
    -- | The threshold for number of succeeded things that will initiate the increase in rate of rollout.
    numberOfSucceededThings :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RateIncreaseCriteria' with the minimum fields required to make a request.
--
-- * 'numberOfNotifiedThings' - The threshold for number of notified things that will initiate the increase in rate of rollout.
-- * 'numberOfSucceededThings' - The threshold for number of succeeded things that will initiate the increase in rate of rollout.
mkRateIncreaseCriteria ::
  RateIncreaseCriteria
mkRateIncreaseCriteria =
  RateIncreaseCriteria'
    { numberOfNotifiedThings = Lude.Nothing,
      numberOfSucceededThings = Lude.Nothing
    }

-- | The threshold for number of notified things that will initiate the increase in rate of rollout.
--
-- /Note:/ Consider using 'numberOfNotifiedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricNumberOfNotifiedThings :: Lens.Lens' RateIncreaseCriteria (Lude.Maybe Lude.Natural)
ricNumberOfNotifiedThings = Lens.lens (numberOfNotifiedThings :: RateIncreaseCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfNotifiedThings = a} :: RateIncreaseCriteria)
{-# DEPRECATED ricNumberOfNotifiedThings "Use generic-lens or generic-optics with 'numberOfNotifiedThings' instead." #-}

-- | The threshold for number of succeeded things that will initiate the increase in rate of rollout.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricNumberOfSucceededThings :: Lens.Lens' RateIncreaseCriteria (Lude.Maybe Lude.Natural)
ricNumberOfSucceededThings = Lens.lens (numberOfSucceededThings :: RateIncreaseCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfSucceededThings = a} :: RateIncreaseCriteria)
{-# DEPRECATED ricNumberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead." #-}

instance Lude.FromJSON RateIncreaseCriteria where
  parseJSON =
    Lude.withObject
      "RateIncreaseCriteria"
      ( \x ->
          RateIncreaseCriteria'
            Lude.<$> (x Lude..:? "numberOfNotifiedThings")
            Lude.<*> (x Lude..:? "numberOfSucceededThings")
      )

instance Lude.ToJSON RateIncreaseCriteria where
  toJSON RateIncreaseCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("numberOfNotifiedThings" Lude..=)
              Lude.<$> numberOfNotifiedThings,
            ("numberOfSucceededThings" Lude..=)
              Lude.<$> numberOfSucceededThings
          ]
      )
