{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
  ( AWSJobRateIncreaseCriteria (..),

    -- * Smart constructor
    mkAWSJobRateIncreaseCriteria,

    -- * Lenses
    ajricNumberOfNotifiedThings,
    ajricNumberOfSucceededThings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'mkAWSJobRateIncreaseCriteria' smart constructor.
data AWSJobRateIncreaseCriteria = AWSJobRateIncreaseCriteria'
  { -- | When this number of things have been notified, it will initiate an increase in the rollout rate.
    numberOfNotifiedThings :: Lude.Maybe Lude.Natural,
    -- | When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
    numberOfSucceededThings :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSJobRateIncreaseCriteria' with the minimum fields required to make a request.
--
-- * 'numberOfNotifiedThings' - When this number of things have been notified, it will initiate an increase in the rollout rate.
-- * 'numberOfSucceededThings' - When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
mkAWSJobRateIncreaseCriteria ::
  AWSJobRateIncreaseCriteria
mkAWSJobRateIncreaseCriteria =
  AWSJobRateIncreaseCriteria'
    { numberOfNotifiedThings =
        Lude.Nothing,
      numberOfSucceededThings = Lude.Nothing
    }

-- | When this number of things have been notified, it will initiate an increase in the rollout rate.
--
-- /Note:/ Consider using 'numberOfNotifiedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajricNumberOfNotifiedThings :: Lens.Lens' AWSJobRateIncreaseCriteria (Lude.Maybe Lude.Natural)
ajricNumberOfNotifiedThings = Lens.lens (numberOfNotifiedThings :: AWSJobRateIncreaseCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfNotifiedThings = a} :: AWSJobRateIncreaseCriteria)
{-# DEPRECATED ajricNumberOfNotifiedThings "Use generic-lens or generic-optics with 'numberOfNotifiedThings' instead." #-}

-- | When this number of things have succeeded in their job execution, it will initiate an increase in the rollout rate.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajricNumberOfSucceededThings :: Lens.Lens' AWSJobRateIncreaseCriteria (Lude.Maybe Lude.Natural)
ajricNumberOfSucceededThings = Lens.lens (numberOfSucceededThings :: AWSJobRateIncreaseCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfSucceededThings = a} :: AWSJobRateIncreaseCriteria)
{-# DEPRECATED ajricNumberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead." #-}

instance Lude.FromJSON AWSJobRateIncreaseCriteria where
  parseJSON =
    Lude.withObject
      "AWSJobRateIncreaseCriteria"
      ( \x ->
          AWSJobRateIncreaseCriteria'
            Lude.<$> (x Lude..:? "numberOfNotifiedThings")
            Lude.<*> (x Lude..:? "numberOfSucceededThings")
      )

instance Lude.ToJSON AWSJobRateIncreaseCriteria where
  toJSON AWSJobRateIncreaseCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("numberOfNotifiedThings" Lude..=)
              Lude.<$> numberOfNotifiedThings,
            ("numberOfSucceededThings" Lude..=)
              Lude.<$> numberOfSucceededThings
          ]
      )
