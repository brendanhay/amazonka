{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimeoutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimeoutConfig
  ( TimeoutConfig (..),

    -- * Smart constructor
    mkTimeoutConfig,

    -- * Lenses
    tcInProgressTimeoutInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /See:/ 'mkTimeoutConfig' smart constructor.
newtype TimeoutConfig = TimeoutConfig'
  { inProgressTimeoutInMinutes ::
      Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeoutConfig' with the minimum fields required to make a request.
--
-- * 'inProgressTimeoutInMinutes' - Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
mkTimeoutConfig ::
  TimeoutConfig
mkTimeoutConfig =
  TimeoutConfig' {inProgressTimeoutInMinutes = Lude.Nothing}

-- | Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'inProgressTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInProgressTimeoutInMinutes :: Lens.Lens' TimeoutConfig (Lude.Maybe Lude.Integer)
tcInProgressTimeoutInMinutes = Lens.lens (inProgressTimeoutInMinutes :: TimeoutConfig -> Lude.Maybe Lude.Integer) (\s a -> s {inProgressTimeoutInMinutes = a} :: TimeoutConfig)
{-# DEPRECATED tcInProgressTimeoutInMinutes "Use generic-lens or generic-optics with 'inProgressTimeoutInMinutes' instead." #-}

instance Lude.FromJSON TimeoutConfig where
  parseJSON =
    Lude.withObject
      "TimeoutConfig"
      ( \x ->
          TimeoutConfig' Lude.<$> (x Lude..:? "inProgressTimeoutInMinutes")
      )

instance Lude.ToJSON TimeoutConfig where
  toJSON TimeoutConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inProgressTimeoutInMinutes" Lude..=)
              Lude.<$> inProgressTimeoutInMinutes
          ]
      )
