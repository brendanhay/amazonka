{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimeoutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TimeoutConfig
  ( TimeoutConfig (..)
  -- * Smart constructor
  , mkTimeoutConfig
  -- * Lenses
  , tcInProgressTimeoutInMinutes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /See:/ 'mkTimeoutConfig' smart constructor.
newtype TimeoutConfig = TimeoutConfig'
  { inProgressTimeoutInMinutes :: Core.Maybe Core.Integer
    -- ^ Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TimeoutConfig' value with any optional fields omitted.
mkTimeoutConfig
    :: TimeoutConfig
mkTimeoutConfig
  = TimeoutConfig'{inProgressTimeoutInMinutes = Core.Nothing}

-- | Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'inProgressTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInProgressTimeoutInMinutes :: Lens.Lens' TimeoutConfig (Core.Maybe Core.Integer)
tcInProgressTimeoutInMinutes = Lens.field @"inProgressTimeoutInMinutes"
{-# INLINEABLE tcInProgressTimeoutInMinutes #-}
{-# DEPRECATED inProgressTimeoutInMinutes "Use generic-lens or generic-optics with 'inProgressTimeoutInMinutes' instead"  #-}

instance Core.FromJSON TimeoutConfig where
        toJSON TimeoutConfig{..}
          = Core.object
              (Core.catMaybes
                 [("inProgressTimeoutInMinutes" Core..=) Core.<$>
                    inProgressTimeoutInMinutes])

instance Core.FromJSON TimeoutConfig where
        parseJSON
          = Core.withObject "TimeoutConfig" Core.$
              \ x ->
                TimeoutConfig' Core.<$> (x Core..:? "inProgressTimeoutInMinutes")
