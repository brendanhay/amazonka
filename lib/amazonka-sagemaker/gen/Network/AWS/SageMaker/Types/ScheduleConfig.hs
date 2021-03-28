{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ScheduleConfig
  ( ScheduleConfig (..)
  -- * Smart constructor
  , mkScheduleConfig
  -- * Lenses
  , scScheduleExpression
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ScheduleExpression as Types

-- | Configuration details about the monitoring schedule.
--
-- /See:/ 'mkScheduleConfig' smart constructor.
newtype ScheduleConfig = ScheduleConfig'
  { scheduleExpression :: Types.ScheduleExpression
    -- ^ A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
--     * If you want to set the job to start every hour, please use the following:
-- @Hourly: cron(0 * ? * * *)@ 
--
--
--     * If you want to start the job daily:
-- @cron(0 [00-23] ? * * *)@ 
--
--
-- For example, the following are valid cron expressions:
--
--     * Daily at noon UTC: @cron(0 12 ? * * *)@ 
--
--
--     * Daily at midnight UTC: @cron(0 0 ? * * *)@ 
--
--
-- To support running every 6, 12 hours, the following are also supported:
-- @cron(0 [00-23]/[01-24] ? * * *)@ 
-- For example, the following are valid cron expressions:
--
--     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@ 
--
--
--     * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleConfig' value with any optional fields omitted.
mkScheduleConfig
    :: Types.ScheduleExpression -- ^ 'scheduleExpression'
    -> ScheduleConfig
mkScheduleConfig scheduleExpression
  = ScheduleConfig'{scheduleExpression}

-- | A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
--     * If you want to set the job to start every hour, please use the following:
-- @Hourly: cron(0 * ? * * *)@ 
--
--
--     * If you want to start the job daily:
-- @cron(0 [00-23] ? * * *)@ 
--
--
-- For example, the following are valid cron expressions:
--
--     * Daily at noon UTC: @cron(0 12 ? * * *)@ 
--
--
--     * Daily at midnight UTC: @cron(0 0 ? * * *)@ 
--
--
-- To support running every 6, 12 hours, the following are also supported:
-- @cron(0 [00-23]/[01-24] ? * * *)@ 
-- For example, the following are valid cron expressions:
--
--     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@ 
--
--
--     * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@ 
--
--
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scScheduleExpression :: Lens.Lens' ScheduleConfig Types.ScheduleExpression
scScheduleExpression = Lens.field @"scheduleExpression"
{-# INLINEABLE scScheduleExpression #-}
{-# DEPRECATED scheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead"  #-}

instance Core.FromJSON ScheduleConfig where
        toJSON ScheduleConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScheduleExpression" Core..= scheduleExpression)])

instance Core.FromJSON ScheduleConfig where
        parseJSON
          = Core.withObject "ScheduleConfig" Core.$
              \ x -> ScheduleConfig' Core.<$> (x Core..: "ScheduleExpression")
