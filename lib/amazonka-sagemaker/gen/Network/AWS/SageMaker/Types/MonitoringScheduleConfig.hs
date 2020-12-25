{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleConfig
  ( MonitoringScheduleConfig (..),

    -- * Smart constructor
    mkMonitoringScheduleConfig,

    -- * Lenses
    mscMonitoringJobDefinition,
    mscScheduleConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringJobDefinition as Types
import qualified Network.AWS.SageMaker.Types.ScheduleConfig as Types

-- | Configures the monitoring schedule and defines the monitoring job.
--
-- /See:/ 'mkMonitoringScheduleConfig' smart constructor.
data MonitoringScheduleConfig = MonitoringScheduleConfig'
  { -- | Defines the monitoring job.
    monitoringJobDefinition :: Types.MonitoringJobDefinition,
    -- | Configures the monitoring schedule.
    scheduleConfig :: Core.Maybe Types.ScheduleConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringScheduleConfig' value with any optional fields omitted.
mkMonitoringScheduleConfig ::
  -- | 'monitoringJobDefinition'
  Types.MonitoringJobDefinition ->
  MonitoringScheduleConfig
mkMonitoringScheduleConfig monitoringJobDefinition =
  MonitoringScheduleConfig'
    { monitoringJobDefinition,
      scheduleConfig = Core.Nothing
    }

-- | Defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscMonitoringJobDefinition :: Lens.Lens' MonitoringScheduleConfig Types.MonitoringJobDefinition
mscMonitoringJobDefinition = Lens.field @"monitoringJobDefinition"
{-# DEPRECATED mscMonitoringJobDefinition "Use generic-lens or generic-optics with 'monitoringJobDefinition' instead." #-}

-- | Configures the monitoring schedule.
--
-- /Note:/ Consider using 'scheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscScheduleConfig :: Lens.Lens' MonitoringScheduleConfig (Core.Maybe Types.ScheduleConfig)
mscScheduleConfig = Lens.field @"scheduleConfig"
{-# DEPRECATED mscScheduleConfig "Use generic-lens or generic-optics with 'scheduleConfig' instead." #-}

instance Core.FromJSON MonitoringScheduleConfig where
  toJSON MonitoringScheduleConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MonitoringJobDefinition" Core..= monitoringJobDefinition),
            ("ScheduleConfig" Core..=) Core.<$> scheduleConfig
          ]
      )

instance Core.FromJSON MonitoringScheduleConfig where
  parseJSON =
    Core.withObject "MonitoringScheduleConfig" Core.$
      \x ->
        MonitoringScheduleConfig'
          Core.<$> (x Core..: "MonitoringJobDefinition")
          Core.<*> (x Core..:? "ScheduleConfig")
