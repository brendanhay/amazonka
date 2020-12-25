{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AlarmConfiguration
  ( AlarmConfiguration (..),

    -- * Smart constructor
    mkAlarmConfiguration,

    -- * Lenses
    acAlarms,
    acEnabled,
    acIgnorePollAlarmFailure,
  )
where

import qualified Network.AWS.CodeDeploy.Types.Alarm as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about alarms associated with the deployment group.
--
-- /See:/ 'mkAlarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { -- | A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
    alarms :: Core.Maybe [Types.Alarm],
    -- | Indicates whether the alarm configuration is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.
    --
    --
    --     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.
    --
    --
    --     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
    ignorePollAlarmFailure :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlarmConfiguration' value with any optional fields omitted.
mkAlarmConfiguration ::
  AlarmConfiguration
mkAlarmConfiguration =
  AlarmConfiguration'
    { alarms = Core.Nothing,
      enabled = Core.Nothing,
      ignorePollAlarmFailure = Core.Nothing
    }

-- | A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAlarms :: Lens.Lens' AlarmConfiguration (Core.Maybe [Types.Alarm])
acAlarms = Lens.field @"alarms"
{-# DEPRECATED acAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | Indicates whether the alarm configuration is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acEnabled :: Lens.Lens' AlarmConfiguration (Core.Maybe Core.Bool)
acEnabled = Lens.field @"enabled"
{-# DEPRECATED acEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.
--
--
--     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.
--
--
--     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
--
--
--
-- /Note:/ Consider using 'ignorePollAlarmFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acIgnorePollAlarmFailure :: Lens.Lens' AlarmConfiguration (Core.Maybe Core.Bool)
acIgnorePollAlarmFailure = Lens.field @"ignorePollAlarmFailure"
{-# DEPRECATED acIgnorePollAlarmFailure "Use generic-lens or generic-optics with 'ignorePollAlarmFailure' instead." #-}

instance Core.FromJSON AlarmConfiguration where
  toJSON AlarmConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("alarms" Core..=) Core.<$> alarms,
            ("enabled" Core..=) Core.<$> enabled,
            ("ignorePollAlarmFailure" Core..=)
              Core.<$> ignorePollAlarmFailure
          ]
      )

instance Core.FromJSON AlarmConfiguration where
  parseJSON =
    Core.withObject "AlarmConfiguration" Core.$
      \x ->
        AlarmConfiguration'
          Core.<$> (x Core..:? "alarms")
          Core.<*> (x Core..:? "enabled")
          Core.<*> (x Core..:? "ignorePollAlarmFailure")
