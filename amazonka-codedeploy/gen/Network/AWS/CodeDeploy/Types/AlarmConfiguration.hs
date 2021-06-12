{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AlarmConfiguration where

import Network.AWS.CodeDeploy.Types.Alarm
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about alarms associated with the deployment group.
--
-- /See:/ 'newAlarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { -- | Indicates whether a deployment should continue if information about the
    -- current state of alarms cannot be retrieved from Amazon CloudWatch. The
    -- default value is false.
    --
    -- -   @true@: The deployment proceeds even if alarm status information
    --     can\'t be retrieved from Amazon CloudWatch.
    --
    -- -   @false@: The deployment stops if alarm status information can\'t be
    --     retrieved from Amazon CloudWatch.
    ignorePollAlarmFailure :: Core.Maybe Core.Bool,
    -- | Indicates whether the alarm configuration is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of alarms configured for the deployment group. A maximum of 10
    -- alarms can be added to a deployment group.
    alarms :: Core.Maybe [Alarm]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AlarmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignorePollAlarmFailure', 'alarmConfiguration_ignorePollAlarmFailure' - Indicates whether a deployment should continue if information about the
-- current state of alarms cannot be retrieved from Amazon CloudWatch. The
-- default value is false.
--
-- -   @true@: The deployment proceeds even if alarm status information
--     can\'t be retrieved from Amazon CloudWatch.
--
-- -   @false@: The deployment stops if alarm status information can\'t be
--     retrieved from Amazon CloudWatch.
--
-- 'enabled', 'alarmConfiguration_enabled' - Indicates whether the alarm configuration is enabled.
--
-- 'alarms', 'alarmConfiguration_alarms' - A list of alarms configured for the deployment group. A maximum of 10
-- alarms can be added to a deployment group.
newAlarmConfiguration ::
  AlarmConfiguration
newAlarmConfiguration =
  AlarmConfiguration'
    { ignorePollAlarmFailure =
        Core.Nothing,
      enabled = Core.Nothing,
      alarms = Core.Nothing
    }

-- | Indicates whether a deployment should continue if information about the
-- current state of alarms cannot be retrieved from Amazon CloudWatch. The
-- default value is false.
--
-- -   @true@: The deployment proceeds even if alarm status information
--     can\'t be retrieved from Amazon CloudWatch.
--
-- -   @false@: The deployment stops if alarm status information can\'t be
--     retrieved from Amazon CloudWatch.
alarmConfiguration_ignorePollAlarmFailure :: Lens.Lens' AlarmConfiguration (Core.Maybe Core.Bool)
alarmConfiguration_ignorePollAlarmFailure = Lens.lens (\AlarmConfiguration' {ignorePollAlarmFailure} -> ignorePollAlarmFailure) (\s@AlarmConfiguration' {} a -> s {ignorePollAlarmFailure = a} :: AlarmConfiguration)

-- | Indicates whether the alarm configuration is enabled.
alarmConfiguration_enabled :: Lens.Lens' AlarmConfiguration (Core.Maybe Core.Bool)
alarmConfiguration_enabled = Lens.lens (\AlarmConfiguration' {enabled} -> enabled) (\s@AlarmConfiguration' {} a -> s {enabled = a} :: AlarmConfiguration)

-- | A list of alarms configured for the deployment group. A maximum of 10
-- alarms can be added to a deployment group.
alarmConfiguration_alarms :: Lens.Lens' AlarmConfiguration (Core.Maybe [Alarm])
alarmConfiguration_alarms = Lens.lens (\AlarmConfiguration' {alarms} -> alarms) (\s@AlarmConfiguration' {} a -> s {alarms = a} :: AlarmConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AlarmConfiguration where
  parseJSON =
    Core.withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            Core.<$> (x Core..:? "ignorePollAlarmFailure")
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "alarms" Core..!= Core.mempty)
      )

instance Core.Hashable AlarmConfiguration

instance Core.NFData AlarmConfiguration

instance Core.ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ignorePollAlarmFailure" Core..=)
              Core.<$> ignorePollAlarmFailure,
            ("enabled" Core..=) Core.<$> enabled,
            ("alarms" Core..=) Core.<$> alarms
          ]
      )
