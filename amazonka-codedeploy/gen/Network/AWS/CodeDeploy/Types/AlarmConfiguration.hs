{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    ignorePollAlarmFailure :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the alarm configuration is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of alarms configured for the deployment group. A maximum of 10
    -- alarms can be added to a deployment group.
    alarms :: Prelude.Maybe [Alarm]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      alarms = Prelude.Nothing
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
alarmConfiguration_ignorePollAlarmFailure :: Lens.Lens' AlarmConfiguration (Prelude.Maybe Prelude.Bool)
alarmConfiguration_ignorePollAlarmFailure = Lens.lens (\AlarmConfiguration' {ignorePollAlarmFailure} -> ignorePollAlarmFailure) (\s@AlarmConfiguration' {} a -> s {ignorePollAlarmFailure = a} :: AlarmConfiguration)

-- | Indicates whether the alarm configuration is enabled.
alarmConfiguration_enabled :: Lens.Lens' AlarmConfiguration (Prelude.Maybe Prelude.Bool)
alarmConfiguration_enabled = Lens.lens (\AlarmConfiguration' {enabled} -> enabled) (\s@AlarmConfiguration' {} a -> s {enabled = a} :: AlarmConfiguration)

-- | A list of alarms configured for the deployment group. A maximum of 10
-- alarms can be added to a deployment group.
alarmConfiguration_alarms :: Lens.Lens' AlarmConfiguration (Prelude.Maybe [Alarm])
alarmConfiguration_alarms = Lens.lens (\AlarmConfiguration' {alarms} -> alarms) (\s@AlarmConfiguration' {} a -> s {alarms = a} :: AlarmConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AlarmConfiguration where
  parseJSON =
    Prelude.withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            Prelude.<$> (x Prelude..:? "ignorePollAlarmFailure")
            Prelude.<*> (x Prelude..:? "enabled")
            Prelude.<*> (x Prelude..:? "alarms" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable AlarmConfiguration

instance Prelude.NFData AlarmConfiguration

instance Prelude.ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ignorePollAlarmFailure" Prelude..=)
              Prelude.<$> ignorePollAlarmFailure,
            ("enabled" Prelude..=) Prelude.<$> enabled,
            ("alarms" Prelude..=) Prelude.<$> alarms
          ]
      )
