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
-- Module      : Amazonka.CodeDeploy.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.AlarmConfiguration where

import Amazonka.CodeDeploy.Types.Alarm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about alarms associated with a deployment or deployment
-- group.
--
-- /See:/ 'newAlarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { -- | A list of alarms configured for the deployment or deployment group. A
    -- maximum of 10 alarms can be added.
    alarms :: Prelude.Maybe [Alarm],
    -- | Indicates whether the alarm configuration is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a deployment should continue if information about the
    -- current state of alarms cannot be retrieved from Amazon CloudWatch. The
    -- default value is false.
    --
    -- -   @true@: The deployment proceeds even if alarm status information
    --     can\'t be retrieved from Amazon CloudWatch.
    --
    -- -   @false@: The deployment stops if alarm status information can\'t be
    --     retrieved from Amazon CloudWatch.
    ignorePollAlarmFailure :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarms', 'alarmConfiguration_alarms' - A list of alarms configured for the deployment or deployment group. A
-- maximum of 10 alarms can be added.
--
-- 'enabled', 'alarmConfiguration_enabled' - Indicates whether the alarm configuration is enabled.
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
newAlarmConfiguration ::
  AlarmConfiguration
newAlarmConfiguration =
  AlarmConfiguration'
    { alarms = Prelude.Nothing,
      enabled = Prelude.Nothing,
      ignorePollAlarmFailure = Prelude.Nothing
    }

-- | A list of alarms configured for the deployment or deployment group. A
-- maximum of 10 alarms can be added.
alarmConfiguration_alarms :: Lens.Lens' AlarmConfiguration (Prelude.Maybe [Alarm])
alarmConfiguration_alarms = Lens.lens (\AlarmConfiguration' {alarms} -> alarms) (\s@AlarmConfiguration' {} a -> s {alarms = a} :: AlarmConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the alarm configuration is enabled.
alarmConfiguration_enabled :: Lens.Lens' AlarmConfiguration (Prelude.Maybe Prelude.Bool)
alarmConfiguration_enabled = Lens.lens (\AlarmConfiguration' {enabled} -> enabled) (\s@AlarmConfiguration' {} a -> s {enabled = a} :: AlarmConfiguration)

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

instance Data.FromJSON AlarmConfiguration where
  parseJSON =
    Data.withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            Prelude.<$> (x Data..:? "alarms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "ignorePollAlarmFailure")
      )

instance Prelude.Hashable AlarmConfiguration where
  hashWithSalt _salt AlarmConfiguration' {..} =
    _salt `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ignorePollAlarmFailure

instance Prelude.NFData AlarmConfiguration where
  rnf AlarmConfiguration' {..} =
    Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ignorePollAlarmFailure

instance Data.ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alarms" Data..=) Prelude.<$> alarms,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("ignorePollAlarmFailure" Data..=)
              Prelude.<$> ignorePollAlarmFailure
          ]
      )
