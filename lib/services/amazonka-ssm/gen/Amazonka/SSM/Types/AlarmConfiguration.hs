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
-- Module      : Amazonka.SSM.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AlarmConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.Alarm

-- | The details for the CloudWatch alarm you want to apply to an automation
-- or command.
--
-- /See:/ 'newAlarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { -- | If you specify @true@ for this value, your automation or command
    -- continue to run even if we can\'t gather information about the state of
    -- your CloudWatch alarm. The default value is @false@.
    ignorePollAlarmFailure :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudWatch alarm specified in the configuration.
    alarms :: Prelude.NonEmpty Alarm
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
-- 'ignorePollAlarmFailure', 'alarmConfiguration_ignorePollAlarmFailure' - If you specify @true@ for this value, your automation or command
-- continue to run even if we can\'t gather information about the state of
-- your CloudWatch alarm. The default value is @false@.
--
-- 'alarms', 'alarmConfiguration_alarms' - The name of the CloudWatch alarm specified in the configuration.
newAlarmConfiguration ::
  -- | 'alarms'
  Prelude.NonEmpty Alarm ->
  AlarmConfiguration
newAlarmConfiguration pAlarms_ =
  AlarmConfiguration'
    { ignorePollAlarmFailure =
        Prelude.Nothing,
      alarms = Lens.coerced Lens.# pAlarms_
    }

-- | If you specify @true@ for this value, your automation or command
-- continue to run even if we can\'t gather information about the state of
-- your CloudWatch alarm. The default value is @false@.
alarmConfiguration_ignorePollAlarmFailure :: Lens.Lens' AlarmConfiguration (Prelude.Maybe Prelude.Bool)
alarmConfiguration_ignorePollAlarmFailure = Lens.lens (\AlarmConfiguration' {ignorePollAlarmFailure} -> ignorePollAlarmFailure) (\s@AlarmConfiguration' {} a -> s {ignorePollAlarmFailure = a} :: AlarmConfiguration)

-- | The name of the CloudWatch alarm specified in the configuration.
alarmConfiguration_alarms :: Lens.Lens' AlarmConfiguration (Prelude.NonEmpty Alarm)
alarmConfiguration_alarms = Lens.lens (\AlarmConfiguration' {alarms} -> alarms) (\s@AlarmConfiguration' {} a -> s {alarms = a} :: AlarmConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON AlarmConfiguration where
  parseJSON =
    Data.withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            Prelude.<$> (x Data..:? "IgnorePollAlarmFailure")
            Prelude.<*> (x Data..: "Alarms")
      )

instance Prelude.Hashable AlarmConfiguration where
  hashWithSalt _salt AlarmConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` ignorePollAlarmFailure
      `Prelude.hashWithSalt` alarms

instance Prelude.NFData AlarmConfiguration where
  rnf AlarmConfiguration' {..} =
    Prelude.rnf ignorePollAlarmFailure
      `Prelude.seq` Prelude.rnf alarms

instance Data.ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IgnorePollAlarmFailure" Data..=)
              Prelude.<$> ignorePollAlarmFailure,
            Prelude.Just ("Alarms" Data..= alarms)
          ]
      )
