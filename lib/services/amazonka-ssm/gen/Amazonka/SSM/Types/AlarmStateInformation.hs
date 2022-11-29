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
-- Module      : Amazonka.SSM.Types.AlarmStateInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AlarmStateInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ExternalAlarmState

-- | The details about the state of your CloudWatch alarm.
--
-- /See:/ 'newAlarmStateInformation' smart constructor.
data AlarmStateInformation = AlarmStateInformation'
  { -- | The name of your CloudWatch alarm.
    name :: Prelude.Text,
    -- | The state of your CloudWatch alarm.
    state :: ExternalAlarmState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmStateInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'alarmStateInformation_name' - The name of your CloudWatch alarm.
--
-- 'state', 'alarmStateInformation_state' - The state of your CloudWatch alarm.
newAlarmStateInformation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'state'
  ExternalAlarmState ->
  AlarmStateInformation
newAlarmStateInformation pName_ pState_ =
  AlarmStateInformation'
    { name = pName_,
      state = pState_
    }

-- | The name of your CloudWatch alarm.
alarmStateInformation_name :: Lens.Lens' AlarmStateInformation Prelude.Text
alarmStateInformation_name = Lens.lens (\AlarmStateInformation' {name} -> name) (\s@AlarmStateInformation' {} a -> s {name = a} :: AlarmStateInformation)

-- | The state of your CloudWatch alarm.
alarmStateInformation_state :: Lens.Lens' AlarmStateInformation ExternalAlarmState
alarmStateInformation_state = Lens.lens (\AlarmStateInformation' {state} -> state) (\s@AlarmStateInformation' {} a -> s {state = a} :: AlarmStateInformation)

instance Core.FromJSON AlarmStateInformation where
  parseJSON =
    Core.withObject
      "AlarmStateInformation"
      ( \x ->
          AlarmStateInformation'
            Prelude.<$> (x Core..: "Name") Prelude.<*> (x Core..: "State")
      )

instance Prelude.Hashable AlarmStateInformation where
  hashWithSalt _salt AlarmStateInformation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData AlarmStateInformation where
  rnf AlarmStateInformation' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf state
