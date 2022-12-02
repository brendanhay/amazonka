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
-- Module      : Amazonka.IoTEvents.Types.AlarmEventActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmEventActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.AlarmAction
import qualified Amazonka.Prelude as Prelude

-- | Contains information about one or more alarm actions.
--
-- /See:/ 'newAlarmEventActions' smart constructor.
data AlarmEventActions = AlarmEventActions'
  { -- | Specifies one or more supported actions to receive notifications when
    -- the alarm state changes.
    alarmActions :: Prelude.Maybe [AlarmAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmEventActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmActions', 'alarmEventActions_alarmActions' - Specifies one or more supported actions to receive notifications when
-- the alarm state changes.
newAlarmEventActions ::
  AlarmEventActions
newAlarmEventActions =
  AlarmEventActions' {alarmActions = Prelude.Nothing}

-- | Specifies one or more supported actions to receive notifications when
-- the alarm state changes.
alarmEventActions_alarmActions :: Lens.Lens' AlarmEventActions (Prelude.Maybe [AlarmAction])
alarmEventActions_alarmActions = Lens.lens (\AlarmEventActions' {alarmActions} -> alarmActions) (\s@AlarmEventActions' {} a -> s {alarmActions = a} :: AlarmEventActions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AlarmEventActions where
  parseJSON =
    Data.withObject
      "AlarmEventActions"
      ( \x ->
          AlarmEventActions'
            Prelude.<$> (x Data..:? "alarmActions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AlarmEventActions where
  hashWithSalt _salt AlarmEventActions' {..} =
    _salt `Prelude.hashWithSalt` alarmActions

instance Prelude.NFData AlarmEventActions where
  rnf AlarmEventActions' {..} = Prelude.rnf alarmActions

instance Data.ToJSON AlarmEventActions where
  toJSON AlarmEventActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("alarmActions" Data..=) Prelude.<$> alarmActions]
      )
