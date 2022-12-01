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
-- Module      : Amazonka.IoTEvents.Types.AlarmNotification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmNotification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.NotificationAction
import qualified Amazonka.Prelude as Prelude

-- | Contains information about one or more notification actions.
--
-- /See:/ 'newAlarmNotification' smart constructor.
data AlarmNotification = AlarmNotification'
  { -- | Contains the notification settings of an alarm model. The settings apply
    -- to all alarms that were created based on this alarm model.
    notificationActions :: Prelude.Maybe (Prelude.NonEmpty NotificationAction)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationActions', 'alarmNotification_notificationActions' - Contains the notification settings of an alarm model. The settings apply
-- to all alarms that were created based on this alarm model.
newAlarmNotification ::
  AlarmNotification
newAlarmNotification =
  AlarmNotification'
    { notificationActions =
        Prelude.Nothing
    }

-- | Contains the notification settings of an alarm model. The settings apply
-- to all alarms that were created based on this alarm model.
alarmNotification_notificationActions :: Lens.Lens' AlarmNotification (Prelude.Maybe (Prelude.NonEmpty NotificationAction))
alarmNotification_notificationActions = Lens.lens (\AlarmNotification' {notificationActions} -> notificationActions) (\s@AlarmNotification' {} a -> s {notificationActions = a} :: AlarmNotification) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AlarmNotification where
  parseJSON =
    Core.withObject
      "AlarmNotification"
      ( \x ->
          AlarmNotification'
            Prelude.<$> (x Core..:? "notificationActions")
      )

instance Prelude.Hashable AlarmNotification where
  hashWithSalt _salt AlarmNotification' {..} =
    _salt `Prelude.hashWithSalt` notificationActions

instance Prelude.NFData AlarmNotification where
  rnf AlarmNotification' {..} =
    Prelude.rnf notificationActions

instance Core.ToJSON AlarmNotification where
  toJSON AlarmNotification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("notificationActions" Core..=)
              Prelude.<$> notificationActions
          ]
      )
