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
-- Module      : Amazonka.IoTEvents.Types.NotificationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.NotificationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.EmailConfiguration
import Amazonka.IoTEvents.Types.NotificationTargetActions
import Amazonka.IoTEvents.Types.SMSConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains the notification settings of an alarm model. The settings apply
-- to all alarms that were created based on this alarm model.
--
-- /See:/ 'newNotificationAction' smart constructor.
data NotificationAction = NotificationAction'
  { -- | Contains the configuration information of email notifications.
    emailConfigurations :: Prelude.Maybe (Prelude.NonEmpty EmailConfiguration),
    -- | Contains the configuration information of SMS notifications.
    smsConfigurations :: Prelude.Maybe (Prelude.NonEmpty SMSConfiguration),
    -- | Specifies an AWS Lambda function to manage alarm notifications. You can
    -- create one or use the
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html AWS Lambda function provided by AWS IoT Events>.
    action :: NotificationTargetActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailConfigurations', 'notificationAction_emailConfigurations' - Contains the configuration information of email notifications.
--
-- 'smsConfigurations', 'notificationAction_smsConfigurations' - Contains the configuration information of SMS notifications.
--
-- 'action', 'notificationAction_action' - Specifies an AWS Lambda function to manage alarm notifications. You can
-- create one or use the
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html AWS Lambda function provided by AWS IoT Events>.
newNotificationAction ::
  -- | 'action'
  NotificationTargetActions ->
  NotificationAction
newNotificationAction pAction_ =
  NotificationAction'
    { emailConfigurations =
        Prelude.Nothing,
      smsConfigurations = Prelude.Nothing,
      action = pAction_
    }

-- | Contains the configuration information of email notifications.
notificationAction_emailConfigurations :: Lens.Lens' NotificationAction (Prelude.Maybe (Prelude.NonEmpty EmailConfiguration))
notificationAction_emailConfigurations = Lens.lens (\NotificationAction' {emailConfigurations} -> emailConfigurations) (\s@NotificationAction' {} a -> s {emailConfigurations = a} :: NotificationAction) Prelude.. Lens.mapping Lens.coerced

-- | Contains the configuration information of SMS notifications.
notificationAction_smsConfigurations :: Lens.Lens' NotificationAction (Prelude.Maybe (Prelude.NonEmpty SMSConfiguration))
notificationAction_smsConfigurations = Lens.lens (\NotificationAction' {smsConfigurations} -> smsConfigurations) (\s@NotificationAction' {} a -> s {smsConfigurations = a} :: NotificationAction) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an AWS Lambda function to manage alarm notifications. You can
-- create one or use the
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html AWS Lambda function provided by AWS IoT Events>.
notificationAction_action :: Lens.Lens' NotificationAction NotificationTargetActions
notificationAction_action = Lens.lens (\NotificationAction' {action} -> action) (\s@NotificationAction' {} a -> s {action = a} :: NotificationAction)

instance Data.FromJSON NotificationAction where
  parseJSON =
    Data.withObject
      "NotificationAction"
      ( \x ->
          NotificationAction'
            Prelude.<$> (x Data..:? "emailConfigurations")
            Prelude.<*> (x Data..:? "smsConfigurations")
            Prelude.<*> (x Data..: "action")
      )

instance Prelude.Hashable NotificationAction where
  hashWithSalt _salt NotificationAction' {..} =
    _salt
      `Prelude.hashWithSalt` emailConfigurations
      `Prelude.hashWithSalt` smsConfigurations
      `Prelude.hashWithSalt` action

instance Prelude.NFData NotificationAction where
  rnf NotificationAction' {..} =
    Prelude.rnf emailConfigurations
      `Prelude.seq` Prelude.rnf smsConfigurations
      `Prelude.seq` Prelude.rnf action

instance Data.ToJSON NotificationAction where
  toJSON NotificationAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("emailConfigurations" Data..=)
              Prelude.<$> emailConfigurations,
            ("smsConfigurations" Data..=)
              Prelude.<$> smsConfigurations,
            Prelude.Just ("action" Data..= action)
          ]
      )
