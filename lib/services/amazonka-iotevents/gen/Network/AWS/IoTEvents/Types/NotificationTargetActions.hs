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
-- Module      : Network.AWS.IoTEvents.Types.NotificationTargetActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.NotificationTargetActions where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.LambdaAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an AWS Lambda function to manage alarm notifications. You can
-- create one or use the
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html AWS Lambda function provided by AWS IoT Events>.
--
-- /See:/ 'newNotificationTargetActions' smart constructor.
data NotificationTargetActions = NotificationTargetActions'
  { lambdaAction :: Prelude.Maybe LambdaAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationTargetActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaAction', 'notificationTargetActions_lambdaAction' - Undocumented member.
newNotificationTargetActions ::
  NotificationTargetActions
newNotificationTargetActions =
  NotificationTargetActions'
    { lambdaAction =
        Prelude.Nothing
    }

-- | Undocumented member.
notificationTargetActions_lambdaAction :: Lens.Lens' NotificationTargetActions (Prelude.Maybe LambdaAction)
notificationTargetActions_lambdaAction = Lens.lens (\NotificationTargetActions' {lambdaAction} -> lambdaAction) (\s@NotificationTargetActions' {} a -> s {lambdaAction = a} :: NotificationTargetActions)

instance Core.FromJSON NotificationTargetActions where
  parseJSON =
    Core.withObject
      "NotificationTargetActions"
      ( \x ->
          NotificationTargetActions'
            Prelude.<$> (x Core..:? "lambdaAction")
      )

instance Prelude.Hashable NotificationTargetActions

instance Prelude.NFData NotificationTargetActions

instance Core.ToJSON NotificationTargetActions where
  toJSON NotificationTargetActions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("lambdaAction" Core..=) Prelude.<$> lambdaAction]
      )
