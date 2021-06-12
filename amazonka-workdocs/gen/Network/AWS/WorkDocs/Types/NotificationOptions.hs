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
-- Module      : Network.AWS.WorkDocs.Types.NotificationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.NotificationOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Set of options which defines notification preferences of given action.
--
-- /See:/ 'newNotificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { -- | Boolean value to indicate an email notification should be sent to the
    -- receipients.
    sendEmail :: Core.Maybe Core.Bool,
    -- | Text value to be included in the email body.
    emailMessage :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotificationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendEmail', 'notificationOptions_sendEmail' - Boolean value to indicate an email notification should be sent to the
-- receipients.
--
-- 'emailMessage', 'notificationOptions_emailMessage' - Text value to be included in the email body.
newNotificationOptions ::
  NotificationOptions
newNotificationOptions =
  NotificationOptions'
    { sendEmail = Core.Nothing,
      emailMessage = Core.Nothing
    }

-- | Boolean value to indicate an email notification should be sent to the
-- receipients.
notificationOptions_sendEmail :: Lens.Lens' NotificationOptions (Core.Maybe Core.Bool)
notificationOptions_sendEmail = Lens.lens (\NotificationOptions' {sendEmail} -> sendEmail) (\s@NotificationOptions' {} a -> s {sendEmail = a} :: NotificationOptions)

-- | Text value to be included in the email body.
notificationOptions_emailMessage :: Lens.Lens' NotificationOptions (Core.Maybe Core.Text)
notificationOptions_emailMessage = Lens.lens (\NotificationOptions' {emailMessage} -> emailMessage) (\s@NotificationOptions' {} a -> s {emailMessage = a} :: NotificationOptions) Core.. Lens.mapping Core._Sensitive

instance Core.Hashable NotificationOptions

instance Core.NFData NotificationOptions

instance Core.ToJSON NotificationOptions where
  toJSON NotificationOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SendEmail" Core..=) Core.<$> sendEmail,
            ("EmailMessage" Core..=) Core.<$> emailMessage
          ]
      )
