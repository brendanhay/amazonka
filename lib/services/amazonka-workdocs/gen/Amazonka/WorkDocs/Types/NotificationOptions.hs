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
-- Module      : Amazonka.WorkDocs.Types.NotificationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.NotificationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Set of options which defines notification preferences of given action.
--
-- /See:/ 'newNotificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { -- | Text value to be included in the email body.
    emailMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Boolean value to indicate an email notification should be sent to the
    -- receipients.
    sendEmail :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailMessage', 'notificationOptions_emailMessage' - Text value to be included in the email body.
--
-- 'sendEmail', 'notificationOptions_sendEmail' - Boolean value to indicate an email notification should be sent to the
-- receipients.
newNotificationOptions ::
  NotificationOptions
newNotificationOptions =
  NotificationOptions'
    { emailMessage =
        Prelude.Nothing,
      sendEmail = Prelude.Nothing
    }

-- | Text value to be included in the email body.
notificationOptions_emailMessage :: Lens.Lens' NotificationOptions (Prelude.Maybe Prelude.Text)
notificationOptions_emailMessage = Lens.lens (\NotificationOptions' {emailMessage} -> emailMessage) (\s@NotificationOptions' {} a -> s {emailMessage = a} :: NotificationOptions) Prelude.. Lens.mapping Core._Sensitive

-- | Boolean value to indicate an email notification should be sent to the
-- receipients.
notificationOptions_sendEmail :: Lens.Lens' NotificationOptions (Prelude.Maybe Prelude.Bool)
notificationOptions_sendEmail = Lens.lens (\NotificationOptions' {sendEmail} -> sendEmail) (\s@NotificationOptions' {} a -> s {sendEmail = a} :: NotificationOptions)

instance Prelude.Hashable NotificationOptions where
  hashWithSalt salt' NotificationOptions' {..} =
    salt' `Prelude.hashWithSalt` sendEmail
      `Prelude.hashWithSalt` emailMessage

instance Prelude.NFData NotificationOptions where
  rnf NotificationOptions' {..} =
    Prelude.rnf emailMessage
      `Prelude.seq` Prelude.rnf sendEmail

instance Core.ToJSON NotificationOptions where
  toJSON NotificationOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EmailMessage" Core..=) Prelude.<$> emailMessage,
            ("SendEmail" Core..=) Prelude.<$> sendEmail
          ]
      )
