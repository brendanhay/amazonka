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
-- Module      : Network.AWS.WorkDocs.Types.NotificationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.NotificationOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Set of options which defines notification preferences of given action.
--
-- /See:/ 'newNotificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { -- | Boolean value to indicate an email notification should be sent to the
    -- receipients.
    sendEmail :: Prelude.Maybe Prelude.Bool,
    -- | Text value to be included in the email body.
    emailMessage :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sendEmail = Prelude.Nothing,
      emailMessage = Prelude.Nothing
    }

-- | Boolean value to indicate an email notification should be sent to the
-- receipients.
notificationOptions_sendEmail :: Lens.Lens' NotificationOptions (Prelude.Maybe Prelude.Bool)
notificationOptions_sendEmail = Lens.lens (\NotificationOptions' {sendEmail} -> sendEmail) (\s@NotificationOptions' {} a -> s {sendEmail = a} :: NotificationOptions)

-- | Text value to be included in the email body.
notificationOptions_emailMessage :: Lens.Lens' NotificationOptions (Prelude.Maybe Prelude.Text)
notificationOptions_emailMessage = Lens.lens (\NotificationOptions' {emailMessage} -> emailMessage) (\s@NotificationOptions' {} a -> s {emailMessage = a} :: NotificationOptions) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.Hashable NotificationOptions

instance Prelude.NFData NotificationOptions

instance Prelude.ToJSON NotificationOptions where
  toJSON NotificationOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SendEmail" Prelude..=) Prelude.<$> sendEmail,
            ("EmailMessage" Prelude..=)
              Prelude.<$> emailMessage
          ]
      )
