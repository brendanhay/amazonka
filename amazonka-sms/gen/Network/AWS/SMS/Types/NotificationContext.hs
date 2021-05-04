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
-- Module      : Network.AWS.SMS.Types.NotificationContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.NotificationContext where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains the status of validating an application.
--
-- /See:/ 'newNotificationContext' smart constructor.
data NotificationContext = NotificationContext'
  { -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the validation.
    status :: Prelude.Maybe ValidationStatus,
    -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotificationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'notificationContext_statusMessage' - The status message.
--
-- 'status', 'notificationContext_status' - The status of the validation.
--
-- 'validationId', 'notificationContext_validationId' - The ID of the validation.
newNotificationContext ::
  NotificationContext
newNotificationContext =
  NotificationContext'
    { statusMessage =
        Prelude.Nothing,
      status = Prelude.Nothing,
      validationId = Prelude.Nothing
    }

-- | The status message.
notificationContext_statusMessage :: Lens.Lens' NotificationContext (Prelude.Maybe Prelude.Text)
notificationContext_statusMessage = Lens.lens (\NotificationContext' {statusMessage} -> statusMessage) (\s@NotificationContext' {} a -> s {statusMessage = a} :: NotificationContext)

-- | The status of the validation.
notificationContext_status :: Lens.Lens' NotificationContext (Prelude.Maybe ValidationStatus)
notificationContext_status = Lens.lens (\NotificationContext' {status} -> status) (\s@NotificationContext' {} a -> s {status = a} :: NotificationContext)

-- | The ID of the validation.
notificationContext_validationId :: Lens.Lens' NotificationContext (Prelude.Maybe Prelude.Text)
notificationContext_validationId = Lens.lens (\NotificationContext' {validationId} -> validationId) (\s@NotificationContext' {} a -> s {validationId = a} :: NotificationContext)

instance Prelude.Hashable NotificationContext

instance Prelude.NFData NotificationContext

instance Prelude.ToJSON NotificationContext where
  toJSON NotificationContext' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("statusMessage" Prelude..=)
              Prelude.<$> statusMessage,
            ("status" Prelude..=) Prelude.<$> status,
            ("validationId" Prelude..=)
              Prelude.<$> validationId
          ]
      )
