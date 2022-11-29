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
-- Module      : Amazonka.SMS.Types.NotificationContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.NotificationContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ValidationStatus

-- | Contains the status of validating an application.
--
-- /See:/ 'newNotificationContext' smart constructor.
data NotificationContext = NotificationContext'
  { -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the validation.
    status :: Prelude.Maybe ValidationStatus,
    -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationId', 'notificationContext_validationId' - The ID of the validation.
--
-- 'status', 'notificationContext_status' - The status of the validation.
--
-- 'statusMessage', 'notificationContext_statusMessage' - The status message.
newNotificationContext ::
  NotificationContext
newNotificationContext =
  NotificationContext'
    { validationId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The ID of the validation.
notificationContext_validationId :: Lens.Lens' NotificationContext (Prelude.Maybe Prelude.Text)
notificationContext_validationId = Lens.lens (\NotificationContext' {validationId} -> validationId) (\s@NotificationContext' {} a -> s {validationId = a} :: NotificationContext)

-- | The status of the validation.
notificationContext_status :: Lens.Lens' NotificationContext (Prelude.Maybe ValidationStatus)
notificationContext_status = Lens.lens (\NotificationContext' {status} -> status) (\s@NotificationContext' {} a -> s {status = a} :: NotificationContext)

-- | The status message.
notificationContext_statusMessage :: Lens.Lens' NotificationContext (Prelude.Maybe Prelude.Text)
notificationContext_statusMessage = Lens.lens (\NotificationContext' {statusMessage} -> statusMessage) (\s@NotificationContext' {} a -> s {statusMessage = a} :: NotificationContext)

instance Prelude.Hashable NotificationContext where
  hashWithSalt _salt NotificationContext' {..} =
    _salt `Prelude.hashWithSalt` validationId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData NotificationContext where
  rnf NotificationContext' {..} =
    Prelude.rnf validationId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage

instance Core.ToJSON NotificationContext where
  toJSON NotificationContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("validationId" Core..=) Prelude.<$> validationId,
            ("status" Core..=) Prelude.<$> status,
            ("statusMessage" Core..=) Prelude.<$> statusMessage
          ]
      )
