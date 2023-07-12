{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MechanicalTurk.UpdateNotificationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateNotificationSettings@ operation creates, updates, disables or
-- re-enables notifications for a HIT type. If you call the
-- UpdateNotificationSettings operation for a HIT type that already has a
-- notification specification, the operation replaces the old specification
-- with a new one. You can call the UpdateNotificationSettings operation to
-- enable or disable notifications for the HIT type, without having to
-- modify the notification specification itself by providing updates to the
-- Active status without specifying a new notification specification. To
-- change the Active status of a HIT type\'s notifications, the HIT type
-- must already have a notification specification, or one must be provided
-- in the same call to @UpdateNotificationSettings@.
module Amazonka.MechanicalTurk.UpdateNotificationSettings
  ( -- * Creating a Request
    UpdateNotificationSettings (..),
    newUpdateNotificationSettings,

    -- * Request Lenses
    updateNotificationSettings_active,
    updateNotificationSettings_notification,
    updateNotificationSettings_hITTypeId,

    -- * Destructuring the Response
    UpdateNotificationSettingsResponse (..),
    newUpdateNotificationSettingsResponse,

    -- * Response Lenses
    updateNotificationSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNotificationSettings' smart constructor.
data UpdateNotificationSettings = UpdateNotificationSettings'
  { -- | Specifies whether notifications are sent for HITs of this HIT type,
    -- according to the notification specification. You must specify either the
    -- Notification parameter or the Active parameter for the call to
    -- UpdateNotificationSettings to succeed.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The notification specification for the HIT type.
    notification :: Prelude.Maybe NotificationSpecification,
    -- | The ID of the HIT type whose notification specification is being
    -- updated.
    hITTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'updateNotificationSettings_active' - Specifies whether notifications are sent for HITs of this HIT type,
-- according to the notification specification. You must specify either the
-- Notification parameter or the Active parameter for the call to
-- UpdateNotificationSettings to succeed.
--
-- 'notification', 'updateNotificationSettings_notification' - The notification specification for the HIT type.
--
-- 'hITTypeId', 'updateNotificationSettings_hITTypeId' - The ID of the HIT type whose notification specification is being
-- updated.
newUpdateNotificationSettings ::
  -- | 'hITTypeId'
  Prelude.Text ->
  UpdateNotificationSettings
newUpdateNotificationSettings pHITTypeId_ =
  UpdateNotificationSettings'
    { active =
        Prelude.Nothing,
      notification = Prelude.Nothing,
      hITTypeId = pHITTypeId_
    }

-- | Specifies whether notifications are sent for HITs of this HIT type,
-- according to the notification specification. You must specify either the
-- Notification parameter or the Active parameter for the call to
-- UpdateNotificationSettings to succeed.
updateNotificationSettings_active :: Lens.Lens' UpdateNotificationSettings (Prelude.Maybe Prelude.Bool)
updateNotificationSettings_active = Lens.lens (\UpdateNotificationSettings' {active} -> active) (\s@UpdateNotificationSettings' {} a -> s {active = a} :: UpdateNotificationSettings)

-- | The notification specification for the HIT type.
updateNotificationSettings_notification :: Lens.Lens' UpdateNotificationSettings (Prelude.Maybe NotificationSpecification)
updateNotificationSettings_notification = Lens.lens (\UpdateNotificationSettings' {notification} -> notification) (\s@UpdateNotificationSettings' {} a -> s {notification = a} :: UpdateNotificationSettings)

-- | The ID of the HIT type whose notification specification is being
-- updated.
updateNotificationSettings_hITTypeId :: Lens.Lens' UpdateNotificationSettings Prelude.Text
updateNotificationSettings_hITTypeId = Lens.lens (\UpdateNotificationSettings' {hITTypeId} -> hITTypeId) (\s@UpdateNotificationSettings' {} a -> s {hITTypeId = a} :: UpdateNotificationSettings)

instance Core.AWSRequest UpdateNotificationSettings where
  type
    AWSResponse UpdateNotificationSettings =
      UpdateNotificationSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotificationSettings where
  hashWithSalt _salt UpdateNotificationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` hITTypeId

instance Prelude.NFData UpdateNotificationSettings where
  rnf UpdateNotificationSettings' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf hITTypeId

instance Data.ToHeaders UpdateNotificationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.UpdateNotificationSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotificationSettings where
  toJSON UpdateNotificationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Active" Data..=) Prelude.<$> active,
            ("Notification" Data..=) Prelude.<$> notification,
            Prelude.Just ("HITTypeId" Data..= hITTypeId)
          ]
      )

instance Data.ToPath UpdateNotificationSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNotificationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotificationSettingsResponse' smart constructor.
data UpdateNotificationSettingsResponse = UpdateNotificationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotificationSettingsResponse_httpStatus' - The response's http status code.
newUpdateNotificationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotificationSettingsResponse
newUpdateNotificationSettingsResponse pHttpStatus_ =
  UpdateNotificationSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotificationSettingsResponse_httpStatus :: Lens.Lens' UpdateNotificationSettingsResponse Prelude.Int
updateNotificationSettingsResponse_httpStatus = Lens.lens (\UpdateNotificationSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateNotificationSettingsResponse' {} a -> s {httpStatus = a} :: UpdateNotificationSettingsResponse)

instance
  Prelude.NFData
    UpdateNotificationSettingsResponse
  where
  rnf UpdateNotificationSettingsResponse' {..} =
    Prelude.rnf httpStatus
