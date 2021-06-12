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
-- Module      : Network.AWS.MechanicalTurk.UpdateNotificationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.MechanicalTurk.UpdateNotificationSettings
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNotificationSettings' smart constructor.
data UpdateNotificationSettings = UpdateNotificationSettings'
  { -- | Specifies whether notifications are sent for HITs of this HIT type,
    -- according to the notification specification. You must specify either the
    -- Notification parameter or the Active parameter for the call to
    -- UpdateNotificationSettings to succeed.
    active :: Core.Maybe Core.Bool,
    -- | The notification specification for the HIT type.
    notification :: Core.Maybe NotificationSpecification,
    -- | The ID of the HIT type whose notification specification is being
    -- updated.
    hITTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateNotificationSettings
newUpdateNotificationSettings pHITTypeId_ =
  UpdateNotificationSettings'
    { active = Core.Nothing,
      notification = Core.Nothing,
      hITTypeId = pHITTypeId_
    }

-- | Specifies whether notifications are sent for HITs of this HIT type,
-- according to the notification specification. You must specify either the
-- Notification parameter or the Active parameter for the call to
-- UpdateNotificationSettings to succeed.
updateNotificationSettings_active :: Lens.Lens' UpdateNotificationSettings (Core.Maybe Core.Bool)
updateNotificationSettings_active = Lens.lens (\UpdateNotificationSettings' {active} -> active) (\s@UpdateNotificationSettings' {} a -> s {active = a} :: UpdateNotificationSettings)

-- | The notification specification for the HIT type.
updateNotificationSettings_notification :: Lens.Lens' UpdateNotificationSettings (Core.Maybe NotificationSpecification)
updateNotificationSettings_notification = Lens.lens (\UpdateNotificationSettings' {notification} -> notification) (\s@UpdateNotificationSettings' {} a -> s {notification = a} :: UpdateNotificationSettings)

-- | The ID of the HIT type whose notification specification is being
-- updated.
updateNotificationSettings_hITTypeId :: Lens.Lens' UpdateNotificationSettings Core.Text
updateNotificationSettings_hITTypeId = Lens.lens (\UpdateNotificationSettings' {hITTypeId} -> hITTypeId) (\s@UpdateNotificationSettings' {} a -> s {hITTypeId = a} :: UpdateNotificationSettings)

instance Core.AWSRequest UpdateNotificationSettings where
  type
    AWSResponse UpdateNotificationSettings =
      UpdateNotificationSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationSettingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateNotificationSettings

instance Core.NFData UpdateNotificationSettings

instance Core.ToHeaders UpdateNotificationSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.UpdateNotificationSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateNotificationSettings where
  toJSON UpdateNotificationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Active" Core..=) Core.<$> active,
            ("Notification" Core..=) Core.<$> notification,
            Core.Just ("HITTypeId" Core..= hITTypeId)
          ]
      )

instance Core.ToPath UpdateNotificationSettings where
  toPath = Core.const "/"

instance Core.ToQuery UpdateNotificationSettings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateNotificationSettingsResponse' smart constructor.
data UpdateNotificationSettingsResponse = UpdateNotificationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateNotificationSettingsResponse
newUpdateNotificationSettingsResponse pHttpStatus_ =
  UpdateNotificationSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotificationSettingsResponse_httpStatus :: Lens.Lens' UpdateNotificationSettingsResponse Core.Int
updateNotificationSettingsResponse_httpStatus = Lens.lens (\UpdateNotificationSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateNotificationSettingsResponse' {} a -> s {httpStatus = a} :: UpdateNotificationSettingsResponse)

instance
  Core.NFData
    UpdateNotificationSettingsResponse
