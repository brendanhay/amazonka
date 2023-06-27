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
-- Module      : Amazonka.RolesAnywhere.ResetNotificationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the /custom notification setting/ to IAM Roles Anywhere default
-- setting.
--
-- __Required permissions:__ @rolesanywhere:ResetNotificationSettings@.
module Amazonka.RolesAnywhere.ResetNotificationSettings
  ( -- * Creating a Request
    ResetNotificationSettings (..),
    newResetNotificationSettings,

    -- * Request Lenses
    resetNotificationSettings_notificationSettingKeys,
    resetNotificationSettings_trustAnchorId,

    -- * Destructuring the Response
    ResetNotificationSettingsResponse (..),
    newResetNotificationSettingsResponse,

    -- * Response Lenses
    resetNotificationSettingsResponse_httpStatus,
    resetNotificationSettingsResponse_trustAnchor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newResetNotificationSettings' smart constructor.
data ResetNotificationSettings = ResetNotificationSettings'
  { -- | A list of notification setting keys to reset. A notification setting key
    -- includes the event and the channel.
    notificationSettingKeys :: [NotificationSettingKey],
    -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetNotificationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationSettingKeys', 'resetNotificationSettings_notificationSettingKeys' - A list of notification setting keys to reset. A notification setting key
-- includes the event and the channel.
--
-- 'trustAnchorId', 'resetNotificationSettings_trustAnchorId' - The unique identifier of the trust anchor.
newResetNotificationSettings ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  ResetNotificationSettings
newResetNotificationSettings pTrustAnchorId_ =
  ResetNotificationSettings'
    { notificationSettingKeys =
        Prelude.mempty,
      trustAnchorId = pTrustAnchorId_
    }

-- | A list of notification setting keys to reset. A notification setting key
-- includes the event and the channel.
resetNotificationSettings_notificationSettingKeys :: Lens.Lens' ResetNotificationSettings [NotificationSettingKey]
resetNotificationSettings_notificationSettingKeys = Lens.lens (\ResetNotificationSettings' {notificationSettingKeys} -> notificationSettingKeys) (\s@ResetNotificationSettings' {} a -> s {notificationSettingKeys = a} :: ResetNotificationSettings) Prelude.. Lens.coerced

-- | The unique identifier of the trust anchor.
resetNotificationSettings_trustAnchorId :: Lens.Lens' ResetNotificationSettings Prelude.Text
resetNotificationSettings_trustAnchorId = Lens.lens (\ResetNotificationSettings' {trustAnchorId} -> trustAnchorId) (\s@ResetNotificationSettings' {} a -> s {trustAnchorId = a} :: ResetNotificationSettings)

instance Core.AWSRequest ResetNotificationSettings where
  type
    AWSResponse ResetNotificationSettings =
      ResetNotificationSettingsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetNotificationSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "trustAnchor")
      )

instance Prelude.Hashable ResetNotificationSettings where
  hashWithSalt _salt ResetNotificationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` notificationSettingKeys
      `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData ResetNotificationSettings where
  rnf ResetNotificationSettings' {..} =
    Prelude.rnf notificationSettingKeys
      `Prelude.seq` Prelude.rnf trustAnchorId

instance Data.ToHeaders ResetNotificationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetNotificationSettings where
  toJSON ResetNotificationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "notificationSettingKeys"
                  Data..= notificationSettingKeys
              ),
            Prelude.Just
              ("trustAnchorId" Data..= trustAnchorId)
          ]
      )

instance Data.ToPath ResetNotificationSettings where
  toPath =
    Prelude.const "/reset-notifications-settings"

instance Data.ToQuery ResetNotificationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetNotificationSettingsResponse' smart constructor.
data ResetNotificationSettingsResponse = ResetNotificationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    trustAnchor :: TrustAnchorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetNotificationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetNotificationSettingsResponse_httpStatus' - The response's http status code.
--
-- 'trustAnchor', 'resetNotificationSettingsResponse_trustAnchor' - Undocumented member.
newResetNotificationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trustAnchor'
  TrustAnchorDetail ->
  ResetNotificationSettingsResponse
newResetNotificationSettingsResponse
  pHttpStatus_
  pTrustAnchor_ =
    ResetNotificationSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        trustAnchor = pTrustAnchor_
      }

-- | The response's http status code.
resetNotificationSettingsResponse_httpStatus :: Lens.Lens' ResetNotificationSettingsResponse Prelude.Int
resetNotificationSettingsResponse_httpStatus = Lens.lens (\ResetNotificationSettingsResponse' {httpStatus} -> httpStatus) (\s@ResetNotificationSettingsResponse' {} a -> s {httpStatus = a} :: ResetNotificationSettingsResponse)

-- | Undocumented member.
resetNotificationSettingsResponse_trustAnchor :: Lens.Lens' ResetNotificationSettingsResponse TrustAnchorDetail
resetNotificationSettingsResponse_trustAnchor = Lens.lens (\ResetNotificationSettingsResponse' {trustAnchor} -> trustAnchor) (\s@ResetNotificationSettingsResponse' {} a -> s {trustAnchor = a} :: ResetNotificationSettingsResponse)

instance
  Prelude.NFData
    ResetNotificationSettingsResponse
  where
  rnf ResetNotificationSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trustAnchor
