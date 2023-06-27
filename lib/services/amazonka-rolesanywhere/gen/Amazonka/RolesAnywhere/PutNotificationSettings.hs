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
-- Module      : Amazonka.RolesAnywhere.PutNotificationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a list of /notification settings/ to a trust anchor.
--
-- A notification setting includes information such as event name,
-- threshold, status of the notification setting, and the channel to
-- notify.
--
-- __Required permissions:__ @rolesanywhere:PutNotificationSettings@.
module Amazonka.RolesAnywhere.PutNotificationSettings
  ( -- * Creating a Request
    PutNotificationSettings (..),
    newPutNotificationSettings,

    -- * Request Lenses
    putNotificationSettings_notificationSettings,
    putNotificationSettings_trustAnchorId,

    -- * Destructuring the Response
    PutNotificationSettingsResponse (..),
    newPutNotificationSettingsResponse,

    -- * Response Lenses
    putNotificationSettingsResponse_httpStatus,
    putNotificationSettingsResponse_trustAnchor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newPutNotificationSettings' smart constructor.
data PutNotificationSettings = PutNotificationSettings'
  { -- | A list of notification settings to be associated to the trust anchor.
    notificationSettings :: [NotificationSetting],
    -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationSettings', 'putNotificationSettings_notificationSettings' - A list of notification settings to be associated to the trust anchor.
--
-- 'trustAnchorId', 'putNotificationSettings_trustAnchorId' - The unique identifier of the trust anchor.
newPutNotificationSettings ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  PutNotificationSettings
newPutNotificationSettings pTrustAnchorId_ =
  PutNotificationSettings'
    { notificationSettings =
        Prelude.mempty,
      trustAnchorId = pTrustAnchorId_
    }

-- | A list of notification settings to be associated to the trust anchor.
putNotificationSettings_notificationSettings :: Lens.Lens' PutNotificationSettings [NotificationSetting]
putNotificationSettings_notificationSettings = Lens.lens (\PutNotificationSettings' {notificationSettings} -> notificationSettings) (\s@PutNotificationSettings' {} a -> s {notificationSettings = a} :: PutNotificationSettings) Prelude.. Lens.coerced

-- | The unique identifier of the trust anchor.
putNotificationSettings_trustAnchorId :: Lens.Lens' PutNotificationSettings Prelude.Text
putNotificationSettings_trustAnchorId = Lens.lens (\PutNotificationSettings' {trustAnchorId} -> trustAnchorId) (\s@PutNotificationSettings' {} a -> s {trustAnchorId = a} :: PutNotificationSettings)

instance Core.AWSRequest PutNotificationSettings where
  type
    AWSResponse PutNotificationSettings =
      PutNotificationSettingsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutNotificationSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "trustAnchor")
      )

instance Prelude.Hashable PutNotificationSettings where
  hashWithSalt _salt PutNotificationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` notificationSettings
      `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData PutNotificationSettings where
  rnf PutNotificationSettings' {..} =
    Prelude.rnf notificationSettings
      `Prelude.seq` Prelude.rnf trustAnchorId

instance Data.ToHeaders PutNotificationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutNotificationSettings where
  toJSON PutNotificationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "notificationSettings"
                  Data..= notificationSettings
              ),
            Prelude.Just
              ("trustAnchorId" Data..= trustAnchorId)
          ]
      )

instance Data.ToPath PutNotificationSettings where
  toPath = Prelude.const "/put-notifications-settings"

instance Data.ToQuery PutNotificationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutNotificationSettingsResponse' smart constructor.
data PutNotificationSettingsResponse = PutNotificationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    trustAnchor :: TrustAnchorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putNotificationSettingsResponse_httpStatus' - The response's http status code.
--
-- 'trustAnchor', 'putNotificationSettingsResponse_trustAnchor' - Undocumented member.
newPutNotificationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trustAnchor'
  TrustAnchorDetail ->
  PutNotificationSettingsResponse
newPutNotificationSettingsResponse
  pHttpStatus_
  pTrustAnchor_ =
    PutNotificationSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        trustAnchor = pTrustAnchor_
      }

-- | The response's http status code.
putNotificationSettingsResponse_httpStatus :: Lens.Lens' PutNotificationSettingsResponse Prelude.Int
putNotificationSettingsResponse_httpStatus = Lens.lens (\PutNotificationSettingsResponse' {httpStatus} -> httpStatus) (\s@PutNotificationSettingsResponse' {} a -> s {httpStatus = a} :: PutNotificationSettingsResponse)

-- | Undocumented member.
putNotificationSettingsResponse_trustAnchor :: Lens.Lens' PutNotificationSettingsResponse TrustAnchorDetail
putNotificationSettingsResponse_trustAnchor = Lens.lens (\PutNotificationSettingsResponse' {trustAnchor} -> trustAnchor) (\s@PutNotificationSettingsResponse' {} a -> s {trustAnchor = a} :: PutNotificationSettingsResponse)

instance
  Prelude.NFData
    PutNotificationSettingsResponse
  where
  rnf PutNotificationSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trustAnchor
