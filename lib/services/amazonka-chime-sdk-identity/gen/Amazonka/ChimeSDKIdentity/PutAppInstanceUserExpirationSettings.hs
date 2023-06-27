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
-- Module      : Amazonka.ChimeSDKIdentity.PutAppInstanceUserExpirationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the number of days before the @AppInstanceUser@ is automatically
-- deleted.
--
-- A background process deletes expired @AppInstanceUsers@ within 6 hours
-- of expiration. Actual deletion times may vary.
--
-- Expired @AppInstanceUsers@ that have not yet been deleted appear as
-- active, and you can update their expiration settings. The system honors
-- the new settings.
module Amazonka.ChimeSDKIdentity.PutAppInstanceUserExpirationSettings
  ( -- * Creating a Request
    PutAppInstanceUserExpirationSettings (..),
    newPutAppInstanceUserExpirationSettings,

    -- * Request Lenses
    putAppInstanceUserExpirationSettings_expirationSettings,
    putAppInstanceUserExpirationSettings_appInstanceUserArn,

    -- * Destructuring the Response
    PutAppInstanceUserExpirationSettingsResponse (..),
    newPutAppInstanceUserExpirationSettingsResponse,

    -- * Response Lenses
    putAppInstanceUserExpirationSettingsResponse_appInstanceUserArn,
    putAppInstanceUserExpirationSettingsResponse_expirationSettings,
    putAppInstanceUserExpirationSettingsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAppInstanceUserExpirationSettings' smart constructor.
data PutAppInstanceUserExpirationSettings = PutAppInstanceUserExpirationSettings'
  { -- | Settings that control the interval after which an @AppInstanceUser@ is
    -- automatically deleted.
    expirationSettings :: Prelude.Maybe ExpirationSettings,
    -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppInstanceUserExpirationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationSettings', 'putAppInstanceUserExpirationSettings_expirationSettings' - Settings that control the interval after which an @AppInstanceUser@ is
-- automatically deleted.
--
-- 'appInstanceUserArn', 'putAppInstanceUserExpirationSettings_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
newPutAppInstanceUserExpirationSettings ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  PutAppInstanceUserExpirationSettings
newPutAppInstanceUserExpirationSettings
  pAppInstanceUserArn_ =
    PutAppInstanceUserExpirationSettings'
      { expirationSettings =
          Prelude.Nothing,
        appInstanceUserArn =
          pAppInstanceUserArn_
      }

-- | Settings that control the interval after which an @AppInstanceUser@ is
-- automatically deleted.
putAppInstanceUserExpirationSettings_expirationSettings :: Lens.Lens' PutAppInstanceUserExpirationSettings (Prelude.Maybe ExpirationSettings)
putAppInstanceUserExpirationSettings_expirationSettings = Lens.lens (\PutAppInstanceUserExpirationSettings' {expirationSettings} -> expirationSettings) (\s@PutAppInstanceUserExpirationSettings' {} a -> s {expirationSettings = a} :: PutAppInstanceUserExpirationSettings)

-- | The ARN of the @AppInstanceUser@.
putAppInstanceUserExpirationSettings_appInstanceUserArn :: Lens.Lens' PutAppInstanceUserExpirationSettings Prelude.Text
putAppInstanceUserExpirationSettings_appInstanceUserArn = Lens.lens (\PutAppInstanceUserExpirationSettings' {appInstanceUserArn} -> appInstanceUserArn) (\s@PutAppInstanceUserExpirationSettings' {} a -> s {appInstanceUserArn = a} :: PutAppInstanceUserExpirationSettings)

instance
  Core.AWSRequest
    PutAppInstanceUserExpirationSettings
  where
  type
    AWSResponse PutAppInstanceUserExpirationSettings =
      PutAppInstanceUserExpirationSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAppInstanceUserExpirationSettingsResponse'
            Prelude.<$> (x Data..?> "AppInstanceUserArn")
            Prelude.<*> (x Data..?> "ExpirationSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAppInstanceUserExpirationSettings
  where
  hashWithSalt
    _salt
    PutAppInstanceUserExpirationSettings' {..} =
      _salt
        `Prelude.hashWithSalt` expirationSettings
        `Prelude.hashWithSalt` appInstanceUserArn

instance
  Prelude.NFData
    PutAppInstanceUserExpirationSettings
  where
  rnf PutAppInstanceUserExpirationSettings' {..} =
    Prelude.rnf expirationSettings
      `Prelude.seq` Prelude.rnf appInstanceUserArn

instance
  Data.ToHeaders
    PutAppInstanceUserExpirationSettings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutAppInstanceUserExpirationSettings
  where
  toJSON PutAppInstanceUserExpirationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpirationSettings" Data..=)
              Prelude.<$> expirationSettings
          ]
      )

instance
  Data.ToPath
    PutAppInstanceUserExpirationSettings
  where
  toPath PutAppInstanceUserExpirationSettings' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn,
        "/expiration-settings"
      ]

instance
  Data.ToQuery
    PutAppInstanceUserExpirationSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppInstanceUserExpirationSettingsResponse' smart constructor.
data PutAppInstanceUserExpirationSettingsResponse = PutAppInstanceUserExpirationSettingsResponse'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | Settings that control the interval after which an @AppInstanceUser@ is
    -- automatically deleted.
    expirationSettings :: Prelude.Maybe ExpirationSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppInstanceUserExpirationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'putAppInstanceUserExpirationSettingsResponse_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'expirationSettings', 'putAppInstanceUserExpirationSettingsResponse_expirationSettings' - Settings that control the interval after which an @AppInstanceUser@ is
-- automatically deleted.
--
-- 'httpStatus', 'putAppInstanceUserExpirationSettingsResponse_httpStatus' - The response's http status code.
newPutAppInstanceUserExpirationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAppInstanceUserExpirationSettingsResponse
newPutAppInstanceUserExpirationSettingsResponse
  pHttpStatus_ =
    PutAppInstanceUserExpirationSettingsResponse'
      { appInstanceUserArn =
          Prelude.Nothing,
        expirationSettings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the @AppInstanceUser@.
putAppInstanceUserExpirationSettingsResponse_appInstanceUserArn :: Lens.Lens' PutAppInstanceUserExpirationSettingsResponse (Prelude.Maybe Prelude.Text)
putAppInstanceUserExpirationSettingsResponse_appInstanceUserArn = Lens.lens (\PutAppInstanceUserExpirationSettingsResponse' {appInstanceUserArn} -> appInstanceUserArn) (\s@PutAppInstanceUserExpirationSettingsResponse' {} a -> s {appInstanceUserArn = a} :: PutAppInstanceUserExpirationSettingsResponse)

-- | Settings that control the interval after which an @AppInstanceUser@ is
-- automatically deleted.
putAppInstanceUserExpirationSettingsResponse_expirationSettings :: Lens.Lens' PutAppInstanceUserExpirationSettingsResponse (Prelude.Maybe ExpirationSettings)
putAppInstanceUserExpirationSettingsResponse_expirationSettings = Lens.lens (\PutAppInstanceUserExpirationSettingsResponse' {expirationSettings} -> expirationSettings) (\s@PutAppInstanceUserExpirationSettingsResponse' {} a -> s {expirationSettings = a} :: PutAppInstanceUserExpirationSettingsResponse)

-- | The response's http status code.
putAppInstanceUserExpirationSettingsResponse_httpStatus :: Lens.Lens' PutAppInstanceUserExpirationSettingsResponse Prelude.Int
putAppInstanceUserExpirationSettingsResponse_httpStatus = Lens.lens (\PutAppInstanceUserExpirationSettingsResponse' {httpStatus} -> httpStatus) (\s@PutAppInstanceUserExpirationSettingsResponse' {} a -> s {httpStatus = a} :: PutAppInstanceUserExpirationSettingsResponse)

instance
  Prelude.NFData
    PutAppInstanceUserExpirationSettingsResponse
  where
  rnf PutAppInstanceUserExpirationSettingsResponse' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf expirationSettings
      `Prelude.seq` Prelude.rnf httpStatus
