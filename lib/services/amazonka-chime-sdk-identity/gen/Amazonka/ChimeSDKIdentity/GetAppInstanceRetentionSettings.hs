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
-- Module      : Amazonka.ChimeSDKIdentity.GetAppInstanceRetentionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the retention settings for an @AppInstance@.
module Amazonka.ChimeSDKIdentity.GetAppInstanceRetentionSettings
  ( -- * Creating a Request
    GetAppInstanceRetentionSettings (..),
    newGetAppInstanceRetentionSettings,

    -- * Request Lenses
    getAppInstanceRetentionSettings_appInstanceArn,

    -- * Destructuring the Response
    GetAppInstanceRetentionSettingsResponse (..),
    newGetAppInstanceRetentionSettingsResponse,

    -- * Response Lenses
    getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    getAppInstanceRetentionSettingsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAppInstanceRetentionSettings' smart constructor.
data GetAppInstanceRetentionSettings = GetAppInstanceRetentionSettings'
  { -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppInstanceRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'getAppInstanceRetentionSettings_appInstanceArn' - The ARN of the @AppInstance@.
newGetAppInstanceRetentionSettings ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  GetAppInstanceRetentionSettings
newGetAppInstanceRetentionSettings pAppInstanceArn_ =
  GetAppInstanceRetentionSettings'
    { appInstanceArn =
        pAppInstanceArn_
    }

-- | The ARN of the @AppInstance@.
getAppInstanceRetentionSettings_appInstanceArn :: Lens.Lens' GetAppInstanceRetentionSettings Prelude.Text
getAppInstanceRetentionSettings_appInstanceArn = Lens.lens (\GetAppInstanceRetentionSettings' {appInstanceArn} -> appInstanceArn) (\s@GetAppInstanceRetentionSettings' {} a -> s {appInstanceArn = a} :: GetAppInstanceRetentionSettings)

instance
  Core.AWSRequest
    GetAppInstanceRetentionSettings
  where
  type
    AWSResponse GetAppInstanceRetentionSettings =
      GetAppInstanceRetentionSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppInstanceRetentionSettingsResponse'
            Prelude.<$> (x Data..?> "AppInstanceRetentionSettings")
            Prelude.<*> (x Data..?> "InitiateDeletionTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAppInstanceRetentionSettings
  where
  hashWithSalt
    _salt
    GetAppInstanceRetentionSettings' {..} =
      _salt `Prelude.hashWithSalt` appInstanceArn

instance
  Prelude.NFData
    GetAppInstanceRetentionSettings
  where
  rnf GetAppInstanceRetentionSettings' {..} =
    Prelude.rnf appInstanceArn

instance
  Data.ToHeaders
    GetAppInstanceRetentionSettings
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAppInstanceRetentionSettings where
  toPath GetAppInstanceRetentionSettings' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/retention-settings"
      ]

instance Data.ToQuery GetAppInstanceRetentionSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppInstanceRetentionSettingsResponse' smart constructor.
data GetAppInstanceRetentionSettingsResponse = GetAppInstanceRetentionSettingsResponse'
  { -- | The retention settings for the @AppInstance@.
    appInstanceRetentionSettings :: Prelude.Maybe AppInstanceRetentionSettings,
    -- | The timestamp representing the time at which the specified items are
    -- retained, in Epoch Seconds.
    initiateDeletionTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppInstanceRetentionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceRetentionSettings', 'getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings' - The retention settings for the @AppInstance@.
--
-- 'initiateDeletionTimestamp', 'getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp' - The timestamp representing the time at which the specified items are
-- retained, in Epoch Seconds.
--
-- 'httpStatus', 'getAppInstanceRetentionSettingsResponse_httpStatus' - The response's http status code.
newGetAppInstanceRetentionSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppInstanceRetentionSettingsResponse
newGetAppInstanceRetentionSettingsResponse
  pHttpStatus_ =
    GetAppInstanceRetentionSettingsResponse'
      { appInstanceRetentionSettings =
          Prelude.Nothing,
        initiateDeletionTimestamp =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The retention settings for the @AppInstance@.
getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings :: Lens.Lens' GetAppInstanceRetentionSettingsResponse (Prelude.Maybe AppInstanceRetentionSettings)
getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings = Lens.lens (\GetAppInstanceRetentionSettingsResponse' {appInstanceRetentionSettings} -> appInstanceRetentionSettings) (\s@GetAppInstanceRetentionSettingsResponse' {} a -> s {appInstanceRetentionSettings = a} :: GetAppInstanceRetentionSettingsResponse)

-- | The timestamp representing the time at which the specified items are
-- retained, in Epoch Seconds.
getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp :: Lens.Lens' GetAppInstanceRetentionSettingsResponse (Prelude.Maybe Prelude.UTCTime)
getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp = Lens.lens (\GetAppInstanceRetentionSettingsResponse' {initiateDeletionTimestamp} -> initiateDeletionTimestamp) (\s@GetAppInstanceRetentionSettingsResponse' {} a -> s {initiateDeletionTimestamp = a} :: GetAppInstanceRetentionSettingsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getAppInstanceRetentionSettingsResponse_httpStatus :: Lens.Lens' GetAppInstanceRetentionSettingsResponse Prelude.Int
getAppInstanceRetentionSettingsResponse_httpStatus = Lens.lens (\GetAppInstanceRetentionSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAppInstanceRetentionSettingsResponse' {} a -> s {httpStatus = a} :: GetAppInstanceRetentionSettingsResponse)

instance
  Prelude.NFData
    GetAppInstanceRetentionSettingsResponse
  where
  rnf GetAppInstanceRetentionSettingsResponse' {..} =
    Prelude.rnf appInstanceRetentionSettings
      `Prelude.seq` Prelude.rnf initiateDeletionTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
