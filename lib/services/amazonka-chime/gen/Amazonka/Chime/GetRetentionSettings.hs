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
-- Module      : Amazonka.Chime.GetRetentionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the retention settings for the specified Amazon Chime Enterprise
-- account. For more information about retention settings, see
-- <https://docs.aws.amazon.com/chime/latest/ag/chat-retention.html Managing Chat Retention Policies>
-- in the /Amazon Chime Administration Guide/.
module Amazonka.Chime.GetRetentionSettings
  ( -- * Creating a Request
    GetRetentionSettings (..),
    newGetRetentionSettings,

    -- * Request Lenses
    getRetentionSettings_accountId,

    -- * Destructuring the Response
    GetRetentionSettingsResponse (..),
    newGetRetentionSettingsResponse,

    -- * Response Lenses
    getRetentionSettingsResponse_initiateDeletionTimestamp,
    getRetentionSettingsResponse_retentionSettings,
    getRetentionSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRetentionSettings' smart constructor.
data GetRetentionSettings = GetRetentionSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getRetentionSettings_accountId' - The Amazon Chime account ID.
newGetRetentionSettings ::
  -- | 'accountId'
  Prelude.Text ->
  GetRetentionSettings
newGetRetentionSettings pAccountId_ =
  GetRetentionSettings' {accountId = pAccountId_}

-- | The Amazon Chime account ID.
getRetentionSettings_accountId :: Lens.Lens' GetRetentionSettings Prelude.Text
getRetentionSettings_accountId = Lens.lens (\GetRetentionSettings' {accountId} -> accountId) (\s@GetRetentionSettings' {} a -> s {accountId = a} :: GetRetentionSettings)

instance Core.AWSRequest GetRetentionSettings where
  type
    AWSResponse GetRetentionSettings =
      GetRetentionSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRetentionSettingsResponse'
            Prelude.<$> (x Core..?> "InitiateDeletionTimestamp")
            Prelude.<*> (x Core..?> "RetentionSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRetentionSettings where
  hashWithSalt _salt GetRetentionSettings' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetRetentionSettings where
  rnf GetRetentionSettings' {..} = Prelude.rnf accountId

instance Core.ToHeaders GetRetentionSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetRetentionSettings where
  toPath GetRetentionSettings' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS accountId,
        "/retention-settings"
      ]

instance Core.ToQuery GetRetentionSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRetentionSettingsResponse' smart constructor.
data GetRetentionSettingsResponse = GetRetentionSettingsResponse'
  { -- | The timestamp representing the time at which the specified items are
    -- permanently deleted, in ISO 8601 format.
    initiateDeletionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The retention settings.
    retentionSettings :: Prelude.Maybe RetentionSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRetentionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiateDeletionTimestamp', 'getRetentionSettingsResponse_initiateDeletionTimestamp' - The timestamp representing the time at which the specified items are
-- permanently deleted, in ISO 8601 format.
--
-- 'retentionSettings', 'getRetentionSettingsResponse_retentionSettings' - The retention settings.
--
-- 'httpStatus', 'getRetentionSettingsResponse_httpStatus' - The response's http status code.
newGetRetentionSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRetentionSettingsResponse
newGetRetentionSettingsResponse pHttpStatus_ =
  GetRetentionSettingsResponse'
    { initiateDeletionTimestamp =
        Prelude.Nothing,
      retentionSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp representing the time at which the specified items are
-- permanently deleted, in ISO 8601 format.
getRetentionSettingsResponse_initiateDeletionTimestamp :: Lens.Lens' GetRetentionSettingsResponse (Prelude.Maybe Prelude.UTCTime)
getRetentionSettingsResponse_initiateDeletionTimestamp = Lens.lens (\GetRetentionSettingsResponse' {initiateDeletionTimestamp} -> initiateDeletionTimestamp) (\s@GetRetentionSettingsResponse' {} a -> s {initiateDeletionTimestamp = a} :: GetRetentionSettingsResponse) Prelude.. Lens.mapping Core._Time

-- | The retention settings.
getRetentionSettingsResponse_retentionSettings :: Lens.Lens' GetRetentionSettingsResponse (Prelude.Maybe RetentionSettings)
getRetentionSettingsResponse_retentionSettings = Lens.lens (\GetRetentionSettingsResponse' {retentionSettings} -> retentionSettings) (\s@GetRetentionSettingsResponse' {} a -> s {retentionSettings = a} :: GetRetentionSettingsResponse)

-- | The response's http status code.
getRetentionSettingsResponse_httpStatus :: Lens.Lens' GetRetentionSettingsResponse Prelude.Int
getRetentionSettingsResponse_httpStatus = Lens.lens (\GetRetentionSettingsResponse' {httpStatus} -> httpStatus) (\s@GetRetentionSettingsResponse' {} a -> s {httpStatus = a} :: GetRetentionSettingsResponse)

instance Prelude.NFData GetRetentionSettingsResponse where
  rnf GetRetentionSettingsResponse' {..} =
    Prelude.rnf initiateDeletionTimestamp
      `Prelude.seq` Prelude.rnf retentionSettings
      `Prelude.seq` Prelude.rnf httpStatus
