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
-- Module      : Amazonka.Chime.PutRetentionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts retention settings for the specified Amazon Chime Enterprise
-- account. We recommend using AWS CloudTrail to monitor usage of this API
-- for your account. For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ag/cloudtrail.html Logging Amazon Chime API Calls with AWS CloudTrail>
-- in the /Amazon Chime Administration Guide/.
--
-- To turn off existing retention settings, remove the number of days from
-- the corresponding __RetentionDays__ field in the __RetentionSettings__
-- object. For more information about retention settings, see
-- <https://docs.aws.amazon.com/chime/latest/ag/chat-retention.html Managing Chat Retention Policies>
-- in the /Amazon Chime Administration Guide/.
module Amazonka.Chime.PutRetentionSettings
  ( -- * Creating a Request
    PutRetentionSettings (..),
    newPutRetentionSettings,

    -- * Request Lenses
    putRetentionSettings_accountId,
    putRetentionSettings_retentionSettings,

    -- * Destructuring the Response
    PutRetentionSettingsResponse (..),
    newPutRetentionSettingsResponse,

    -- * Response Lenses
    putRetentionSettingsResponse_initiateDeletionTimestamp,
    putRetentionSettingsResponse_retentionSettings,
    putRetentionSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRetentionSettings' smart constructor.
data PutRetentionSettings = PutRetentionSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The retention settings.
    retentionSettings :: RetentionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'putRetentionSettings_accountId' - The Amazon Chime account ID.
--
-- 'retentionSettings', 'putRetentionSettings_retentionSettings' - The retention settings.
newPutRetentionSettings ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'retentionSettings'
  RetentionSettings ->
  PutRetentionSettings
newPutRetentionSettings
  pAccountId_
  pRetentionSettings_ =
    PutRetentionSettings'
      { accountId = pAccountId_,
        retentionSettings = pRetentionSettings_
      }

-- | The Amazon Chime account ID.
putRetentionSettings_accountId :: Lens.Lens' PutRetentionSettings Prelude.Text
putRetentionSettings_accountId = Lens.lens (\PutRetentionSettings' {accountId} -> accountId) (\s@PutRetentionSettings' {} a -> s {accountId = a} :: PutRetentionSettings)

-- | The retention settings.
putRetentionSettings_retentionSettings :: Lens.Lens' PutRetentionSettings RetentionSettings
putRetentionSettings_retentionSettings = Lens.lens (\PutRetentionSettings' {retentionSettings} -> retentionSettings) (\s@PutRetentionSettings' {} a -> s {retentionSettings = a} :: PutRetentionSettings)

instance Core.AWSRequest PutRetentionSettings where
  type
    AWSResponse PutRetentionSettings =
      PutRetentionSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRetentionSettingsResponse'
            Prelude.<$> (x Data..?> "InitiateDeletionTimestamp")
            Prelude.<*> (x Data..?> "RetentionSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRetentionSettings where
  hashWithSalt _salt PutRetentionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` retentionSettings

instance Prelude.NFData PutRetentionSettings where
  rnf PutRetentionSettings' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf retentionSettings

instance Data.ToHeaders PutRetentionSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutRetentionSettings where
  toJSON PutRetentionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RetentionSettings" Data..= retentionSettings)
          ]
      )

instance Data.ToPath PutRetentionSettings where
  toPath PutRetentionSettings' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/retention-settings"
      ]

instance Data.ToQuery PutRetentionSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRetentionSettingsResponse' smart constructor.
data PutRetentionSettingsResponse = PutRetentionSettingsResponse'
  { -- | The timestamp representing the time at which the specified items are
    -- permanently deleted, in ISO 8601 format.
    initiateDeletionTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The retention settings.
    retentionSettings :: Prelude.Maybe RetentionSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiateDeletionTimestamp', 'putRetentionSettingsResponse_initiateDeletionTimestamp' - The timestamp representing the time at which the specified items are
-- permanently deleted, in ISO 8601 format.
--
-- 'retentionSettings', 'putRetentionSettingsResponse_retentionSettings' - The retention settings.
--
-- 'httpStatus', 'putRetentionSettingsResponse_httpStatus' - The response's http status code.
newPutRetentionSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRetentionSettingsResponse
newPutRetentionSettingsResponse pHttpStatus_ =
  PutRetentionSettingsResponse'
    { initiateDeletionTimestamp =
        Prelude.Nothing,
      retentionSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp representing the time at which the specified items are
-- permanently deleted, in ISO 8601 format.
putRetentionSettingsResponse_initiateDeletionTimestamp :: Lens.Lens' PutRetentionSettingsResponse (Prelude.Maybe Prelude.UTCTime)
putRetentionSettingsResponse_initiateDeletionTimestamp = Lens.lens (\PutRetentionSettingsResponse' {initiateDeletionTimestamp} -> initiateDeletionTimestamp) (\s@PutRetentionSettingsResponse' {} a -> s {initiateDeletionTimestamp = a} :: PutRetentionSettingsResponse) Prelude.. Lens.mapping Data._Time

-- | The retention settings.
putRetentionSettingsResponse_retentionSettings :: Lens.Lens' PutRetentionSettingsResponse (Prelude.Maybe RetentionSettings)
putRetentionSettingsResponse_retentionSettings = Lens.lens (\PutRetentionSettingsResponse' {retentionSettings} -> retentionSettings) (\s@PutRetentionSettingsResponse' {} a -> s {retentionSettings = a} :: PutRetentionSettingsResponse)

-- | The response's http status code.
putRetentionSettingsResponse_httpStatus :: Lens.Lens' PutRetentionSettingsResponse Prelude.Int
putRetentionSettingsResponse_httpStatus = Lens.lens (\PutRetentionSettingsResponse' {httpStatus} -> httpStatus) (\s@PutRetentionSettingsResponse' {} a -> s {httpStatus = a} :: PutRetentionSettingsResponse)

instance Prelude.NFData PutRetentionSettingsResponse where
  rnf PutRetentionSettingsResponse' {..} =
    Prelude.rnf initiateDeletionTimestamp
      `Prelude.seq` Prelude.rnf retentionSettings
      `Prelude.seq` Prelude.rnf httpStatus
