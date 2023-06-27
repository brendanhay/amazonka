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
-- Module      : Amazonka.ChimeSdkVoice.GetPhoneNumberSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the phone number settings for the administrator\'s AWS
-- account, such as the default outbound calling name.
module Amazonka.ChimeSdkVoice.GetPhoneNumberSettings
  ( -- * Creating a Request
    GetPhoneNumberSettings (..),
    newGetPhoneNumberSettings,

    -- * Destructuring the Response
    GetPhoneNumberSettingsResponse (..),
    newGetPhoneNumberSettingsResponse,

    -- * Response Lenses
    getPhoneNumberSettingsResponse_callingName,
    getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp,
    getPhoneNumberSettingsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPhoneNumberSettings' smart constructor.
data GetPhoneNumberSettings = GetPhoneNumberSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPhoneNumberSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPhoneNumberSettings ::
  GetPhoneNumberSettings
newGetPhoneNumberSettings = GetPhoneNumberSettings'

instance Core.AWSRequest GetPhoneNumberSettings where
  type
    AWSResponse GetPhoneNumberSettings =
      GetPhoneNumberSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPhoneNumberSettingsResponse'
            Prelude.<$> (x Data..?> "CallingName")
            Prelude.<*> (x Data..?> "CallingNameUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPhoneNumberSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPhoneNumberSettings where
  rnf _ = ()

instance Data.ToHeaders GetPhoneNumberSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPhoneNumberSettings where
  toPath = Prelude.const "/settings/phone-number"

instance Data.ToQuery GetPhoneNumberSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPhoneNumberSettingsResponse' smart constructor.
data GetPhoneNumberSettingsResponse = GetPhoneNumberSettingsResponse'
  { -- | The default outbound calling name for the account.
    callingName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The updated outbound calling name timestamp, in ISO 8601 format.
    callingNameUpdatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPhoneNumberSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callingName', 'getPhoneNumberSettingsResponse_callingName' - The default outbound calling name for the account.
--
-- 'callingNameUpdatedTimestamp', 'getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp' - The updated outbound calling name timestamp, in ISO 8601 format.
--
-- 'httpStatus', 'getPhoneNumberSettingsResponse_httpStatus' - The response's http status code.
newGetPhoneNumberSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPhoneNumberSettingsResponse
newGetPhoneNumberSettingsResponse pHttpStatus_ =
  GetPhoneNumberSettingsResponse'
    { callingName =
        Prelude.Nothing,
      callingNameUpdatedTimestamp =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default outbound calling name for the account.
getPhoneNumberSettingsResponse_callingName :: Lens.Lens' GetPhoneNumberSettingsResponse (Prelude.Maybe Prelude.Text)
getPhoneNumberSettingsResponse_callingName = Lens.lens (\GetPhoneNumberSettingsResponse' {callingName} -> callingName) (\s@GetPhoneNumberSettingsResponse' {} a -> s {callingName = a} :: GetPhoneNumberSettingsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The updated outbound calling name timestamp, in ISO 8601 format.
getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp :: Lens.Lens' GetPhoneNumberSettingsResponse (Prelude.Maybe Prelude.UTCTime)
getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp = Lens.lens (\GetPhoneNumberSettingsResponse' {callingNameUpdatedTimestamp} -> callingNameUpdatedTimestamp) (\s@GetPhoneNumberSettingsResponse' {} a -> s {callingNameUpdatedTimestamp = a} :: GetPhoneNumberSettingsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getPhoneNumberSettingsResponse_httpStatus :: Lens.Lens' GetPhoneNumberSettingsResponse Prelude.Int
getPhoneNumberSettingsResponse_httpStatus = Lens.lens (\GetPhoneNumberSettingsResponse' {httpStatus} -> httpStatus) (\s@GetPhoneNumberSettingsResponse' {} a -> s {httpStatus = a} :: GetPhoneNumberSettingsResponse)

instance
  Prelude.NFData
    GetPhoneNumberSettingsResponse
  where
  rnf GetPhoneNumberSettingsResponse' {..} =
    Prelude.rnf callingName
      `Prelude.seq` Prelude.rnf callingNameUpdatedTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
