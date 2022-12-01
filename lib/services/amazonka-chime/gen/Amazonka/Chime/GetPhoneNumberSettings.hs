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
-- Module      : Amazonka.Chime.GetPhoneNumberSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the phone number settings for the administrator\'s AWS
-- account, such as the default outbound calling name.
module Amazonka.Chime.GetPhoneNumberSettings
  ( -- * Creating a Request
    GetPhoneNumberSettings (..),
    newGetPhoneNumberSettings,

    -- * Destructuring the Response
    GetPhoneNumberSettingsResponse (..),
    newGetPhoneNumberSettingsResponse,

    -- * Response Lenses
    getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp,
    getPhoneNumberSettingsResponse_callingName,
    getPhoneNumberSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "CallingNameUpdatedTimestamp")
            Prelude.<*> (x Core..?> "CallingName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPhoneNumberSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPhoneNumberSettings where
  rnf _ = ()

instance Core.ToHeaders GetPhoneNumberSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetPhoneNumberSettings where
  toPath = Prelude.const "/settings/phone-number"

instance Core.ToQuery GetPhoneNumberSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPhoneNumberSettingsResponse' smart constructor.
data GetPhoneNumberSettingsResponse = GetPhoneNumberSettingsResponse'
  { -- | The updated outbound calling name timestamp, in ISO 8601 format.
    callingNameUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The default outbound calling name for the account.
    callingName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
-- 'callingNameUpdatedTimestamp', 'getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp' - The updated outbound calling name timestamp, in ISO 8601 format.
--
-- 'callingName', 'getPhoneNumberSettingsResponse_callingName' - The default outbound calling name for the account.
--
-- 'httpStatus', 'getPhoneNumberSettingsResponse_httpStatus' - The response's http status code.
newGetPhoneNumberSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPhoneNumberSettingsResponse
newGetPhoneNumberSettingsResponse pHttpStatus_ =
  GetPhoneNumberSettingsResponse'
    { callingNameUpdatedTimestamp =
        Prelude.Nothing,
      callingName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated outbound calling name timestamp, in ISO 8601 format.
getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp :: Lens.Lens' GetPhoneNumberSettingsResponse (Prelude.Maybe Prelude.UTCTime)
getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp = Lens.lens (\GetPhoneNumberSettingsResponse' {callingNameUpdatedTimestamp} -> callingNameUpdatedTimestamp) (\s@GetPhoneNumberSettingsResponse' {} a -> s {callingNameUpdatedTimestamp = a} :: GetPhoneNumberSettingsResponse) Prelude.. Lens.mapping Core._Time

-- | The default outbound calling name for the account.
getPhoneNumberSettingsResponse_callingName :: Lens.Lens' GetPhoneNumberSettingsResponse (Prelude.Maybe Prelude.Text)
getPhoneNumberSettingsResponse_callingName = Lens.lens (\GetPhoneNumberSettingsResponse' {callingName} -> callingName) (\s@GetPhoneNumberSettingsResponse' {} a -> s {callingName = a} :: GetPhoneNumberSettingsResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getPhoneNumberSettingsResponse_httpStatus :: Lens.Lens' GetPhoneNumberSettingsResponse Prelude.Int
getPhoneNumberSettingsResponse_httpStatus = Lens.lens (\GetPhoneNumberSettingsResponse' {httpStatus} -> httpStatus) (\s@GetPhoneNumberSettingsResponse' {} a -> s {httpStatus = a} :: GetPhoneNumberSettingsResponse)

instance
  Prelude.NFData
    GetPhoneNumberSettingsResponse
  where
  rnf GetPhoneNumberSettingsResponse' {..} =
    Prelude.rnf callingNameUpdatedTimestamp
      `Prelude.seq` Prelude.rnf callingName
      `Prelude.seq` Prelude.rnf httpStatus
