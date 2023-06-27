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
-- Module      : Amazonka.ChimeSdkVoice.RestorePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a deleted phone number.
module Amazonka.ChimeSdkVoice.RestorePhoneNumber
  ( -- * Creating a Request
    RestorePhoneNumber (..),
    newRestorePhoneNumber,

    -- * Request Lenses
    restorePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    RestorePhoneNumberResponse (..),
    newRestorePhoneNumberResponse,

    -- * Response Lenses
    restorePhoneNumberResponse_phoneNumber,
    restorePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestorePhoneNumber' smart constructor.
data RestorePhoneNumber = RestorePhoneNumber'
  { -- | The ID of the phone number being restored.
    phoneNumberId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestorePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'restorePhoneNumber_phoneNumberId' - The ID of the phone number being restored.
newRestorePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  RestorePhoneNumber
newRestorePhoneNumber pPhoneNumberId_ =
  RestorePhoneNumber'
    { phoneNumberId =
        Data._Sensitive Lens.# pPhoneNumberId_
    }

-- | The ID of the phone number being restored.
restorePhoneNumber_phoneNumberId :: Lens.Lens' RestorePhoneNumber Prelude.Text
restorePhoneNumber_phoneNumberId = Lens.lens (\RestorePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@RestorePhoneNumber' {} a -> s {phoneNumberId = a} :: RestorePhoneNumber) Prelude.. Data._Sensitive

instance Core.AWSRequest RestorePhoneNumber where
  type
    AWSResponse RestorePhoneNumber =
      RestorePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestorePhoneNumberResponse'
            Prelude.<$> (x Data..?> "PhoneNumber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestorePhoneNumber where
  hashWithSalt _salt RestorePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData RestorePhoneNumber where
  rnf RestorePhoneNumber' {..} =
    Prelude.rnf phoneNumberId

instance Data.ToHeaders RestorePhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RestorePhoneNumber where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RestorePhoneNumber where
  toPath RestorePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-numbers/", Data.toBS phoneNumberId]

instance Data.ToQuery RestorePhoneNumber where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=restore"])

-- | /See:/ 'newRestorePhoneNumberResponse' smart constructor.
data RestorePhoneNumberResponse = RestorePhoneNumberResponse'
  { -- | The restored phone number.
    phoneNumber :: Prelude.Maybe PhoneNumber,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestorePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'restorePhoneNumberResponse_phoneNumber' - The restored phone number.
--
-- 'httpStatus', 'restorePhoneNumberResponse_httpStatus' - The response's http status code.
newRestorePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestorePhoneNumberResponse
newRestorePhoneNumberResponse pHttpStatus_ =
  RestorePhoneNumberResponse'
    { phoneNumber =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The restored phone number.
restorePhoneNumberResponse_phoneNumber :: Lens.Lens' RestorePhoneNumberResponse (Prelude.Maybe PhoneNumber)
restorePhoneNumberResponse_phoneNumber = Lens.lens (\RestorePhoneNumberResponse' {phoneNumber} -> phoneNumber) (\s@RestorePhoneNumberResponse' {} a -> s {phoneNumber = a} :: RestorePhoneNumberResponse)

-- | The response's http status code.
restorePhoneNumberResponse_httpStatus :: Lens.Lens' RestorePhoneNumberResponse Prelude.Int
restorePhoneNumberResponse_httpStatus = Lens.lens (\RestorePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@RestorePhoneNumberResponse' {} a -> s {httpStatus = a} :: RestorePhoneNumberResponse)

instance Prelude.NFData RestorePhoneNumberResponse where
  rnf RestorePhoneNumberResponse' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf httpStatus
