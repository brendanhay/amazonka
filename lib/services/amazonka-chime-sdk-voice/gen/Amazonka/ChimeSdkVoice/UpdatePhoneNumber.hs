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
-- Module      : Amazonka.ChimeSdkVoice.UpdatePhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.UpdatePhoneNumber
  ( -- * Creating a Request
    UpdatePhoneNumber (..),
    newUpdatePhoneNumber,

    -- * Request Lenses
    updatePhoneNumber_callingName,
    updatePhoneNumber_productType,
    updatePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    UpdatePhoneNumberResponse (..),
    newUpdatePhoneNumberResponse,

    -- * Response Lenses
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePhoneNumber' smart constructor.
data UpdatePhoneNumber = UpdatePhoneNumber'
  { callingName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    productType :: Prelude.Maybe PhoneNumberProductType,
    phoneNumberId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callingName', 'updatePhoneNumber_callingName' - Undocumented member.
--
-- 'productType', 'updatePhoneNumber_productType' - Undocumented member.
--
-- 'phoneNumberId', 'updatePhoneNumber_phoneNumberId' - Undocumented member.
newUpdatePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  UpdatePhoneNumber
newUpdatePhoneNumber pPhoneNumberId_ =
  UpdatePhoneNumber'
    { callingName = Prelude.Nothing,
      productType = Prelude.Nothing,
      phoneNumberId =
        Data._Sensitive Lens.# pPhoneNumberId_
    }

-- | Undocumented member.
updatePhoneNumber_callingName :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Text)
updatePhoneNumber_callingName = Lens.lens (\UpdatePhoneNumber' {callingName} -> callingName) (\s@UpdatePhoneNumber' {} a -> s {callingName = a} :: UpdatePhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
updatePhoneNumber_productType :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe PhoneNumberProductType)
updatePhoneNumber_productType = Lens.lens (\UpdatePhoneNumber' {productType} -> productType) (\s@UpdatePhoneNumber' {} a -> s {productType = a} :: UpdatePhoneNumber)

-- | Undocumented member.
updatePhoneNumber_phoneNumberId :: Lens.Lens' UpdatePhoneNumber Prelude.Text
updatePhoneNumber_phoneNumberId = Lens.lens (\UpdatePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumber' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumber) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdatePhoneNumber where
  type
    AWSResponse UpdatePhoneNumber =
      UpdatePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePhoneNumberResponse'
            Prelude.<$> (x Data..?> "PhoneNumber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePhoneNumber where
  hashWithSalt _salt UpdatePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` callingName
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData UpdatePhoneNumber where
  rnf UpdatePhoneNumber' {..} =
    Prelude.rnf callingName
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf phoneNumberId

instance Data.ToHeaders UpdatePhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePhoneNumber where
  toJSON UpdatePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CallingName" Data..=) Prelude.<$> callingName,
            ("ProductType" Data..=) Prelude.<$> productType
          ]
      )

instance Data.ToPath UpdatePhoneNumber where
  toPath UpdatePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-numbers/", Data.toBS phoneNumberId]

instance Data.ToQuery UpdatePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePhoneNumberResponse' smart constructor.
data UpdatePhoneNumberResponse = UpdatePhoneNumberResponse'
  { phoneNumber :: Prelude.Maybe PhoneNumber,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'updatePhoneNumberResponse_phoneNumber' - Undocumented member.
--
-- 'httpStatus', 'updatePhoneNumberResponse_httpStatus' - The response's http status code.
newUpdatePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePhoneNumberResponse
newUpdatePhoneNumberResponse pHttpStatus_ =
  UpdatePhoneNumberResponse'
    { phoneNumber =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updatePhoneNumberResponse_phoneNumber :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe PhoneNumber)
updatePhoneNumberResponse_phoneNumber = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumber} -> phoneNumber) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumber = a} :: UpdatePhoneNumberResponse)

-- | The response's http status code.
updatePhoneNumberResponse_httpStatus :: Lens.Lens' UpdatePhoneNumberResponse Prelude.Int
updatePhoneNumberResponse_httpStatus = Lens.lens (\UpdatePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@UpdatePhoneNumberResponse' {} a -> s {httpStatus = a} :: UpdatePhoneNumberResponse)

instance Prelude.NFData UpdatePhoneNumberResponse where
  rnf UpdatePhoneNumberResponse' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf httpStatus
