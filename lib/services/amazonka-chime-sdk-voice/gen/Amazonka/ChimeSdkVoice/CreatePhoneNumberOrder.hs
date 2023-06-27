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
-- Module      : Amazonka.ChimeSdkVoice.CreatePhoneNumberOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an order for phone numbers to be provisioned. For numbers
-- outside the U.S., you must use the Amazon Chime SDK SIP media
-- application dial-in product type.
module Amazonka.ChimeSdkVoice.CreatePhoneNumberOrder
  ( -- * Creating a Request
    CreatePhoneNumberOrder (..),
    newCreatePhoneNumberOrder,

    -- * Request Lenses
    createPhoneNumberOrder_productType,
    createPhoneNumberOrder_e164PhoneNumbers,

    -- * Destructuring the Response
    CreatePhoneNumberOrderResponse (..),
    newCreatePhoneNumberOrderResponse,

    -- * Response Lenses
    createPhoneNumberOrderResponse_phoneNumberOrder,
    createPhoneNumberOrderResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePhoneNumberOrder' smart constructor.
data CreatePhoneNumberOrder = CreatePhoneNumberOrder'
  { -- | The phone number product type.
    productType :: PhoneNumberProductType,
    -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePhoneNumberOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productType', 'createPhoneNumberOrder_productType' - The phone number product type.
--
-- 'e164PhoneNumbers', 'createPhoneNumberOrder_e164PhoneNumbers' - List of phone numbers, in E.164 format.
newCreatePhoneNumberOrder ::
  -- | 'productType'
  PhoneNumberProductType ->
  CreatePhoneNumberOrder
newCreatePhoneNumberOrder pProductType_ =
  CreatePhoneNumberOrder'
    { productType =
        pProductType_,
      e164PhoneNumbers = Prelude.mempty
    }

-- | The phone number product type.
createPhoneNumberOrder_productType :: Lens.Lens' CreatePhoneNumberOrder PhoneNumberProductType
createPhoneNumberOrder_productType = Lens.lens (\CreatePhoneNumberOrder' {productType} -> productType) (\s@CreatePhoneNumberOrder' {} a -> s {productType = a} :: CreatePhoneNumberOrder)

-- | List of phone numbers, in E.164 format.
createPhoneNumberOrder_e164PhoneNumbers :: Lens.Lens' CreatePhoneNumberOrder [Prelude.Text]
createPhoneNumberOrder_e164PhoneNumbers = Lens.lens (\CreatePhoneNumberOrder' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@CreatePhoneNumberOrder' {} a -> s {e164PhoneNumbers = a} :: CreatePhoneNumberOrder) Prelude.. Lens.coerced

instance Core.AWSRequest CreatePhoneNumberOrder where
  type
    AWSResponse CreatePhoneNumberOrder =
      CreatePhoneNumberOrderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePhoneNumberOrderResponse'
            Prelude.<$> (x Data..?> "PhoneNumberOrder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePhoneNumberOrder where
  hashWithSalt _salt CreatePhoneNumberOrder' {..} =
    _salt
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` e164PhoneNumbers

instance Prelude.NFData CreatePhoneNumberOrder where
  rnf CreatePhoneNumberOrder' {..} =
    Prelude.rnf productType
      `Prelude.seq` Prelude.rnf e164PhoneNumbers

instance Data.ToHeaders CreatePhoneNumberOrder where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePhoneNumberOrder where
  toJSON CreatePhoneNumberOrder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProductType" Data..= productType),
            Prelude.Just
              ("E164PhoneNumbers" Data..= e164PhoneNumbers)
          ]
      )

instance Data.ToPath CreatePhoneNumberOrder where
  toPath = Prelude.const "/phone-number-orders"

instance Data.ToQuery CreatePhoneNumberOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePhoneNumberOrderResponse' smart constructor.
data CreatePhoneNumberOrderResponse = CreatePhoneNumberOrderResponse'
  { -- | The phone number order details.
    phoneNumberOrder :: Prelude.Maybe PhoneNumberOrder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePhoneNumberOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberOrder', 'createPhoneNumberOrderResponse_phoneNumberOrder' - The phone number order details.
--
-- 'httpStatus', 'createPhoneNumberOrderResponse_httpStatus' - The response's http status code.
newCreatePhoneNumberOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePhoneNumberOrderResponse
newCreatePhoneNumberOrderResponse pHttpStatus_ =
  CreatePhoneNumberOrderResponse'
    { phoneNumberOrder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The phone number order details.
createPhoneNumberOrderResponse_phoneNumberOrder :: Lens.Lens' CreatePhoneNumberOrderResponse (Prelude.Maybe PhoneNumberOrder)
createPhoneNumberOrderResponse_phoneNumberOrder = Lens.lens (\CreatePhoneNumberOrderResponse' {phoneNumberOrder} -> phoneNumberOrder) (\s@CreatePhoneNumberOrderResponse' {} a -> s {phoneNumberOrder = a} :: CreatePhoneNumberOrderResponse)

-- | The response's http status code.
createPhoneNumberOrderResponse_httpStatus :: Lens.Lens' CreatePhoneNumberOrderResponse Prelude.Int
createPhoneNumberOrderResponse_httpStatus = Lens.lens (\CreatePhoneNumberOrderResponse' {httpStatus} -> httpStatus) (\s@CreatePhoneNumberOrderResponse' {} a -> s {httpStatus = a} :: CreatePhoneNumberOrderResponse)

instance
  Prelude.NFData
    CreatePhoneNumberOrderResponse
  where
  rnf CreatePhoneNumberOrderResponse' {..} =
    Prelude.rnf phoneNumberOrder
      `Prelude.seq` Prelude.rnf httpStatus
