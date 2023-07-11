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
-- Module      : Amazonka.ChimeSdkVoice.GetPhoneNumberOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetPhoneNumberOrder
  ( -- * Creating a Request
    GetPhoneNumberOrder (..),
    newGetPhoneNumberOrder,

    -- * Request Lenses
    getPhoneNumberOrder_phoneNumberOrderId,

    -- * Destructuring the Response
    GetPhoneNumberOrderResponse (..),
    newGetPhoneNumberOrderResponse,

    -- * Response Lenses
    getPhoneNumberOrderResponse_phoneNumberOrder,
    getPhoneNumberOrderResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPhoneNumberOrder' smart constructor.
data GetPhoneNumberOrder = GetPhoneNumberOrder'
  { phoneNumberOrderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPhoneNumberOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberOrderId', 'getPhoneNumberOrder_phoneNumberOrderId' - Undocumented member.
newGetPhoneNumberOrder ::
  -- | 'phoneNumberOrderId'
  Prelude.Text ->
  GetPhoneNumberOrder
newGetPhoneNumberOrder pPhoneNumberOrderId_ =
  GetPhoneNumberOrder'
    { phoneNumberOrderId =
        pPhoneNumberOrderId_
    }

-- | Undocumented member.
getPhoneNumberOrder_phoneNumberOrderId :: Lens.Lens' GetPhoneNumberOrder Prelude.Text
getPhoneNumberOrder_phoneNumberOrderId = Lens.lens (\GetPhoneNumberOrder' {phoneNumberOrderId} -> phoneNumberOrderId) (\s@GetPhoneNumberOrder' {} a -> s {phoneNumberOrderId = a} :: GetPhoneNumberOrder)

instance Core.AWSRequest GetPhoneNumberOrder where
  type
    AWSResponse GetPhoneNumberOrder =
      GetPhoneNumberOrderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPhoneNumberOrderResponse'
            Prelude.<$> (x Data..?> "PhoneNumberOrder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPhoneNumberOrder where
  hashWithSalt _salt GetPhoneNumberOrder' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberOrderId

instance Prelude.NFData GetPhoneNumberOrder where
  rnf GetPhoneNumberOrder' {..} =
    Prelude.rnf phoneNumberOrderId

instance Data.ToHeaders GetPhoneNumberOrder where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPhoneNumberOrder where
  toPath GetPhoneNumberOrder' {..} =
    Prelude.mconcat
      [ "/phone-number-orders/",
        Data.toBS phoneNumberOrderId
      ]

instance Data.ToQuery GetPhoneNumberOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPhoneNumberOrderResponse' smart constructor.
data GetPhoneNumberOrderResponse = GetPhoneNumberOrderResponse'
  { phoneNumberOrder :: Prelude.Maybe PhoneNumberOrder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPhoneNumberOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberOrder', 'getPhoneNumberOrderResponse_phoneNumberOrder' - Undocumented member.
--
-- 'httpStatus', 'getPhoneNumberOrderResponse_httpStatus' - The response's http status code.
newGetPhoneNumberOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPhoneNumberOrderResponse
newGetPhoneNumberOrderResponse pHttpStatus_ =
  GetPhoneNumberOrderResponse'
    { phoneNumberOrder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getPhoneNumberOrderResponse_phoneNumberOrder :: Lens.Lens' GetPhoneNumberOrderResponse (Prelude.Maybe PhoneNumberOrder)
getPhoneNumberOrderResponse_phoneNumberOrder = Lens.lens (\GetPhoneNumberOrderResponse' {phoneNumberOrder} -> phoneNumberOrder) (\s@GetPhoneNumberOrderResponse' {} a -> s {phoneNumberOrder = a} :: GetPhoneNumberOrderResponse)

-- | The response's http status code.
getPhoneNumberOrderResponse_httpStatus :: Lens.Lens' GetPhoneNumberOrderResponse Prelude.Int
getPhoneNumberOrderResponse_httpStatus = Lens.lens (\GetPhoneNumberOrderResponse' {httpStatus} -> httpStatus) (\s@GetPhoneNumberOrderResponse' {} a -> s {httpStatus = a} :: GetPhoneNumberOrderResponse)

instance Prelude.NFData GetPhoneNumberOrderResponse where
  rnf GetPhoneNumberOrderResponse' {..} =
    Prelude.rnf phoneNumberOrder
      `Prelude.seq` Prelude.rnf httpStatus
