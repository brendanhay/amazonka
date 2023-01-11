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
-- Module      : Amazonka.Outposts.GetOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified order.
module Amazonka.Outposts.GetOrder
  ( -- * Creating a Request
    GetOrder (..),
    newGetOrder,

    -- * Request Lenses
    getOrder_orderId,

    -- * Destructuring the Response
    GetOrderResponse (..),
    newGetOrderResponse,

    -- * Response Lenses
    getOrderResponse_order,
    getOrderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOrder' smart constructor.
data GetOrder = GetOrder'
  { -- | The ID of the order.
    orderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderId', 'getOrder_orderId' - The ID of the order.
newGetOrder ::
  -- | 'orderId'
  Prelude.Text ->
  GetOrder
newGetOrder pOrderId_ =
  GetOrder' {orderId = pOrderId_}

-- | The ID of the order.
getOrder_orderId :: Lens.Lens' GetOrder Prelude.Text
getOrder_orderId = Lens.lens (\GetOrder' {orderId} -> orderId) (\s@GetOrder' {} a -> s {orderId = a} :: GetOrder)

instance Core.AWSRequest GetOrder where
  type AWSResponse GetOrder = GetOrderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrderResponse'
            Prelude.<$> (x Data..?> "Order")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOrder where
  hashWithSalt _salt GetOrder' {..} =
    _salt `Prelude.hashWithSalt` orderId

instance Prelude.NFData GetOrder where
  rnf GetOrder' {..} = Prelude.rnf orderId

instance Data.ToHeaders GetOrder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetOrder where
  toPath GetOrder' {..} =
    Prelude.mconcat ["/orders/", Data.toBS orderId]

instance Data.ToQuery GetOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOrderResponse' smart constructor.
data GetOrderResponse = GetOrderResponse'
  { order :: Prelude.Maybe Order,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'getOrderResponse_order' - Undocumented member.
--
-- 'httpStatus', 'getOrderResponse_httpStatus' - The response's http status code.
newGetOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOrderResponse
newGetOrderResponse pHttpStatus_ =
  GetOrderResponse'
    { order = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getOrderResponse_order :: Lens.Lens' GetOrderResponse (Prelude.Maybe Order)
getOrderResponse_order = Lens.lens (\GetOrderResponse' {order} -> order) (\s@GetOrderResponse' {} a -> s {order = a} :: GetOrderResponse)

-- | The response's http status code.
getOrderResponse_httpStatus :: Lens.Lens' GetOrderResponse Prelude.Int
getOrderResponse_httpStatus = Lens.lens (\GetOrderResponse' {httpStatus} -> httpStatus) (\s@GetOrderResponse' {} a -> s {httpStatus = a} :: GetOrderResponse)

instance Prelude.NFData GetOrderResponse where
  rnf GetOrderResponse' {..} =
    Prelude.rnf order
      `Prelude.seq` Prelude.rnf httpStatus
