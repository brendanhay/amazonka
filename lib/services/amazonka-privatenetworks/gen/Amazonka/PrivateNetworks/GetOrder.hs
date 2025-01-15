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
-- Module      : Amazonka.PrivateNetworks.GetOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified order.
module Amazonka.PrivateNetworks.GetOrder
  ( -- * Creating a Request
    GetOrder (..),
    newGetOrder,

    -- * Request Lenses
    getOrder_orderArn,

    -- * Destructuring the Response
    GetOrderResponse (..),
    newGetOrderResponse,

    -- * Response Lenses
    getOrderResponse_tags,
    getOrderResponse_httpStatus,
    getOrderResponse_order,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOrder' smart constructor.
data GetOrder = GetOrder'
  { -- | The Amazon Resource Name (ARN) of the order.
    orderArn :: Prelude.Text
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
-- 'orderArn', 'getOrder_orderArn' - The Amazon Resource Name (ARN) of the order.
newGetOrder ::
  -- | 'orderArn'
  Prelude.Text ->
  GetOrder
newGetOrder pOrderArn_ =
  GetOrder' {orderArn = pOrderArn_}

-- | The Amazon Resource Name (ARN) of the order.
getOrder_orderArn :: Lens.Lens' GetOrder Prelude.Text
getOrder_orderArn = Lens.lens (\GetOrder' {orderArn} -> orderArn) (\s@GetOrder' {} a -> s {orderArn = a} :: GetOrder)

instance Core.AWSRequest GetOrder where
  type AWSResponse GetOrder = GetOrderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrderResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "order")
      )

instance Prelude.Hashable GetOrder where
  hashWithSalt _salt GetOrder' {..} =
    _salt `Prelude.hashWithSalt` orderArn

instance Prelude.NFData GetOrder where
  rnf GetOrder' {..} = Prelude.rnf orderArn

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
    Prelude.mconcat ["/v1/orders/", Data.toBS orderArn]

instance Data.ToQuery GetOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOrderResponse' smart constructor.
data GetOrderResponse = GetOrderResponse'
  { -- | The order tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the order.
    order :: Order
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getOrderResponse_tags' - The order tags.
--
-- 'httpStatus', 'getOrderResponse_httpStatus' - The response's http status code.
--
-- 'order', 'getOrderResponse_order' - Information about the order.
newGetOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'order'
  Order ->
  GetOrderResponse
newGetOrderResponse pHttpStatus_ pOrder_ =
  GetOrderResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      order = pOrder_
    }

-- | The order tags.
getOrderResponse_tags :: Lens.Lens' GetOrderResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getOrderResponse_tags = Lens.lens (\GetOrderResponse' {tags} -> tags) (\s@GetOrderResponse' {} a -> s {tags = a} :: GetOrderResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getOrderResponse_httpStatus :: Lens.Lens' GetOrderResponse Prelude.Int
getOrderResponse_httpStatus = Lens.lens (\GetOrderResponse' {httpStatus} -> httpStatus) (\s@GetOrderResponse' {} a -> s {httpStatus = a} :: GetOrderResponse)

-- | Information about the order.
getOrderResponse_order :: Lens.Lens' GetOrderResponse Order
getOrderResponse_order = Lens.lens (\GetOrderResponse' {order} -> order) (\s@GetOrderResponse' {} a -> s {order = a} :: GetOrderResponse)

instance Prelude.NFData GetOrderResponse where
  rnf GetOrderResponse' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf order
