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
-- Module      : Amazonka.Outposts.CreateOrder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an order for an Outpost.
module Amazonka.Outposts.CreateOrder
  ( -- * Creating a Request
    CreateOrder (..),
    newCreateOrder,

    -- * Request Lenses
    createOrder_paymentTerm,
    createOrder_outpostIdentifier,
    createOrder_lineItems,
    createOrder_paymentOption,

    -- * Destructuring the Response
    CreateOrderResponse (..),
    newCreateOrderResponse,

    -- * Response Lenses
    createOrderResponse_order,
    createOrderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOrder' smart constructor.
data CreateOrder = CreateOrder'
  { -- | The payment terms.
    paymentTerm :: Prelude.Maybe PaymentTerm,
    -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostIdentifier :: Prelude.Text,
    -- | The line items that make up the order.
    lineItems :: Prelude.NonEmpty LineItemRequest,
    -- | The payment option.
    paymentOption :: PaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paymentTerm', 'createOrder_paymentTerm' - The payment terms.
--
-- 'outpostIdentifier', 'createOrder_outpostIdentifier' - The ID or the Amazon Resource Name (ARN) of the Outpost.
--
-- 'lineItems', 'createOrder_lineItems' - The line items that make up the order.
--
-- 'paymentOption', 'createOrder_paymentOption' - The payment option.
newCreateOrder ::
  -- | 'outpostIdentifier'
  Prelude.Text ->
  -- | 'lineItems'
  Prelude.NonEmpty LineItemRequest ->
  -- | 'paymentOption'
  PaymentOption ->
  CreateOrder
newCreateOrder
  pOutpostIdentifier_
  pLineItems_
  pPaymentOption_ =
    CreateOrder'
      { paymentTerm = Prelude.Nothing,
        outpostIdentifier = pOutpostIdentifier_,
        lineItems = Lens.coerced Lens.# pLineItems_,
        paymentOption = pPaymentOption_
      }

-- | The payment terms.
createOrder_paymentTerm :: Lens.Lens' CreateOrder (Prelude.Maybe PaymentTerm)
createOrder_paymentTerm = Lens.lens (\CreateOrder' {paymentTerm} -> paymentTerm) (\s@CreateOrder' {} a -> s {paymentTerm = a} :: CreateOrder)

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
createOrder_outpostIdentifier :: Lens.Lens' CreateOrder Prelude.Text
createOrder_outpostIdentifier = Lens.lens (\CreateOrder' {outpostIdentifier} -> outpostIdentifier) (\s@CreateOrder' {} a -> s {outpostIdentifier = a} :: CreateOrder)

-- | The line items that make up the order.
createOrder_lineItems :: Lens.Lens' CreateOrder (Prelude.NonEmpty LineItemRequest)
createOrder_lineItems = Lens.lens (\CreateOrder' {lineItems} -> lineItems) (\s@CreateOrder' {} a -> s {lineItems = a} :: CreateOrder) Prelude.. Lens.coerced

-- | The payment option.
createOrder_paymentOption :: Lens.Lens' CreateOrder PaymentOption
createOrder_paymentOption = Lens.lens (\CreateOrder' {paymentOption} -> paymentOption) (\s@CreateOrder' {} a -> s {paymentOption = a} :: CreateOrder)

instance Core.AWSRequest CreateOrder where
  type AWSResponse CreateOrder = CreateOrderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrderResponse'
            Prelude.<$> (x Core..?> "Order")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOrder where
  hashWithSalt _salt CreateOrder' {..} =
    _salt `Prelude.hashWithSalt` paymentTerm
      `Prelude.hashWithSalt` outpostIdentifier
      `Prelude.hashWithSalt` lineItems
      `Prelude.hashWithSalt` paymentOption

instance Prelude.NFData CreateOrder where
  rnf CreateOrder' {..} =
    Prelude.rnf paymentTerm
      `Prelude.seq` Prelude.rnf outpostIdentifier
      `Prelude.seq` Prelude.rnf lineItems
      `Prelude.seq` Prelude.rnf paymentOption

instance Core.ToHeaders CreateOrder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOrder where
  toJSON CreateOrder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PaymentTerm" Core..=) Prelude.<$> paymentTerm,
            Prelude.Just
              ("OutpostIdentifier" Core..= outpostIdentifier),
            Prelude.Just ("LineItems" Core..= lineItems),
            Prelude.Just
              ("PaymentOption" Core..= paymentOption)
          ]
      )

instance Core.ToPath CreateOrder where
  toPath = Prelude.const "/orders"

instance Core.ToQuery CreateOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOrderResponse' smart constructor.
data CreateOrderResponse = CreateOrderResponse'
  { -- | Information about this order.
    order :: Prelude.Maybe Order,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'createOrderResponse_order' - Information about this order.
--
-- 'httpStatus', 'createOrderResponse_httpStatus' - The response's http status code.
newCreateOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOrderResponse
newCreateOrderResponse pHttpStatus_ =
  CreateOrderResponse'
    { order = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about this order.
createOrderResponse_order :: Lens.Lens' CreateOrderResponse (Prelude.Maybe Order)
createOrderResponse_order = Lens.lens (\CreateOrderResponse' {order} -> order) (\s@CreateOrderResponse' {} a -> s {order = a} :: CreateOrderResponse)

-- | The response's http status code.
createOrderResponse_httpStatus :: Lens.Lens' CreateOrderResponse Prelude.Int
createOrderResponse_httpStatus = Lens.lens (\CreateOrderResponse' {httpStatus} -> httpStatus) (\s@CreateOrderResponse' {} a -> s {httpStatus = a} :: CreateOrderResponse)

instance Prelude.NFData CreateOrderResponse where
  rnf CreateOrderResponse' {..} =
    Prelude.rnf order
      `Prelude.seq` Prelude.rnf httpStatus
