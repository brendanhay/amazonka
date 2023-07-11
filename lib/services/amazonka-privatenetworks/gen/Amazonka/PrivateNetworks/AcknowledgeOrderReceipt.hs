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
-- Module      : Amazonka.PrivateNetworks.AcknowledgeOrderReceipt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Acknowledges that the specified network order was received.
module Amazonka.PrivateNetworks.AcknowledgeOrderReceipt
  ( -- * Creating a Request
    AcknowledgeOrderReceipt (..),
    newAcknowledgeOrderReceipt,

    -- * Request Lenses
    acknowledgeOrderReceipt_orderArn,

    -- * Destructuring the Response
    AcknowledgeOrderReceiptResponse (..),
    newAcknowledgeOrderReceiptResponse,

    -- * Response Lenses
    acknowledgeOrderReceiptResponse_httpStatus,
    acknowledgeOrderReceiptResponse_order,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcknowledgeOrderReceipt' smart constructor.
data AcknowledgeOrderReceipt = AcknowledgeOrderReceipt'
  { -- | The Amazon Resource Name (ARN) of the order.
    orderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeOrderReceipt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderArn', 'acknowledgeOrderReceipt_orderArn' - The Amazon Resource Name (ARN) of the order.
newAcknowledgeOrderReceipt ::
  -- | 'orderArn'
  Prelude.Text ->
  AcknowledgeOrderReceipt
newAcknowledgeOrderReceipt pOrderArn_ =
  AcknowledgeOrderReceipt' {orderArn = pOrderArn_}

-- | The Amazon Resource Name (ARN) of the order.
acknowledgeOrderReceipt_orderArn :: Lens.Lens' AcknowledgeOrderReceipt Prelude.Text
acknowledgeOrderReceipt_orderArn = Lens.lens (\AcknowledgeOrderReceipt' {orderArn} -> orderArn) (\s@AcknowledgeOrderReceipt' {} a -> s {orderArn = a} :: AcknowledgeOrderReceipt)

instance Core.AWSRequest AcknowledgeOrderReceipt where
  type
    AWSResponse AcknowledgeOrderReceipt =
      AcknowledgeOrderReceiptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeOrderReceiptResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "order")
      )

instance Prelude.Hashable AcknowledgeOrderReceipt where
  hashWithSalt _salt AcknowledgeOrderReceipt' {..} =
    _salt `Prelude.hashWithSalt` orderArn

instance Prelude.NFData AcknowledgeOrderReceipt where
  rnf AcknowledgeOrderReceipt' {..} =
    Prelude.rnf orderArn

instance Data.ToHeaders AcknowledgeOrderReceipt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcknowledgeOrderReceipt where
  toJSON AcknowledgeOrderReceipt' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("orderArn" Data..= orderArn)]
      )

instance Data.ToPath AcknowledgeOrderReceipt where
  toPath = Prelude.const "/v1/orders/acknowledge"

instance Data.ToQuery AcknowledgeOrderReceipt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcknowledgeOrderReceiptResponse' smart constructor.
data AcknowledgeOrderReceiptResponse = AcknowledgeOrderReceiptResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the order.
    order :: Order
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeOrderReceiptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acknowledgeOrderReceiptResponse_httpStatus' - The response's http status code.
--
-- 'order', 'acknowledgeOrderReceiptResponse_order' - Information about the order.
newAcknowledgeOrderReceiptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'order'
  Order ->
  AcknowledgeOrderReceiptResponse
newAcknowledgeOrderReceiptResponse
  pHttpStatus_
  pOrder_ =
    AcknowledgeOrderReceiptResponse'
      { httpStatus =
          pHttpStatus_,
        order = pOrder_
      }

-- | The response's http status code.
acknowledgeOrderReceiptResponse_httpStatus :: Lens.Lens' AcknowledgeOrderReceiptResponse Prelude.Int
acknowledgeOrderReceiptResponse_httpStatus = Lens.lens (\AcknowledgeOrderReceiptResponse' {httpStatus} -> httpStatus) (\s@AcknowledgeOrderReceiptResponse' {} a -> s {httpStatus = a} :: AcknowledgeOrderReceiptResponse)

-- | Information about the order.
acknowledgeOrderReceiptResponse_order :: Lens.Lens' AcknowledgeOrderReceiptResponse Order
acknowledgeOrderReceiptResponse_order = Lens.lens (\AcknowledgeOrderReceiptResponse' {order} -> order) (\s@AcknowledgeOrderReceiptResponse' {} a -> s {order = a} :: AcknowledgeOrderReceiptResponse)

instance
  Prelude.NFData
    AcknowledgeOrderReceiptResponse
  where
  rnf AcknowledgeOrderReceiptResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf order
