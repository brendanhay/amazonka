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
-- Module      : Amazonka.Outposts.CancelOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified order for an Outpost.
module Amazonka.Outposts.CancelOrder
  ( -- * Creating a Request
    CancelOrder (..),
    newCancelOrder,

    -- * Request Lenses
    cancelOrder_orderId,

    -- * Destructuring the Response
    CancelOrderResponse (..),
    newCancelOrderResponse,

    -- * Response Lenses
    cancelOrderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelOrder' smart constructor.
data CancelOrder = CancelOrder'
  { -- | The ID of the order.
    orderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderId', 'cancelOrder_orderId' - The ID of the order.
newCancelOrder ::
  -- | 'orderId'
  Prelude.Text ->
  CancelOrder
newCancelOrder pOrderId_ =
  CancelOrder' {orderId = pOrderId_}

-- | The ID of the order.
cancelOrder_orderId :: Lens.Lens' CancelOrder Prelude.Text
cancelOrder_orderId = Lens.lens (\CancelOrder' {orderId} -> orderId) (\s@CancelOrder' {} a -> s {orderId = a} :: CancelOrder)

instance Core.AWSRequest CancelOrder where
  type AWSResponse CancelOrder = CancelOrderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelOrderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelOrder where
  hashWithSalt _salt CancelOrder' {..} =
    _salt `Prelude.hashWithSalt` orderId

instance Prelude.NFData CancelOrder where
  rnf CancelOrder' {..} = Prelude.rnf orderId

instance Data.ToHeaders CancelOrder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelOrder where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelOrder where
  toPath CancelOrder' {..} =
    Prelude.mconcat
      ["/orders/", Data.toBS orderId, "/cancel"]

instance Data.ToQuery CancelOrder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelOrderResponse' smart constructor.
data CancelOrderResponse = CancelOrderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelOrderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelOrderResponse_httpStatus' - The response's http status code.
newCancelOrderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelOrderResponse
newCancelOrderResponse pHttpStatus_ =
  CancelOrderResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelOrderResponse_httpStatus :: Lens.Lens' CancelOrderResponse Prelude.Int
cancelOrderResponse_httpStatus = Lens.lens (\CancelOrderResponse' {httpStatus} -> httpStatus) (\s@CancelOrderResponse' {} a -> s {httpStatus = a} :: CancelOrderResponse)

instance Prelude.NFData CancelOrderResponse where
  rnf CancelOrderResponse' {..} = Prelude.rnf httpStatus
