{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Types.Order
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Order where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.OrderStatus
import Amazonka.Outposts.Types.PaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Information about an order.
--
-- /See:/ 'newOrder' smart constructor.
data Order = Order'
  { -- | The ID of the Outpost in the order.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | The fulfillment date of the order.
    orderFulfilledDate :: Prelude.Maybe Data.POSIX,
    -- | The line items for the order
    lineItems :: Prelude.Maybe [LineItem],
    -- | The status of the order.
    --
    -- -   @PREPARING@ - Order is received and being prepared.
    --
    -- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
    --     To get more details, see the line item status.
    --
    -- -   @COMPLETED@ - Order is complete.
    --
    -- -   @CANCELLED@ - Order is cancelled.
    --
    -- -   @ERROR@ - Customer should contact support.
    --
    -- The following status are deprecated: @RECEIVED@, @PENDING@,
    -- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
    status :: Prelude.Maybe OrderStatus,
    -- | The ID of the order.
    orderId :: Prelude.Maybe Prelude.Text,
    -- | The submission date for the order.
    orderSubmissionDate :: Prelude.Maybe Data.POSIX,
    -- | The payment option for the order.
    paymentOption :: Prelude.Maybe PaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Order' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostId', 'order_outpostId' - The ID of the Outpost in the order.
--
-- 'orderFulfilledDate', 'order_orderFulfilledDate' - The fulfillment date of the order.
--
-- 'lineItems', 'order_lineItems' - The line items for the order
--
-- 'status', 'order_status' - The status of the order.
--
-- -   @PREPARING@ - Order is received and being prepared.
--
-- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
--     To get more details, see the line item status.
--
-- -   @COMPLETED@ - Order is complete.
--
-- -   @CANCELLED@ - Order is cancelled.
--
-- -   @ERROR@ - Customer should contact support.
--
-- The following status are deprecated: @RECEIVED@, @PENDING@,
-- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
--
-- 'orderId', 'order_orderId' - The ID of the order.
--
-- 'orderSubmissionDate', 'order_orderSubmissionDate' - The submission date for the order.
--
-- 'paymentOption', 'order_paymentOption' - The payment option for the order.
newOrder ::
  Order
newOrder =
  Order'
    { outpostId = Prelude.Nothing,
      orderFulfilledDate = Prelude.Nothing,
      lineItems = Prelude.Nothing,
      status = Prelude.Nothing,
      orderId = Prelude.Nothing,
      orderSubmissionDate = Prelude.Nothing,
      paymentOption = Prelude.Nothing
    }

-- | The ID of the Outpost in the order.
order_outpostId :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_outpostId = Lens.lens (\Order' {outpostId} -> outpostId) (\s@Order' {} a -> s {outpostId = a} :: Order)

-- | The fulfillment date of the order.
order_orderFulfilledDate :: Lens.Lens' Order (Prelude.Maybe Prelude.UTCTime)
order_orderFulfilledDate = Lens.lens (\Order' {orderFulfilledDate} -> orderFulfilledDate) (\s@Order' {} a -> s {orderFulfilledDate = a} :: Order) Prelude.. Lens.mapping Data._Time

-- | The line items for the order
order_lineItems :: Lens.Lens' Order (Prelude.Maybe [LineItem])
order_lineItems = Lens.lens (\Order' {lineItems} -> lineItems) (\s@Order' {} a -> s {lineItems = a} :: Order) Prelude.. Lens.mapping Lens.coerced

-- | The status of the order.
--
-- -   @PREPARING@ - Order is received and being prepared.
--
-- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
--     To get more details, see the line item status.
--
-- -   @COMPLETED@ - Order is complete.
--
-- -   @CANCELLED@ - Order is cancelled.
--
-- -   @ERROR@ - Customer should contact support.
--
-- The following status are deprecated: @RECEIVED@, @PENDING@,
-- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
order_status :: Lens.Lens' Order (Prelude.Maybe OrderStatus)
order_status = Lens.lens (\Order' {status} -> status) (\s@Order' {} a -> s {status = a} :: Order)

-- | The ID of the order.
order_orderId :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_orderId = Lens.lens (\Order' {orderId} -> orderId) (\s@Order' {} a -> s {orderId = a} :: Order)

-- | The submission date for the order.
order_orderSubmissionDate :: Lens.Lens' Order (Prelude.Maybe Prelude.UTCTime)
order_orderSubmissionDate = Lens.lens (\Order' {orderSubmissionDate} -> orderSubmissionDate) (\s@Order' {} a -> s {orderSubmissionDate = a} :: Order) Prelude.. Lens.mapping Data._Time

-- | The payment option for the order.
order_paymentOption :: Lens.Lens' Order (Prelude.Maybe PaymentOption)
order_paymentOption = Lens.lens (\Order' {paymentOption} -> paymentOption) (\s@Order' {} a -> s {paymentOption = a} :: Order)

instance Data.FromJSON Order where
  parseJSON =
    Data.withObject
      "Order"
      ( \x ->
          Order'
            Prelude.<$> (x Data..:? "OutpostId")
            Prelude.<*> (x Data..:? "OrderFulfilledDate")
            Prelude.<*> (x Data..:? "LineItems" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "OrderId")
            Prelude.<*> (x Data..:? "OrderSubmissionDate")
            Prelude.<*> (x Data..:? "PaymentOption")
      )

instance Prelude.Hashable Order where
  hashWithSalt _salt Order' {..} =
    _salt `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` orderFulfilledDate
      `Prelude.hashWithSalt` lineItems
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` orderId
      `Prelude.hashWithSalt` orderSubmissionDate
      `Prelude.hashWithSalt` paymentOption

instance Prelude.NFData Order where
  rnf Order' {..} =
    Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf orderFulfilledDate
      `Prelude.seq` Prelude.rnf lineItems
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf orderId
      `Prelude.seq` Prelude.rnf orderSubmissionDate
      `Prelude.seq` Prelude.rnf paymentOption
