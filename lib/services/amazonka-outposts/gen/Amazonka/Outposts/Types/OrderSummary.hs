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
-- Module      : Amazonka.Outposts.Types.OrderSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.OrderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.LineItemStatus
import Amazonka.Outposts.Types.OrderStatus
import Amazonka.Outposts.Types.OrderType
import qualified Amazonka.Prelude as Prelude

-- | A summary of line items in your order.
--
-- /See:/ 'newOrderSummary' smart constructor.
data OrderSummary = OrderSummary'
  { -- | The status of all line items in the order.
    lineItemCountsByStatus :: Prelude.Maybe (Prelude.HashMap LineItemStatus Prelude.Natural),
    -- | The fulfilment date for the order.
    orderFulfilledDate :: Prelude.Maybe Data.POSIX,
    -- | The ID of the order.
    orderId :: Prelude.Maybe Prelude.Text,
    -- | The submission date for the order.
    orderSubmissionDate :: Prelude.Maybe Data.POSIX,
    -- | The type of order.
    orderType :: Prelude.Maybe OrderType,
    -- | The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | The status of the order.
    --
    -- -   @PREPARING@ - Order is received and is being prepared.
    --
    -- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
    --     For more information, see the @LineItem@ status.
    --
    -- -   @COMPLETED@ - Order is complete.
    --
    -- -   @CANCELLED@ - Order is cancelled.
    --
    -- -   @ERROR@ - Customer should contact support.
    --
    -- The following statuses are deprecated: @RECEIVED@, @PENDING@,
    -- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
    status :: Prelude.Maybe OrderStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineItemCountsByStatus', 'orderSummary_lineItemCountsByStatus' - The status of all line items in the order.
--
-- 'orderFulfilledDate', 'orderSummary_orderFulfilledDate' - The fulfilment date for the order.
--
-- 'orderId', 'orderSummary_orderId' - The ID of the order.
--
-- 'orderSubmissionDate', 'orderSummary_orderSubmissionDate' - The submission date for the order.
--
-- 'orderType', 'orderSummary_orderType' - The type of order.
--
-- 'outpostId', 'orderSummary_outpostId' - The ID of the Outpost.
--
-- 'status', 'orderSummary_status' - The status of the order.
--
-- -   @PREPARING@ - Order is received and is being prepared.
--
-- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
--     For more information, see the @LineItem@ status.
--
-- -   @COMPLETED@ - Order is complete.
--
-- -   @CANCELLED@ - Order is cancelled.
--
-- -   @ERROR@ - Customer should contact support.
--
-- The following statuses are deprecated: @RECEIVED@, @PENDING@,
-- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
newOrderSummary ::
  OrderSummary
newOrderSummary =
  OrderSummary'
    { lineItemCountsByStatus =
        Prelude.Nothing,
      orderFulfilledDate = Prelude.Nothing,
      orderId = Prelude.Nothing,
      orderSubmissionDate = Prelude.Nothing,
      orderType = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The status of all line items in the order.
orderSummary_lineItemCountsByStatus :: Lens.Lens' OrderSummary (Prelude.Maybe (Prelude.HashMap LineItemStatus Prelude.Natural))
orderSummary_lineItemCountsByStatus = Lens.lens (\OrderSummary' {lineItemCountsByStatus} -> lineItemCountsByStatus) (\s@OrderSummary' {} a -> s {lineItemCountsByStatus = a} :: OrderSummary) Prelude.. Lens.mapping Lens.coerced

-- | The fulfilment date for the order.
orderSummary_orderFulfilledDate :: Lens.Lens' OrderSummary (Prelude.Maybe Prelude.UTCTime)
orderSummary_orderFulfilledDate = Lens.lens (\OrderSummary' {orderFulfilledDate} -> orderFulfilledDate) (\s@OrderSummary' {} a -> s {orderFulfilledDate = a} :: OrderSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the order.
orderSummary_orderId :: Lens.Lens' OrderSummary (Prelude.Maybe Prelude.Text)
orderSummary_orderId = Lens.lens (\OrderSummary' {orderId} -> orderId) (\s@OrderSummary' {} a -> s {orderId = a} :: OrderSummary)

-- | The submission date for the order.
orderSummary_orderSubmissionDate :: Lens.Lens' OrderSummary (Prelude.Maybe Prelude.UTCTime)
orderSummary_orderSubmissionDate = Lens.lens (\OrderSummary' {orderSubmissionDate} -> orderSubmissionDate) (\s@OrderSummary' {} a -> s {orderSubmissionDate = a} :: OrderSummary) Prelude.. Lens.mapping Data._Time

-- | The type of order.
orderSummary_orderType :: Lens.Lens' OrderSummary (Prelude.Maybe OrderType)
orderSummary_orderType = Lens.lens (\OrderSummary' {orderType} -> orderType) (\s@OrderSummary' {} a -> s {orderType = a} :: OrderSummary)

-- | The ID of the Outpost.
orderSummary_outpostId :: Lens.Lens' OrderSummary (Prelude.Maybe Prelude.Text)
orderSummary_outpostId = Lens.lens (\OrderSummary' {outpostId} -> outpostId) (\s@OrderSummary' {} a -> s {outpostId = a} :: OrderSummary)

-- | The status of the order.
--
-- -   @PREPARING@ - Order is received and is being prepared.
--
-- -   @IN_PROGRESS@ - Order is either being built, shipped, or installed.
--     For more information, see the @LineItem@ status.
--
-- -   @COMPLETED@ - Order is complete.
--
-- -   @CANCELLED@ - Order is cancelled.
--
-- -   @ERROR@ - Customer should contact support.
--
-- The following statuses are deprecated: @RECEIVED@, @PENDING@,
-- @PROCESSING@, @INSTALLING@, and @FULFILLED@.
orderSummary_status :: Lens.Lens' OrderSummary (Prelude.Maybe OrderStatus)
orderSummary_status = Lens.lens (\OrderSummary' {status} -> status) (\s@OrderSummary' {} a -> s {status = a} :: OrderSummary)

instance Data.FromJSON OrderSummary where
  parseJSON =
    Data.withObject
      "OrderSummary"
      ( \x ->
          OrderSummary'
            Prelude.<$> ( x
                            Data..:? "LineItemCountsByStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OrderFulfilledDate")
            Prelude.<*> (x Data..:? "OrderId")
            Prelude.<*> (x Data..:? "OrderSubmissionDate")
            Prelude.<*> (x Data..:? "OrderType")
            Prelude.<*> (x Data..:? "OutpostId")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable OrderSummary where
  hashWithSalt _salt OrderSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lineItemCountsByStatus
      `Prelude.hashWithSalt` orderFulfilledDate
      `Prelude.hashWithSalt` orderId
      `Prelude.hashWithSalt` orderSubmissionDate
      `Prelude.hashWithSalt` orderType
      `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` status

instance Prelude.NFData OrderSummary where
  rnf OrderSummary' {..} =
    Prelude.rnf lineItemCountsByStatus
      `Prelude.seq` Prelude.rnf orderFulfilledDate
      `Prelude.seq` Prelude.rnf orderId
      `Prelude.seq` Prelude.rnf orderSubmissionDate
      `Prelude.seq` Prelude.rnf orderType
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf status
