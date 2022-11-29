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
-- Module      : Amazonka.DynamoDB.TransactWriteItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactWriteItems@ is a synchronous write operation that groups up to
-- 100 action requests. These actions can target items in different tables,
-- but not in different Amazon Web Services accounts or Regions, and no two
-- actions can target the same item. For example, you cannot both
-- @ConditionCheck@ and @Update@ the same item. The aggregate size of the
-- items in the transaction cannot exceed 4 MB.
--
-- The actions are completed atomically so that either all of them succeed,
-- or all of them fail. They are defined by the following objects:
--
-- -   @Put@  —   Initiates a @PutItem@ operation to write a new item. This
--     structure specifies the primary key of the item to be written, the
--     name of the table to write it in, an optional condition expression
--     that must be satisfied for the write to succeed, a list of the
--     item\'s attributes, and a field indicating whether to retrieve the
--     item\'s attributes if the condition is not met.
--
-- -   @Update@  —   Initiates an @UpdateItem@ operation to update an
--     existing item. This structure specifies the primary key of the item
--     to be updated, the name of the table where it resides, an optional
--     condition expression that must be satisfied for the update to
--     succeed, an expression that defines one or more attributes to be
--     updated, and a field indicating whether to retrieve the item\'s
--     attributes if the condition is not met.
--
-- -   @Delete@  —   Initiates a @DeleteItem@ operation to delete an
--     existing item. This structure specifies the primary key of the item
--     to be deleted, the name of the table where it resides, an optional
--     condition expression that must be satisfied for the deletion to
--     succeed, and a field indicating whether to retrieve the item\'s
--     attributes if the condition is not met.
--
-- -   @ConditionCheck@  —   Applies a condition to an item that is not
--     being modified by the transaction. This structure specifies the
--     primary key of the item to be checked, the name of the table where
--     it resides, a condition expression that must be satisfied for the
--     transaction to succeed, and a field indicating whether to retrieve
--     the item\'s attributes if the condition is not met.
--
-- DynamoDB rejects the entire @TransactWriteItems@ request if any of the
-- following is true:
--
-- -   A condition in one of the condition expressions is not met.
--
-- -   An ongoing operation is in the process of updating the same item.
--
-- -   There is insufficient provisioned capacity for the transaction to be
--     completed.
--
-- -   An item size becomes too large (bigger than 400 KB), a local
--     secondary index (LSI) becomes too large, or a similar validation
--     error occurs because of changes made by the transaction.
--
-- -   The aggregate size of the items in the transaction exceeds 4 MB.
--
-- -   There is a user error, such as an invalid data format.
module Amazonka.DynamoDB.TransactWriteItems
  ( -- * Creating a Request
    TransactWriteItems (..),
    newTransactWriteItems,

    -- * Request Lenses
    transactWriteItems_clientRequestToken,
    transactWriteItems_returnConsumedCapacity,
    transactWriteItems_returnItemCollectionMetrics,
    transactWriteItems_transactItems,

    -- * Destructuring the Response
    TransactWriteItemsResponse (..),
    newTransactWriteItemsResponse,

    -- * Response Lenses
    transactWriteItemsResponse_consumedCapacity,
    transactWriteItemsResponse_itemCollectionMetrics,
    transactWriteItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTransactWriteItems' smart constructor.
data TransactWriteItems = TransactWriteItems'
  { -- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@
    -- idempotent, meaning that multiple identical calls have the same effect
    -- as one single call.
    --
    -- Although multiple identical calls using the same client request token
    -- produce the same result on the server (no side effects), the responses
    -- to the calls might not be the same. If the @ReturnConsumedCapacity>@
    -- parameter is set, then the initial @TransactWriteItems@ call returns the
    -- amount of write capacity units consumed in making the changes.
    -- Subsequent @TransactWriteItems@ calls with the same client token return
    -- the number of read capacity units consumed in reading the item.
    --
    -- A client request token is valid for 10 minutes after the first request
    -- that uses it is completed. After 10 minutes, any request with the same
    -- client token is treated as a new request. Do not resubmit the same
    -- request with the same client token for more than 10 minutes, or the
    -- result might not be idempotent.
    --
    -- If you submit a request with the same client token but a change in other
    -- parameters within the 10-minute idempotency window, DynamoDB returns an
    -- @IdempotentParameterMismatch@ exception.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to
    -- @SIZE@, the response includes statistics about item collections (if
    -- any), that were modified during the operation and are returned in the
    -- response. If set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Prelude.Maybe ReturnItemCollectionMetrics,
    -- | An ordered array of up to 100 @TransactWriteItem@ objects, each of which
    -- contains a @ConditionCheck@, @Put@, @Update@, or @Delete@ object. These
    -- can operate on items in different tables, but the tables must reside in
    -- the same Amazon Web Services account and Region, and no two of them can
    -- operate on the same item.
    transactItems :: Prelude.NonEmpty TransactWriteItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransactWriteItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'transactWriteItems_clientRequestToken' - Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@
-- idempotent, meaning that multiple identical calls have the same effect
-- as one single call.
--
-- Although multiple identical calls using the same client request token
-- produce the same result on the server (no side effects), the responses
-- to the calls might not be the same. If the @ReturnConsumedCapacity>@
-- parameter is set, then the initial @TransactWriteItems@ call returns the
-- amount of write capacity units consumed in making the changes.
-- Subsequent @TransactWriteItems@ calls with the same client token return
-- the number of read capacity units consumed in reading the item.
--
-- A client request token is valid for 10 minutes after the first request
-- that uses it is completed. After 10 minutes, any request with the same
-- client token is treated as a new request. Do not resubmit the same
-- request with the same client token for more than 10 minutes, or the
-- result might not be idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 10-minute idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
--
-- 'returnConsumedCapacity', 'transactWriteItems_returnConsumedCapacity' - Undocumented member.
--
-- 'returnItemCollectionMetrics', 'transactWriteItems_returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections (if
-- any), that were modified during the operation and are returned in the
-- response. If set to @NONE@ (the default), no statistics are returned.
--
-- 'transactItems', 'transactWriteItems_transactItems' - An ordered array of up to 100 @TransactWriteItem@ objects, each of which
-- contains a @ConditionCheck@, @Put@, @Update@, or @Delete@ object. These
-- can operate on items in different tables, but the tables must reside in
-- the same Amazon Web Services account and Region, and no two of them can
-- operate on the same item.
newTransactWriteItems ::
  -- | 'transactItems'
  Prelude.NonEmpty TransactWriteItem ->
  TransactWriteItems
newTransactWriteItems pTransactItems_ =
  TransactWriteItems'
    { clientRequestToken =
        Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      returnItemCollectionMetrics = Prelude.Nothing,
      transactItems = Lens.coerced Lens.# pTransactItems_
    }

-- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@
-- idempotent, meaning that multiple identical calls have the same effect
-- as one single call.
--
-- Although multiple identical calls using the same client request token
-- produce the same result on the server (no side effects), the responses
-- to the calls might not be the same. If the @ReturnConsumedCapacity>@
-- parameter is set, then the initial @TransactWriteItems@ call returns the
-- amount of write capacity units consumed in making the changes.
-- Subsequent @TransactWriteItems@ calls with the same client token return
-- the number of read capacity units consumed in reading the item.
--
-- A client request token is valid for 10 minutes after the first request
-- that uses it is completed. After 10 minutes, any request with the same
-- client token is treated as a new request. Do not resubmit the same
-- request with the same client token for more than 10 minutes, or the
-- result might not be idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 10-minute idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
transactWriteItems_clientRequestToken :: Lens.Lens' TransactWriteItems (Prelude.Maybe Prelude.Text)
transactWriteItems_clientRequestToken = Lens.lens (\TransactWriteItems' {clientRequestToken} -> clientRequestToken) (\s@TransactWriteItems' {} a -> s {clientRequestToken = a} :: TransactWriteItems)

-- | Undocumented member.
transactWriteItems_returnConsumedCapacity :: Lens.Lens' TransactWriteItems (Prelude.Maybe ReturnConsumedCapacity)
transactWriteItems_returnConsumedCapacity = Lens.lens (\TransactWriteItems' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@TransactWriteItems' {} a -> s {returnConsumedCapacity = a} :: TransactWriteItems)

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections (if
-- any), that were modified during the operation and are returned in the
-- response. If set to @NONE@ (the default), no statistics are returned.
transactWriteItems_returnItemCollectionMetrics :: Lens.Lens' TransactWriteItems (Prelude.Maybe ReturnItemCollectionMetrics)
transactWriteItems_returnItemCollectionMetrics = Lens.lens (\TransactWriteItems' {returnItemCollectionMetrics} -> returnItemCollectionMetrics) (\s@TransactWriteItems' {} a -> s {returnItemCollectionMetrics = a} :: TransactWriteItems)

-- | An ordered array of up to 100 @TransactWriteItem@ objects, each of which
-- contains a @ConditionCheck@, @Put@, @Update@, or @Delete@ object. These
-- can operate on items in different tables, but the tables must reside in
-- the same Amazon Web Services account and Region, and no two of them can
-- operate on the same item.
transactWriteItems_transactItems :: Lens.Lens' TransactWriteItems (Prelude.NonEmpty TransactWriteItem)
transactWriteItems_transactItems = Lens.lens (\TransactWriteItems' {transactItems} -> transactItems) (\s@TransactWriteItems' {} a -> s {transactItems = a} :: TransactWriteItems) Prelude.. Lens.coerced

instance Core.AWSRequest TransactWriteItems where
  type
    AWSResponse TransactWriteItems =
      TransactWriteItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TransactWriteItemsResponse'
            Prelude.<$> ( x Core..?> "ConsumedCapacity"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "ItemCollectionMetrics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransactWriteItems where
  hashWithSalt _salt TransactWriteItems' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` returnItemCollectionMetrics
      `Prelude.hashWithSalt` transactItems

instance Prelude.NFData TransactWriteItems where
  rnf TransactWriteItems' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf returnItemCollectionMetrics
      `Prelude.seq` Prelude.rnf transactItems

instance Core.ToHeaders TransactWriteItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.TransactWriteItems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TransactWriteItems where
  toJSON TransactWriteItems' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("ReturnConsumedCapacity" Core..=)
              Prelude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Core..=)
              Prelude.<$> returnItemCollectionMetrics,
            Prelude.Just
              ("TransactItems" Core..= transactItems)
          ]
      )

instance Core.ToPath TransactWriteItems where
  toPath = Prelude.const "/"

instance Core.ToQuery TransactWriteItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTransactWriteItemsResponse' smart constructor.
data TransactWriteItemsResponse = TransactWriteItemsResponse'
  { -- | The capacity units consumed by the entire @TransactWriteItems@
    -- operation. The values of the list are ordered according to the ordering
    -- of the @TransactItems@ request parameter.
    consumedCapacity :: Prelude.Maybe [ConsumedCapacity],
    -- | A list of tables that were processed by @TransactWriteItems@ and, for
    -- each table, information about any item collections that were affected by
    -- individual @UpdateItem@, @PutItem@, or @DeleteItem@ operations.
    itemCollectionMetrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ItemCollectionMetrics]),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransactWriteItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'transactWriteItemsResponse_consumedCapacity' - The capacity units consumed by the entire @TransactWriteItems@
-- operation. The values of the list are ordered according to the ordering
-- of the @TransactItems@ request parameter.
--
-- 'itemCollectionMetrics', 'transactWriteItemsResponse_itemCollectionMetrics' - A list of tables that were processed by @TransactWriteItems@ and, for
-- each table, information about any item collections that were affected by
-- individual @UpdateItem@, @PutItem@, or @DeleteItem@ operations.
--
-- 'httpStatus', 'transactWriteItemsResponse_httpStatus' - The response's http status code.
newTransactWriteItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TransactWriteItemsResponse
newTransactWriteItemsResponse pHttpStatus_ =
  TransactWriteItemsResponse'
    { consumedCapacity =
        Prelude.Nothing,
      itemCollectionMetrics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity units consumed by the entire @TransactWriteItems@
-- operation. The values of the list are ordered according to the ordering
-- of the @TransactItems@ request parameter.
transactWriteItemsResponse_consumedCapacity :: Lens.Lens' TransactWriteItemsResponse (Prelude.Maybe [ConsumedCapacity])
transactWriteItemsResponse_consumedCapacity = Lens.lens (\TransactWriteItemsResponse' {consumedCapacity} -> consumedCapacity) (\s@TransactWriteItemsResponse' {} a -> s {consumedCapacity = a} :: TransactWriteItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tables that were processed by @TransactWriteItems@ and, for
-- each table, information about any item collections that were affected by
-- individual @UpdateItem@, @PutItem@, or @DeleteItem@ operations.
transactWriteItemsResponse_itemCollectionMetrics :: Lens.Lens' TransactWriteItemsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ItemCollectionMetrics]))
transactWriteItemsResponse_itemCollectionMetrics = Lens.lens (\TransactWriteItemsResponse' {itemCollectionMetrics} -> itemCollectionMetrics) (\s@TransactWriteItemsResponse' {} a -> s {itemCollectionMetrics = a} :: TransactWriteItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
transactWriteItemsResponse_httpStatus :: Lens.Lens' TransactWriteItemsResponse Prelude.Int
transactWriteItemsResponse_httpStatus = Lens.lens (\TransactWriteItemsResponse' {httpStatus} -> httpStatus) (\s@TransactWriteItemsResponse' {} a -> s {httpStatus = a} :: TransactWriteItemsResponse)

instance Prelude.NFData TransactWriteItemsResponse where
  rnf TransactWriteItemsResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf itemCollectionMetrics
      `Prelude.seq` Prelude.rnf httpStatus
