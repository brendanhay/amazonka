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
-- Module      : Network.AWS.DynamoDB.TransactGetItems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactGetItems@ is a synchronous operation that atomically retrieves
-- multiple items from one or more tables (but not from indexes) in a
-- single account and Region. A @TransactGetItems@ call can contain up to
-- 25 @TransactGetItem@ objects, each of which contains a @Get@ structure
-- that specifies an item to retrieve from a table in the account and
-- Region. A call to @TransactGetItems@ cannot retrieve items from tables
-- in more than one AWS account or Region. The aggregate size of the items
-- in the transaction cannot exceed 4 MB.
--
-- DynamoDB rejects the entire @TransactGetItems@ request if any of the
-- following is true:
--
-- -   A conflicting operation is in the process of updating an item to be
--     read.
--
-- -   There is insufficient provisioned capacity for the transaction to be
--     completed.
--
-- -   There is a user error, such as an invalid data format.
--
-- -   The aggregate size of the items in the transaction cannot exceed 4
--     MB.
module Network.AWS.DynamoDB.TransactGetItems
  ( -- * Creating a Request
    TransactGetItems (..),
    newTransactGetItems,

    -- * Request Lenses
    transactGetItems_returnConsumedCapacity,
    transactGetItems_transactItems,

    -- * Destructuring the Response
    TransactGetItemsResponse (..),
    newTransactGetItemsResponse,

    -- * Response Lenses
    transactGetItemsResponse_consumedCapacity,
    transactGetItemsResponse_responses,
    transactGetItemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTransactGetItems' smart constructor.
data TransactGetItems = TransactGetItems'
  { -- | A value of @TOTAL@ causes consumed capacity information to be returned,
    -- and a value of @NONE@ prevents that information from being returned. No
    -- other value is valid.
    returnConsumedCapacity :: Core.Maybe ReturnConsumedCapacity,
    -- | An ordered array of up to 25 @TransactGetItem@ objects, each of which
    -- contains a @Get@ structure.
    transactItems :: Core.NonEmpty TransactGetItem
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransactGetItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnConsumedCapacity', 'transactGetItems_returnConsumedCapacity' - A value of @TOTAL@ causes consumed capacity information to be returned,
-- and a value of @NONE@ prevents that information from being returned. No
-- other value is valid.
--
-- 'transactItems', 'transactGetItems_transactItems' - An ordered array of up to 25 @TransactGetItem@ objects, each of which
-- contains a @Get@ structure.
newTransactGetItems ::
  -- | 'transactItems'
  Core.NonEmpty TransactGetItem ->
  TransactGetItems
newTransactGetItems pTransactItems_ =
  TransactGetItems'
    { returnConsumedCapacity =
        Core.Nothing,
      transactItems = Lens._Coerce Lens.# pTransactItems_
    }

-- | A value of @TOTAL@ causes consumed capacity information to be returned,
-- and a value of @NONE@ prevents that information from being returned. No
-- other value is valid.
transactGetItems_returnConsumedCapacity :: Lens.Lens' TransactGetItems (Core.Maybe ReturnConsumedCapacity)
transactGetItems_returnConsumedCapacity = Lens.lens (\TransactGetItems' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@TransactGetItems' {} a -> s {returnConsumedCapacity = a} :: TransactGetItems)

-- | An ordered array of up to 25 @TransactGetItem@ objects, each of which
-- contains a @Get@ structure.
transactGetItems_transactItems :: Lens.Lens' TransactGetItems (Core.NonEmpty TransactGetItem)
transactGetItems_transactItems = Lens.lens (\TransactGetItems' {transactItems} -> transactItems) (\s@TransactGetItems' {} a -> s {transactItems = a} :: TransactGetItems) Core.. Lens._Coerce

instance Core.AWSRequest TransactGetItems where
  type
    AWSResponse TransactGetItems =
      TransactGetItemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TransactGetItemsResponse'
            Core.<$> (x Core..?> "ConsumedCapacity" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Responses")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TransactGetItems

instance Core.NFData TransactGetItems

instance Core.ToHeaders TransactGetItems where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.TransactGetItems" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TransactGetItems where
  toJSON TransactGetItems' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReturnConsumedCapacity" Core..=)
              Core.<$> returnConsumedCapacity,
            Core.Just ("TransactItems" Core..= transactItems)
          ]
      )

instance Core.ToPath TransactGetItems where
  toPath = Core.const "/"

instance Core.ToQuery TransactGetItems where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTransactGetItemsResponse' smart constructor.
data TransactGetItemsResponse = TransactGetItemsResponse'
  { -- | If the /ReturnConsumedCapacity/ value was @TOTAL@, this is an array of
    -- @ConsumedCapacity@ objects, one for each table addressed by
    -- @TransactGetItem@ objects in the /TransactItems/ parameter. These
    -- @ConsumedCapacity@ objects report the read-capacity units consumed by
    -- the @TransactGetItems@ call in that table.
    consumedCapacity :: Core.Maybe [ConsumedCapacity],
    -- | An ordered array of up to 25 @ItemResponse@ objects, each of which
    -- corresponds to the @TransactGetItem@ object in the same position in the
    -- /TransactItems/ array. Each @ItemResponse@ object contains a Map of the
    -- name-value pairs that are the projected attributes of the requested
    -- item.
    --
    -- If a requested item could not be retrieved, the corresponding
    -- @ItemResponse@ object is Null, or if the requested item has no projected
    -- attributes, the corresponding @ItemResponse@ object is an empty Map.
    responses :: Core.Maybe (Core.NonEmpty ItemResponse),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransactGetItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'transactGetItemsResponse_consumedCapacity' - If the /ReturnConsumedCapacity/ value was @TOTAL@, this is an array of
-- @ConsumedCapacity@ objects, one for each table addressed by
-- @TransactGetItem@ objects in the /TransactItems/ parameter. These
-- @ConsumedCapacity@ objects report the read-capacity units consumed by
-- the @TransactGetItems@ call in that table.
--
-- 'responses', 'transactGetItemsResponse_responses' - An ordered array of up to 25 @ItemResponse@ objects, each of which
-- corresponds to the @TransactGetItem@ object in the same position in the
-- /TransactItems/ array. Each @ItemResponse@ object contains a Map of the
-- name-value pairs that are the projected attributes of the requested
-- item.
--
-- If a requested item could not be retrieved, the corresponding
-- @ItemResponse@ object is Null, or if the requested item has no projected
-- attributes, the corresponding @ItemResponse@ object is an empty Map.
--
-- 'httpStatus', 'transactGetItemsResponse_httpStatus' - The response's http status code.
newTransactGetItemsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TransactGetItemsResponse
newTransactGetItemsResponse pHttpStatus_ =
  TransactGetItemsResponse'
    { consumedCapacity =
        Core.Nothing,
      responses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the /ReturnConsumedCapacity/ value was @TOTAL@, this is an array of
-- @ConsumedCapacity@ objects, one for each table addressed by
-- @TransactGetItem@ objects in the /TransactItems/ parameter. These
-- @ConsumedCapacity@ objects report the read-capacity units consumed by
-- the @TransactGetItems@ call in that table.
transactGetItemsResponse_consumedCapacity :: Lens.Lens' TransactGetItemsResponse (Core.Maybe [ConsumedCapacity])
transactGetItemsResponse_consumedCapacity = Lens.lens (\TransactGetItemsResponse' {consumedCapacity} -> consumedCapacity) (\s@TransactGetItemsResponse' {} a -> s {consumedCapacity = a} :: TransactGetItemsResponse) Core.. Lens.mapping Lens._Coerce

-- | An ordered array of up to 25 @ItemResponse@ objects, each of which
-- corresponds to the @TransactGetItem@ object in the same position in the
-- /TransactItems/ array. Each @ItemResponse@ object contains a Map of the
-- name-value pairs that are the projected attributes of the requested
-- item.
--
-- If a requested item could not be retrieved, the corresponding
-- @ItemResponse@ object is Null, or if the requested item has no projected
-- attributes, the corresponding @ItemResponse@ object is an empty Map.
transactGetItemsResponse_responses :: Lens.Lens' TransactGetItemsResponse (Core.Maybe (Core.NonEmpty ItemResponse))
transactGetItemsResponse_responses = Lens.lens (\TransactGetItemsResponse' {responses} -> responses) (\s@TransactGetItemsResponse' {} a -> s {responses = a} :: TransactGetItemsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
transactGetItemsResponse_httpStatus :: Lens.Lens' TransactGetItemsResponse Core.Int
transactGetItemsResponse_httpStatus = Lens.lens (\TransactGetItemsResponse' {httpStatus} -> httpStatus) (\s@TransactGetItemsResponse' {} a -> s {httpStatus = a} :: TransactGetItemsResponse)

instance Core.NFData TransactGetItemsResponse
