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
-- Module      : Amazonka.DynamoDB.GetItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetItem@ operation returns a set of attributes for the item with
-- the given primary key. If there is no matching item, @GetItem@ does not
-- return any data and there will be no @Item@ element in the response.
--
-- @GetItem@ provides an eventually consistent read by default. If your
-- application requires a strongly consistent read, set @ConsistentRead@ to
-- @true@. Although a strongly consistent read might take more time than an
-- eventually consistent read, it always returns the last updated value.
module Amazonka.DynamoDB.GetItem
  ( -- * Creating a Request
    GetItem (..),
    newGetItem,

    -- * Request Lenses
    getItem_attributesToGet,
    getItem_consistentRead,
    getItem_expressionAttributeNames,
    getItem_projectionExpression,
    getItem_returnConsumedCapacity,
    getItem_tableName,
    getItem_key,

    -- * Destructuring the Response
    GetItemResponse (..),
    newGetItemResponse,

    -- * Response Lenses
    getItemResponse_consumedCapacity,
    getItemResponse_item,
    getItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetItem@ operation.
--
-- /See:/ 'newGetItem' smart constructor.
data GetItem = GetItem'
  { -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
    -- in the /Amazon DynamoDB Developer Guide/.
    attributesToGet :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Determines the read consistency model: If set to @true@, then the
    -- operation uses strongly consistent reads; otherwise, the operation uses
    -- eventually consistent reads.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | One or more substitution tokens for attribute names in an expression.
    -- The following are some use cases for using @ExpressionAttributeNames@:
    --
    -- -   To access an attribute whose name conflicts with a DynamoDB reserved
    --     word.
    --
    -- -   To create a placeholder for repeating occurrences of an attribute
    --     name in an expression.
    --
    -- -   To prevent special characters in an attribute name from being
    --     misinterpreted in an expression.
    --
    -- Use the __#__ character in an expression to dereference an attribute
    -- name. For example, consider the following attribute name:
    --
    -- -   @Percentile@
    --
    -- The name of this attribute conflicts with a reserved word, so it cannot
    -- be used directly in an expression. (For the complete list of reserved
    -- words, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    -- in the /Amazon DynamoDB Developer Guide/). To work around this, you
    -- could specify the following for @ExpressionAttributeNames@:
    --
    -- -   @{\"#P\":\"Percentile\"}@
    --
    -- You could then use this substitution in an expression, as in this
    -- example:
    --
    -- -   @#P = :val@
    --
    -- Tokens that begin with the __:__ character are /expression attribute
    -- values/, which are placeholders for the actual value at runtime.
    --
    -- For more information on expression attribute names, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A string that identifies one or more attributes to retrieve from the
    -- table. These attributes can include scalars, sets, or elements of a JSON
    -- document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes are returned.
    -- If any of the requested attributes are not found, they do not appear in
    -- the result.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | The name of the table containing the requested item.
    tableName :: Prelude.Text,
    -- | A map of attribute names to @AttributeValue@ objects, representing the
    -- primary key of the item to retrieve.
    --
    -- For the primary key, you must provide all of the attributes. For
    -- example, with a simple primary key, you only need to provide a value for
    -- the partition key. For a composite primary key, you must provide values
    -- for both the partition key and the sort key.
    key :: Prelude.HashMap Prelude.Text AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributesToGet', 'getItem_attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'consistentRead', 'getItem_consistentRead' - Determines the read consistency model: If set to @true@, then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
--
-- 'expressionAttributeNames', 'getItem_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'projectionExpression', 'getItem_projectionExpression' - A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes are returned.
-- If any of the requested attributes are not found, they do not appear in
-- the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'getItem_returnConsumedCapacity' - Undocumented member.
--
-- 'tableName', 'getItem_tableName' - The name of the table containing the requested item.
--
-- 'key', 'getItem_key' - A map of attribute names to @AttributeValue@ objects, representing the
-- primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
newGetItem ::
  -- | 'tableName'
  Prelude.Text ->
  GetItem
newGetItem pTableName_ =
  GetItem'
    { attributesToGet = Prelude.Nothing,
      consistentRead = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      projectionExpression = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      tableName = pTableName_,
      key = Prelude.mempty
    }

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
getItem_attributesToGet :: Lens.Lens' GetItem (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getItem_attributesToGet = Lens.lens (\GetItem' {attributesToGet} -> attributesToGet) (\s@GetItem' {} a -> s {attributesToGet = a} :: GetItem) Prelude.. Lens.mapping Lens.coerced

-- | Determines the read consistency model: If set to @true@, then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
getItem_consistentRead :: Lens.Lens' GetItem (Prelude.Maybe Prelude.Bool)
getItem_consistentRead = Lens.lens (\GetItem' {consistentRead} -> consistentRead) (\s@GetItem' {} a -> s {consistentRead = a} :: GetItem)

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
getItem_expressionAttributeNames :: Lens.Lens' GetItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getItem_expressionAttributeNames = Lens.lens (\GetItem' {expressionAttributeNames} -> expressionAttributeNames) (\s@GetItem' {} a -> s {expressionAttributeNames = a} :: GetItem) Prelude.. Lens.mapping Lens.coerced

-- | A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes are returned.
-- If any of the requested attributes are not found, they do not appear in
-- the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
getItem_projectionExpression :: Lens.Lens' GetItem (Prelude.Maybe Prelude.Text)
getItem_projectionExpression = Lens.lens (\GetItem' {projectionExpression} -> projectionExpression) (\s@GetItem' {} a -> s {projectionExpression = a} :: GetItem)

-- | Undocumented member.
getItem_returnConsumedCapacity :: Lens.Lens' GetItem (Prelude.Maybe ReturnConsumedCapacity)
getItem_returnConsumedCapacity = Lens.lens (\GetItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@GetItem' {} a -> s {returnConsumedCapacity = a} :: GetItem)

-- | The name of the table containing the requested item.
getItem_tableName :: Lens.Lens' GetItem Prelude.Text
getItem_tableName = Lens.lens (\GetItem' {tableName} -> tableName) (\s@GetItem' {} a -> s {tableName = a} :: GetItem)

-- | A map of attribute names to @AttributeValue@ objects, representing the
-- primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
getItem_key :: Lens.Lens' GetItem (Prelude.HashMap Prelude.Text AttributeValue)
getItem_key = Lens.lens (\GetItem' {key} -> key) (\s@GetItem' {} a -> s {key = a} :: GetItem) Prelude.. Lens.coerced

instance Core.AWSRequest GetItem where
  type AWSResponse GetItem = GetItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetItemResponse'
            Prelude.<$> (x Data..?> "ConsumedCapacity")
            Prelude.<*> (x Data..?> "Item" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetItem where
  hashWithSalt _salt GetItem' {..} =
    _salt `Prelude.hashWithSalt` attributesToGet
      `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` projectionExpression
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetItem where
  rnf GetItem' {..} =
    Prelude.rnf attributesToGet
      `Prelude.seq` Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf projectionExpression
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DynamoDB_20120810.GetItem" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetItem where
  toJSON GetItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributesToGet" Data..=)
              Prelude.<$> attributesToGet,
            ("ConsistentRead" Data..=)
              Prelude.<$> consistentRead,
            ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ProjectionExpression" Data..=)
              Prelude.<$> projectionExpression,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("Key" Data..= key)
          ]
      )

instance Data.ToPath GetItem where
  toPath = Prelude.const "/"

instance Data.ToQuery GetItem where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetItem@ operation.
--
-- /See:/ 'newGetItemResponse' smart constructor.
data GetItemResponse = GetItemResponse'
  { -- | The capacity units consumed by the @GetItem@ operation. The data
    -- returned includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | A map of attribute names to @AttributeValue@ objects, as specified by
    -- @ProjectionExpression@.
    item :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'getItemResponse_consumedCapacity' - The capacity units consumed by the @GetItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'item', 'getItemResponse_item' - A map of attribute names to @AttributeValue@ objects, as specified by
-- @ProjectionExpression@.
--
-- 'httpStatus', 'getItemResponse_httpStatus' - The response's http status code.
newGetItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetItemResponse
newGetItemResponse pHttpStatus_ =
  GetItemResponse'
    { consumedCapacity =
        Prelude.Nothing,
      item = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity units consumed by the @GetItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
-- in the /Amazon DynamoDB Developer Guide/.
getItemResponse_consumedCapacity :: Lens.Lens' GetItemResponse (Prelude.Maybe ConsumedCapacity)
getItemResponse_consumedCapacity = Lens.lens (\GetItemResponse' {consumedCapacity} -> consumedCapacity) (\s@GetItemResponse' {} a -> s {consumedCapacity = a} :: GetItemResponse)

-- | A map of attribute names to @AttributeValue@ objects, as specified by
-- @ProjectionExpression@.
getItemResponse_item :: Lens.Lens' GetItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
getItemResponse_item = Lens.lens (\GetItemResponse' {item} -> item) (\s@GetItemResponse' {} a -> s {item = a} :: GetItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getItemResponse_httpStatus :: Lens.Lens' GetItemResponse Prelude.Int
getItemResponse_httpStatus = Lens.lens (\GetItemResponse' {httpStatus} -> httpStatus) (\s@GetItemResponse' {} a -> s {httpStatus = a} :: GetItemResponse)

instance Prelude.NFData GetItemResponse where
  rnf GetItemResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf item
      `Prelude.seq` Prelude.rnf httpStatus
