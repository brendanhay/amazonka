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
-- Module      : Amazonka.DynamoDB.Types.Put
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Put where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a request to perform a @PutItem@ operation.
--
-- /See:/ 'newPut' smart constructor.
data Put = Put'
  { -- | A condition that must be satisfied in order for a conditional update to
    -- succeed.
    conditionExpression :: Prelude.Maybe Prelude.Text,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
    -- the valid values are: NONE and ALL_OLD.
    returnValuesOnConditionCheckFailure :: Prelude.Maybe ReturnValuesOnConditionCheckFailure,
    -- | A map of attribute name to attribute values, representing the primary
    -- key of the item to be written by @PutItem@. All of the table\'s primary
    -- key attributes must be specified, and their data types must match those
    -- of the table\'s key schema. If any attributes are present in the item
    -- that are part of an index key schema for the table, their types must
    -- match the index key schema.
    item :: Prelude.HashMap Prelude.Text AttributeValue,
    -- | Name of the table in which to write the item.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Put' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionExpression', 'put_conditionExpression' - A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- 'expressionAttributeNames', 'put_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'expressionAttributeValues', 'put_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'put_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
--
-- 'item', 'put_item' - A map of attribute name to attribute values, representing the primary
-- key of the item to be written by @PutItem@. All of the table\'s primary
-- key attributes must be specified, and their data types must match those
-- of the table\'s key schema. If any attributes are present in the item
-- that are part of an index key schema for the table, their types must
-- match the index key schema.
--
-- 'tableName', 'put_tableName' - Name of the table in which to write the item.
newPut ::
  -- | 'tableName'
  Prelude.Text ->
  Put
newPut pTableName_ =
  Put'
    { conditionExpression = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      returnValuesOnConditionCheckFailure =
        Prelude.Nothing,
      item = Prelude.mempty,
      tableName = pTableName_
    }

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
put_conditionExpression :: Lens.Lens' Put (Prelude.Maybe Prelude.Text)
put_conditionExpression = Lens.lens (\Put' {conditionExpression} -> conditionExpression) (\s@Put' {} a -> s {conditionExpression = a} :: Put)

-- | One or more substitution tokens for attribute names in an expression.
put_expressionAttributeNames :: Lens.Lens' Put (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
put_expressionAttributeNames = Lens.lens (\Put' {expressionAttributeNames} -> expressionAttributeNames) (\s@Put' {} a -> s {expressionAttributeNames = a} :: Put) Prelude.. Lens.mapping Lens.coerced

-- | One or more values that can be substituted in an expression.
put_expressionAttributeValues :: Lens.Lens' Put (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
put_expressionAttributeValues = Lens.lens (\Put' {expressionAttributeValues} -> expressionAttributeValues) (\s@Put' {} a -> s {expressionAttributeValues = a} :: Put) Prelude.. Lens.mapping Lens.coerced

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
put_returnValuesOnConditionCheckFailure :: Lens.Lens' Put (Prelude.Maybe ReturnValuesOnConditionCheckFailure)
put_returnValuesOnConditionCheckFailure = Lens.lens (\Put' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@Put' {} a -> s {returnValuesOnConditionCheckFailure = a} :: Put)

-- | A map of attribute name to attribute values, representing the primary
-- key of the item to be written by @PutItem@. All of the table\'s primary
-- key attributes must be specified, and their data types must match those
-- of the table\'s key schema. If any attributes are present in the item
-- that are part of an index key schema for the table, their types must
-- match the index key schema.
put_item :: Lens.Lens' Put (Prelude.HashMap Prelude.Text AttributeValue)
put_item = Lens.lens (\Put' {item} -> item) (\s@Put' {} a -> s {item = a} :: Put) Prelude.. Lens.coerced

-- | Name of the table in which to write the item.
put_tableName :: Lens.Lens' Put Prelude.Text
put_tableName = Lens.lens (\Put' {tableName} -> tableName) (\s@Put' {} a -> s {tableName = a} :: Put)

instance Prelude.Hashable Put where
  hashWithSalt _salt Put' {..} =
    _salt
      `Prelude.hashWithSalt` conditionExpression
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` returnValuesOnConditionCheckFailure
      `Prelude.hashWithSalt` item
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Put where
  rnf Put' {..} =
    Prelude.rnf conditionExpression `Prelude.seq`
      Prelude.rnf expressionAttributeNames `Prelude.seq`
        Prelude.rnf expressionAttributeValues `Prelude.seq`
          Prelude.rnf returnValuesOnConditionCheckFailure `Prelude.seq`
            Prelude.rnf item `Prelude.seq`
              Prelude.rnf tableName

instance Data.ToJSON Put where
  toJSON Put' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionExpression" Data..=)
              Prelude.<$> conditionExpression,
            ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Data..=)
              Prelude.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Data..=)
              Prelude.<$> returnValuesOnConditionCheckFailure,
            Prelude.Just ("Item" Data..= item),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
