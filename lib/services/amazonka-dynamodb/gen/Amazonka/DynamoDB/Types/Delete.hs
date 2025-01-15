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
-- Module      : Amazonka.DynamoDB.Types.Delete
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Delete where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a request to perform a @DeleteItem@ operation.
--
-- /See:/ 'newDelete' smart constructor.
data Delete = Delete'
  { -- | A condition that must be satisfied in order for a conditional delete to
    -- succeed.
    conditionExpression :: Prelude.Maybe Prelude.Text,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
    -- the valid values are: NONE and ALL_OLD.
    returnValuesOnConditionCheckFailure :: Prelude.Maybe ReturnValuesOnConditionCheckFailure,
    -- | The primary key of the item to be deleted. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Prelude.HashMap Prelude.Text AttributeValue,
    -- | Name of the table in which the item to be deleted resides.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Delete' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionExpression', 'delete_conditionExpression' - A condition that must be satisfied in order for a conditional delete to
-- succeed.
--
-- 'expressionAttributeNames', 'delete_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'expressionAttributeValues', 'delete_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'delete_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
--
-- 'key', 'delete_key' - The primary key of the item to be deleted. Each element consists of an
-- attribute name and a value for that attribute.
--
-- 'tableName', 'delete_tableName' - Name of the table in which the item to be deleted resides.
newDelete ::
  -- | 'tableName'
  Prelude.Text ->
  Delete
newDelete pTableName_ =
  Delete'
    { conditionExpression = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      returnValuesOnConditionCheckFailure =
        Prelude.Nothing,
      key = Prelude.mempty,
      tableName = pTableName_
    }

-- | A condition that must be satisfied in order for a conditional delete to
-- succeed.
delete_conditionExpression :: Lens.Lens' Delete (Prelude.Maybe Prelude.Text)
delete_conditionExpression = Lens.lens (\Delete' {conditionExpression} -> conditionExpression) (\s@Delete' {} a -> s {conditionExpression = a} :: Delete)

-- | One or more substitution tokens for attribute names in an expression.
delete_expressionAttributeNames :: Lens.Lens' Delete (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
delete_expressionAttributeNames = Lens.lens (\Delete' {expressionAttributeNames} -> expressionAttributeNames) (\s@Delete' {} a -> s {expressionAttributeNames = a} :: Delete) Prelude.. Lens.mapping Lens.coerced

-- | One or more values that can be substituted in an expression.
delete_expressionAttributeValues :: Lens.Lens' Delete (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
delete_expressionAttributeValues = Lens.lens (\Delete' {expressionAttributeValues} -> expressionAttributeValues) (\s@Delete' {} a -> s {expressionAttributeValues = a} :: Delete) Prelude.. Lens.mapping Lens.coerced

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
delete_returnValuesOnConditionCheckFailure :: Lens.Lens' Delete (Prelude.Maybe ReturnValuesOnConditionCheckFailure)
delete_returnValuesOnConditionCheckFailure = Lens.lens (\Delete' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@Delete' {} a -> s {returnValuesOnConditionCheckFailure = a} :: Delete)

-- | The primary key of the item to be deleted. Each element consists of an
-- attribute name and a value for that attribute.
delete_key :: Lens.Lens' Delete (Prelude.HashMap Prelude.Text AttributeValue)
delete_key = Lens.lens (\Delete' {key} -> key) (\s@Delete' {} a -> s {key = a} :: Delete) Prelude.. Lens.coerced

-- | Name of the table in which the item to be deleted resides.
delete_tableName :: Lens.Lens' Delete Prelude.Text
delete_tableName = Lens.lens (\Delete' {tableName} -> tableName) (\s@Delete' {} a -> s {tableName = a} :: Delete)

instance Prelude.Hashable Delete where
  hashWithSalt _salt Delete' {..} =
    _salt
      `Prelude.hashWithSalt` conditionExpression
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` returnValuesOnConditionCheckFailure
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Delete where
  rnf Delete' {..} =
    Prelude.rnf conditionExpression `Prelude.seq`
      Prelude.rnf expressionAttributeNames `Prelude.seq`
        Prelude.rnf expressionAttributeValues `Prelude.seq`
          Prelude.rnf returnValuesOnConditionCheckFailure `Prelude.seq`
            Prelude.rnf key `Prelude.seq`
              Prelude.rnf tableName

instance Data.ToJSON Delete where
  toJSON Delete' {..} =
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
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
