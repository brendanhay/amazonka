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
-- Module      : Network.AWS.DynamoDB.Types.Delete
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Delete where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens

-- | Represents a request to perform a @DeleteItem@ operation.
--
-- /See:/ 'newDelete' smart constructor.
data Delete = Delete'
  { -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
    -- the valid values are: NONE and ALL_OLD.
    returnValuesOnConditionCheckFailure :: Core.Maybe ReturnValuesOnConditionCheckFailure,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A condition that must be satisfied in order for a conditional delete to
    -- succeed.
    conditionExpression :: Core.Maybe Core.Text,
    -- | The primary key of the item to be deleted. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Core.HashMap Core.Text AttributeValue,
    -- | Name of the table in which the item to be deleted resides.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Delete' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionAttributeValues', 'delete_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'delete_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
--
-- 'expressionAttributeNames', 'delete_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'conditionExpression', 'delete_conditionExpression' - A condition that must be satisfied in order for a conditional delete to
-- succeed.
--
-- 'key', 'delete_key' - The primary key of the item to be deleted. Each element consists of an
-- attribute name and a value for that attribute.
--
-- 'tableName', 'delete_tableName' - Name of the table in which the item to be deleted resides.
newDelete ::
  -- | 'tableName'
  Core.Text ->
  Delete
newDelete pTableName_ =
  Delete'
    { expressionAttributeValues = Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      conditionExpression = Core.Nothing,
      key = Core.mempty,
      tableName = pTableName_
    }

-- | One or more values that can be substituted in an expression.
delete_expressionAttributeValues :: Lens.Lens' Delete (Core.Maybe (Core.HashMap Core.Text AttributeValue))
delete_expressionAttributeValues = Lens.lens (\Delete' {expressionAttributeValues} -> expressionAttributeValues) (\s@Delete' {} a -> s {expressionAttributeValues = a} :: Delete) Core.. Lens.mapping Lens._Coerce

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE and ALL_OLD.
delete_returnValuesOnConditionCheckFailure :: Lens.Lens' Delete (Core.Maybe ReturnValuesOnConditionCheckFailure)
delete_returnValuesOnConditionCheckFailure = Lens.lens (\Delete' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@Delete' {} a -> s {returnValuesOnConditionCheckFailure = a} :: Delete)

-- | One or more substitution tokens for attribute names in an expression.
delete_expressionAttributeNames :: Lens.Lens' Delete (Core.Maybe (Core.HashMap Core.Text Core.Text))
delete_expressionAttributeNames = Lens.lens (\Delete' {expressionAttributeNames} -> expressionAttributeNames) (\s@Delete' {} a -> s {expressionAttributeNames = a} :: Delete) Core.. Lens.mapping Lens._Coerce

-- | A condition that must be satisfied in order for a conditional delete to
-- succeed.
delete_conditionExpression :: Lens.Lens' Delete (Core.Maybe Core.Text)
delete_conditionExpression = Lens.lens (\Delete' {conditionExpression} -> conditionExpression) (\s@Delete' {} a -> s {conditionExpression = a} :: Delete)

-- | The primary key of the item to be deleted. Each element consists of an
-- attribute name and a value for that attribute.
delete_key :: Lens.Lens' Delete (Core.HashMap Core.Text AttributeValue)
delete_key = Lens.lens (\Delete' {key} -> key) (\s@Delete' {} a -> s {key = a} :: Delete) Core.. Lens._Coerce

-- | Name of the table in which the item to be deleted resides.
delete_tableName :: Lens.Lens' Delete Core.Text
delete_tableName = Lens.lens (\Delete' {tableName} -> tableName) (\s@Delete' {} a -> s {tableName = a} :: Delete)

instance Core.Hashable Delete

instance Core.NFData Delete

instance Core.ToJSON Delete where
  toJSON Delete' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Core..=)
              Core.<$> returnValuesOnConditionCheckFailure,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ConditionExpression" Core..=)
              Core.<$> conditionExpression,
            Core.Just ("Key" Core..= key),
            Core.Just ("TableName" Core..= tableName)
          ]
      )
