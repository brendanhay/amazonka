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
-- Module      : Network.AWS.DynamoDB.Types.Update
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Update where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens

-- | Represents a request to perform an @UpdateItem@ operation.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
    -- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
    returnValuesOnConditionCheckFailure :: Core.Maybe ReturnValuesOnConditionCheckFailure,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A condition that must be satisfied in order for a conditional update to
    -- succeed.
    conditionExpression :: Core.Maybe Core.Text,
    -- | The primary key of the item to be updated. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Core.HashMap Core.Text AttributeValue,
    -- | An expression that defines one or more attributes to be updated, the
    -- action to be performed on them, and new value(s) for them.
    updateExpression :: Core.Text,
    -- | Name of the table for the @UpdateItem@ request.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Update' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionAttributeValues', 'update_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'update_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
--
-- 'expressionAttributeNames', 'update_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'conditionExpression', 'update_conditionExpression' - A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- 'key', 'update_key' - The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
--
-- 'updateExpression', 'update_updateExpression' - An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new value(s) for them.
--
-- 'tableName', 'update_tableName' - Name of the table for the @UpdateItem@ request.
newUpdate ::
  -- | 'updateExpression'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  Update
newUpdate pUpdateExpression_ pTableName_ =
  Update'
    { expressionAttributeValues = Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      conditionExpression = Core.Nothing,
      key = Core.mempty,
      updateExpression = pUpdateExpression_,
      tableName = pTableName_
    }

-- | One or more values that can be substituted in an expression.
update_expressionAttributeValues :: Lens.Lens' Update (Core.Maybe (Core.HashMap Core.Text AttributeValue))
update_expressionAttributeValues = Lens.lens (\Update' {expressionAttributeValues} -> expressionAttributeValues) (\s@Update' {} a -> s {expressionAttributeValues = a} :: Update) Core.. Lens.mapping Lens._Coerce

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
update_returnValuesOnConditionCheckFailure :: Lens.Lens' Update (Core.Maybe ReturnValuesOnConditionCheckFailure)
update_returnValuesOnConditionCheckFailure = Lens.lens (\Update' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@Update' {} a -> s {returnValuesOnConditionCheckFailure = a} :: Update)

-- | One or more substitution tokens for attribute names in an expression.
update_expressionAttributeNames :: Lens.Lens' Update (Core.Maybe (Core.HashMap Core.Text Core.Text))
update_expressionAttributeNames = Lens.lens (\Update' {expressionAttributeNames} -> expressionAttributeNames) (\s@Update' {} a -> s {expressionAttributeNames = a} :: Update) Core.. Lens.mapping Lens._Coerce

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
update_conditionExpression :: Lens.Lens' Update (Core.Maybe Core.Text)
update_conditionExpression = Lens.lens (\Update' {conditionExpression} -> conditionExpression) (\s@Update' {} a -> s {conditionExpression = a} :: Update)

-- | The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
update_key :: Lens.Lens' Update (Core.HashMap Core.Text AttributeValue)
update_key = Lens.lens (\Update' {key} -> key) (\s@Update' {} a -> s {key = a} :: Update) Core.. Lens._Coerce

-- | An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new value(s) for them.
update_updateExpression :: Lens.Lens' Update Core.Text
update_updateExpression = Lens.lens (\Update' {updateExpression} -> updateExpression) (\s@Update' {} a -> s {updateExpression = a} :: Update)

-- | Name of the table for the @UpdateItem@ request.
update_tableName :: Lens.Lens' Update Core.Text
update_tableName = Lens.lens (\Update' {tableName} -> tableName) (\s@Update' {} a -> s {tableName = a} :: Update)

instance Core.Hashable Update

instance Core.NFData Update

instance Core.ToJSON Update where
  toJSON Update' {..} =
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
            Core.Just
              ("UpdateExpression" Core..= updateExpression),
            Core.Just ("TableName" Core..= tableName)
          ]
      )
