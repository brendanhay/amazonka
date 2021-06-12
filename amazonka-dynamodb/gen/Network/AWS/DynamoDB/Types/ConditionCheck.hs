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
-- Module      : Network.AWS.DynamoDB.Types.ConditionCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConditionCheck where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens

-- | Represents a request to perform a check that an item exists or to check
-- the condition of specific attributes of the item.
--
-- /See:/ 'newConditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @ConditionCheck@ condition fails. For
    -- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
    -- ALL_OLD.
    returnValuesOnConditionCheckFailure :: Core.Maybe ReturnValuesOnConditionCheckFailure,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The primary key of the item to be checked. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Core.HashMap Core.Text AttributeValue,
    -- | Name of the table for the check item request.
    tableName :: Core.Text,
    -- | A condition that must be satisfied in order for a conditional update to
    -- succeed.
    conditionExpression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConditionCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionAttributeValues', 'conditionCheck_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'conditionCheck_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @ConditionCheck@ condition fails. For
-- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
-- ALL_OLD.
--
-- 'expressionAttributeNames', 'conditionCheck_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'key', 'conditionCheck_key' - The primary key of the item to be checked. Each element consists of an
-- attribute name and a value for that attribute.
--
-- 'tableName', 'conditionCheck_tableName' - Name of the table for the check item request.
--
-- 'conditionExpression', 'conditionCheck_conditionExpression' - A condition that must be satisfied in order for a conditional update to
-- succeed.
newConditionCheck ::
  -- | 'tableName'
  Core.Text ->
  -- | 'conditionExpression'
  Core.Text ->
  ConditionCheck
newConditionCheck pTableName_ pConditionExpression_ =
  ConditionCheck'
    { expressionAttributeValues =
        Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      key = Core.mempty,
      tableName = pTableName_,
      conditionExpression = pConditionExpression_
    }

-- | One or more values that can be substituted in an expression.
conditionCheck_expressionAttributeValues :: Lens.Lens' ConditionCheck (Core.Maybe (Core.HashMap Core.Text AttributeValue))
conditionCheck_expressionAttributeValues = Lens.lens (\ConditionCheck' {expressionAttributeValues} -> expressionAttributeValues) (\s@ConditionCheck' {} a -> s {expressionAttributeValues = a} :: ConditionCheck) Core.. Lens.mapping Lens._Coerce

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @ConditionCheck@ condition fails. For
-- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
-- ALL_OLD.
conditionCheck_returnValuesOnConditionCheckFailure :: Lens.Lens' ConditionCheck (Core.Maybe ReturnValuesOnConditionCheckFailure)
conditionCheck_returnValuesOnConditionCheckFailure = Lens.lens (\ConditionCheck' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@ConditionCheck' {} a -> s {returnValuesOnConditionCheckFailure = a} :: ConditionCheck)

-- | One or more substitution tokens for attribute names in an expression.
conditionCheck_expressionAttributeNames :: Lens.Lens' ConditionCheck (Core.Maybe (Core.HashMap Core.Text Core.Text))
conditionCheck_expressionAttributeNames = Lens.lens (\ConditionCheck' {expressionAttributeNames} -> expressionAttributeNames) (\s@ConditionCheck' {} a -> s {expressionAttributeNames = a} :: ConditionCheck) Core.. Lens.mapping Lens._Coerce

-- | The primary key of the item to be checked. Each element consists of an
-- attribute name and a value for that attribute.
conditionCheck_key :: Lens.Lens' ConditionCheck (Core.HashMap Core.Text AttributeValue)
conditionCheck_key = Lens.lens (\ConditionCheck' {key} -> key) (\s@ConditionCheck' {} a -> s {key = a} :: ConditionCheck) Core.. Lens._Coerce

-- | Name of the table for the check item request.
conditionCheck_tableName :: Lens.Lens' ConditionCheck Core.Text
conditionCheck_tableName = Lens.lens (\ConditionCheck' {tableName} -> tableName) (\s@ConditionCheck' {} a -> s {tableName = a} :: ConditionCheck)

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
conditionCheck_conditionExpression :: Lens.Lens' ConditionCheck Core.Text
conditionCheck_conditionExpression = Lens.lens (\ConditionCheck' {conditionExpression} -> conditionExpression) (\s@ConditionCheck' {} a -> s {conditionExpression = a} :: ConditionCheck)

instance Core.Hashable ConditionCheck

instance Core.NFData ConditionCheck

instance Core.ToJSON ConditionCheck where
  toJSON ConditionCheck' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Core..=)
              Core.<$> returnValuesOnConditionCheckFailure,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            Core.Just ("Key" Core..= key),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("ConditionExpression" Core..= conditionExpression)
          ]
      )
