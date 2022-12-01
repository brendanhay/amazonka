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
-- Module      : Amazonka.DynamoDB.Types.ConditionCheck
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ConditionCheck where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a request to perform a check that an item exists or to check
-- the condition of specific attributes of the item.
--
-- /See:/ 'newConditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @ConditionCheck@ condition fails. For
    -- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
    -- ALL_OLD.
    returnValuesOnConditionCheckFailure :: Prelude.Maybe ReturnValuesOnConditionCheckFailure,
    -- | The primary key of the item to be checked. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Prelude.HashMap Prelude.Text AttributeValue,
    -- | Name of the table for the check item request.
    tableName :: Prelude.Text,
    -- | A condition that must be satisfied in order for a conditional update to
    -- succeed.
    conditionExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'expressionAttributeNames', 'conditionCheck_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'conditionCheck_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @ConditionCheck@ condition fails. For
-- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
-- ALL_OLD.
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
  Prelude.Text ->
  -- | 'conditionExpression'
  Prelude.Text ->
  ConditionCheck
newConditionCheck pTableName_ pConditionExpression_ =
  ConditionCheck'
    { expressionAttributeValues =
        Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      returnValuesOnConditionCheckFailure =
        Prelude.Nothing,
      key = Prelude.mempty,
      tableName = pTableName_,
      conditionExpression = pConditionExpression_
    }

-- | One or more values that can be substituted in an expression.
conditionCheck_expressionAttributeValues :: Lens.Lens' ConditionCheck (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
conditionCheck_expressionAttributeValues = Lens.lens (\ConditionCheck' {expressionAttributeValues} -> expressionAttributeValues) (\s@ConditionCheck' {} a -> s {expressionAttributeValues = a} :: ConditionCheck) Prelude.. Lens.mapping Lens.coerced

-- | One or more substitution tokens for attribute names in an expression.
conditionCheck_expressionAttributeNames :: Lens.Lens' ConditionCheck (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
conditionCheck_expressionAttributeNames = Lens.lens (\ConditionCheck' {expressionAttributeNames} -> expressionAttributeNames) (\s@ConditionCheck' {} a -> s {expressionAttributeNames = a} :: ConditionCheck) Prelude.. Lens.mapping Lens.coerced

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @ConditionCheck@ condition fails. For
-- @ReturnValuesOnConditionCheckFailure@, the valid values are: NONE and
-- ALL_OLD.
conditionCheck_returnValuesOnConditionCheckFailure :: Lens.Lens' ConditionCheck (Prelude.Maybe ReturnValuesOnConditionCheckFailure)
conditionCheck_returnValuesOnConditionCheckFailure = Lens.lens (\ConditionCheck' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@ConditionCheck' {} a -> s {returnValuesOnConditionCheckFailure = a} :: ConditionCheck)

-- | The primary key of the item to be checked. Each element consists of an
-- attribute name and a value for that attribute.
conditionCheck_key :: Lens.Lens' ConditionCheck (Prelude.HashMap Prelude.Text AttributeValue)
conditionCheck_key = Lens.lens (\ConditionCheck' {key} -> key) (\s@ConditionCheck' {} a -> s {key = a} :: ConditionCheck) Prelude.. Lens.coerced

-- | Name of the table for the check item request.
conditionCheck_tableName :: Lens.Lens' ConditionCheck Prelude.Text
conditionCheck_tableName = Lens.lens (\ConditionCheck' {tableName} -> tableName) (\s@ConditionCheck' {} a -> s {tableName = a} :: ConditionCheck)

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
conditionCheck_conditionExpression :: Lens.Lens' ConditionCheck Prelude.Text
conditionCheck_conditionExpression = Lens.lens (\ConditionCheck' {conditionExpression} -> conditionExpression) (\s@ConditionCheck' {} a -> s {conditionExpression = a} :: ConditionCheck)

instance Prelude.Hashable ConditionCheck where
  hashWithSalt _salt ConditionCheck' {..} =
    _salt
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` returnValuesOnConditionCheckFailure
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` conditionExpression

instance Prelude.NFData ConditionCheck where
  rnf ConditionCheck' {..} =
    Prelude.rnf expressionAttributeValues
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf returnValuesOnConditionCheckFailure
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf conditionExpression

instance Core.ToJSON ConditionCheck where
  toJSON ConditionCheck' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExpressionAttributeValues" Core..=)
              Prelude.<$> expressionAttributeValues,
            ("ExpressionAttributeNames" Core..=)
              Prelude.<$> expressionAttributeNames,
            ("ReturnValuesOnConditionCheckFailure" Core..=)
              Prelude.<$> returnValuesOnConditionCheckFailure,
            Prelude.Just ("Key" Core..= key),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just
              ("ConditionExpression" Core..= conditionExpression)
          ]
      )
