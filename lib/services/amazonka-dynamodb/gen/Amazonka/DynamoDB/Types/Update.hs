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
-- Module      : Amazonka.DynamoDB.Types.Update
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Update where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a request to perform an @UpdateItem@ operation.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | A condition that must be satisfied in order for a conditional update to
    -- succeed.
    conditionExpression :: Prelude.Maybe Prelude.Text,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
    -- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
    -- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
    returnValuesOnConditionCheckFailure :: Prelude.Maybe ReturnValuesOnConditionCheckFailure,
    -- | The primary key of the item to be updated. Each element consists of an
    -- attribute name and a value for that attribute.
    key :: Prelude.HashMap Prelude.Text AttributeValue,
    -- | An expression that defines one or more attributes to be updated, the
    -- action to be performed on them, and new value(s) for them.
    updateExpression :: Prelude.Text,
    -- | Name of the table for the @UpdateItem@ request.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Update' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionExpression', 'update_conditionExpression' - A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- 'expressionAttributeNames', 'update_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- 'expressionAttributeValues', 'update_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- 'returnValuesOnConditionCheckFailure', 'update_returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  Update
newUpdate pUpdateExpression_ pTableName_ =
  Update'
    { conditionExpression = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      returnValuesOnConditionCheckFailure =
        Prelude.Nothing,
      key = Prelude.mempty,
      updateExpression = pUpdateExpression_,
      tableName = pTableName_
    }

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
update_conditionExpression :: Lens.Lens' Update (Prelude.Maybe Prelude.Text)
update_conditionExpression = Lens.lens (\Update' {conditionExpression} -> conditionExpression) (\s@Update' {} a -> s {conditionExpression = a} :: Update)

-- | One or more substitution tokens for attribute names in an expression.
update_expressionAttributeNames :: Lens.Lens' Update (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
update_expressionAttributeNames = Lens.lens (\Update' {expressionAttributeNames} -> expressionAttributeNames) (\s@Update' {} a -> s {expressionAttributeNames = a} :: Update) Prelude.. Lens.mapping Lens.coerced

-- | One or more values that can be substituted in an expression.
update_expressionAttributeValues :: Lens.Lens' Update (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
update_expressionAttributeValues = Lens.lens (\Update' {expressionAttributeValues} -> expressionAttributeValues) (\s@Update' {} a -> s {expressionAttributeValues = a} :: Update) Prelude.. Lens.mapping Lens.coerced

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if
-- the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@,
-- the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
update_returnValuesOnConditionCheckFailure :: Lens.Lens' Update (Prelude.Maybe ReturnValuesOnConditionCheckFailure)
update_returnValuesOnConditionCheckFailure = Lens.lens (\Update' {returnValuesOnConditionCheckFailure} -> returnValuesOnConditionCheckFailure) (\s@Update' {} a -> s {returnValuesOnConditionCheckFailure = a} :: Update)

-- | The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
update_key :: Lens.Lens' Update (Prelude.HashMap Prelude.Text AttributeValue)
update_key = Lens.lens (\Update' {key} -> key) (\s@Update' {} a -> s {key = a} :: Update) Prelude.. Lens.coerced

-- | An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new value(s) for them.
update_updateExpression :: Lens.Lens' Update Prelude.Text
update_updateExpression = Lens.lens (\Update' {updateExpression} -> updateExpression) (\s@Update' {} a -> s {updateExpression = a} :: Update)

-- | Name of the table for the @UpdateItem@ request.
update_tableName :: Lens.Lens' Update Prelude.Text
update_tableName = Lens.lens (\Update' {tableName} -> tableName) (\s@Update' {} a -> s {tableName = a} :: Update)

instance Prelude.Hashable Update where
  hashWithSalt _salt Update' {..} =
    _salt
      `Prelude.hashWithSalt` conditionExpression
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` returnValuesOnConditionCheckFailure
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` updateExpression
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Update where
  rnf Update' {..} =
    Prelude.rnf conditionExpression
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf expressionAttributeValues
      `Prelude.seq` Prelude.rnf returnValuesOnConditionCheckFailure
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf updateExpression
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToJSON Update where
  toJSON Update' {..} =
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
            Prelude.Just
              ("UpdateExpression" Data..= updateExpression),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
