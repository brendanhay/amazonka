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
-- Module      : Amazonka.DynamoDB.Types.KeysAndAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.KeysAndAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of primary keys and, for each key, the attributes to
-- retrieve from the table.
--
-- For each primary key, you must provide /all/ of the key attributes. For
-- example, with a simple primary key, you only need to provide the
-- partition key. For a composite primary key, you must provide /both/ the
-- partition key and the sort key.
--
-- /See:/ 'newKeysAndAttributes' smart constructor.
data KeysAndAttributes = KeysAndAttributes'
  { -- | The consistency of a read operation. If set to @true@, then a strongly
    -- consistent read is used; otherwise, an eventually consistent read is
    -- used.
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
    -- in the /Amazon DynamoDB Developer Guide/.
    attributesToGet :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A string that identifies one or more attributes to retrieve from the
    -- table. These attributes can include scalars, sets, or elements of a JSON
    -- document. The attributes in the @ProjectionExpression@ must be separated
    -- by commas.
    --
    -- If no attribute names are specified, then all attributes will be
    -- returned. If any of the requested attributes are not found, they will
    -- not appear in the result.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The primary key attribute values that define the items and the
    -- attributes associated with the items.
    keys :: Prelude.NonEmpty (Prelude.HashMap Prelude.Text AttributeValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeysAndAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistentRead', 'keysAndAttributes_consistentRead' - The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
--
-- 'expressionAttributeNames', 'keysAndAttributes_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'attributesToGet', 'keysAndAttributes_attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'projectionExpression', 'keysAndAttributes_projectionExpression' - A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the @ProjectionExpression@ must be separated
-- by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'keys', 'keysAndAttributes_keys' - The primary key attribute values that define the items and the
-- attributes associated with the items.
newKeysAndAttributes ::
  -- | 'keys'
  Prelude.NonEmpty (Prelude.HashMap Prelude.Text AttributeValue) ->
  KeysAndAttributes
newKeysAndAttributes pKeys_ =
  KeysAndAttributes'
    { consistentRead =
        Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      attributesToGet = Prelude.Nothing,
      projectionExpression = Prelude.Nothing,
      keys = Lens.coerced Lens.# pKeys_
    }

-- | The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
keysAndAttributes_consistentRead :: Lens.Lens' KeysAndAttributes (Prelude.Maybe Prelude.Bool)
keysAndAttributes_consistentRead = Lens.lens (\KeysAndAttributes' {consistentRead} -> consistentRead) (\s@KeysAndAttributes' {} a -> s {consistentRead = a} :: KeysAndAttributes)

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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
keysAndAttributes_expressionAttributeNames :: Lens.Lens' KeysAndAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
keysAndAttributes_expressionAttributeNames = Lens.lens (\KeysAndAttributes' {expressionAttributeNames} -> expressionAttributeNames) (\s@KeysAndAttributes' {} a -> s {expressionAttributeNames = a} :: KeysAndAttributes) Prelude.. Lens.mapping Lens.coerced

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
keysAndAttributes_attributesToGet :: Lens.Lens' KeysAndAttributes (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
keysAndAttributes_attributesToGet = Lens.lens (\KeysAndAttributes' {attributesToGet} -> attributesToGet) (\s@KeysAndAttributes' {} a -> s {attributesToGet = a} :: KeysAndAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the @ProjectionExpression@ must be separated
-- by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
keysAndAttributes_projectionExpression :: Lens.Lens' KeysAndAttributes (Prelude.Maybe Prelude.Text)
keysAndAttributes_projectionExpression = Lens.lens (\KeysAndAttributes' {projectionExpression} -> projectionExpression) (\s@KeysAndAttributes' {} a -> s {projectionExpression = a} :: KeysAndAttributes)

-- | The primary key attribute values that define the items and the
-- attributes associated with the items.
keysAndAttributes_keys :: Lens.Lens' KeysAndAttributes (Prelude.NonEmpty (Prelude.HashMap Prelude.Text AttributeValue))
keysAndAttributes_keys = Lens.lens (\KeysAndAttributes' {keys} -> keys) (\s@KeysAndAttributes' {} a -> s {keys = a} :: KeysAndAttributes) Prelude.. Lens.coerced

instance Core.FromJSON KeysAndAttributes where
  parseJSON =
    Core.withObject
      "KeysAndAttributes"
      ( \x ->
          KeysAndAttributes'
            Prelude.<$> (x Core..:? "ConsistentRead")
            Prelude.<*> ( x Core..:? "ExpressionAttributeNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AttributesToGet")
            Prelude.<*> (x Core..:? "ProjectionExpression")
            Prelude.<*> (x Core..: "Keys")
      )

instance Prelude.Hashable KeysAndAttributes where
  hashWithSalt _salt KeysAndAttributes' {..} =
    _salt `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` attributesToGet
      `Prelude.hashWithSalt` projectionExpression
      `Prelude.hashWithSalt` keys

instance Prelude.NFData KeysAndAttributes where
  rnf KeysAndAttributes' {..} =
    Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf attributesToGet
      `Prelude.seq` Prelude.rnf projectionExpression
      `Prelude.seq` Prelude.rnf keys

instance Core.ToJSON KeysAndAttributes where
  toJSON KeysAndAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConsistentRead" Core..=)
              Prelude.<$> consistentRead,
            ("ExpressionAttributeNames" Core..=)
              Prelude.<$> expressionAttributeNames,
            ("AttributesToGet" Core..=)
              Prelude.<$> attributesToGet,
            ("ProjectionExpression" Core..=)
              Prelude.<$> projectionExpression,
            Prelude.Just ("Keys" Core..= keys)
          ]
      )
