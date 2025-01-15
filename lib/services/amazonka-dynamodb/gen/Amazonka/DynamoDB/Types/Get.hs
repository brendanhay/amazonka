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
-- Module      : Amazonka.DynamoDB.Types.Get
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Get where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Specifies an item and related attribute values to retrieve in a
-- @TransactGetItem@ object.
--
-- /See:/ 'newGet' smart constructor.
data Get = Get'
  { -- | One or more substitution tokens for attribute names in the
    -- ProjectionExpression parameter.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A string that identifies one or more attributes of the specified item to
    -- retrieve from the table. The attributes in the expression must be
    -- separated by commas. If no attribute names are specified, then all
    -- attributes of the specified item are returned. If any of the requested
    -- attributes are not found, they do not appear in the result.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A map of attribute names to @AttributeValue@ objects that specifies the
    -- primary key of the item to retrieve.
    key :: Prelude.HashMap Prelude.Text AttributeValue,
    -- | The name of the table from which to retrieve the specified item.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Get' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionAttributeNames', 'get_expressionAttributeNames' - One or more substitution tokens for attribute names in the
-- ProjectionExpression parameter.
--
-- 'projectionExpression', 'get_projectionExpression' - A string that identifies one or more attributes of the specified item to
-- retrieve from the table. The attributes in the expression must be
-- separated by commas. If no attribute names are specified, then all
-- attributes of the specified item are returned. If any of the requested
-- attributes are not found, they do not appear in the result.
--
-- 'key', 'get_key' - A map of attribute names to @AttributeValue@ objects that specifies the
-- primary key of the item to retrieve.
--
-- 'tableName', 'get_tableName' - The name of the table from which to retrieve the specified item.
newGet ::
  -- | 'tableName'
  Prelude.Text ->
  Get
newGet pTableName_ =
  Get'
    { expressionAttributeNames = Prelude.Nothing,
      projectionExpression = Prelude.Nothing,
      key = Prelude.mempty,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in the
-- ProjectionExpression parameter.
get_expressionAttributeNames :: Lens.Lens' Get (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
get_expressionAttributeNames = Lens.lens (\Get' {expressionAttributeNames} -> expressionAttributeNames) (\s@Get' {} a -> s {expressionAttributeNames = a} :: Get) Prelude.. Lens.mapping Lens.coerced

-- | A string that identifies one or more attributes of the specified item to
-- retrieve from the table. The attributes in the expression must be
-- separated by commas. If no attribute names are specified, then all
-- attributes of the specified item are returned. If any of the requested
-- attributes are not found, they do not appear in the result.
get_projectionExpression :: Lens.Lens' Get (Prelude.Maybe Prelude.Text)
get_projectionExpression = Lens.lens (\Get' {projectionExpression} -> projectionExpression) (\s@Get' {} a -> s {projectionExpression = a} :: Get)

-- | A map of attribute names to @AttributeValue@ objects that specifies the
-- primary key of the item to retrieve.
get_key :: Lens.Lens' Get (Prelude.HashMap Prelude.Text AttributeValue)
get_key = Lens.lens (\Get' {key} -> key) (\s@Get' {} a -> s {key = a} :: Get) Prelude.. Lens.coerced

-- | The name of the table from which to retrieve the specified item.
get_tableName :: Lens.Lens' Get Prelude.Text
get_tableName = Lens.lens (\Get' {tableName} -> tableName) (\s@Get' {} a -> s {tableName = a} :: Get)

instance Prelude.Hashable Get where
  hashWithSalt _salt Get' {..} =
    _salt
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` projectionExpression
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Get where
  rnf Get' {..} =
    Prelude.rnf expressionAttributeNames `Prelude.seq`
      Prelude.rnf projectionExpression `Prelude.seq`
        Prelude.rnf key `Prelude.seq`
          Prelude.rnf tableName

instance Data.ToJSON Get where
  toJSON Get' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ProjectionExpression" Data..=)
              Prelude.<$> projectionExpression,
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
