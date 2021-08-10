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
-- Module      : Network.AWS.DynamoDB.Types.Get
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Get where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an item and related attribute values to retrieve in a
-- @TransactGetItem@ object.
--
-- /See:/ 'newGet' smart constructor.
data Get = Get'
  { -- | A string that identifies one or more attributes of the specified item to
    -- retrieve from the table. The attributes in the expression must be
    -- separated by commas. If no attribute names are specified, then all
    -- attributes of the specified item are returned. If any of the requested
    -- attributes are not found, they do not appear in the result.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    -- | One or more substitution tokens for attribute names in the
    -- ProjectionExpression parameter.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'projectionExpression', 'get_projectionExpression' - A string that identifies one or more attributes of the specified item to
-- retrieve from the table. The attributes in the expression must be
-- separated by commas. If no attribute names are specified, then all
-- attributes of the specified item are returned. If any of the requested
-- attributes are not found, they do not appear in the result.
--
-- 'expressionAttributeNames', 'get_expressionAttributeNames' - One or more substitution tokens for attribute names in the
-- ProjectionExpression parameter.
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
    { projectionExpression = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      key = Prelude.mempty,
      tableName = pTableName_
    }

-- | A string that identifies one or more attributes of the specified item to
-- retrieve from the table. The attributes in the expression must be
-- separated by commas. If no attribute names are specified, then all
-- attributes of the specified item are returned. If any of the requested
-- attributes are not found, they do not appear in the result.
get_projectionExpression :: Lens.Lens' Get (Prelude.Maybe Prelude.Text)
get_projectionExpression = Lens.lens (\Get' {projectionExpression} -> projectionExpression) (\s@Get' {} a -> s {projectionExpression = a} :: Get)

-- | One or more substitution tokens for attribute names in the
-- ProjectionExpression parameter.
get_expressionAttributeNames :: Lens.Lens' Get (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
get_expressionAttributeNames = Lens.lens (\Get' {expressionAttributeNames} -> expressionAttributeNames) (\s@Get' {} a -> s {expressionAttributeNames = a} :: Get) Prelude.. Lens.mapping Lens._Coerce

-- | A map of attribute names to @AttributeValue@ objects that specifies the
-- primary key of the item to retrieve.
get_key :: Lens.Lens' Get (Prelude.HashMap Prelude.Text AttributeValue)
get_key = Lens.lens (\Get' {key} -> key) (\s@Get' {} a -> s {key = a} :: Get) Prelude.. Lens._Coerce

-- | The name of the table from which to retrieve the specified item.
get_tableName :: Lens.Lens' Get Prelude.Text
get_tableName = Lens.lens (\Get' {tableName} -> tableName) (\s@Get' {} a -> s {tableName = a} :: Get)

instance Prelude.Hashable Get

instance Prelude.NFData Get

instance Core.ToJSON Get where
  toJSON Get' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProjectionExpression" Core..=)
              Prelude.<$> projectionExpression,
            ("ExpressionAttributeNames" Core..=)
              Prelude.<$> expressionAttributeNames,
            Prelude.Just ("Key" Core..= key),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )
