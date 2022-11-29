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
-- Module      : Amazonka.DynamoDB.Types.GlobalSecondaryIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalSecondaryIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.ProvisionedThroughput
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'newGlobalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    --
    -- For current minimum and maximum provisioned throughput values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | The name of the global secondary index. The name must be unique among
    -- all other indexes on this table.
    indexName :: Prelude.Text,
    -- | The complete key schema for a global secondary index, which consists of
    -- one or more pairs of attribute names and key types:
    --
    -- -   @HASH@ - partition key
    --
    -- -   @RANGE@ - sort key
    --
    -- The partition key of an item is also known as its /hash attribute/. The
    -- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
    -- hash function to evenly distribute data items across partitions, based
    -- on their partition key values.
    --
    -- The sort key of an item is also known as its /range attribute/. The term
    -- \"range attribute\" derives from the way DynamoDB stores items with the
    -- same partition key physically close together, in sorted order by the
    -- sort key value.
    keySchema :: Prelude.NonEmpty KeySchemaElement,
    -- | Represents attributes that are copied (projected) from the table into
    -- the global secondary index. These are in addition to the primary key
    -- attributes and index key attributes, which are automatically projected.
    projection :: Projection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'globalSecondaryIndex_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'indexName', 'globalSecondaryIndex_indexName' - The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
--
-- 'keySchema', 'globalSecondaryIndex_keySchema' - The complete key schema for a global secondary index, which consists of
-- one or more pairs of attribute names and key types:
--
-- -   @HASH@ - partition key
--
-- -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
--
-- 'projection', 'globalSecondaryIndex_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
newGlobalSecondaryIndex ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'keySchema'
  Prelude.NonEmpty KeySchemaElement ->
  -- | 'projection'
  Projection ->
  GlobalSecondaryIndex
newGlobalSecondaryIndex
  pIndexName_
  pKeySchema_
  pProjection_ =
    GlobalSecondaryIndex'
      { provisionedThroughput =
          Prelude.Nothing,
        indexName = pIndexName_,
        keySchema = Lens.coerced Lens.# pKeySchema_,
        projection = pProjection_
      }

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
globalSecondaryIndex_provisionedThroughput :: Lens.Lens' GlobalSecondaryIndex (Prelude.Maybe ProvisionedThroughput)
globalSecondaryIndex_provisionedThroughput = Lens.lens (\GlobalSecondaryIndex' {provisionedThroughput} -> provisionedThroughput) (\s@GlobalSecondaryIndex' {} a -> s {provisionedThroughput = a} :: GlobalSecondaryIndex)

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
globalSecondaryIndex_indexName :: Lens.Lens' GlobalSecondaryIndex Prelude.Text
globalSecondaryIndex_indexName = Lens.lens (\GlobalSecondaryIndex' {indexName} -> indexName) (\s@GlobalSecondaryIndex' {} a -> s {indexName = a} :: GlobalSecondaryIndex)

-- | The complete key schema for a global secondary index, which consists of
-- one or more pairs of attribute names and key types:
--
-- -   @HASH@ - partition key
--
-- -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
globalSecondaryIndex_keySchema :: Lens.Lens' GlobalSecondaryIndex (Prelude.NonEmpty KeySchemaElement)
globalSecondaryIndex_keySchema = Lens.lens (\GlobalSecondaryIndex' {keySchema} -> keySchema) (\s@GlobalSecondaryIndex' {} a -> s {keySchema = a} :: GlobalSecondaryIndex) Prelude.. Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
globalSecondaryIndex_projection :: Lens.Lens' GlobalSecondaryIndex Projection
globalSecondaryIndex_projection = Lens.lens (\GlobalSecondaryIndex' {projection} -> projection) (\s@GlobalSecondaryIndex' {} a -> s {projection = a} :: GlobalSecondaryIndex)

instance Core.FromJSON GlobalSecondaryIndex where
  parseJSON =
    Core.withObject
      "GlobalSecondaryIndex"
      ( \x ->
          GlobalSecondaryIndex'
            Prelude.<$> (x Core..:? "ProvisionedThroughput")
            Prelude.<*> (x Core..: "IndexName")
            Prelude.<*> (x Core..: "KeySchema")
            Prelude.<*> (x Core..: "Projection")
      )

instance Prelude.Hashable GlobalSecondaryIndex where
  hashWithSalt _salt GlobalSecondaryIndex' {..} =
    _salt `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` projection

instance Prelude.NFData GlobalSecondaryIndex where
  rnf GlobalSecondaryIndex' {..} =
    Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection

instance Core.ToJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProvisionedThroughput" Core..=)
              Prelude.<$> provisionedThroughput,
            Prelude.Just ("IndexName" Core..= indexName),
            Prelude.Just ("KeySchema" Core..= keySchema),
            Prelude.Just ("Projection" Core..= projection)
          ]
      )
