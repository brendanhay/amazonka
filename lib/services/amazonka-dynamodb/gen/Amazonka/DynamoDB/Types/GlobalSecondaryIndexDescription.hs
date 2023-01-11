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
-- Module      : Amazonka.DynamoDB.Types.GlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalSecondaryIndexDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.IndexStatus
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.ProvisionedThroughputDescription
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'newGlobalSecondaryIndexDescription' smart constructor.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
  { -- | Indicates whether the index is currently backfilling. /Backfilling/ is
    -- the process of reading items from the table and determining whether they
    -- can be added to the index. (Not all items will qualify: For example, a
    -- partition key cannot have any duplicate values.) If an item can be added
    -- to the index, DynamoDB will do so. After all items have been processed,
    -- the backfilling operation is complete and @Backfilling@ is false.
    --
    -- You can delete an index that is being created during the @Backfilling@
    -- phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true.
    -- You can\'t delete the index that is being created when @IndexStatus@ is
    -- set to CREATING and @Backfilling@ is false.
    --
    -- For indexes that were created during a @CreateTable@ operation, the
    -- @Backfilling@ attribute does not appear in the @DescribeTable@ output.
    backfilling :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The total size of the specified index, in bytes. DynamoDB updates this
    -- value approximately every six hours. Recent changes might not be
    -- reflected in this value.
    indexSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The current state of the global secondary index:
    --
    -- -   @CREATING@ - The index is being created.
    --
    -- -   @UPDATING@ - The index is being updated.
    --
    -- -   @DELETING@ - The index is being deleted.
    --
    -- -   @ACTIVE@ - The index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus,
    -- | The number of items in the specified index. DynamoDB updates this value
    -- approximately every six hours. Recent changes might not be reflected in
    -- this value.
    itemCount :: Prelude.Maybe Prelude.Integer,
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
    keySchema :: Prelude.Maybe (Prelude.NonEmpty KeySchemaElement),
    -- | Represents attributes that are copied (projected) from the table into
    -- the global secondary index. These are in addition to the primary key
    -- attributes and index key attributes, which are automatically projected.
    projection :: Prelude.Maybe Projection,
    -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    --
    -- For current minimum and maximum provisioned throughput values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughputDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backfilling', 'globalSecondaryIndexDescription_backfilling' - Indicates whether the index is currently backfilling. /Backfilling/ is
-- the process of reading items from the table and determining whether they
-- can be added to the index. (Not all items will qualify: For example, a
-- partition key cannot have any duplicate values.) If an item can be added
-- to the index, DynamoDB will do so. After all items have been processed,
-- the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@
-- phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true.
-- You can\'t delete the index that is being created when @IndexStatus@ is
-- set to CREATING and @Backfilling@ is false.
--
-- For indexes that were created during a @CreateTable@ operation, the
-- @Backfilling@ attribute does not appear in the @DescribeTable@ output.
--
-- 'indexArn', 'globalSecondaryIndexDescription_indexArn' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- 'indexName', 'globalSecondaryIndexDescription_indexName' - The name of the global secondary index.
--
-- 'indexSizeBytes', 'globalSecondaryIndexDescription_indexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- 'indexStatus', 'globalSecondaryIndexDescription_indexStatus' - The current state of the global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The index is being updated.
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
--
-- 'itemCount', 'globalSecondaryIndexDescription_itemCount' - The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
--
-- 'keySchema', 'globalSecondaryIndexDescription_keySchema' - The complete key schema for a global secondary index, which consists of
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
-- 'projection', 'globalSecondaryIndexDescription_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
--
-- 'provisionedThroughput', 'globalSecondaryIndexDescription_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
newGlobalSecondaryIndexDescription ::
  GlobalSecondaryIndexDescription
newGlobalSecondaryIndexDescription =
  GlobalSecondaryIndexDescription'
    { backfilling =
        Prelude.Nothing,
      indexArn = Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexSizeBytes = Prelude.Nothing,
      indexStatus = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing
    }

-- | Indicates whether the index is currently backfilling. /Backfilling/ is
-- the process of reading items from the table and determining whether they
-- can be added to the index. (Not all items will qualify: For example, a
-- partition key cannot have any duplicate values.) If an item can be added
-- to the index, DynamoDB will do so. After all items have been processed,
-- the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@
-- phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true.
-- You can\'t delete the index that is being created when @IndexStatus@ is
-- set to CREATING and @Backfilling@ is false.
--
-- For indexes that were created during a @CreateTable@ operation, the
-- @Backfilling@ attribute does not appear in the @DescribeTable@ output.
globalSecondaryIndexDescription_backfilling :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Bool)
globalSecondaryIndexDescription_backfilling = Lens.lens (\GlobalSecondaryIndexDescription' {backfilling} -> backfilling) (\s@GlobalSecondaryIndexDescription' {} a -> s {backfilling = a} :: GlobalSecondaryIndexDescription)

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
globalSecondaryIndexDescription_indexArn :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
globalSecondaryIndexDescription_indexArn = Lens.lens (\GlobalSecondaryIndexDescription' {indexArn} -> indexArn) (\s@GlobalSecondaryIndexDescription' {} a -> s {indexArn = a} :: GlobalSecondaryIndexDescription)

-- | The name of the global secondary index.
globalSecondaryIndexDescription_indexName :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
globalSecondaryIndexDescription_indexName = Lens.lens (\GlobalSecondaryIndexDescription' {indexName} -> indexName) (\s@GlobalSecondaryIndexDescription' {} a -> s {indexName = a} :: GlobalSecondaryIndexDescription)

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
globalSecondaryIndexDescription_indexSizeBytes :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Integer)
globalSecondaryIndexDescription_indexSizeBytes = Lens.lens (\GlobalSecondaryIndexDescription' {indexSizeBytes} -> indexSizeBytes) (\s@GlobalSecondaryIndexDescription' {} a -> s {indexSizeBytes = a} :: GlobalSecondaryIndexDescription)

-- | The current state of the global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The index is being updated.
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
globalSecondaryIndexDescription_indexStatus :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe IndexStatus)
globalSecondaryIndexDescription_indexStatus = Lens.lens (\GlobalSecondaryIndexDescription' {indexStatus} -> indexStatus) (\s@GlobalSecondaryIndexDescription' {} a -> s {indexStatus = a} :: GlobalSecondaryIndexDescription)

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
globalSecondaryIndexDescription_itemCount :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Integer)
globalSecondaryIndexDescription_itemCount = Lens.lens (\GlobalSecondaryIndexDescription' {itemCount} -> itemCount) (\s@GlobalSecondaryIndexDescription' {} a -> s {itemCount = a} :: GlobalSecondaryIndexDescription)

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
globalSecondaryIndexDescription_keySchema :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
globalSecondaryIndexDescription_keySchema = Lens.lens (\GlobalSecondaryIndexDescription' {keySchema} -> keySchema) (\s@GlobalSecondaryIndexDescription' {} a -> s {keySchema = a} :: GlobalSecondaryIndexDescription) Prelude.. Lens.mapping Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
globalSecondaryIndexDescription_projection :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe Projection)
globalSecondaryIndexDescription_projection = Lens.lens (\GlobalSecondaryIndexDescription' {projection} -> projection) (\s@GlobalSecondaryIndexDescription' {} a -> s {projection = a} :: GlobalSecondaryIndexDescription)

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
globalSecondaryIndexDescription_provisionedThroughput :: Lens.Lens' GlobalSecondaryIndexDescription (Prelude.Maybe ProvisionedThroughputDescription)
globalSecondaryIndexDescription_provisionedThroughput = Lens.lens (\GlobalSecondaryIndexDescription' {provisionedThroughput} -> provisionedThroughput) (\s@GlobalSecondaryIndexDescription' {} a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexDescription)

instance
  Data.FromJSON
    GlobalSecondaryIndexDescription
  where
  parseJSON =
    Data.withObject
      "GlobalSecondaryIndexDescription"
      ( \x ->
          GlobalSecondaryIndexDescription'
            Prelude.<$> (x Data..:? "Backfilling")
            Prelude.<*> (x Data..:? "IndexArn")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "IndexSizeBytes")
            Prelude.<*> (x Data..:? "IndexStatus")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "KeySchema")
            Prelude.<*> (x Data..:? "Projection")
            Prelude.<*> (x Data..:? "ProvisionedThroughput")
      )

instance
  Prelude.Hashable
    GlobalSecondaryIndexDescription
  where
  hashWithSalt
    _salt
    GlobalSecondaryIndexDescription' {..} =
      _salt `Prelude.hashWithSalt` backfilling
        `Prelude.hashWithSalt` indexArn
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` indexSizeBytes
        `Prelude.hashWithSalt` indexStatus
        `Prelude.hashWithSalt` itemCount
        `Prelude.hashWithSalt` keySchema
        `Prelude.hashWithSalt` projection
        `Prelude.hashWithSalt` provisionedThroughput

instance
  Prelude.NFData
    GlobalSecondaryIndexDescription
  where
  rnf GlobalSecondaryIndexDescription' {..} =
    Prelude.rnf backfilling
      `Prelude.seq` Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf indexSizeBytes
      `Prelude.seq` Prelude.rnf indexStatus
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection
      `Prelude.seq` Prelude.rnf provisionedThroughput
