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
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a global secondary index for the table when
-- the backup was created.
--
-- /See:/ 'newGlobalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Core.Text,
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
    keySchema :: Core.Maybe (Core.NonEmpty KeySchemaElement),
    -- | Represents attributes that are copied (projected) from the table into
    -- the global secondary index. These are in addition to the primary key
    -- attributes and index key attributes, which are automatically projected.
    projection :: Core.Maybe Projection,
    -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    provisionedThroughput :: Core.Maybe ProvisionedThroughput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'globalSecondaryIndexInfo_indexName' - The name of the global secondary index.
--
-- 'keySchema', 'globalSecondaryIndexInfo_keySchema' - The complete key schema for a global secondary index, which consists of
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
-- 'projection', 'globalSecondaryIndexInfo_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
--
-- 'provisionedThroughput', 'globalSecondaryIndexInfo_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
newGlobalSecondaryIndexInfo ::
  GlobalSecondaryIndexInfo
newGlobalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { indexName = Core.Nothing,
      keySchema = Core.Nothing,
      projection = Core.Nothing,
      provisionedThroughput = Core.Nothing
    }

-- | The name of the global secondary index.
globalSecondaryIndexInfo_indexName :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe Core.Text)
globalSecondaryIndexInfo_indexName = Lens.lens (\GlobalSecondaryIndexInfo' {indexName} -> indexName) (\s@GlobalSecondaryIndexInfo' {} a -> s {indexName = a} :: GlobalSecondaryIndexInfo)

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
globalSecondaryIndexInfo_keySchema :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe (Core.NonEmpty KeySchemaElement))
globalSecondaryIndexInfo_keySchema = Lens.lens (\GlobalSecondaryIndexInfo' {keySchema} -> keySchema) (\s@GlobalSecondaryIndexInfo' {} a -> s {keySchema = a} :: GlobalSecondaryIndexInfo) Core.. Lens.mapping Lens._Coerce

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
globalSecondaryIndexInfo_projection :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe Projection)
globalSecondaryIndexInfo_projection = Lens.lens (\GlobalSecondaryIndexInfo' {projection} -> projection) (\s@GlobalSecondaryIndexInfo' {} a -> s {projection = a} :: GlobalSecondaryIndexInfo)

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
globalSecondaryIndexInfo_provisionedThroughput :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe ProvisionedThroughput)
globalSecondaryIndexInfo_provisionedThroughput = Lens.lens (\GlobalSecondaryIndexInfo' {provisionedThroughput} -> provisionedThroughput) (\s@GlobalSecondaryIndexInfo' {} a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexInfo)

instance Core.FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    Core.withObject
      "GlobalSecondaryIndexInfo"
      ( \x ->
          GlobalSecondaryIndexInfo'
            Core.<$> (x Core..:? "IndexName")
            Core.<*> (x Core..:? "KeySchema")
            Core.<*> (x Core..:? "Projection")
            Core.<*> (x Core..:? "ProvisionedThroughput")
      )

instance Core.Hashable GlobalSecondaryIndexInfo

instance Core.NFData GlobalSecondaryIndexInfo
