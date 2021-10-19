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
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a global secondary index for the table when
-- the backup was created.
--
-- /See:/ 'newGlobalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
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
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'globalSecondaryIndexInfo_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
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
-- 'indexName', 'globalSecondaryIndexInfo_indexName' - The name of the global secondary index.
newGlobalSecondaryIndexInfo ::
  GlobalSecondaryIndexInfo
newGlobalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { provisionedThroughput =
        Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
globalSecondaryIndexInfo_provisionedThroughput :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe ProvisionedThroughput)
globalSecondaryIndexInfo_provisionedThroughput = Lens.lens (\GlobalSecondaryIndexInfo' {provisionedThroughput} -> provisionedThroughput) (\s@GlobalSecondaryIndexInfo' {} a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexInfo)

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
globalSecondaryIndexInfo_keySchema :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
globalSecondaryIndexInfo_keySchema = Lens.lens (\GlobalSecondaryIndexInfo' {keySchema} -> keySchema) (\s@GlobalSecondaryIndexInfo' {} a -> s {keySchema = a} :: GlobalSecondaryIndexInfo) Prelude.. Lens.mapping Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
globalSecondaryIndexInfo_projection :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe Projection)
globalSecondaryIndexInfo_projection = Lens.lens (\GlobalSecondaryIndexInfo' {projection} -> projection) (\s@GlobalSecondaryIndexInfo' {} a -> s {projection = a} :: GlobalSecondaryIndexInfo)

-- | The name of the global secondary index.
globalSecondaryIndexInfo_indexName :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe Prelude.Text)
globalSecondaryIndexInfo_indexName = Lens.lens (\GlobalSecondaryIndexInfo' {indexName} -> indexName) (\s@GlobalSecondaryIndexInfo' {} a -> s {indexName = a} :: GlobalSecondaryIndexInfo)

instance Core.FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    Core.withObject
      "GlobalSecondaryIndexInfo"
      ( \x ->
          GlobalSecondaryIndexInfo'
            Prelude.<$> (x Core..:? "ProvisionedThroughput")
            Prelude.<*> (x Core..:? "KeySchema")
            Prelude.<*> (x Core..:? "Projection")
            Prelude.<*> (x Core..:? "IndexName")
      )

instance Prelude.Hashable GlobalSecondaryIndexInfo

instance Prelude.NFData GlobalSecondaryIndexInfo
