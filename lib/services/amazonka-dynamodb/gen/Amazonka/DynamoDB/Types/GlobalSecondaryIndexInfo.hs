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
-- Module      : Amazonka.DynamoDB.Types.GlobalSecondaryIndexInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalSecondaryIndexInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.ProvisionedThroughput
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a global secondary index for the table when
-- the backup was created.
--
-- /See:/ 'newGlobalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
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
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput
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
    { indexName =
        Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing
    }

-- | The name of the global secondary index.
globalSecondaryIndexInfo_indexName :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe Prelude.Text)
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
globalSecondaryIndexInfo_keySchema :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
globalSecondaryIndexInfo_keySchema = Lens.lens (\GlobalSecondaryIndexInfo' {keySchema} -> keySchema) (\s@GlobalSecondaryIndexInfo' {} a -> s {keySchema = a} :: GlobalSecondaryIndexInfo) Prelude.. Lens.mapping Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
globalSecondaryIndexInfo_projection :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe Projection)
globalSecondaryIndexInfo_projection = Lens.lens (\GlobalSecondaryIndexInfo' {projection} -> projection) (\s@GlobalSecondaryIndexInfo' {} a -> s {projection = a} :: GlobalSecondaryIndexInfo)

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
globalSecondaryIndexInfo_provisionedThroughput :: Lens.Lens' GlobalSecondaryIndexInfo (Prelude.Maybe ProvisionedThroughput)
globalSecondaryIndexInfo_provisionedThroughput = Lens.lens (\GlobalSecondaryIndexInfo' {provisionedThroughput} -> provisionedThroughput) (\s@GlobalSecondaryIndexInfo' {} a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexInfo)

instance Data.FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    Data.withObject
      "GlobalSecondaryIndexInfo"
      ( \x ->
          GlobalSecondaryIndexInfo'
            Prelude.<$> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "KeySchema")
            Prelude.<*> (x Data..:? "Projection")
            Prelude.<*> (x Data..:? "ProvisionedThroughput")
      )

instance Prelude.Hashable GlobalSecondaryIndexInfo where
  hashWithSalt _salt GlobalSecondaryIndexInfo' {..} =
    _salt
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` projection
      `Prelude.hashWithSalt` provisionedThroughput

instance Prelude.NFData GlobalSecondaryIndexInfo where
  rnf GlobalSecondaryIndexInfo' {..} =
    Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection
      `Prelude.seq` Prelude.rnf provisionedThroughput
