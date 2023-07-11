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
-- Module      : Amazonka.DynamoDB.Types.LocalSecondaryIndexDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.LocalSecondaryIndexDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'newLocalSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | Represents the name of the local secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The total size of the specified index, in bytes. DynamoDB updates this
    -- value approximately every six hours. Recent changes might not be
    -- reflected in this value.
    indexSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of items in the specified index. DynamoDB updates this value
    -- approximately every six hours. Recent changes might not be reflected in
    -- this value.
    itemCount :: Prelude.Maybe Prelude.Integer,
    -- | The complete key schema for the local secondary index, consisting of one
    -- or more pairs of attribute names and key types:
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
    projection :: Prelude.Maybe Projection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalSecondaryIndexDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexArn', 'localSecondaryIndexDescription_indexArn' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- 'indexName', 'localSecondaryIndexDescription_indexName' - Represents the name of the local secondary index.
--
-- 'indexSizeBytes', 'localSecondaryIndexDescription_indexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- 'itemCount', 'localSecondaryIndexDescription_itemCount' - The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
--
-- 'keySchema', 'localSecondaryIndexDescription_keySchema' - The complete key schema for the local secondary index, consisting of one
-- or more pairs of attribute names and key types:
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
-- 'projection', 'localSecondaryIndexDescription_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
newLocalSecondaryIndexDescription ::
  LocalSecondaryIndexDescription
newLocalSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { indexArn =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexSizeBytes = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
localSecondaryIndexDescription_indexArn :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
localSecondaryIndexDescription_indexArn = Lens.lens (\LocalSecondaryIndexDescription' {indexArn} -> indexArn) (\s@LocalSecondaryIndexDescription' {} a -> s {indexArn = a} :: LocalSecondaryIndexDescription)

-- | Represents the name of the local secondary index.
localSecondaryIndexDescription_indexName :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
localSecondaryIndexDescription_indexName = Lens.lens (\LocalSecondaryIndexDescription' {indexName} -> indexName) (\s@LocalSecondaryIndexDescription' {} a -> s {indexName = a} :: LocalSecondaryIndexDescription)

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
localSecondaryIndexDescription_indexSizeBytes :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Integer)
localSecondaryIndexDescription_indexSizeBytes = Lens.lens (\LocalSecondaryIndexDescription' {indexSizeBytes} -> indexSizeBytes) (\s@LocalSecondaryIndexDescription' {} a -> s {indexSizeBytes = a} :: LocalSecondaryIndexDescription)

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
localSecondaryIndexDescription_itemCount :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Integer)
localSecondaryIndexDescription_itemCount = Lens.lens (\LocalSecondaryIndexDescription' {itemCount} -> itemCount) (\s@LocalSecondaryIndexDescription' {} a -> s {itemCount = a} :: LocalSecondaryIndexDescription)

-- | The complete key schema for the local secondary index, consisting of one
-- or more pairs of attribute names and key types:
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
localSecondaryIndexDescription_keySchema :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
localSecondaryIndexDescription_keySchema = Lens.lens (\LocalSecondaryIndexDescription' {keySchema} -> keySchema) (\s@LocalSecondaryIndexDescription' {} a -> s {keySchema = a} :: LocalSecondaryIndexDescription) Prelude.. Lens.mapping Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
localSecondaryIndexDescription_projection :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Projection)
localSecondaryIndexDescription_projection = Lens.lens (\LocalSecondaryIndexDescription' {projection} -> projection) (\s@LocalSecondaryIndexDescription' {} a -> s {projection = a} :: LocalSecondaryIndexDescription)

instance Data.FromJSON LocalSecondaryIndexDescription where
  parseJSON =
    Data.withObject
      "LocalSecondaryIndexDescription"
      ( \x ->
          LocalSecondaryIndexDescription'
            Prelude.<$> (x Data..:? "IndexArn")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "IndexSizeBytes")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "KeySchema")
            Prelude.<*> (x Data..:? "Projection")
      )

instance
  Prelude.Hashable
    LocalSecondaryIndexDescription
  where
  hashWithSalt
    _salt
    LocalSecondaryIndexDescription' {..} =
      _salt
        `Prelude.hashWithSalt` indexArn
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` indexSizeBytes
        `Prelude.hashWithSalt` itemCount
        `Prelude.hashWithSalt` keySchema
        `Prelude.hashWithSalt` projection

instance
  Prelude.NFData
    LocalSecondaryIndexDescription
  where
  rnf LocalSecondaryIndexDescription' {..} =
    Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf indexSizeBytes
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection
