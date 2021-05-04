{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'newLocalSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { -- | Represents the name of the local secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Resource Name (ARN) that uniquely identifies the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | Represents attributes that are copied (projected) from the table into
    -- the global secondary index. These are in addition to the primary key
    -- attributes and index key attributes, which are automatically projected.
    projection :: Prelude.Maybe Projection,
    -- | The total size of the specified index, in bytes. DynamoDB updates this
    -- value approximately every six hours. Recent changes might not be
    -- reflected in this value.
    indexSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of items in the specified index. DynamoDB updates this value
    -- approximately every six hours. Recent changes might not be reflected in
    -- this value.
    itemCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LocalSecondaryIndexDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'localSecondaryIndexDescription_indexName' - Represents the name of the local secondary index.
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
-- 'indexArn', 'localSecondaryIndexDescription_indexArn' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- 'projection', 'localSecondaryIndexDescription_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
--
-- 'indexSizeBytes', 'localSecondaryIndexDescription_indexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- 'itemCount', 'localSecondaryIndexDescription_itemCount' - The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
newLocalSecondaryIndexDescription ::
  LocalSecondaryIndexDescription
newLocalSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { indexName =
        Prelude.Nothing,
      keySchema = Prelude.Nothing,
      indexArn = Prelude.Nothing,
      projection = Prelude.Nothing,
      indexSizeBytes = Prelude.Nothing,
      itemCount = Prelude.Nothing
    }

-- | Represents the name of the local secondary index.
localSecondaryIndexDescription_indexName :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
localSecondaryIndexDescription_indexName = Lens.lens (\LocalSecondaryIndexDescription' {indexName} -> indexName) (\s@LocalSecondaryIndexDescription' {} a -> s {indexName = a} :: LocalSecondaryIndexDescription)

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
localSecondaryIndexDescription_keySchema = Lens.lens (\LocalSecondaryIndexDescription' {keySchema} -> keySchema) (\s@LocalSecondaryIndexDescription' {} a -> s {keySchema = a} :: LocalSecondaryIndexDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
localSecondaryIndexDescription_indexArn :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
localSecondaryIndexDescription_indexArn = Lens.lens (\LocalSecondaryIndexDescription' {indexArn} -> indexArn) (\s@LocalSecondaryIndexDescription' {} a -> s {indexArn = a} :: LocalSecondaryIndexDescription)

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
localSecondaryIndexDescription_projection :: Lens.Lens' LocalSecondaryIndexDescription (Prelude.Maybe Projection)
localSecondaryIndexDescription_projection = Lens.lens (\LocalSecondaryIndexDescription' {projection} -> projection) (\s@LocalSecondaryIndexDescription' {} a -> s {projection = a} :: LocalSecondaryIndexDescription)

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

instance
  Prelude.FromJSON
    LocalSecondaryIndexDescription
  where
  parseJSON =
    Prelude.withObject
      "LocalSecondaryIndexDescription"
      ( \x ->
          LocalSecondaryIndexDescription'
            Prelude.<$> (x Prelude..:? "IndexName")
            Prelude.<*> (x Prelude..:? "KeySchema")
            Prelude.<*> (x Prelude..:? "IndexArn")
            Prelude.<*> (x Prelude..:? "Projection")
            Prelude.<*> (x Prelude..:? "IndexSizeBytes")
            Prelude.<*> (x Prelude..:? "ItemCount")
      )

instance
  Prelude.Hashable
    LocalSecondaryIndexDescription

instance
  Prelude.NFData
    LocalSecondaryIndexDescription
