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
-- Module      : Amazonka.DynamoDB.Types.LocalSecondaryIndexInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.LocalSecondaryIndexInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a local secondary index for the table when
-- the backup was created.
--
-- /See:/ 'newLocalSecondaryIndexInfo' smart constructor.
data LocalSecondaryIndexInfo = LocalSecondaryIndexInfo'
  { -- | Represents the name of the local secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The complete key schema for a local secondary index, which consists of
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
    projection :: Prelude.Maybe Projection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalSecondaryIndexInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'localSecondaryIndexInfo_indexName' - Represents the name of the local secondary index.
--
-- 'keySchema', 'localSecondaryIndexInfo_keySchema' - The complete key schema for a local secondary index, which consists of
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
-- 'projection', 'localSecondaryIndexInfo_projection' - Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
newLocalSecondaryIndexInfo ::
  LocalSecondaryIndexInfo
newLocalSecondaryIndexInfo =
  LocalSecondaryIndexInfo'
    { indexName =
        Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing
    }

-- | Represents the name of the local secondary index.
localSecondaryIndexInfo_indexName :: Lens.Lens' LocalSecondaryIndexInfo (Prelude.Maybe Prelude.Text)
localSecondaryIndexInfo_indexName = Lens.lens (\LocalSecondaryIndexInfo' {indexName} -> indexName) (\s@LocalSecondaryIndexInfo' {} a -> s {indexName = a} :: LocalSecondaryIndexInfo)

-- | The complete key schema for a local secondary index, which consists of
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
localSecondaryIndexInfo_keySchema :: Lens.Lens' LocalSecondaryIndexInfo (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
localSecondaryIndexInfo_keySchema = Lens.lens (\LocalSecondaryIndexInfo' {keySchema} -> keySchema) (\s@LocalSecondaryIndexInfo' {} a -> s {keySchema = a} :: LocalSecondaryIndexInfo) Prelude.. Lens.mapping Lens.coerced

-- | Represents attributes that are copied (projected) from the table into
-- the global secondary index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
localSecondaryIndexInfo_projection :: Lens.Lens' LocalSecondaryIndexInfo (Prelude.Maybe Projection)
localSecondaryIndexInfo_projection = Lens.lens (\LocalSecondaryIndexInfo' {projection} -> projection) (\s@LocalSecondaryIndexInfo' {} a -> s {projection = a} :: LocalSecondaryIndexInfo)

instance Data.FromJSON LocalSecondaryIndexInfo where
  parseJSON =
    Data.withObject
      "LocalSecondaryIndexInfo"
      ( \x ->
          LocalSecondaryIndexInfo'
            Prelude.<$> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "KeySchema")
            Prelude.<*> (x Data..:? "Projection")
      )

instance Prelude.Hashable LocalSecondaryIndexInfo where
  hashWithSalt _salt LocalSecondaryIndexInfo' {..} =
    _salt
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` projection

instance Prelude.NFData LocalSecondaryIndexInfo where
  rnf LocalSecondaryIndexInfo' {..} =
    Prelude.rnf indexName `Prelude.seq`
      Prelude.rnf keySchema `Prelude.seq`
        Prelude.rnf projection
