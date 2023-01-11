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
-- Module      : Amazonka.KeySpaces.Types.SchemaDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.SchemaDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.ClusteringKey
import Amazonka.KeySpaces.Types.ColumnDefinition
import Amazonka.KeySpaces.Types.PartitionKey
import Amazonka.KeySpaces.Types.StaticColumn
import qualified Amazonka.Prelude as Prelude

-- | Describes the schema of the table.
--
-- /See:/ 'newSchemaDefinition' smart constructor.
data SchemaDefinition = SchemaDefinition'
  { -- | The columns that are part of the clustering key of the table.
    clusteringKeys :: Prelude.Maybe [ClusteringKey],
    -- | The columns that have been defined as @STATIC@. Static columns store
    -- values that are shared by all rows in the same partition.
    staticColumns :: Prelude.Maybe [StaticColumn],
    -- | The regular columns of the table.
    allColumns :: Prelude.NonEmpty ColumnDefinition,
    -- | The columns that are part of the partition key of the table .
    partitionKeys :: Prelude.NonEmpty PartitionKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusteringKeys', 'schemaDefinition_clusteringKeys' - The columns that are part of the clustering key of the table.
--
-- 'staticColumns', 'schemaDefinition_staticColumns' - The columns that have been defined as @STATIC@. Static columns store
-- values that are shared by all rows in the same partition.
--
-- 'allColumns', 'schemaDefinition_allColumns' - The regular columns of the table.
--
-- 'partitionKeys', 'schemaDefinition_partitionKeys' - The columns that are part of the partition key of the table .
newSchemaDefinition ::
  -- | 'allColumns'
  Prelude.NonEmpty ColumnDefinition ->
  -- | 'partitionKeys'
  Prelude.NonEmpty PartitionKey ->
  SchemaDefinition
newSchemaDefinition pAllColumns_ pPartitionKeys_ =
  SchemaDefinition'
    { clusteringKeys = Prelude.Nothing,
      staticColumns = Prelude.Nothing,
      allColumns = Lens.coerced Lens.# pAllColumns_,
      partitionKeys = Lens.coerced Lens.# pPartitionKeys_
    }

-- | The columns that are part of the clustering key of the table.
schemaDefinition_clusteringKeys :: Lens.Lens' SchemaDefinition (Prelude.Maybe [ClusteringKey])
schemaDefinition_clusteringKeys = Lens.lens (\SchemaDefinition' {clusteringKeys} -> clusteringKeys) (\s@SchemaDefinition' {} a -> s {clusteringKeys = a} :: SchemaDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The columns that have been defined as @STATIC@. Static columns store
-- values that are shared by all rows in the same partition.
schemaDefinition_staticColumns :: Lens.Lens' SchemaDefinition (Prelude.Maybe [StaticColumn])
schemaDefinition_staticColumns = Lens.lens (\SchemaDefinition' {staticColumns} -> staticColumns) (\s@SchemaDefinition' {} a -> s {staticColumns = a} :: SchemaDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The regular columns of the table.
schemaDefinition_allColumns :: Lens.Lens' SchemaDefinition (Prelude.NonEmpty ColumnDefinition)
schemaDefinition_allColumns = Lens.lens (\SchemaDefinition' {allColumns} -> allColumns) (\s@SchemaDefinition' {} a -> s {allColumns = a} :: SchemaDefinition) Prelude.. Lens.coerced

-- | The columns that are part of the partition key of the table .
schemaDefinition_partitionKeys :: Lens.Lens' SchemaDefinition (Prelude.NonEmpty PartitionKey)
schemaDefinition_partitionKeys = Lens.lens (\SchemaDefinition' {partitionKeys} -> partitionKeys) (\s@SchemaDefinition' {} a -> s {partitionKeys = a} :: SchemaDefinition) Prelude.. Lens.coerced

instance Data.FromJSON SchemaDefinition where
  parseJSON =
    Data.withObject
      "SchemaDefinition"
      ( \x ->
          SchemaDefinition'
            Prelude.<$> (x Data..:? "clusteringKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "staticColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "allColumns")
            Prelude.<*> (x Data..: "partitionKeys")
      )

instance Prelude.Hashable SchemaDefinition where
  hashWithSalt _salt SchemaDefinition' {..} =
    _salt `Prelude.hashWithSalt` clusteringKeys
      `Prelude.hashWithSalt` staticColumns
      `Prelude.hashWithSalt` allColumns
      `Prelude.hashWithSalt` partitionKeys

instance Prelude.NFData SchemaDefinition where
  rnf SchemaDefinition' {..} =
    Prelude.rnf clusteringKeys
      `Prelude.seq` Prelude.rnf staticColumns
      `Prelude.seq` Prelude.rnf allColumns
      `Prelude.seq` Prelude.rnf partitionKeys

instance Data.ToJSON SchemaDefinition where
  toJSON SchemaDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clusteringKeys" Data..=)
              Prelude.<$> clusteringKeys,
            ("staticColumns" Data..=) Prelude.<$> staticColumns,
            Prelude.Just ("allColumns" Data..= allColumns),
            Prelude.Just
              ("partitionKeys" Data..= partitionKeys)
          ]
      )
