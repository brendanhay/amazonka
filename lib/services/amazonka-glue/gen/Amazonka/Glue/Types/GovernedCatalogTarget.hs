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
-- Module      : Amazonka.Glue.Types.GovernedCatalogTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GovernedCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CatalogSchemaChangePolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data target that writes to Amazon S3 using the Glue Data
-- Catalog.
--
-- /See:/ 'newGovernedCatalogTarget' smart constructor.
data GovernedCatalogTarget = GovernedCatalogTarget'
  { -- | A policy that specifies update behavior for the governed catalog.
    schemaChangePolicy :: Prelude.Maybe CatalogSchemaChangePolicy,
    -- | Specifies native partitioning using a sequence of keys.
    partitionKeys :: Prelude.Maybe [[Prelude.Text]],
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the table in the database to write to.
    table :: Prelude.Text,
    -- | The name of the database to write to.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GovernedCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaChangePolicy', 'governedCatalogTarget_schemaChangePolicy' - A policy that specifies update behavior for the governed catalog.
--
-- 'partitionKeys', 'governedCatalogTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'name', 'governedCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 'governedCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'table', 'governedCatalogTarget_table' - The name of the table in the database to write to.
--
-- 'database', 'governedCatalogTarget_database' - The name of the database to write to.
newGovernedCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  GovernedCatalogTarget
newGovernedCatalogTarget
  pName_
  pInputs_
  pTable_
  pDatabase_ =
    GovernedCatalogTarget'
      { schemaChangePolicy =
          Prelude.Nothing,
        partitionKeys = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        table = pTable_,
        database = pDatabase_
      }

-- | A policy that specifies update behavior for the governed catalog.
governedCatalogTarget_schemaChangePolicy :: Lens.Lens' GovernedCatalogTarget (Prelude.Maybe CatalogSchemaChangePolicy)
governedCatalogTarget_schemaChangePolicy = Lens.lens (\GovernedCatalogTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@GovernedCatalogTarget' {} a -> s {schemaChangePolicy = a} :: GovernedCatalogTarget)

-- | Specifies native partitioning using a sequence of keys.
governedCatalogTarget_partitionKeys :: Lens.Lens' GovernedCatalogTarget (Prelude.Maybe [[Prelude.Text]])
governedCatalogTarget_partitionKeys = Lens.lens (\GovernedCatalogTarget' {partitionKeys} -> partitionKeys) (\s@GovernedCatalogTarget' {} a -> s {partitionKeys = a} :: GovernedCatalogTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data target.
governedCatalogTarget_name :: Lens.Lens' GovernedCatalogTarget Prelude.Text
governedCatalogTarget_name = Lens.lens (\GovernedCatalogTarget' {name} -> name) (\s@GovernedCatalogTarget' {} a -> s {name = a} :: GovernedCatalogTarget)

-- | The nodes that are inputs to the data target.
governedCatalogTarget_inputs :: Lens.Lens' GovernedCatalogTarget (Prelude.NonEmpty Prelude.Text)
governedCatalogTarget_inputs = Lens.lens (\GovernedCatalogTarget' {inputs} -> inputs) (\s@GovernedCatalogTarget' {} a -> s {inputs = a} :: GovernedCatalogTarget) Prelude.. Lens.coerced

-- | The name of the table in the database to write to.
governedCatalogTarget_table :: Lens.Lens' GovernedCatalogTarget Prelude.Text
governedCatalogTarget_table = Lens.lens (\GovernedCatalogTarget' {table} -> table) (\s@GovernedCatalogTarget' {} a -> s {table = a} :: GovernedCatalogTarget)

-- | The name of the database to write to.
governedCatalogTarget_database :: Lens.Lens' GovernedCatalogTarget Prelude.Text
governedCatalogTarget_database = Lens.lens (\GovernedCatalogTarget' {database} -> database) (\s@GovernedCatalogTarget' {} a -> s {database = a} :: GovernedCatalogTarget)

instance Data.FromJSON GovernedCatalogTarget where
  parseJSON =
    Data.withObject
      "GovernedCatalogTarget"
      ( \x ->
          GovernedCatalogTarget'
            Prelude.<$> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable GovernedCatalogTarget where
  hashWithSalt _salt GovernedCatalogTarget' {..} =
    _salt `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database

instance Prelude.NFData GovernedCatalogTarget where
  rnf GovernedCatalogTarget' {..} =
    Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON GovernedCatalogTarget where
  toJSON GovernedCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Table" Data..= table),
            Prelude.Just ("Database" Data..= database)
          ]
      )
