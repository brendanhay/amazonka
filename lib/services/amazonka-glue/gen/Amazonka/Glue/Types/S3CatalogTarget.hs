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
-- Module      : Amazonka.Glue.Types.S3CatalogTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3CatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CatalogSchemaChangePolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data target that writes to Amazon S3 using the Glue Data
-- Catalog.
--
-- /See:/ 'newS3CatalogTarget' smart constructor.
data S3CatalogTarget = S3CatalogTarget'
  { -- | A policy that specifies update behavior for the crawler.
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
-- Create a value of 'S3CatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaChangePolicy', 's3CatalogTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'partitionKeys', 's3CatalogTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'name', 's3CatalogTarget_name' - The name of the data target.
--
-- 'inputs', 's3CatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'table', 's3CatalogTarget_table' - The name of the table in the database to write to.
--
-- 'database', 's3CatalogTarget_database' - The name of the database to write to.
newS3CatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  S3CatalogTarget
newS3CatalogTarget pName_ pInputs_ pTable_ pDatabase_ =
  S3CatalogTarget'
    { schemaChangePolicy =
        Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      table = pTable_,
      database = pDatabase_
    }

-- | A policy that specifies update behavior for the crawler.
s3CatalogTarget_schemaChangePolicy :: Lens.Lens' S3CatalogTarget (Prelude.Maybe CatalogSchemaChangePolicy)
s3CatalogTarget_schemaChangePolicy = Lens.lens (\S3CatalogTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3CatalogTarget' {} a -> s {schemaChangePolicy = a} :: S3CatalogTarget)

-- | Specifies native partitioning using a sequence of keys.
s3CatalogTarget_partitionKeys :: Lens.Lens' S3CatalogTarget (Prelude.Maybe [[Prelude.Text]])
s3CatalogTarget_partitionKeys = Lens.lens (\S3CatalogTarget' {partitionKeys} -> partitionKeys) (\s@S3CatalogTarget' {} a -> s {partitionKeys = a} :: S3CatalogTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data target.
s3CatalogTarget_name :: Lens.Lens' S3CatalogTarget Prelude.Text
s3CatalogTarget_name = Lens.lens (\S3CatalogTarget' {name} -> name) (\s@S3CatalogTarget' {} a -> s {name = a} :: S3CatalogTarget)

-- | The nodes that are inputs to the data target.
s3CatalogTarget_inputs :: Lens.Lens' S3CatalogTarget (Prelude.NonEmpty Prelude.Text)
s3CatalogTarget_inputs = Lens.lens (\S3CatalogTarget' {inputs} -> inputs) (\s@S3CatalogTarget' {} a -> s {inputs = a} :: S3CatalogTarget) Prelude.. Lens.coerced

-- | The name of the table in the database to write to.
s3CatalogTarget_table :: Lens.Lens' S3CatalogTarget Prelude.Text
s3CatalogTarget_table = Lens.lens (\S3CatalogTarget' {table} -> table) (\s@S3CatalogTarget' {} a -> s {table = a} :: S3CatalogTarget)

-- | The name of the database to write to.
s3CatalogTarget_database :: Lens.Lens' S3CatalogTarget Prelude.Text
s3CatalogTarget_database = Lens.lens (\S3CatalogTarget' {database} -> database) (\s@S3CatalogTarget' {} a -> s {database = a} :: S3CatalogTarget)

instance Core.FromJSON S3CatalogTarget where
  parseJSON =
    Core.withObject
      "S3CatalogTarget"
      ( \x ->
          S3CatalogTarget'
            Prelude.<$> (x Core..:? "SchemaChangePolicy")
            Prelude.<*> (x Core..:? "PartitionKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..: "Table")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable S3CatalogTarget where
  hashWithSalt _salt S3CatalogTarget' {..} =
    _salt `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database

instance Prelude.NFData S3CatalogTarget where
  rnf S3CatalogTarget' {..} =
    Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON S3CatalogTarget where
  toJSON S3CatalogTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaChangePolicy" Core..=)
              Prelude.<$> schemaChangePolicy,
            ("PartitionKeys" Core..=) Prelude.<$> partitionKeys,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("Table" Core..= table),
            Prelude.Just ("Database" Core..= database)
          ]
      )
