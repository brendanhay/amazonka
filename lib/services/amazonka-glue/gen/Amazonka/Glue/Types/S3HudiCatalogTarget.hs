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
-- Module      : Amazonka.Glue.Types.S3HudiCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3HudiCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CatalogSchemaChangePolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that writes to a Hudi data source in the Glue Data
-- Catalog.
--
-- /See:/ 'newS3HudiCatalogTarget' smart constructor.
data S3HudiCatalogTarget = S3HudiCatalogTarget'
  { -- | Specifies native partitioning using a sequence of keys.
    partitionKeys :: Prelude.Maybe [[Prelude.Text]],
    -- | A policy that specifies update behavior for the crawler.
    schemaChangePolicy :: Prelude.Maybe CatalogSchemaChangePolicy,
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the table in the database to write to.
    table :: Prelude.Text,
    -- | The name of the database to write to.
    database :: Prelude.Text,
    -- | Specifies additional connection options for the connector.
    additionalOptions :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3HudiCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionKeys', 's3HudiCatalogTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'schemaChangePolicy', 's3HudiCatalogTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'name', 's3HudiCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 's3HudiCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'table', 's3HudiCatalogTarget_table' - The name of the table in the database to write to.
--
-- 'database', 's3HudiCatalogTarget_database' - The name of the database to write to.
--
-- 'additionalOptions', 's3HudiCatalogTarget_additionalOptions' - Specifies additional connection options for the connector.
newS3HudiCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  S3HudiCatalogTarget
newS3HudiCatalogTarget
  pName_
  pInputs_
  pTable_
  pDatabase_ =
    S3HudiCatalogTarget'
      { partitionKeys =
          Prelude.Nothing,
        schemaChangePolicy = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        table = pTable_,
        database = pDatabase_,
        additionalOptions = Prelude.mempty
      }

-- | Specifies native partitioning using a sequence of keys.
s3HudiCatalogTarget_partitionKeys :: Lens.Lens' S3HudiCatalogTarget (Prelude.Maybe [[Prelude.Text]])
s3HudiCatalogTarget_partitionKeys = Lens.lens (\S3HudiCatalogTarget' {partitionKeys} -> partitionKeys) (\s@S3HudiCatalogTarget' {} a -> s {partitionKeys = a} :: S3HudiCatalogTarget) Prelude.. Lens.mapping Lens.coerced

-- | A policy that specifies update behavior for the crawler.
s3HudiCatalogTarget_schemaChangePolicy :: Lens.Lens' S3HudiCatalogTarget (Prelude.Maybe CatalogSchemaChangePolicy)
s3HudiCatalogTarget_schemaChangePolicy = Lens.lens (\S3HudiCatalogTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3HudiCatalogTarget' {} a -> s {schemaChangePolicy = a} :: S3HudiCatalogTarget)

-- | The name of the data target.
s3HudiCatalogTarget_name :: Lens.Lens' S3HudiCatalogTarget Prelude.Text
s3HudiCatalogTarget_name = Lens.lens (\S3HudiCatalogTarget' {name} -> name) (\s@S3HudiCatalogTarget' {} a -> s {name = a} :: S3HudiCatalogTarget)

-- | The nodes that are inputs to the data target.
s3HudiCatalogTarget_inputs :: Lens.Lens' S3HudiCatalogTarget (Prelude.NonEmpty Prelude.Text)
s3HudiCatalogTarget_inputs = Lens.lens (\S3HudiCatalogTarget' {inputs} -> inputs) (\s@S3HudiCatalogTarget' {} a -> s {inputs = a} :: S3HudiCatalogTarget) Prelude.. Lens.coerced

-- | The name of the table in the database to write to.
s3HudiCatalogTarget_table :: Lens.Lens' S3HudiCatalogTarget Prelude.Text
s3HudiCatalogTarget_table = Lens.lens (\S3HudiCatalogTarget' {table} -> table) (\s@S3HudiCatalogTarget' {} a -> s {table = a} :: S3HudiCatalogTarget)

-- | The name of the database to write to.
s3HudiCatalogTarget_database :: Lens.Lens' S3HudiCatalogTarget Prelude.Text
s3HudiCatalogTarget_database = Lens.lens (\S3HudiCatalogTarget' {database} -> database) (\s@S3HudiCatalogTarget' {} a -> s {database = a} :: S3HudiCatalogTarget)

-- | Specifies additional connection options for the connector.
s3HudiCatalogTarget_additionalOptions :: Lens.Lens' S3HudiCatalogTarget (Prelude.HashMap Prelude.Text Prelude.Text)
s3HudiCatalogTarget_additionalOptions = Lens.lens (\S3HudiCatalogTarget' {additionalOptions} -> additionalOptions) (\s@S3HudiCatalogTarget' {} a -> s {additionalOptions = a} :: S3HudiCatalogTarget) Prelude.. Lens.coerced

instance Data.FromJSON S3HudiCatalogTarget where
  parseJSON =
    Data.withObject
      "S3HudiCatalogTarget"
      ( \x ->
          S3HudiCatalogTarget'
            Prelude.<$> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable S3HudiCatalogTarget where
  hashWithSalt _salt S3HudiCatalogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` additionalOptions

instance Prelude.NFData S3HudiCatalogTarget where
  rnf S3HudiCatalogTarget' {..} =
    Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf additionalOptions

instance Data.ToJSON S3HudiCatalogTarget where
  toJSON S3HudiCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Table" Data..= table),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just
              ("AdditionalOptions" Data..= additionalOptions)
          ]
      )
