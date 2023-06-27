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
-- Module      : Amazonka.Glue.Types.S3DeltaCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3DeltaCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CatalogSchemaChangePolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that writes to a Delta Lake data source in the Glue
-- Data Catalog.
--
-- /See:/ 'newS3DeltaCatalogTarget' smart constructor.
data S3DeltaCatalogTarget = S3DeltaCatalogTarget'
  { -- | Specifies additional connection options for the connector.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies native partitioning using a sequence of keys.
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
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DeltaCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOptions', 's3DeltaCatalogTarget_additionalOptions' - Specifies additional connection options for the connector.
--
-- 'partitionKeys', 's3DeltaCatalogTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'schemaChangePolicy', 's3DeltaCatalogTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'name', 's3DeltaCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 's3DeltaCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'table', 's3DeltaCatalogTarget_table' - The name of the table in the database to write to.
--
-- 'database', 's3DeltaCatalogTarget_database' - The name of the database to write to.
newS3DeltaCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  S3DeltaCatalogTarget
newS3DeltaCatalogTarget
  pName_
  pInputs_
  pTable_
  pDatabase_ =
    S3DeltaCatalogTarget'
      { additionalOptions =
          Prelude.Nothing,
        partitionKeys = Prelude.Nothing,
        schemaChangePolicy = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        table = pTable_,
        database = pDatabase_
      }

-- | Specifies additional connection options for the connector.
s3DeltaCatalogTarget_additionalOptions :: Lens.Lens' S3DeltaCatalogTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3DeltaCatalogTarget_additionalOptions = Lens.lens (\S3DeltaCatalogTarget' {additionalOptions} -> additionalOptions) (\s@S3DeltaCatalogTarget' {} a -> s {additionalOptions = a} :: S3DeltaCatalogTarget) Prelude.. Lens.mapping Lens.coerced

-- | Specifies native partitioning using a sequence of keys.
s3DeltaCatalogTarget_partitionKeys :: Lens.Lens' S3DeltaCatalogTarget (Prelude.Maybe [[Prelude.Text]])
s3DeltaCatalogTarget_partitionKeys = Lens.lens (\S3DeltaCatalogTarget' {partitionKeys} -> partitionKeys) (\s@S3DeltaCatalogTarget' {} a -> s {partitionKeys = a} :: S3DeltaCatalogTarget) Prelude.. Lens.mapping Lens.coerced

-- | A policy that specifies update behavior for the crawler.
s3DeltaCatalogTarget_schemaChangePolicy :: Lens.Lens' S3DeltaCatalogTarget (Prelude.Maybe CatalogSchemaChangePolicy)
s3DeltaCatalogTarget_schemaChangePolicy = Lens.lens (\S3DeltaCatalogTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3DeltaCatalogTarget' {} a -> s {schemaChangePolicy = a} :: S3DeltaCatalogTarget)

-- | The name of the data target.
s3DeltaCatalogTarget_name :: Lens.Lens' S3DeltaCatalogTarget Prelude.Text
s3DeltaCatalogTarget_name = Lens.lens (\S3DeltaCatalogTarget' {name} -> name) (\s@S3DeltaCatalogTarget' {} a -> s {name = a} :: S3DeltaCatalogTarget)

-- | The nodes that are inputs to the data target.
s3DeltaCatalogTarget_inputs :: Lens.Lens' S3DeltaCatalogTarget (Prelude.NonEmpty Prelude.Text)
s3DeltaCatalogTarget_inputs = Lens.lens (\S3DeltaCatalogTarget' {inputs} -> inputs) (\s@S3DeltaCatalogTarget' {} a -> s {inputs = a} :: S3DeltaCatalogTarget) Prelude.. Lens.coerced

-- | The name of the table in the database to write to.
s3DeltaCatalogTarget_table :: Lens.Lens' S3DeltaCatalogTarget Prelude.Text
s3DeltaCatalogTarget_table = Lens.lens (\S3DeltaCatalogTarget' {table} -> table) (\s@S3DeltaCatalogTarget' {} a -> s {table = a} :: S3DeltaCatalogTarget)

-- | The name of the database to write to.
s3DeltaCatalogTarget_database :: Lens.Lens' S3DeltaCatalogTarget Prelude.Text
s3DeltaCatalogTarget_database = Lens.lens (\S3DeltaCatalogTarget' {database} -> database) (\s@S3DeltaCatalogTarget' {} a -> s {database = a} :: S3DeltaCatalogTarget)

instance Data.FromJSON S3DeltaCatalogTarget where
  parseJSON =
    Data.withObject
      "S3DeltaCatalogTarget"
      ( \x ->
          S3DeltaCatalogTarget'
            Prelude.<$> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable S3DeltaCatalogTarget where
  hashWithSalt _salt S3DeltaCatalogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database

instance Prelude.NFData S3DeltaCatalogTarget where
  rnf S3DeltaCatalogTarget' {..} =
    Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON S3DeltaCatalogTarget where
  toJSON S3DeltaCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Table" Data..= table),
            Prelude.Just ("Database" Data..= database)
          ]
      )
