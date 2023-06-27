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
-- Module      : Amazonka.Glue.Types.S3CatalogDeltaSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3CatalogDeltaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Delta Lake data source that is registered in the Glue Data
-- Catalog. The data source must be stored in Amazon S3.
--
-- /See:/ 'newS3CatalogDeltaSource' smart constructor.
data S3CatalogDeltaSource = S3CatalogDeltaSource'
  { -- | Specifies additional connection options.
    additionalDeltaOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the data schema for the Delta Lake source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the Delta Lake data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3CatalogDeltaSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDeltaOptions', 's3CatalogDeltaSource_additionalDeltaOptions' - Specifies additional connection options.
--
-- 'outputSchemas', 's3CatalogDeltaSource_outputSchemas' - Specifies the data schema for the Delta Lake source.
--
-- 'name', 's3CatalogDeltaSource_name' - The name of the Delta Lake data source.
--
-- 'database', 's3CatalogDeltaSource_database' - The name of the database to read from.
--
-- 'table', 's3CatalogDeltaSource_table' - The name of the table in the database to read from.
newS3CatalogDeltaSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  S3CatalogDeltaSource
newS3CatalogDeltaSource pName_ pDatabase_ pTable_ =
  S3CatalogDeltaSource'
    { additionalDeltaOptions =
        Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Specifies additional connection options.
s3CatalogDeltaSource_additionalDeltaOptions :: Lens.Lens' S3CatalogDeltaSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3CatalogDeltaSource_additionalDeltaOptions = Lens.lens (\S3CatalogDeltaSource' {additionalDeltaOptions} -> additionalDeltaOptions) (\s@S3CatalogDeltaSource' {} a -> s {additionalDeltaOptions = a} :: S3CatalogDeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data schema for the Delta Lake source.
s3CatalogDeltaSource_outputSchemas :: Lens.Lens' S3CatalogDeltaSource (Prelude.Maybe [GlueSchema])
s3CatalogDeltaSource_outputSchemas = Lens.lens (\S3CatalogDeltaSource' {outputSchemas} -> outputSchemas) (\s@S3CatalogDeltaSource' {} a -> s {outputSchemas = a} :: S3CatalogDeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Delta Lake data source.
s3CatalogDeltaSource_name :: Lens.Lens' S3CatalogDeltaSource Prelude.Text
s3CatalogDeltaSource_name = Lens.lens (\S3CatalogDeltaSource' {name} -> name) (\s@S3CatalogDeltaSource' {} a -> s {name = a} :: S3CatalogDeltaSource)

-- | The name of the database to read from.
s3CatalogDeltaSource_database :: Lens.Lens' S3CatalogDeltaSource Prelude.Text
s3CatalogDeltaSource_database = Lens.lens (\S3CatalogDeltaSource' {database} -> database) (\s@S3CatalogDeltaSource' {} a -> s {database = a} :: S3CatalogDeltaSource)

-- | The name of the table in the database to read from.
s3CatalogDeltaSource_table :: Lens.Lens' S3CatalogDeltaSource Prelude.Text
s3CatalogDeltaSource_table = Lens.lens (\S3CatalogDeltaSource' {table} -> table) (\s@S3CatalogDeltaSource' {} a -> s {table = a} :: S3CatalogDeltaSource)

instance Data.FromJSON S3CatalogDeltaSource where
  parseJSON =
    Data.withObject
      "S3CatalogDeltaSource"
      ( \x ->
          S3CatalogDeltaSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalDeltaOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable S3CatalogDeltaSource where
  hashWithSalt _salt S3CatalogDeltaSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDeltaOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData S3CatalogDeltaSource where
  rnf S3CatalogDeltaSource' {..} =
    Prelude.rnf additionalDeltaOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON S3CatalogDeltaSource where
  toJSON S3CatalogDeltaSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalDeltaOptions" Data..=)
              Prelude.<$> additionalDeltaOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
