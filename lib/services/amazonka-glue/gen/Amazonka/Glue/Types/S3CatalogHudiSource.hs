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
-- Module      : Amazonka.Glue.Types.S3CatalogHudiSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3CatalogHudiSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Hudi data source that is registered in the Glue Data
-- Catalog. The Hudi data source must be stored in Amazon S3.
--
-- /See:/ 'newS3CatalogHudiSource' smart constructor.
data S3CatalogHudiSource = S3CatalogHudiSource'
  { -- | Specifies additional connection options.
    additionalHudiOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the data schema for the Hudi source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the Hudi data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3CatalogHudiSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalHudiOptions', 's3CatalogHudiSource_additionalHudiOptions' - Specifies additional connection options.
--
-- 'outputSchemas', 's3CatalogHudiSource_outputSchemas' - Specifies the data schema for the Hudi source.
--
-- 'name', 's3CatalogHudiSource_name' - The name of the Hudi data source.
--
-- 'database', 's3CatalogHudiSource_database' - The name of the database to read from.
--
-- 'table', 's3CatalogHudiSource_table' - The name of the table in the database to read from.
newS3CatalogHudiSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  S3CatalogHudiSource
newS3CatalogHudiSource pName_ pDatabase_ pTable_ =
  S3CatalogHudiSource'
    { additionalHudiOptions =
        Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Specifies additional connection options.
s3CatalogHudiSource_additionalHudiOptions :: Lens.Lens' S3CatalogHudiSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3CatalogHudiSource_additionalHudiOptions = Lens.lens (\S3CatalogHudiSource' {additionalHudiOptions} -> additionalHudiOptions) (\s@S3CatalogHudiSource' {} a -> s {additionalHudiOptions = a} :: S3CatalogHudiSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data schema for the Hudi source.
s3CatalogHudiSource_outputSchemas :: Lens.Lens' S3CatalogHudiSource (Prelude.Maybe [GlueSchema])
s3CatalogHudiSource_outputSchemas = Lens.lens (\S3CatalogHudiSource' {outputSchemas} -> outputSchemas) (\s@S3CatalogHudiSource' {} a -> s {outputSchemas = a} :: S3CatalogHudiSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Hudi data source.
s3CatalogHudiSource_name :: Lens.Lens' S3CatalogHudiSource Prelude.Text
s3CatalogHudiSource_name = Lens.lens (\S3CatalogHudiSource' {name} -> name) (\s@S3CatalogHudiSource' {} a -> s {name = a} :: S3CatalogHudiSource)

-- | The name of the database to read from.
s3CatalogHudiSource_database :: Lens.Lens' S3CatalogHudiSource Prelude.Text
s3CatalogHudiSource_database = Lens.lens (\S3CatalogHudiSource' {database} -> database) (\s@S3CatalogHudiSource' {} a -> s {database = a} :: S3CatalogHudiSource)

-- | The name of the table in the database to read from.
s3CatalogHudiSource_table :: Lens.Lens' S3CatalogHudiSource Prelude.Text
s3CatalogHudiSource_table = Lens.lens (\S3CatalogHudiSource' {table} -> table) (\s@S3CatalogHudiSource' {} a -> s {table = a} :: S3CatalogHudiSource)

instance Data.FromJSON S3CatalogHudiSource where
  parseJSON =
    Data.withObject
      "S3CatalogHudiSource"
      ( \x ->
          S3CatalogHudiSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalHudiOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable S3CatalogHudiSource where
  hashWithSalt _salt S3CatalogHudiSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalHudiOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData S3CatalogHudiSource where
  rnf S3CatalogHudiSource' {..} =
    Prelude.rnf additionalHudiOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON S3CatalogHudiSource where
  toJSON S3CatalogHudiSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalHudiOptions" Data..=)
              Prelude.<$> additionalHudiOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
