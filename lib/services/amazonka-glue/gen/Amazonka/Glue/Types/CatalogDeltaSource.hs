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
-- Module      : Amazonka.Glue.Types.CatalogDeltaSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogDeltaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Delta Lake data source that is registered in the Glue Data
-- Catalog.
--
-- /See:/ 'newCatalogDeltaSource' smart constructor.
data CatalogDeltaSource = CatalogDeltaSource'
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
-- Create a value of 'CatalogDeltaSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDeltaOptions', 'catalogDeltaSource_additionalDeltaOptions' - Specifies additional connection options.
--
-- 'outputSchemas', 'catalogDeltaSource_outputSchemas' - Specifies the data schema for the Delta Lake source.
--
-- 'name', 'catalogDeltaSource_name' - The name of the Delta Lake data source.
--
-- 'database', 'catalogDeltaSource_database' - The name of the database to read from.
--
-- 'table', 'catalogDeltaSource_table' - The name of the table in the database to read from.
newCatalogDeltaSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  CatalogDeltaSource
newCatalogDeltaSource pName_ pDatabase_ pTable_ =
  CatalogDeltaSource'
    { additionalDeltaOptions =
        Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Specifies additional connection options.
catalogDeltaSource_additionalDeltaOptions :: Lens.Lens' CatalogDeltaSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
catalogDeltaSource_additionalDeltaOptions = Lens.lens (\CatalogDeltaSource' {additionalDeltaOptions} -> additionalDeltaOptions) (\s@CatalogDeltaSource' {} a -> s {additionalDeltaOptions = a} :: CatalogDeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data schema for the Delta Lake source.
catalogDeltaSource_outputSchemas :: Lens.Lens' CatalogDeltaSource (Prelude.Maybe [GlueSchema])
catalogDeltaSource_outputSchemas = Lens.lens (\CatalogDeltaSource' {outputSchemas} -> outputSchemas) (\s@CatalogDeltaSource' {} a -> s {outputSchemas = a} :: CatalogDeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Delta Lake data source.
catalogDeltaSource_name :: Lens.Lens' CatalogDeltaSource Prelude.Text
catalogDeltaSource_name = Lens.lens (\CatalogDeltaSource' {name} -> name) (\s@CatalogDeltaSource' {} a -> s {name = a} :: CatalogDeltaSource)

-- | The name of the database to read from.
catalogDeltaSource_database :: Lens.Lens' CatalogDeltaSource Prelude.Text
catalogDeltaSource_database = Lens.lens (\CatalogDeltaSource' {database} -> database) (\s@CatalogDeltaSource' {} a -> s {database = a} :: CatalogDeltaSource)

-- | The name of the table in the database to read from.
catalogDeltaSource_table :: Lens.Lens' CatalogDeltaSource Prelude.Text
catalogDeltaSource_table = Lens.lens (\CatalogDeltaSource' {table} -> table) (\s@CatalogDeltaSource' {} a -> s {table = a} :: CatalogDeltaSource)

instance Data.FromJSON CatalogDeltaSource where
  parseJSON =
    Data.withObject
      "CatalogDeltaSource"
      ( \x ->
          CatalogDeltaSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalDeltaOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable CatalogDeltaSource where
  hashWithSalt _salt CatalogDeltaSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDeltaOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData CatalogDeltaSource where
  rnf CatalogDeltaSource' {..} =
    Prelude.rnf additionalDeltaOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON CatalogDeltaSource where
  toJSON CatalogDeltaSource' {..} =
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
