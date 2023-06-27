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
-- Module      : Amazonka.Glue.Types.CatalogHudiSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogHudiSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Hudi data source that is registered in the Glue Data
-- Catalog.
--
-- /See:/ 'newCatalogHudiSource' smart constructor.
data CatalogHudiSource = CatalogHudiSource'
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
-- Create a value of 'CatalogHudiSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalHudiOptions', 'catalogHudiSource_additionalHudiOptions' - Specifies additional connection options.
--
-- 'outputSchemas', 'catalogHudiSource_outputSchemas' - Specifies the data schema for the Hudi source.
--
-- 'name', 'catalogHudiSource_name' - The name of the Hudi data source.
--
-- 'database', 'catalogHudiSource_database' - The name of the database to read from.
--
-- 'table', 'catalogHudiSource_table' - The name of the table in the database to read from.
newCatalogHudiSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  CatalogHudiSource
newCatalogHudiSource pName_ pDatabase_ pTable_ =
  CatalogHudiSource'
    { additionalHudiOptions =
        Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Specifies additional connection options.
catalogHudiSource_additionalHudiOptions :: Lens.Lens' CatalogHudiSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
catalogHudiSource_additionalHudiOptions = Lens.lens (\CatalogHudiSource' {additionalHudiOptions} -> additionalHudiOptions) (\s@CatalogHudiSource' {} a -> s {additionalHudiOptions = a} :: CatalogHudiSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data schema for the Hudi source.
catalogHudiSource_outputSchemas :: Lens.Lens' CatalogHudiSource (Prelude.Maybe [GlueSchema])
catalogHudiSource_outputSchemas = Lens.lens (\CatalogHudiSource' {outputSchemas} -> outputSchemas) (\s@CatalogHudiSource' {} a -> s {outputSchemas = a} :: CatalogHudiSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Hudi data source.
catalogHudiSource_name :: Lens.Lens' CatalogHudiSource Prelude.Text
catalogHudiSource_name = Lens.lens (\CatalogHudiSource' {name} -> name) (\s@CatalogHudiSource' {} a -> s {name = a} :: CatalogHudiSource)

-- | The name of the database to read from.
catalogHudiSource_database :: Lens.Lens' CatalogHudiSource Prelude.Text
catalogHudiSource_database = Lens.lens (\CatalogHudiSource' {database} -> database) (\s@CatalogHudiSource' {} a -> s {database = a} :: CatalogHudiSource)

-- | The name of the table in the database to read from.
catalogHudiSource_table :: Lens.Lens' CatalogHudiSource Prelude.Text
catalogHudiSource_table = Lens.lens (\CatalogHudiSource' {table} -> table) (\s@CatalogHudiSource' {} a -> s {table = a} :: CatalogHudiSource)

instance Data.FromJSON CatalogHudiSource where
  parseJSON =
    Data.withObject
      "CatalogHudiSource"
      ( \x ->
          CatalogHudiSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalHudiOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable CatalogHudiSource where
  hashWithSalt _salt CatalogHudiSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalHudiOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData CatalogHudiSource where
  rnf CatalogHudiSource' {..} =
    Prelude.rnf additionalHudiOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON CatalogHudiSource where
  toJSON CatalogHudiSource' {..} =
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
