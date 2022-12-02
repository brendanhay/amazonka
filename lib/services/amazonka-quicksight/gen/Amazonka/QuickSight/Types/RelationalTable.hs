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
-- Module      : Amazonka.QuickSight.Types.RelationalTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RelationalTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.InputColumn

-- | A physical table type for relational data sources.
--
-- /See:/ 'newRelationalTable' smart constructor.
data RelationalTable = RelationalTable'
  { -- | The catalog associated with a table.
    catalog :: Prelude.Maybe Prelude.Text,
    -- | The schema name. This name applies to certain relational database
    -- engines.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the data source.
    dataSourceArn :: Prelude.Text,
    -- | The name of the relational table.
    name :: Prelude.Text,
    -- | The column schema of the table.
    inputColumns :: Prelude.NonEmpty InputColumn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalog', 'relationalTable_catalog' - The catalog associated with a table.
--
-- 'schema', 'relationalTable_schema' - The schema name. This name applies to certain relational database
-- engines.
--
-- 'dataSourceArn', 'relationalTable_dataSourceArn' - The Amazon Resource Name (ARN) for the data source.
--
-- 'name', 'relationalTable_name' - The name of the relational table.
--
-- 'inputColumns', 'relationalTable_inputColumns' - The column schema of the table.
newRelationalTable ::
  -- | 'dataSourceArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'inputColumns'
  Prelude.NonEmpty InputColumn ->
  RelationalTable
newRelationalTable
  pDataSourceArn_
  pName_
  pInputColumns_ =
    RelationalTable'
      { catalog = Prelude.Nothing,
        schema = Prelude.Nothing,
        dataSourceArn = pDataSourceArn_,
        name = pName_,
        inputColumns = Lens.coerced Lens.# pInputColumns_
      }

-- | The catalog associated with a table.
relationalTable_catalog :: Lens.Lens' RelationalTable (Prelude.Maybe Prelude.Text)
relationalTable_catalog = Lens.lens (\RelationalTable' {catalog} -> catalog) (\s@RelationalTable' {} a -> s {catalog = a} :: RelationalTable)

-- | The schema name. This name applies to certain relational database
-- engines.
relationalTable_schema :: Lens.Lens' RelationalTable (Prelude.Maybe Prelude.Text)
relationalTable_schema = Lens.lens (\RelationalTable' {schema} -> schema) (\s@RelationalTable' {} a -> s {schema = a} :: RelationalTable)

-- | The Amazon Resource Name (ARN) for the data source.
relationalTable_dataSourceArn :: Lens.Lens' RelationalTable Prelude.Text
relationalTable_dataSourceArn = Lens.lens (\RelationalTable' {dataSourceArn} -> dataSourceArn) (\s@RelationalTable' {} a -> s {dataSourceArn = a} :: RelationalTable)

-- | The name of the relational table.
relationalTable_name :: Lens.Lens' RelationalTable Prelude.Text
relationalTable_name = Lens.lens (\RelationalTable' {name} -> name) (\s@RelationalTable' {} a -> s {name = a} :: RelationalTable)

-- | The column schema of the table.
relationalTable_inputColumns :: Lens.Lens' RelationalTable (Prelude.NonEmpty InputColumn)
relationalTable_inputColumns = Lens.lens (\RelationalTable' {inputColumns} -> inputColumns) (\s@RelationalTable' {} a -> s {inputColumns = a} :: RelationalTable) Prelude.. Lens.coerced

instance Data.FromJSON RelationalTable where
  parseJSON =
    Data.withObject
      "RelationalTable"
      ( \x ->
          RelationalTable'
            Prelude.<$> (x Data..:? "Catalog")
            Prelude.<*> (x Data..:? "Schema")
            Prelude.<*> (x Data..: "DataSourceArn")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "InputColumns")
      )

instance Prelude.Hashable RelationalTable where
  hashWithSalt _salt RelationalTable' {..} =
    _salt `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` dataSourceArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputColumns

instance Prelude.NFData RelationalTable where
  rnf RelationalTable' {..} =
    Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf dataSourceArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputColumns

instance Data.ToJSON RelationalTable where
  toJSON RelationalTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Catalog" Data..=) Prelude.<$> catalog,
            ("Schema" Data..=) Prelude.<$> schema,
            Prelude.Just ("DataSourceArn" Data..= dataSourceArn),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("InputColumns" Data..= inputColumns)
          ]
      )
