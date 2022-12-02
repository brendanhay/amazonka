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
-- Module      : Amazonka.FinSpaceData.Types.SchemaDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.SchemaDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ColumnDefinition
import qualified Amazonka.Prelude as Prelude

-- | Definition for a schema on a tabular Dataset.
--
-- /See:/ 'newSchemaDefinition' smart constructor.
data SchemaDefinition = SchemaDefinition'
  { -- | List of column definitions.
    columns :: Prelude.Maybe [ColumnDefinition],
    -- | List of column names used for primary key.
    primaryKeyColumns :: Prelude.Maybe [Prelude.Text]
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
-- 'columns', 'schemaDefinition_columns' - List of column definitions.
--
-- 'primaryKeyColumns', 'schemaDefinition_primaryKeyColumns' - List of column names used for primary key.
newSchemaDefinition ::
  SchemaDefinition
newSchemaDefinition =
  SchemaDefinition'
    { columns = Prelude.Nothing,
      primaryKeyColumns = Prelude.Nothing
    }

-- | List of column definitions.
schemaDefinition_columns :: Lens.Lens' SchemaDefinition (Prelude.Maybe [ColumnDefinition])
schemaDefinition_columns = Lens.lens (\SchemaDefinition' {columns} -> columns) (\s@SchemaDefinition' {} a -> s {columns = a} :: SchemaDefinition) Prelude.. Lens.mapping Lens.coerced

-- | List of column names used for primary key.
schemaDefinition_primaryKeyColumns :: Lens.Lens' SchemaDefinition (Prelude.Maybe [Prelude.Text])
schemaDefinition_primaryKeyColumns = Lens.lens (\SchemaDefinition' {primaryKeyColumns} -> primaryKeyColumns) (\s@SchemaDefinition' {} a -> s {primaryKeyColumns = a} :: SchemaDefinition) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SchemaDefinition where
  parseJSON =
    Data.withObject
      "SchemaDefinition"
      ( \x ->
          SchemaDefinition'
            Prelude.<$> (x Data..:? "columns" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "primaryKeyColumns"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SchemaDefinition where
  hashWithSalt _salt SchemaDefinition' {..} =
    _salt `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` primaryKeyColumns

instance Prelude.NFData SchemaDefinition where
  rnf SchemaDefinition' {..} =
    Prelude.rnf columns
      `Prelude.seq` Prelude.rnf primaryKeyColumns

instance Data.ToJSON SchemaDefinition where
  toJSON SchemaDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("columns" Data..=) Prelude.<$> columns,
            ("primaryKeyColumns" Data..=)
              Prelude.<$> primaryKeyColumns
          ]
      )
