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
-- Module      : Amazonka.QuickSight.Types.ColumnGroupSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnGroupSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnGroupColumnSchema

-- | The column group schema.
--
-- /See:/ 'newColumnGroupSchema' smart constructor.
data ColumnGroupSchema = ColumnGroupSchema'
  { -- | A structure containing the list of schemas for column group columns.
    columnGroupColumnSchemaList :: Prelude.Maybe [ColumnGroupColumnSchema],
    -- | The name of the column group schema.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnGroupSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnGroupColumnSchemaList', 'columnGroupSchema_columnGroupColumnSchemaList' - A structure containing the list of schemas for column group columns.
--
-- 'name', 'columnGroupSchema_name' - The name of the column group schema.
newColumnGroupSchema ::
  ColumnGroupSchema
newColumnGroupSchema =
  ColumnGroupSchema'
    { columnGroupColumnSchemaList =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A structure containing the list of schemas for column group columns.
columnGroupSchema_columnGroupColumnSchemaList :: Lens.Lens' ColumnGroupSchema (Prelude.Maybe [ColumnGroupColumnSchema])
columnGroupSchema_columnGroupColumnSchemaList = Lens.lens (\ColumnGroupSchema' {columnGroupColumnSchemaList} -> columnGroupColumnSchemaList) (\s@ColumnGroupSchema' {} a -> s {columnGroupColumnSchemaList = a} :: ColumnGroupSchema) Prelude.. Lens.mapping Lens.coerced

-- | The name of the column group schema.
columnGroupSchema_name :: Lens.Lens' ColumnGroupSchema (Prelude.Maybe Prelude.Text)
columnGroupSchema_name = Lens.lens (\ColumnGroupSchema' {name} -> name) (\s@ColumnGroupSchema' {} a -> s {name = a} :: ColumnGroupSchema)

instance Data.FromJSON ColumnGroupSchema where
  parseJSON =
    Data.withObject
      "ColumnGroupSchema"
      ( \x ->
          ColumnGroupSchema'
            Prelude.<$> ( x Data..:? "ColumnGroupColumnSchemaList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ColumnGroupSchema where
  hashWithSalt _salt ColumnGroupSchema' {..} =
    _salt
      `Prelude.hashWithSalt` columnGroupColumnSchemaList
      `Prelude.hashWithSalt` name

instance Prelude.NFData ColumnGroupSchema where
  rnf ColumnGroupSchema' {..} =
    Prelude.rnf columnGroupColumnSchemaList
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON ColumnGroupSchema where
  toJSON ColumnGroupSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnGroupColumnSchemaList" Data..=)
              Prelude.<$> columnGroupColumnSchemaList,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
