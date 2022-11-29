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
-- Module      : Amazonka.Glue.Types.SchemaColumn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair representing a column and data type that this transform
-- can run against. The @Schema@ parameter of the @MLTransform@ may contain
-- up to 100 of these structures.
--
-- /See:/ 'newSchemaColumn' smart constructor.
data SchemaColumn = SchemaColumn'
  { -- | The name of the column.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of data in the column.
    dataType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'schemaColumn_name' - The name of the column.
--
-- 'dataType', 'schemaColumn_dataType' - The type of data in the column.
newSchemaColumn ::
  SchemaColumn
newSchemaColumn =
  SchemaColumn'
    { name = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | The name of the column.
schemaColumn_name :: Lens.Lens' SchemaColumn (Prelude.Maybe Prelude.Text)
schemaColumn_name = Lens.lens (\SchemaColumn' {name} -> name) (\s@SchemaColumn' {} a -> s {name = a} :: SchemaColumn)

-- | The type of data in the column.
schemaColumn_dataType :: Lens.Lens' SchemaColumn (Prelude.Maybe Prelude.Text)
schemaColumn_dataType = Lens.lens (\SchemaColumn' {dataType} -> dataType) (\s@SchemaColumn' {} a -> s {dataType = a} :: SchemaColumn)

instance Core.FromJSON SchemaColumn where
  parseJSON =
    Core.withObject
      "SchemaColumn"
      ( \x ->
          SchemaColumn'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DataType")
      )

instance Prelude.Hashable SchemaColumn where
  hashWithSalt _salt SchemaColumn' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData SchemaColumn where
  rnf SchemaColumn' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf dataType

instance Core.ToJSON SchemaColumn where
  toJSON SchemaColumn' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("DataType" Core..=) Prelude.<$> dataType
          ]
      )
