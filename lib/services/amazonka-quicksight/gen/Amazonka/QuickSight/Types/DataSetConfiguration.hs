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
-- Module      : Amazonka.QuickSight.Types.DataSetConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnGroupSchema
import Amazonka.QuickSight.Types.DataSetSchema

-- | Dataset configuration.
--
-- /See:/ 'newDataSetConfiguration' smart constructor.
data DataSetConfiguration = DataSetConfiguration'
  { -- | Dataset schema.
    dataSetSchema :: Prelude.Maybe DataSetSchema,
    -- | A structure containing the list of column group schemas.
    columnGroupSchemaList :: Prelude.Maybe [ColumnGroupSchema],
    -- | Placeholder.
    placeholder :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetSchema', 'dataSetConfiguration_dataSetSchema' - Dataset schema.
--
-- 'columnGroupSchemaList', 'dataSetConfiguration_columnGroupSchemaList' - A structure containing the list of column group schemas.
--
-- 'placeholder', 'dataSetConfiguration_placeholder' - Placeholder.
newDataSetConfiguration ::
  DataSetConfiguration
newDataSetConfiguration =
  DataSetConfiguration'
    { dataSetSchema =
        Prelude.Nothing,
      columnGroupSchemaList = Prelude.Nothing,
      placeholder = Prelude.Nothing
    }

-- | Dataset schema.
dataSetConfiguration_dataSetSchema :: Lens.Lens' DataSetConfiguration (Prelude.Maybe DataSetSchema)
dataSetConfiguration_dataSetSchema = Lens.lens (\DataSetConfiguration' {dataSetSchema} -> dataSetSchema) (\s@DataSetConfiguration' {} a -> s {dataSetSchema = a} :: DataSetConfiguration)

-- | A structure containing the list of column group schemas.
dataSetConfiguration_columnGroupSchemaList :: Lens.Lens' DataSetConfiguration (Prelude.Maybe [ColumnGroupSchema])
dataSetConfiguration_columnGroupSchemaList = Lens.lens (\DataSetConfiguration' {columnGroupSchemaList} -> columnGroupSchemaList) (\s@DataSetConfiguration' {} a -> s {columnGroupSchemaList = a} :: DataSetConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Placeholder.
dataSetConfiguration_placeholder :: Lens.Lens' DataSetConfiguration (Prelude.Maybe Prelude.Text)
dataSetConfiguration_placeholder = Lens.lens (\DataSetConfiguration' {placeholder} -> placeholder) (\s@DataSetConfiguration' {} a -> s {placeholder = a} :: DataSetConfiguration)

instance Data.FromJSON DataSetConfiguration where
  parseJSON =
    Data.withObject
      "DataSetConfiguration"
      ( \x ->
          DataSetConfiguration'
            Prelude.<$> (x Data..:? "DataSetSchema")
            Prelude.<*> ( x Data..:? "ColumnGroupSchemaList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Placeholder")
      )

instance Prelude.Hashable DataSetConfiguration where
  hashWithSalt _salt DataSetConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dataSetSchema
      `Prelude.hashWithSalt` columnGroupSchemaList
      `Prelude.hashWithSalt` placeholder

instance Prelude.NFData DataSetConfiguration where
  rnf DataSetConfiguration' {..} =
    Prelude.rnf dataSetSchema
      `Prelude.seq` Prelude.rnf columnGroupSchemaList
      `Prelude.seq` Prelude.rnf placeholder
