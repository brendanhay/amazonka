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
-- Module      : Network.AWS.QuickSight.Types.DataSetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSetConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.ColumnGroupSchema
import Network.AWS.QuickSight.Types.DataSetSchema

-- | Dataset configuration.
--
-- /See:/ 'newDataSetConfiguration' smart constructor.
data DataSetConfiguration = DataSetConfiguration'
  { -- | A structure containing the list of column group schemas.
    columnGroupSchemaList :: Prelude.Maybe [ColumnGroupSchema],
    -- | Dataset schema.
    dataSetSchema :: Prelude.Maybe DataSetSchema,
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
-- 'columnGroupSchemaList', 'dataSetConfiguration_columnGroupSchemaList' - A structure containing the list of column group schemas.
--
-- 'dataSetSchema', 'dataSetConfiguration_dataSetSchema' - Dataset schema.
--
-- 'placeholder', 'dataSetConfiguration_placeholder' - Placeholder.
newDataSetConfiguration ::
  DataSetConfiguration
newDataSetConfiguration =
  DataSetConfiguration'
    { columnGroupSchemaList =
        Prelude.Nothing,
      dataSetSchema = Prelude.Nothing,
      placeholder = Prelude.Nothing
    }

-- | A structure containing the list of column group schemas.
dataSetConfiguration_columnGroupSchemaList :: Lens.Lens' DataSetConfiguration (Prelude.Maybe [ColumnGroupSchema])
dataSetConfiguration_columnGroupSchemaList = Lens.lens (\DataSetConfiguration' {columnGroupSchemaList} -> columnGroupSchemaList) (\s@DataSetConfiguration' {} a -> s {columnGroupSchemaList = a} :: DataSetConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Dataset schema.
dataSetConfiguration_dataSetSchema :: Lens.Lens' DataSetConfiguration (Prelude.Maybe DataSetSchema)
dataSetConfiguration_dataSetSchema = Lens.lens (\DataSetConfiguration' {dataSetSchema} -> dataSetSchema) (\s@DataSetConfiguration' {} a -> s {dataSetSchema = a} :: DataSetConfiguration)

-- | Placeholder.
dataSetConfiguration_placeholder :: Lens.Lens' DataSetConfiguration (Prelude.Maybe Prelude.Text)
dataSetConfiguration_placeholder = Lens.lens (\DataSetConfiguration' {placeholder} -> placeholder) (\s@DataSetConfiguration' {} a -> s {placeholder = a} :: DataSetConfiguration)

instance Core.FromJSON DataSetConfiguration where
  parseJSON =
    Core.withObject
      "DataSetConfiguration"
      ( \x ->
          DataSetConfiguration'
            Prelude.<$> ( x Core..:? "ColumnGroupSchemaList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DataSetSchema")
            Prelude.<*> (x Core..:? "Placeholder")
      )

instance Prelude.Hashable DataSetConfiguration

instance Prelude.NFData DataSetConfiguration
