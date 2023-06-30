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
-- Module      : Amazonka.QuickSight.Types.DataSetSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnSchema

-- | Dataset schema.
--
-- /See:/ 'newDataSetSchema' smart constructor.
data DataSetSchema = DataSetSchema'
  { -- | A structure containing the list of column schemas.
    columnSchemaList :: Prelude.Maybe [ColumnSchema]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnSchemaList', 'dataSetSchema_columnSchemaList' - A structure containing the list of column schemas.
newDataSetSchema ::
  DataSetSchema
newDataSetSchema =
  DataSetSchema' {columnSchemaList = Prelude.Nothing}

-- | A structure containing the list of column schemas.
dataSetSchema_columnSchemaList :: Lens.Lens' DataSetSchema (Prelude.Maybe [ColumnSchema])
dataSetSchema_columnSchemaList = Lens.lens (\DataSetSchema' {columnSchemaList} -> columnSchemaList) (\s@DataSetSchema' {} a -> s {columnSchemaList = a} :: DataSetSchema) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataSetSchema where
  parseJSON =
    Data.withObject
      "DataSetSchema"
      ( \x ->
          DataSetSchema'
            Prelude.<$> ( x
                            Data..:? "ColumnSchemaList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataSetSchema where
  hashWithSalt _salt DataSetSchema' {..} =
    _salt `Prelude.hashWithSalt` columnSchemaList

instance Prelude.NFData DataSetSchema where
  rnf DataSetSchema' {..} = Prelude.rnf columnSchemaList

instance Data.ToJSON DataSetSchema where
  toJSON DataSetSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnSchemaList" Data..=)
              Prelude.<$> columnSchemaList
          ]
      )
