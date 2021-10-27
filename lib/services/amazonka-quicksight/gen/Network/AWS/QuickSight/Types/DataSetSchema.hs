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
-- Module      : Network.AWS.QuickSight.Types.DataSetSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSetSchema where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.ColumnSchema

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

instance Core.FromJSON DataSetSchema where
  parseJSON =
    Core.withObject
      "DataSetSchema"
      ( \x ->
          DataSetSchema'
            Prelude.<$> ( x Core..:? "ColumnSchemaList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataSetSchema

instance Prelude.NFData DataSetSchema
