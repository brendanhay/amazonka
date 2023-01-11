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
-- Module      : Amazonka.QuickSight.Types.PivotTableDataPathOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableDataPathOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathValue

-- | The data path options for the pivot table field options.
--
-- /See:/ 'newPivotTableDataPathOption' smart constructor.
data PivotTableDataPathOption = PivotTableDataPathOption'
  { -- | The width of the data path option.
    width :: Prelude.Maybe Prelude.Text,
    -- | The list of data path values for the data path options.
    dataPathList :: [DataPathValue]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableDataPathOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'width', 'pivotTableDataPathOption_width' - The width of the data path option.
--
-- 'dataPathList', 'pivotTableDataPathOption_dataPathList' - The list of data path values for the data path options.
newPivotTableDataPathOption ::
  PivotTableDataPathOption
newPivotTableDataPathOption =
  PivotTableDataPathOption'
    { width = Prelude.Nothing,
      dataPathList = Prelude.mempty
    }

-- | The width of the data path option.
pivotTableDataPathOption_width :: Lens.Lens' PivotTableDataPathOption (Prelude.Maybe Prelude.Text)
pivotTableDataPathOption_width = Lens.lens (\PivotTableDataPathOption' {width} -> width) (\s@PivotTableDataPathOption' {} a -> s {width = a} :: PivotTableDataPathOption)

-- | The list of data path values for the data path options.
pivotTableDataPathOption_dataPathList :: Lens.Lens' PivotTableDataPathOption [DataPathValue]
pivotTableDataPathOption_dataPathList = Lens.lens (\PivotTableDataPathOption' {dataPathList} -> dataPathList) (\s@PivotTableDataPathOption' {} a -> s {dataPathList = a} :: PivotTableDataPathOption) Prelude.. Lens.coerced

instance Data.FromJSON PivotTableDataPathOption where
  parseJSON =
    Data.withObject
      "PivotTableDataPathOption"
      ( \x ->
          PivotTableDataPathOption'
            Prelude.<$> (x Data..:? "Width")
            Prelude.<*> (x Data..:? "DataPathList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PivotTableDataPathOption where
  hashWithSalt _salt PivotTableDataPathOption' {..} =
    _salt `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` dataPathList

instance Prelude.NFData PivotTableDataPathOption where
  rnf PivotTableDataPathOption' {..} =
    Prelude.rnf width
      `Prelude.seq` Prelude.rnf dataPathList

instance Data.ToJSON PivotTableDataPathOption where
  toJSON PivotTableDataPathOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Width" Data..=) Prelude.<$> width,
            Prelude.Just ("DataPathList" Data..= dataPathList)
          ]
      )
