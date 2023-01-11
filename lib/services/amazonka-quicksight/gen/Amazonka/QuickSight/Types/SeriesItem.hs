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
-- Module      : Amazonka.QuickSight.Types.SeriesItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SeriesItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataFieldSeriesItem
import Amazonka.QuickSight.Types.FieldSeriesItem

-- | The series item configuration of a line chart.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newSeriesItem' smart constructor.
data SeriesItem = SeriesItem'
  { -- | The data field series item configuration of a line chart.
    dataFieldSeriesItem :: Prelude.Maybe DataFieldSeriesItem,
    -- | The field series item configuration of a line chart.
    fieldSeriesItem :: Prelude.Maybe FieldSeriesItem
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SeriesItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataFieldSeriesItem', 'seriesItem_dataFieldSeriesItem' - The data field series item configuration of a line chart.
--
-- 'fieldSeriesItem', 'seriesItem_fieldSeriesItem' - The field series item configuration of a line chart.
newSeriesItem ::
  SeriesItem
newSeriesItem =
  SeriesItem'
    { dataFieldSeriesItem = Prelude.Nothing,
      fieldSeriesItem = Prelude.Nothing
    }

-- | The data field series item configuration of a line chart.
seriesItem_dataFieldSeriesItem :: Lens.Lens' SeriesItem (Prelude.Maybe DataFieldSeriesItem)
seriesItem_dataFieldSeriesItem = Lens.lens (\SeriesItem' {dataFieldSeriesItem} -> dataFieldSeriesItem) (\s@SeriesItem' {} a -> s {dataFieldSeriesItem = a} :: SeriesItem)

-- | The field series item configuration of a line chart.
seriesItem_fieldSeriesItem :: Lens.Lens' SeriesItem (Prelude.Maybe FieldSeriesItem)
seriesItem_fieldSeriesItem = Lens.lens (\SeriesItem' {fieldSeriesItem} -> fieldSeriesItem) (\s@SeriesItem' {} a -> s {fieldSeriesItem = a} :: SeriesItem)

instance Data.FromJSON SeriesItem where
  parseJSON =
    Data.withObject
      "SeriesItem"
      ( \x ->
          SeriesItem'
            Prelude.<$> (x Data..:? "DataFieldSeriesItem")
            Prelude.<*> (x Data..:? "FieldSeriesItem")
      )

instance Prelude.Hashable SeriesItem where
  hashWithSalt _salt SeriesItem' {..} =
    _salt `Prelude.hashWithSalt` dataFieldSeriesItem
      `Prelude.hashWithSalt` fieldSeriesItem

instance Prelude.NFData SeriesItem where
  rnf SeriesItem' {..} =
    Prelude.rnf dataFieldSeriesItem
      `Prelude.seq` Prelude.rnf fieldSeriesItem

instance Data.ToJSON SeriesItem where
  toJSON SeriesItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataFieldSeriesItem" Data..=)
              Prelude.<$> dataFieldSeriesItem,
            ("FieldSeriesItem" Data..=)
              Prelude.<$> fieldSeriesItem
          ]
      )
