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
-- Module      : Amazonka.QuickSight.Types.TableInlineVisualization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableInlineVisualization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataBarsOptions

-- | The inline visualization of a specific type to display within a chart.
--
-- /See:/ 'newTableInlineVisualization' smart constructor.
data TableInlineVisualization = TableInlineVisualization'
  { -- | The configuration of the inline visualization of the data bars within a
    -- chart.
    dataBars :: Prelude.Maybe DataBarsOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableInlineVisualization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataBars', 'tableInlineVisualization_dataBars' - The configuration of the inline visualization of the data bars within a
-- chart.
newTableInlineVisualization ::
  TableInlineVisualization
newTableInlineVisualization =
  TableInlineVisualization'
    { dataBars =
        Prelude.Nothing
    }

-- | The configuration of the inline visualization of the data bars within a
-- chart.
tableInlineVisualization_dataBars :: Lens.Lens' TableInlineVisualization (Prelude.Maybe DataBarsOptions)
tableInlineVisualization_dataBars = Lens.lens (\TableInlineVisualization' {dataBars} -> dataBars) (\s@TableInlineVisualization' {} a -> s {dataBars = a} :: TableInlineVisualization)

instance Data.FromJSON TableInlineVisualization where
  parseJSON =
    Data.withObject
      "TableInlineVisualization"
      ( \x ->
          TableInlineVisualization'
            Prelude.<$> (x Data..:? "DataBars")
      )

instance Prelude.Hashable TableInlineVisualization where
  hashWithSalt _salt TableInlineVisualization' {..} =
    _salt `Prelude.hashWithSalt` dataBars

instance Prelude.NFData TableInlineVisualization where
  rnf TableInlineVisualization' {..} =
    Prelude.rnf dataBars

instance Data.ToJSON TableInlineVisualization where
  toJSON TableInlineVisualization' {..} =
    Data.object
      ( Prelude.catMaybes
          [("DataBars" Data..=) Prelude.<$> dataBars]
      )
