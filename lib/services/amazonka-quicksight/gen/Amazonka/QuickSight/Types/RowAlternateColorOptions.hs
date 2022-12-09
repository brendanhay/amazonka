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
-- Module      : Amazonka.QuickSight.Types.RowAlternateColorOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowAlternateColorOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.WidgetStatus

-- | Determines the row alternate color options.
--
-- /See:/ 'newRowAlternateColorOptions' smart constructor.
data RowAlternateColorOptions = RowAlternateColorOptions'
  { -- | Determines the list of row alternate colors.
    rowAlternateColors :: Prelude.Maybe [Prelude.Text],
    -- | Determines the widget status.
    status :: Prelude.Maybe WidgetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RowAlternateColorOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowAlternateColors', 'rowAlternateColorOptions_rowAlternateColors' - Determines the list of row alternate colors.
--
-- 'status', 'rowAlternateColorOptions_status' - Determines the widget status.
newRowAlternateColorOptions ::
  RowAlternateColorOptions
newRowAlternateColorOptions =
  RowAlternateColorOptions'
    { rowAlternateColors =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Determines the list of row alternate colors.
rowAlternateColorOptions_rowAlternateColors :: Lens.Lens' RowAlternateColorOptions (Prelude.Maybe [Prelude.Text])
rowAlternateColorOptions_rowAlternateColors = Lens.lens (\RowAlternateColorOptions' {rowAlternateColors} -> rowAlternateColors) (\s@RowAlternateColorOptions' {} a -> s {rowAlternateColors = a} :: RowAlternateColorOptions) Prelude.. Lens.mapping Lens.coerced

-- | Determines the widget status.
rowAlternateColorOptions_status :: Lens.Lens' RowAlternateColorOptions (Prelude.Maybe WidgetStatus)
rowAlternateColorOptions_status = Lens.lens (\RowAlternateColorOptions' {status} -> status) (\s@RowAlternateColorOptions' {} a -> s {status = a} :: RowAlternateColorOptions)

instance Data.FromJSON RowAlternateColorOptions where
  parseJSON =
    Data.withObject
      "RowAlternateColorOptions"
      ( \x ->
          RowAlternateColorOptions'
            Prelude.<$> ( x Data..:? "RowAlternateColors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable RowAlternateColorOptions where
  hashWithSalt _salt RowAlternateColorOptions' {..} =
    _salt `Prelude.hashWithSalt` rowAlternateColors
      `Prelude.hashWithSalt` status

instance Prelude.NFData RowAlternateColorOptions where
  rnf RowAlternateColorOptions' {..} =
    Prelude.rnf rowAlternateColors
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON RowAlternateColorOptions where
  toJSON RowAlternateColorOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RowAlternateColors" Data..=)
              Prelude.<$> rowAlternateColors,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
