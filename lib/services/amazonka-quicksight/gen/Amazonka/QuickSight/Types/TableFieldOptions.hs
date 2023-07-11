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
-- Module      : Amazonka.QuickSight.Types.TableFieldOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldOption

-- | The field options for a table visual.
--
-- /See:/ 'newTableFieldOptions' smart constructor.
data TableFieldOptions = TableFieldOptions'
  { -- | The order of field IDs of the field options for a table visual.
    order :: Prelude.Maybe [Prelude.Text],
    -- | The selected field options for the table field options.
    selectedFieldOptions :: Prelude.Maybe [TableFieldOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'tableFieldOptions_order' - The order of field IDs of the field options for a table visual.
--
-- 'selectedFieldOptions', 'tableFieldOptions_selectedFieldOptions' - The selected field options for the table field options.
newTableFieldOptions ::
  TableFieldOptions
newTableFieldOptions =
  TableFieldOptions'
    { order = Prelude.Nothing,
      selectedFieldOptions = Prelude.Nothing
    }

-- | The order of field IDs of the field options for a table visual.
tableFieldOptions_order :: Lens.Lens' TableFieldOptions (Prelude.Maybe [Prelude.Text])
tableFieldOptions_order = Lens.lens (\TableFieldOptions' {order} -> order) (\s@TableFieldOptions' {} a -> s {order = a} :: TableFieldOptions) Prelude.. Lens.mapping Lens.coerced

-- | The selected field options for the table field options.
tableFieldOptions_selectedFieldOptions :: Lens.Lens' TableFieldOptions (Prelude.Maybe [TableFieldOption])
tableFieldOptions_selectedFieldOptions = Lens.lens (\TableFieldOptions' {selectedFieldOptions} -> selectedFieldOptions) (\s@TableFieldOptions' {} a -> s {selectedFieldOptions = a} :: TableFieldOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableFieldOptions where
  parseJSON =
    Data.withObject
      "TableFieldOptions"
      ( \x ->
          TableFieldOptions'
            Prelude.<$> (x Data..:? "Order" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "SelectedFieldOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TableFieldOptions where
  hashWithSalt _salt TableFieldOptions' {..} =
    _salt
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` selectedFieldOptions

instance Prelude.NFData TableFieldOptions where
  rnf TableFieldOptions' {..} =
    Prelude.rnf order
      `Prelude.seq` Prelude.rnf selectedFieldOptions

instance Data.ToJSON TableFieldOptions where
  toJSON TableFieldOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Order" Data..=) Prelude.<$> order,
            ("SelectedFieldOptions" Data..=)
              Prelude.<$> selectedFieldOptions
          ]
      )
