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
-- Module      : Amazonka.DataBrew.Types.ColumnSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ColumnSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Selector of a column from a dataset for profile job configuration. One
-- selector includes either a column name or a regular expression.
--
-- /See:/ 'newColumnSelector' smart constructor.
data ColumnSelector = ColumnSelector'
  { -- | The name of a column from a dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | A regular expression for selecting a column from a dataset.
    regex :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnSelector_name' - The name of a column from a dataset.
--
-- 'regex', 'columnSelector_regex' - A regular expression for selecting a column from a dataset.
newColumnSelector ::
  ColumnSelector
newColumnSelector =
  ColumnSelector'
    { name = Prelude.Nothing,
      regex = Prelude.Nothing
    }

-- | The name of a column from a dataset.
columnSelector_name :: Lens.Lens' ColumnSelector (Prelude.Maybe Prelude.Text)
columnSelector_name = Lens.lens (\ColumnSelector' {name} -> name) (\s@ColumnSelector' {} a -> s {name = a} :: ColumnSelector)

-- | A regular expression for selecting a column from a dataset.
columnSelector_regex :: Lens.Lens' ColumnSelector (Prelude.Maybe Prelude.Text)
columnSelector_regex = Lens.lens (\ColumnSelector' {regex} -> regex) (\s@ColumnSelector' {} a -> s {regex = a} :: ColumnSelector)

instance Data.FromJSON ColumnSelector where
  parseJSON =
    Data.withObject
      "ColumnSelector"
      ( \x ->
          ColumnSelector'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Regex")
      )

instance Prelude.Hashable ColumnSelector where
  hashWithSalt _salt ColumnSelector' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regex

instance Prelude.NFData ColumnSelector where
  rnf ColumnSelector' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf regex

instance Data.ToJSON ColumnSelector where
  toJSON ColumnSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Regex" Data..=) Prelude.<$> regex
          ]
      )
