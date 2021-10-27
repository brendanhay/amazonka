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
-- Module      : Network.AWS.DataBrew.Types.ColumnSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.ColumnSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Selector of a column from a dataset for profile job configuration. One
-- selector includes either a column name or a regular expression.
--
-- /See:/ 'newColumnSelector' smart constructor.
data ColumnSelector = ColumnSelector'
  { -- | A regular expression for selecting a column from a dataset.
    regex :: Prelude.Maybe Prelude.Text,
    -- | The name of a column from a dataset.
    name :: Prelude.Maybe Prelude.Text
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
-- 'regex', 'columnSelector_regex' - A regular expression for selecting a column from a dataset.
--
-- 'name', 'columnSelector_name' - The name of a column from a dataset.
newColumnSelector ::
  ColumnSelector
newColumnSelector =
  ColumnSelector'
    { regex = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A regular expression for selecting a column from a dataset.
columnSelector_regex :: Lens.Lens' ColumnSelector (Prelude.Maybe Prelude.Text)
columnSelector_regex = Lens.lens (\ColumnSelector' {regex} -> regex) (\s@ColumnSelector' {} a -> s {regex = a} :: ColumnSelector)

-- | The name of a column from a dataset.
columnSelector_name :: Lens.Lens' ColumnSelector (Prelude.Maybe Prelude.Text)
columnSelector_name = Lens.lens (\ColumnSelector' {name} -> name) (\s@ColumnSelector' {} a -> s {name = a} :: ColumnSelector)

instance Core.FromJSON ColumnSelector where
  parseJSON =
    Core.withObject
      "ColumnSelector"
      ( \x ->
          ColumnSelector'
            Prelude.<$> (x Core..:? "Regex") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable ColumnSelector

instance Prelude.NFData ColumnSelector

instance Core.ToJSON ColumnSelector where
  toJSON ColumnSelector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Regex" Core..=) Prelude.<$> regex,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
