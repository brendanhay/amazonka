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
-- Module      : Amazonka.QuickSight.Types.FieldFolder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldFolder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A FieldFolder element is a folder that contains fields and nested
-- subfolders.
--
-- /See:/ 'newFieldFolder' smart constructor.
data FieldFolder = FieldFolder'
  { -- | A folder has a list of columns. A column can only be in one folder.
    columns :: Prelude.Maybe [Prelude.Text],
    -- | The description for a field folder.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'fieldFolder_columns' - A folder has a list of columns. A column can only be in one folder.
--
-- 'description', 'fieldFolder_description' - The description for a field folder.
newFieldFolder ::
  FieldFolder
newFieldFolder =
  FieldFolder'
    { columns = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A folder has a list of columns. A column can only be in one folder.
fieldFolder_columns :: Lens.Lens' FieldFolder (Prelude.Maybe [Prelude.Text])
fieldFolder_columns = Lens.lens (\FieldFolder' {columns} -> columns) (\s@FieldFolder' {} a -> s {columns = a} :: FieldFolder) Prelude.. Lens.mapping Lens.coerced

-- | The description for a field folder.
fieldFolder_description :: Lens.Lens' FieldFolder (Prelude.Maybe Prelude.Text)
fieldFolder_description = Lens.lens (\FieldFolder' {description} -> description) (\s@FieldFolder' {} a -> s {description = a} :: FieldFolder)

instance Data.FromJSON FieldFolder where
  parseJSON =
    Data.withObject
      "FieldFolder"
      ( \x ->
          FieldFolder'
            Prelude.<$> (x Data..:? "columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable FieldFolder where
  hashWithSalt _salt FieldFolder' {..} =
    _salt `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` description

instance Prelude.NFData FieldFolder where
  rnf FieldFolder' {..} =
    Prelude.rnf columns
      `Prelude.seq` Prelude.rnf description

instance Data.ToJSON FieldFolder where
  toJSON FieldFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("columns" Data..=) Prelude.<$> columns,
            ("description" Data..=) Prelude.<$> description
          ]
      )
