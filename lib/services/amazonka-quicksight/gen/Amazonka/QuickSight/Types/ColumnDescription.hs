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
-- Module      : Amazonka.QuickSight.Types.ColumnDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata that contains a description for a column.
--
-- /See:/ 'newColumnDescription' smart constructor.
data ColumnDescription = ColumnDescription'
  { -- | The text of a description for a column.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'columnDescription_text' - The text of a description for a column.
newColumnDescription ::
  ColumnDescription
newColumnDescription =
  ColumnDescription' {text = Prelude.Nothing}

-- | The text of a description for a column.
columnDescription_text :: Lens.Lens' ColumnDescription (Prelude.Maybe Prelude.Text)
columnDescription_text = Lens.lens (\ColumnDescription' {text} -> text) (\s@ColumnDescription' {} a -> s {text = a} :: ColumnDescription)

instance Data.FromJSON ColumnDescription where
  parseJSON =
    Data.withObject
      "ColumnDescription"
      ( \x ->
          ColumnDescription' Prelude.<$> (x Data..:? "Text")
      )

instance Prelude.Hashable ColumnDescription where
  hashWithSalt _salt ColumnDescription' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData ColumnDescription where
  rnf ColumnDescription' {..} = Prelude.rnf text

instance Data.ToJSON ColumnDescription where
  toJSON ColumnDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Text" Data..=) Prelude.<$> text]
      )
