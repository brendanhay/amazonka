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
-- Module      : Amazonka.QuickSight.Types.CellValueSynonym
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CellValueSynonym where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents the cell value synonym.
--
-- /See:/ 'newCellValueSynonym' smart constructor.
data CellValueSynonym = CellValueSynonym'
  { -- | The cell value.
    cellValue :: Prelude.Maybe Prelude.Text,
    -- | Other names or aliases for the cell value.
    synonyms :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CellValueSynonym' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellValue', 'cellValueSynonym_cellValue' - The cell value.
--
-- 'synonyms', 'cellValueSynonym_synonyms' - Other names or aliases for the cell value.
newCellValueSynonym ::
  CellValueSynonym
newCellValueSynonym =
  CellValueSynonym'
    { cellValue = Prelude.Nothing,
      synonyms = Prelude.Nothing
    }

-- | The cell value.
cellValueSynonym_cellValue :: Lens.Lens' CellValueSynonym (Prelude.Maybe Prelude.Text)
cellValueSynonym_cellValue = Lens.lens (\CellValueSynonym' {cellValue} -> cellValue) (\s@CellValueSynonym' {} a -> s {cellValue = a} :: CellValueSynonym)

-- | Other names or aliases for the cell value.
cellValueSynonym_synonyms :: Lens.Lens' CellValueSynonym (Prelude.Maybe [Prelude.Text])
cellValueSynonym_synonyms = Lens.lens (\CellValueSynonym' {synonyms} -> synonyms) (\s@CellValueSynonym' {} a -> s {synonyms = a} :: CellValueSynonym) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CellValueSynonym where
  parseJSON =
    Data.withObject
      "CellValueSynonym"
      ( \x ->
          CellValueSynonym'
            Prelude.<$> (x Data..:? "CellValue")
            Prelude.<*> (x Data..:? "Synonyms" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CellValueSynonym where
  hashWithSalt _salt CellValueSynonym' {..} =
    _salt
      `Prelude.hashWithSalt` cellValue
      `Prelude.hashWithSalt` synonyms

instance Prelude.NFData CellValueSynonym where
  rnf CellValueSynonym' {..} =
    Prelude.rnf cellValue
      `Prelude.seq` Prelude.rnf synonyms

instance Data.ToJSON CellValueSynonym where
  toJSON CellValueSynonym' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CellValue" Data..=) Prelude.<$> cellValue,
            ("Synonyms" Data..=) Prelude.<$> synonyms
          ]
      )
