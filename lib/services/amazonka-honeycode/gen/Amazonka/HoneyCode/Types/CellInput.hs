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
-- Module      : Amazonka.HoneyCode.Types.CellInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.CellInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CellInput object contains the data needed to create or update cells in a
-- table.
--
-- CellInput object has only a facts field or a fact field, but not both. A
-- 400 bad request will be thrown if both fact and facts field are present.
--
-- /See:/ 'newCellInput' smart constructor.
data CellInput = CellInput'
  { -- | A list representing the values that are entered into a ROWSET cell.
    -- Facts list can have either only values or rowIDs, and rowIDs should from
    -- the same table.
    facts :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | Fact represents the data that is entered into a cell. This data can be
    -- free text or a formula. Formulas need to start with the equals (=) sign.
    fact :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CellInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facts', 'cellInput_facts' - A list representing the values that are entered into a ROWSET cell.
-- Facts list can have either only values or rowIDs, and rowIDs should from
-- the same table.
--
-- 'fact', 'cellInput_fact' - Fact represents the data that is entered into a cell. This data can be
-- free text or a formula. Formulas need to start with the equals (=) sign.
newCellInput ::
  CellInput
newCellInput =
  CellInput'
    { facts = Prelude.Nothing,
      fact = Prelude.Nothing
    }

-- | A list representing the values that are entered into a ROWSET cell.
-- Facts list can have either only values or rowIDs, and rowIDs should from
-- the same table.
cellInput_facts :: Lens.Lens' CellInput (Prelude.Maybe [Prelude.Text])
cellInput_facts = Lens.lens (\CellInput' {facts} -> facts) (\s@CellInput' {} a -> s {facts = a} :: CellInput) Prelude.. Lens.mapping Lens.coerced

-- | Fact represents the data that is entered into a cell. This data can be
-- free text or a formula. Formulas need to start with the equals (=) sign.
cellInput_fact :: Lens.Lens' CellInput (Prelude.Maybe Prelude.Text)
cellInput_fact = Lens.lens (\CellInput' {fact} -> fact) (\s@CellInput' {} a -> s {fact = a} :: CellInput) Prelude.. Lens.mapping Data._Sensitive

instance Prelude.Hashable CellInput where
  hashWithSalt _salt CellInput' {..} =
    _salt `Prelude.hashWithSalt` facts
      `Prelude.hashWithSalt` fact

instance Prelude.NFData CellInput where
  rnf CellInput' {..} =
    Prelude.rnf facts `Prelude.seq` Prelude.rnf fact

instance Data.ToJSON CellInput where
  toJSON CellInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("facts" Data..=) Prelude.<$> facts,
            ("fact" Data..=) Prelude.<$> fact
          ]
      )
