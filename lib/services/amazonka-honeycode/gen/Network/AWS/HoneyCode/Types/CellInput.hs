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
-- Module      : Network.AWS.HoneyCode.Types.CellInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Types.CellInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | CellInput object contains the data needed to create or update cells in a
-- table.
--
-- /See:/ 'newCellInput' smart constructor.
data CellInput = CellInput'
  { -- | Fact represents the data that is entered into a cell. This data can be
    -- free text or a formula. Formulas need to start with the equals (=) sign.
    fact :: Prelude.Maybe (Core.Sensitive Prelude.Text)
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
-- 'fact', 'cellInput_fact' - Fact represents the data that is entered into a cell. This data can be
-- free text or a formula. Formulas need to start with the equals (=) sign.
newCellInput ::
  CellInput
newCellInput = CellInput' {fact = Prelude.Nothing}

-- | Fact represents the data that is entered into a cell. This data can be
-- free text or a formula. Formulas need to start with the equals (=) sign.
cellInput_fact :: Lens.Lens' CellInput (Prelude.Maybe Prelude.Text)
cellInput_fact = Lens.lens (\CellInput' {fact} -> fact) (\s@CellInput' {} a -> s {fact = a} :: CellInput) Prelude.. Lens.mapping Core._Sensitive

instance Prelude.Hashable CellInput

instance Prelude.NFData CellInput

instance Core.ToJSON CellInput where
  toJSON CellInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [("fact" Core..=) Prelude.<$> fact]
      )
