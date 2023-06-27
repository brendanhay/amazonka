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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldCollapseStateOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldCollapseStateOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableFieldCollapseState
import Amazonka.QuickSight.Types.PivotTableFieldCollapseStateTarget

-- | The collapse state options for the pivot table field options.
--
-- /See:/ 'newPivotTableFieldCollapseStateOption' smart constructor.
data PivotTableFieldCollapseStateOption = PivotTableFieldCollapseStateOption'
  { -- | The state of the field target of a pivot table. Choose one of the
    -- following options:
    --
    -- -   @COLLAPSED@
    --
    -- -   @EXPANDED@
    state :: Prelude.Maybe PivotTableFieldCollapseState,
    -- | A tagged-union object that sets the collapse state.
    target :: PivotTableFieldCollapseStateTarget
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldCollapseStateOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'pivotTableFieldCollapseStateOption_state' - The state of the field target of a pivot table. Choose one of the
-- following options:
--
-- -   @COLLAPSED@
--
-- -   @EXPANDED@
--
-- 'target', 'pivotTableFieldCollapseStateOption_target' - A tagged-union object that sets the collapse state.
newPivotTableFieldCollapseStateOption ::
  -- | 'target'
  PivotTableFieldCollapseStateTarget ->
  PivotTableFieldCollapseStateOption
newPivotTableFieldCollapseStateOption pTarget_ =
  PivotTableFieldCollapseStateOption'
    { state =
        Prelude.Nothing,
      target = pTarget_
    }

-- | The state of the field target of a pivot table. Choose one of the
-- following options:
--
-- -   @COLLAPSED@
--
-- -   @EXPANDED@
pivotTableFieldCollapseStateOption_state :: Lens.Lens' PivotTableFieldCollapseStateOption (Prelude.Maybe PivotTableFieldCollapseState)
pivotTableFieldCollapseStateOption_state = Lens.lens (\PivotTableFieldCollapseStateOption' {state} -> state) (\s@PivotTableFieldCollapseStateOption' {} a -> s {state = a} :: PivotTableFieldCollapseStateOption)

-- | A tagged-union object that sets the collapse state.
pivotTableFieldCollapseStateOption_target :: Lens.Lens' PivotTableFieldCollapseStateOption PivotTableFieldCollapseStateTarget
pivotTableFieldCollapseStateOption_target = Lens.lens (\PivotTableFieldCollapseStateOption' {target} -> target) (\s@PivotTableFieldCollapseStateOption' {} a -> s {target = a} :: PivotTableFieldCollapseStateOption)

instance
  Data.FromJSON
    PivotTableFieldCollapseStateOption
  where
  parseJSON =
    Data.withObject
      "PivotTableFieldCollapseStateOption"
      ( \x ->
          PivotTableFieldCollapseStateOption'
            Prelude.<$> (x Data..:? "State")
            Prelude.<*> (x Data..: "Target")
      )

instance
  Prelude.Hashable
    PivotTableFieldCollapseStateOption
  where
  hashWithSalt
    _salt
    PivotTableFieldCollapseStateOption' {..} =
      _salt
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` target

instance
  Prelude.NFData
    PivotTableFieldCollapseStateOption
  where
  rnf PivotTableFieldCollapseStateOption' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf target

instance
  Data.ToJSON
    PivotTableFieldCollapseStateOption
  where
  toJSON PivotTableFieldCollapseStateOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("State" Data..=) Prelude.<$> state,
            Prelude.Just ("Target" Data..= target)
          ]
      )
