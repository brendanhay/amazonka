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
-- Module      : Amazonka.QuickSight.Types.PivotTableConditionalFormattingScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableConditionalFormattingScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingScopeRole

-- | The scope of the cell for conditional formatting.
--
-- /See:/ 'newPivotTableConditionalFormattingScope' smart constructor.
data PivotTableConditionalFormattingScope = PivotTableConditionalFormattingScope'
  { -- | The role (field, field total, grand total) of the cell for conditional
    -- formatting.
    role' :: Prelude.Maybe PivotTableConditionalFormattingScopeRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableConditionalFormattingScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'pivotTableConditionalFormattingScope_role' - The role (field, field total, grand total) of the cell for conditional
-- formatting.
newPivotTableConditionalFormattingScope ::
  PivotTableConditionalFormattingScope
newPivotTableConditionalFormattingScope =
  PivotTableConditionalFormattingScope'
    { role' =
        Prelude.Nothing
    }

-- | The role (field, field total, grand total) of the cell for conditional
-- formatting.
pivotTableConditionalFormattingScope_role :: Lens.Lens' PivotTableConditionalFormattingScope (Prelude.Maybe PivotTableConditionalFormattingScopeRole)
pivotTableConditionalFormattingScope_role = Lens.lens (\PivotTableConditionalFormattingScope' {role'} -> role') (\s@PivotTableConditionalFormattingScope' {} a -> s {role' = a} :: PivotTableConditionalFormattingScope)

instance
  Data.FromJSON
    PivotTableConditionalFormattingScope
  where
  parseJSON =
    Data.withObject
      "PivotTableConditionalFormattingScope"
      ( \x ->
          PivotTableConditionalFormattingScope'
            Prelude.<$> (x Data..:? "Role")
      )

instance
  Prelude.Hashable
    PivotTableConditionalFormattingScope
  where
  hashWithSalt
    _salt
    PivotTableConditionalFormattingScope' {..} =
      _salt `Prelude.hashWithSalt` role'

instance
  Prelude.NFData
    PivotTableConditionalFormattingScope
  where
  rnf PivotTableConditionalFormattingScope' {..} =
    Prelude.rnf role'

instance
  Data.ToJSON
    PivotTableConditionalFormattingScope
  where
  toJSON PivotTableConditionalFormattingScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Role" Data..=) Prelude.<$> role']
      )
