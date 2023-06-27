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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldCollapseStateTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldCollapseStateTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathValue

-- | The target of a pivot table field collapse state.
--
-- /See:/ 'newPivotTableFieldCollapseStateTarget' smart constructor.
data PivotTableFieldCollapseStateTarget = PivotTableFieldCollapseStateTarget'
  { -- | The data path of the pivot table\'s header. Used to set the collapse
    -- state.
    fieldDataPathValues :: Prelude.Maybe [DataPathValue],
    -- | The field ID of the pivot table that the collapse state needs to be set
    -- to.
    fieldId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldCollapseStateTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldDataPathValues', 'pivotTableFieldCollapseStateTarget_fieldDataPathValues' - The data path of the pivot table\'s header. Used to set the collapse
-- state.
--
-- 'fieldId', 'pivotTableFieldCollapseStateTarget_fieldId' - The field ID of the pivot table that the collapse state needs to be set
-- to.
newPivotTableFieldCollapseStateTarget ::
  PivotTableFieldCollapseStateTarget
newPivotTableFieldCollapseStateTarget =
  PivotTableFieldCollapseStateTarget'
    { fieldDataPathValues =
        Prelude.Nothing,
      fieldId = Prelude.Nothing
    }

-- | The data path of the pivot table\'s header. Used to set the collapse
-- state.
pivotTableFieldCollapseStateTarget_fieldDataPathValues :: Lens.Lens' PivotTableFieldCollapseStateTarget (Prelude.Maybe [DataPathValue])
pivotTableFieldCollapseStateTarget_fieldDataPathValues = Lens.lens (\PivotTableFieldCollapseStateTarget' {fieldDataPathValues} -> fieldDataPathValues) (\s@PivotTableFieldCollapseStateTarget' {} a -> s {fieldDataPathValues = a} :: PivotTableFieldCollapseStateTarget) Prelude.. Lens.mapping Lens.coerced

-- | The field ID of the pivot table that the collapse state needs to be set
-- to.
pivotTableFieldCollapseStateTarget_fieldId :: Lens.Lens' PivotTableFieldCollapseStateTarget (Prelude.Maybe Prelude.Text)
pivotTableFieldCollapseStateTarget_fieldId = Lens.lens (\PivotTableFieldCollapseStateTarget' {fieldId} -> fieldId) (\s@PivotTableFieldCollapseStateTarget' {} a -> s {fieldId = a} :: PivotTableFieldCollapseStateTarget)

instance
  Data.FromJSON
    PivotTableFieldCollapseStateTarget
  where
  parseJSON =
    Data.withObject
      "PivotTableFieldCollapseStateTarget"
      ( \x ->
          PivotTableFieldCollapseStateTarget'
            Prelude.<$> ( x
                            Data..:? "FieldDataPathValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldId")
      )

instance
  Prelude.Hashable
    PivotTableFieldCollapseStateTarget
  where
  hashWithSalt
    _salt
    PivotTableFieldCollapseStateTarget' {..} =
      _salt
        `Prelude.hashWithSalt` fieldDataPathValues
        `Prelude.hashWithSalt` fieldId

instance
  Prelude.NFData
    PivotTableFieldCollapseStateTarget
  where
  rnf PivotTableFieldCollapseStateTarget' {..} =
    Prelude.rnf fieldDataPathValues
      `Prelude.seq` Prelude.rnf fieldId

instance
  Data.ToJSON
    PivotTableFieldCollapseStateTarget
  where
  toJSON PivotTableFieldCollapseStateTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldDataPathValues" Data..=)
              Prelude.<$> fieldDataPathValues,
            ("FieldId" Data..=) Prelude.<$> fieldId
          ]
      )
