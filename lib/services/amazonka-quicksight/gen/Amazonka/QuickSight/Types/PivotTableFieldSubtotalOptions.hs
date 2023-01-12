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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldSubtotalOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldSubtotalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The optional configuration of subtotals cells.
--
-- /See:/ 'newPivotTableFieldSubtotalOptions' smart constructor.
data PivotTableFieldSubtotalOptions = PivotTableFieldSubtotalOptions'
  { -- | The field ID of the subtotal options.
    fieldId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldSubtotalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'pivotTableFieldSubtotalOptions_fieldId' - The field ID of the subtotal options.
newPivotTableFieldSubtotalOptions ::
  PivotTableFieldSubtotalOptions
newPivotTableFieldSubtotalOptions =
  PivotTableFieldSubtotalOptions'
    { fieldId =
        Prelude.Nothing
    }

-- | The field ID of the subtotal options.
pivotTableFieldSubtotalOptions_fieldId :: Lens.Lens' PivotTableFieldSubtotalOptions (Prelude.Maybe Prelude.Text)
pivotTableFieldSubtotalOptions_fieldId = Lens.lens (\PivotTableFieldSubtotalOptions' {fieldId} -> fieldId) (\s@PivotTableFieldSubtotalOptions' {} a -> s {fieldId = a} :: PivotTableFieldSubtotalOptions)

instance Data.FromJSON PivotTableFieldSubtotalOptions where
  parseJSON =
    Data.withObject
      "PivotTableFieldSubtotalOptions"
      ( \x ->
          PivotTableFieldSubtotalOptions'
            Prelude.<$> (x Data..:? "FieldId")
      )

instance
  Prelude.Hashable
    PivotTableFieldSubtotalOptions
  where
  hashWithSalt
    _salt
    PivotTableFieldSubtotalOptions' {..} =
      _salt `Prelude.hashWithSalt` fieldId

instance
  Prelude.NFData
    PivotTableFieldSubtotalOptions
  where
  rnf PivotTableFieldSubtotalOptions' {..} =
    Prelude.rnf fieldId

instance Data.ToJSON PivotTableFieldSubtotalOptions where
  toJSON PivotTableFieldSubtotalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FieldId" Data..=) Prelude.<$> fieldId]
      )
