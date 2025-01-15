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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The selected field options for the pivot table field options.
--
-- /See:/ 'newPivotTableFieldOption' smart constructor.
data PivotTableFieldOption = PivotTableFieldOption'
  { -- | The custom label of the pivot table field.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the pivot table field.
    visibility :: Prelude.Maybe Visibility,
    -- | The field ID of the pivot table field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'pivotTableFieldOption_customLabel' - The custom label of the pivot table field.
--
-- 'visibility', 'pivotTableFieldOption_visibility' - The visibility of the pivot table field.
--
-- 'fieldId', 'pivotTableFieldOption_fieldId' - The field ID of the pivot table field.
newPivotTableFieldOption ::
  -- | 'fieldId'
  Prelude.Text ->
  PivotTableFieldOption
newPivotTableFieldOption pFieldId_ =
  PivotTableFieldOption'
    { customLabel =
        Prelude.Nothing,
      visibility = Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The custom label of the pivot table field.
pivotTableFieldOption_customLabel :: Lens.Lens' PivotTableFieldOption (Prelude.Maybe Prelude.Text)
pivotTableFieldOption_customLabel = Lens.lens (\PivotTableFieldOption' {customLabel} -> customLabel) (\s@PivotTableFieldOption' {} a -> s {customLabel = a} :: PivotTableFieldOption)

-- | The visibility of the pivot table field.
pivotTableFieldOption_visibility :: Lens.Lens' PivotTableFieldOption (Prelude.Maybe Visibility)
pivotTableFieldOption_visibility = Lens.lens (\PivotTableFieldOption' {visibility} -> visibility) (\s@PivotTableFieldOption' {} a -> s {visibility = a} :: PivotTableFieldOption)

-- | The field ID of the pivot table field.
pivotTableFieldOption_fieldId :: Lens.Lens' PivotTableFieldOption Prelude.Text
pivotTableFieldOption_fieldId = Lens.lens (\PivotTableFieldOption' {fieldId} -> fieldId) (\s@PivotTableFieldOption' {} a -> s {fieldId = a} :: PivotTableFieldOption)

instance Data.FromJSON PivotTableFieldOption where
  parseJSON =
    Data.withObject
      "PivotTableFieldOption"
      ( \x ->
          PivotTableFieldOption'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..: "FieldId")
      )

instance Prelude.Hashable PivotTableFieldOption where
  hashWithSalt _salt PivotTableFieldOption' {..} =
    _salt
      `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData PivotTableFieldOption where
  rnf PivotTableFieldOption' {..} =
    Prelude.rnf customLabel `Prelude.seq`
      Prelude.rnf visibility `Prelude.seq`
        Prelude.rnf fieldId

instance Data.ToJSON PivotTableFieldOption where
  toJSON PivotTableFieldOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("Visibility" Data..=) Prelude.<$> visibility,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
