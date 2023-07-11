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
-- Module      : Amazonka.QuickSight.Types.FieldTooltipItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldTooltipItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The tooltip item for the fields.
--
-- /See:/ 'newFieldTooltipItem' smart constructor.
data FieldTooltipItem = FieldTooltipItem'
  { -- | The label of the tooltip item.
    label :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the tooltip item.
    visibility :: Prelude.Maybe Visibility,
    -- | The unique ID of the field that is targeted by the tooltip.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldTooltipItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'fieldTooltipItem_label' - The label of the tooltip item.
--
-- 'visibility', 'fieldTooltipItem_visibility' - The visibility of the tooltip item.
--
-- 'fieldId', 'fieldTooltipItem_fieldId' - The unique ID of the field that is targeted by the tooltip.
newFieldTooltipItem ::
  -- | 'fieldId'
  Prelude.Text ->
  FieldTooltipItem
newFieldTooltipItem pFieldId_ =
  FieldTooltipItem'
    { label = Prelude.Nothing,
      visibility = Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The label of the tooltip item.
fieldTooltipItem_label :: Lens.Lens' FieldTooltipItem (Prelude.Maybe Prelude.Text)
fieldTooltipItem_label = Lens.lens (\FieldTooltipItem' {label} -> label) (\s@FieldTooltipItem' {} a -> s {label = a} :: FieldTooltipItem)

-- | The visibility of the tooltip item.
fieldTooltipItem_visibility :: Lens.Lens' FieldTooltipItem (Prelude.Maybe Visibility)
fieldTooltipItem_visibility = Lens.lens (\FieldTooltipItem' {visibility} -> visibility) (\s@FieldTooltipItem' {} a -> s {visibility = a} :: FieldTooltipItem)

-- | The unique ID of the field that is targeted by the tooltip.
fieldTooltipItem_fieldId :: Lens.Lens' FieldTooltipItem Prelude.Text
fieldTooltipItem_fieldId = Lens.lens (\FieldTooltipItem' {fieldId} -> fieldId) (\s@FieldTooltipItem' {} a -> s {fieldId = a} :: FieldTooltipItem)

instance Data.FromJSON FieldTooltipItem where
  parseJSON =
    Data.withObject
      "FieldTooltipItem"
      ( \x ->
          FieldTooltipItem'
            Prelude.<$> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..: "FieldId")
      )

instance Prelude.Hashable FieldTooltipItem where
  hashWithSalt _salt FieldTooltipItem' {..} =
    _salt
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData FieldTooltipItem where
  rnf FieldTooltipItem' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToJSON FieldTooltipItem where
  toJSON FieldTooltipItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Label" Data..=) Prelude.<$> label,
            ("Visibility" Data..=) Prelude.<$> visibility,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
