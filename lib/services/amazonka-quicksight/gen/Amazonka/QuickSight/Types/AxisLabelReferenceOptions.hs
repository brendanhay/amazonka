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
-- Module      : Amazonka.QuickSight.Types.AxisLabelReferenceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisLabelReferenceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | The reference that specifies where the axis label is applied to.
--
-- /See:/ 'newAxisLabelReferenceOptions' smart constructor.
data AxisLabelReferenceOptions = AxisLabelReferenceOptions'
  { -- | The field that the axis label is targeted to.
    fieldId :: Prelude.Text,
    -- | The column that the axis label is targeted to.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisLabelReferenceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'axisLabelReferenceOptions_fieldId' - The field that the axis label is targeted to.
--
-- 'column', 'axisLabelReferenceOptions_column' - The column that the axis label is targeted to.
newAxisLabelReferenceOptions ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  AxisLabelReferenceOptions
newAxisLabelReferenceOptions pFieldId_ pColumn_ =
  AxisLabelReferenceOptions'
    { fieldId = pFieldId_,
      column = pColumn_
    }

-- | The field that the axis label is targeted to.
axisLabelReferenceOptions_fieldId :: Lens.Lens' AxisLabelReferenceOptions Prelude.Text
axisLabelReferenceOptions_fieldId = Lens.lens (\AxisLabelReferenceOptions' {fieldId} -> fieldId) (\s@AxisLabelReferenceOptions' {} a -> s {fieldId = a} :: AxisLabelReferenceOptions)

-- | The column that the axis label is targeted to.
axisLabelReferenceOptions_column :: Lens.Lens' AxisLabelReferenceOptions ColumnIdentifier
axisLabelReferenceOptions_column = Lens.lens (\AxisLabelReferenceOptions' {column} -> column) (\s@AxisLabelReferenceOptions' {} a -> s {column = a} :: AxisLabelReferenceOptions)

instance Data.FromJSON AxisLabelReferenceOptions where
  parseJSON =
    Data.withObject
      "AxisLabelReferenceOptions"
      ( \x ->
          AxisLabelReferenceOptions'
            Prelude.<$> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable AxisLabelReferenceOptions where
  hashWithSalt _salt AxisLabelReferenceOptions' {..} =
    _salt
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData AxisLabelReferenceOptions where
  rnf AxisLabelReferenceOptions' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON AxisLabelReferenceOptions where
  toJSON AxisLabelReferenceOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
