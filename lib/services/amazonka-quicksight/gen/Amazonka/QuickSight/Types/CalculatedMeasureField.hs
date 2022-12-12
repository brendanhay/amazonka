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
-- Module      : Amazonka.QuickSight.Types.CalculatedMeasureField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CalculatedMeasureField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The table calculation measure field for pivot tables.
--
-- /See:/ 'newCalculatedMeasureField' smart constructor.
data CalculatedMeasureField = CalculatedMeasureField'
  { -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The expression in the table calculation.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedMeasureField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'calculatedMeasureField_fieldId' - The custom field ID.
--
-- 'expression', 'calculatedMeasureField_expression' - The expression in the table calculation.
newCalculatedMeasureField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
  CalculatedMeasureField
newCalculatedMeasureField pFieldId_ pExpression_ =
  CalculatedMeasureField'
    { fieldId = pFieldId_,
      expression = Data._Sensitive Lens.# pExpression_
    }

-- | The custom field ID.
calculatedMeasureField_fieldId :: Lens.Lens' CalculatedMeasureField Prelude.Text
calculatedMeasureField_fieldId = Lens.lens (\CalculatedMeasureField' {fieldId} -> fieldId) (\s@CalculatedMeasureField' {} a -> s {fieldId = a} :: CalculatedMeasureField)

-- | The expression in the table calculation.
calculatedMeasureField_expression :: Lens.Lens' CalculatedMeasureField Prelude.Text
calculatedMeasureField_expression = Lens.lens (\CalculatedMeasureField' {expression} -> expression) (\s@CalculatedMeasureField' {} a -> s {expression = a} :: CalculatedMeasureField) Prelude.. Data._Sensitive

instance Data.FromJSON CalculatedMeasureField where
  parseJSON =
    Data.withObject
      "CalculatedMeasureField"
      ( \x ->
          CalculatedMeasureField'
            Prelude.<$> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable CalculatedMeasureField where
  hashWithSalt _salt CalculatedMeasureField' {..} =
    _salt `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` expression

instance Prelude.NFData CalculatedMeasureField where
  rnf CalculatedMeasureField' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON CalculatedMeasureField where
  toJSON CalculatedMeasureField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
