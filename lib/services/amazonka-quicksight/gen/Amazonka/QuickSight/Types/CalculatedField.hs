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
-- Module      : Amazonka.QuickSight.Types.CalculatedField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CalculatedField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The calculated field of an analysis.
--
-- /See:/ 'newCalculatedField' smart constructor.
data CalculatedField = CalculatedField'
  { -- | The data set that is used in this calculated field.
    dataSetIdentifier :: Prelude.Text,
    -- | The name of the calculated field.
    name :: Prelude.Text,
    -- | The expression of the calculated field.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetIdentifier', 'calculatedField_dataSetIdentifier' - The data set that is used in this calculated field.
--
-- 'name', 'calculatedField_name' - The name of the calculated field.
--
-- 'expression', 'calculatedField_expression' - The expression of the calculated field.
newCalculatedField ::
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
  CalculatedField
newCalculatedField
  pDataSetIdentifier_
  pName_
  pExpression_ =
    CalculatedField'
      { dataSetIdentifier =
          pDataSetIdentifier_,
        name = pName_,
        expression = Data._Sensitive Lens.# pExpression_
      }

-- | The data set that is used in this calculated field.
calculatedField_dataSetIdentifier :: Lens.Lens' CalculatedField Prelude.Text
calculatedField_dataSetIdentifier = Lens.lens (\CalculatedField' {dataSetIdentifier} -> dataSetIdentifier) (\s@CalculatedField' {} a -> s {dataSetIdentifier = a} :: CalculatedField)

-- | The name of the calculated field.
calculatedField_name :: Lens.Lens' CalculatedField Prelude.Text
calculatedField_name = Lens.lens (\CalculatedField' {name} -> name) (\s@CalculatedField' {} a -> s {name = a} :: CalculatedField)

-- | The expression of the calculated field.
calculatedField_expression :: Lens.Lens' CalculatedField Prelude.Text
calculatedField_expression = Lens.lens (\CalculatedField' {expression} -> expression) (\s@CalculatedField' {} a -> s {expression = a} :: CalculatedField) Prelude.. Data._Sensitive

instance Data.FromJSON CalculatedField where
  parseJSON =
    Data.withObject
      "CalculatedField"
      ( \x ->
          CalculatedField'
            Prelude.<$> (x Data..: "DataSetIdentifier")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable CalculatedField where
  hashWithSalt _salt CalculatedField' {..} =
    _salt `Prelude.hashWithSalt` dataSetIdentifier
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` expression

instance Prelude.NFData CalculatedField where
  rnf CalculatedField' {..} =
    Prelude.rnf dataSetIdentifier
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON CalculatedField where
  toJSON CalculatedField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
