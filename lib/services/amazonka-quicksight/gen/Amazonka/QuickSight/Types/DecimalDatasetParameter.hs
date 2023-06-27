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
-- Module      : Amazonka.QuickSight.Types.DecimalDatasetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalDatasetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DatasetParameterValueType
import Amazonka.QuickSight.Types.DecimalDatasetParameterDefaultValues

-- | A decimal parameter for a dataset.
--
-- /See:/ 'newDecimalDatasetParameter' smart constructor.
data DecimalDatasetParameter = DecimalDatasetParameter'
  { -- | A list of default values for a given decimal parameter. This structure
    -- only accepts static values.
    defaultValues :: Prelude.Maybe DecimalDatasetParameterDefaultValues,
    -- | An identifier for the decimal parameter created in the dataset.
    id :: Prelude.Text,
    -- | The name of the decimal parameter that is created in the dataset.
    name :: Prelude.Text,
    -- | The value type of the dataset parameter. Valid values are @single value@
    -- or @multi value@.
    valueType :: DatasetParameterValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalDatasetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'decimalDatasetParameter_defaultValues' - A list of default values for a given decimal parameter. This structure
-- only accepts static values.
--
-- 'id', 'decimalDatasetParameter_id' - An identifier for the decimal parameter created in the dataset.
--
-- 'name', 'decimalDatasetParameter_name' - The name of the decimal parameter that is created in the dataset.
--
-- 'valueType', 'decimalDatasetParameter_valueType' - The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
newDecimalDatasetParameter ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'valueType'
  DatasetParameterValueType ->
  DecimalDatasetParameter
newDecimalDatasetParameter pId_ pName_ pValueType_ =
  DecimalDatasetParameter'
    { defaultValues =
        Prelude.Nothing,
      id = pId_,
      name = pName_,
      valueType = pValueType_
    }

-- | A list of default values for a given decimal parameter. This structure
-- only accepts static values.
decimalDatasetParameter_defaultValues :: Lens.Lens' DecimalDatasetParameter (Prelude.Maybe DecimalDatasetParameterDefaultValues)
decimalDatasetParameter_defaultValues = Lens.lens (\DecimalDatasetParameter' {defaultValues} -> defaultValues) (\s@DecimalDatasetParameter' {} a -> s {defaultValues = a} :: DecimalDatasetParameter)

-- | An identifier for the decimal parameter created in the dataset.
decimalDatasetParameter_id :: Lens.Lens' DecimalDatasetParameter Prelude.Text
decimalDatasetParameter_id = Lens.lens (\DecimalDatasetParameter' {id} -> id) (\s@DecimalDatasetParameter' {} a -> s {id = a} :: DecimalDatasetParameter)

-- | The name of the decimal parameter that is created in the dataset.
decimalDatasetParameter_name :: Lens.Lens' DecimalDatasetParameter Prelude.Text
decimalDatasetParameter_name = Lens.lens (\DecimalDatasetParameter' {name} -> name) (\s@DecimalDatasetParameter' {} a -> s {name = a} :: DecimalDatasetParameter)

-- | The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
decimalDatasetParameter_valueType :: Lens.Lens' DecimalDatasetParameter DatasetParameterValueType
decimalDatasetParameter_valueType = Lens.lens (\DecimalDatasetParameter' {valueType} -> valueType) (\s@DecimalDatasetParameter' {} a -> s {valueType = a} :: DecimalDatasetParameter)

instance Data.FromJSON DecimalDatasetParameter where
  parseJSON =
    Data.withObject
      "DecimalDatasetParameter"
      ( \x ->
          DecimalDatasetParameter'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ValueType")
      )

instance Prelude.Hashable DecimalDatasetParameter where
  hashWithSalt _salt DecimalDatasetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueType

instance Prelude.NFData DecimalDatasetParameter where
  rnf DecimalDatasetParameter' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueType

instance Data.ToJSON DecimalDatasetParameter where
  toJSON DecimalDatasetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ValueType" Data..= valueType)
          ]
      )
