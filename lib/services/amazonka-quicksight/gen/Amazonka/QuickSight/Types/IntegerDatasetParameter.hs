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
-- Module      : Amazonka.QuickSight.Types.IntegerDatasetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerDatasetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DatasetParameterValueType
import Amazonka.QuickSight.Types.IntegerDatasetParameterDefaultValues

-- | An integer parameter for a dataset.
--
-- /See:/ 'newIntegerDatasetParameter' smart constructor.
data IntegerDatasetParameter = IntegerDatasetParameter'
  { -- | A list of default values for a given integer parameter. This structure
    -- only accepts static values.
    defaultValues :: Prelude.Maybe IntegerDatasetParameterDefaultValues,
    -- | An identifier for the integer parameter created in the dataset.
    id :: Prelude.Text,
    -- | The name of the integer parameter that is created in the dataset.
    name :: Prelude.Text,
    -- | The value type of the dataset parameter. Valid values are @single value@
    -- or @multi value@.
    valueType :: DatasetParameterValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerDatasetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'integerDatasetParameter_defaultValues' - A list of default values for a given integer parameter. This structure
-- only accepts static values.
--
-- 'id', 'integerDatasetParameter_id' - An identifier for the integer parameter created in the dataset.
--
-- 'name', 'integerDatasetParameter_name' - The name of the integer parameter that is created in the dataset.
--
-- 'valueType', 'integerDatasetParameter_valueType' - The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
newIntegerDatasetParameter ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'valueType'
  DatasetParameterValueType ->
  IntegerDatasetParameter
newIntegerDatasetParameter pId_ pName_ pValueType_ =
  IntegerDatasetParameter'
    { defaultValues =
        Prelude.Nothing,
      id = pId_,
      name = pName_,
      valueType = pValueType_
    }

-- | A list of default values for a given integer parameter. This structure
-- only accepts static values.
integerDatasetParameter_defaultValues :: Lens.Lens' IntegerDatasetParameter (Prelude.Maybe IntegerDatasetParameterDefaultValues)
integerDatasetParameter_defaultValues = Lens.lens (\IntegerDatasetParameter' {defaultValues} -> defaultValues) (\s@IntegerDatasetParameter' {} a -> s {defaultValues = a} :: IntegerDatasetParameter)

-- | An identifier for the integer parameter created in the dataset.
integerDatasetParameter_id :: Lens.Lens' IntegerDatasetParameter Prelude.Text
integerDatasetParameter_id = Lens.lens (\IntegerDatasetParameter' {id} -> id) (\s@IntegerDatasetParameter' {} a -> s {id = a} :: IntegerDatasetParameter)

-- | The name of the integer parameter that is created in the dataset.
integerDatasetParameter_name :: Lens.Lens' IntegerDatasetParameter Prelude.Text
integerDatasetParameter_name = Lens.lens (\IntegerDatasetParameter' {name} -> name) (\s@IntegerDatasetParameter' {} a -> s {name = a} :: IntegerDatasetParameter)

-- | The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
integerDatasetParameter_valueType :: Lens.Lens' IntegerDatasetParameter DatasetParameterValueType
integerDatasetParameter_valueType = Lens.lens (\IntegerDatasetParameter' {valueType} -> valueType) (\s@IntegerDatasetParameter' {} a -> s {valueType = a} :: IntegerDatasetParameter)

instance Data.FromJSON IntegerDatasetParameter where
  parseJSON =
    Data.withObject
      "IntegerDatasetParameter"
      ( \x ->
          IntegerDatasetParameter'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ValueType")
      )

instance Prelude.Hashable IntegerDatasetParameter where
  hashWithSalt _salt IntegerDatasetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueType

instance Prelude.NFData IntegerDatasetParameter where
  rnf IntegerDatasetParameter' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueType

instance Data.ToJSON IntegerDatasetParameter where
  toJSON IntegerDatasetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ValueType" Data..= valueType)
          ]
      )
