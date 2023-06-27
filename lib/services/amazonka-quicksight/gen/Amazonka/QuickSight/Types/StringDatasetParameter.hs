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
-- Module      : Amazonka.QuickSight.Types.StringDatasetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringDatasetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DatasetParameterValueType
import Amazonka.QuickSight.Types.StringDatasetParameterDefaultValues

-- | A string parameter for a dataset.
--
-- /See:/ 'newStringDatasetParameter' smart constructor.
data StringDatasetParameter = StringDatasetParameter'
  { -- | A list of default values for a given string dataset parameter type. This
    -- structure only accepts static values.
    defaultValues :: Prelude.Maybe StringDatasetParameterDefaultValues,
    -- | An identifier for the string parameter that is created in the dataset.
    id :: Prelude.Text,
    -- | The name of the string parameter that is created in the dataset.
    name :: Prelude.Text,
    -- | The value type of the dataset parameter. Valid values are @single value@
    -- or @multi value@.
    valueType :: DatasetParameterValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringDatasetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'stringDatasetParameter_defaultValues' - A list of default values for a given string dataset parameter type. This
-- structure only accepts static values.
--
-- 'id', 'stringDatasetParameter_id' - An identifier for the string parameter that is created in the dataset.
--
-- 'name', 'stringDatasetParameter_name' - The name of the string parameter that is created in the dataset.
--
-- 'valueType', 'stringDatasetParameter_valueType' - The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
newStringDatasetParameter ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'valueType'
  DatasetParameterValueType ->
  StringDatasetParameter
newStringDatasetParameter pId_ pName_ pValueType_ =
  StringDatasetParameter'
    { defaultValues =
        Prelude.Nothing,
      id = pId_,
      name = pName_,
      valueType = pValueType_
    }

-- | A list of default values for a given string dataset parameter type. This
-- structure only accepts static values.
stringDatasetParameter_defaultValues :: Lens.Lens' StringDatasetParameter (Prelude.Maybe StringDatasetParameterDefaultValues)
stringDatasetParameter_defaultValues = Lens.lens (\StringDatasetParameter' {defaultValues} -> defaultValues) (\s@StringDatasetParameter' {} a -> s {defaultValues = a} :: StringDatasetParameter)

-- | An identifier for the string parameter that is created in the dataset.
stringDatasetParameter_id :: Lens.Lens' StringDatasetParameter Prelude.Text
stringDatasetParameter_id = Lens.lens (\StringDatasetParameter' {id} -> id) (\s@StringDatasetParameter' {} a -> s {id = a} :: StringDatasetParameter)

-- | The name of the string parameter that is created in the dataset.
stringDatasetParameter_name :: Lens.Lens' StringDatasetParameter Prelude.Text
stringDatasetParameter_name = Lens.lens (\StringDatasetParameter' {name} -> name) (\s@StringDatasetParameter' {} a -> s {name = a} :: StringDatasetParameter)

-- | The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
stringDatasetParameter_valueType :: Lens.Lens' StringDatasetParameter DatasetParameterValueType
stringDatasetParameter_valueType = Lens.lens (\StringDatasetParameter' {valueType} -> valueType) (\s@StringDatasetParameter' {} a -> s {valueType = a} :: StringDatasetParameter)

instance Data.FromJSON StringDatasetParameter where
  parseJSON =
    Data.withObject
      "StringDatasetParameter"
      ( \x ->
          StringDatasetParameter'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ValueType")
      )

instance Prelude.Hashable StringDatasetParameter where
  hashWithSalt _salt StringDatasetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueType

instance Prelude.NFData StringDatasetParameter where
  rnf StringDatasetParameter' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueType

instance Data.ToJSON StringDatasetParameter where
  toJSON StringDatasetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ValueType" Data..= valueType)
          ]
      )
