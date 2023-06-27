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
-- Module      : Amazonka.QuickSight.Types.DateTimeDatasetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeDatasetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DatasetParameterValueType
import Amazonka.QuickSight.Types.DateTimeDatasetParameterDefaultValues
import Amazonka.QuickSight.Types.TimeGranularity

-- | A date time parameter for a dataset.
--
-- /See:/ 'newDateTimeDatasetParameter' smart constructor.
data DateTimeDatasetParameter = DateTimeDatasetParameter'
  { -- | A list of default values for a given date time parameter. This structure
    -- only accepts static values.
    defaultValues :: Prelude.Maybe DateTimeDatasetParameterDefaultValues,
    -- | The time granularity of the date time parameter.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | An identifier for the parameter that is created in the dataset.
    id :: Prelude.Text,
    -- | The name of the date time parameter that is created in the dataset.
    name :: Prelude.Text,
    -- | The value type of the dataset parameter. Valid values are @single value@
    -- or @multi value@.
    valueType :: DatasetParameterValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeDatasetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'dateTimeDatasetParameter_defaultValues' - A list of default values for a given date time parameter. This structure
-- only accepts static values.
--
-- 'timeGranularity', 'dateTimeDatasetParameter_timeGranularity' - The time granularity of the date time parameter.
--
-- 'id', 'dateTimeDatasetParameter_id' - An identifier for the parameter that is created in the dataset.
--
-- 'name', 'dateTimeDatasetParameter_name' - The name of the date time parameter that is created in the dataset.
--
-- 'valueType', 'dateTimeDatasetParameter_valueType' - The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
newDateTimeDatasetParameter ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'valueType'
  DatasetParameterValueType ->
  DateTimeDatasetParameter
newDateTimeDatasetParameter pId_ pName_ pValueType_ =
  DateTimeDatasetParameter'
    { defaultValues =
        Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      id = pId_,
      name = pName_,
      valueType = pValueType_
    }

-- | A list of default values for a given date time parameter. This structure
-- only accepts static values.
dateTimeDatasetParameter_defaultValues :: Lens.Lens' DateTimeDatasetParameter (Prelude.Maybe DateTimeDatasetParameterDefaultValues)
dateTimeDatasetParameter_defaultValues = Lens.lens (\DateTimeDatasetParameter' {defaultValues} -> defaultValues) (\s@DateTimeDatasetParameter' {} a -> s {defaultValues = a} :: DateTimeDatasetParameter)

-- | The time granularity of the date time parameter.
dateTimeDatasetParameter_timeGranularity :: Lens.Lens' DateTimeDatasetParameter (Prelude.Maybe TimeGranularity)
dateTimeDatasetParameter_timeGranularity = Lens.lens (\DateTimeDatasetParameter' {timeGranularity} -> timeGranularity) (\s@DateTimeDatasetParameter' {} a -> s {timeGranularity = a} :: DateTimeDatasetParameter)

-- | An identifier for the parameter that is created in the dataset.
dateTimeDatasetParameter_id :: Lens.Lens' DateTimeDatasetParameter Prelude.Text
dateTimeDatasetParameter_id = Lens.lens (\DateTimeDatasetParameter' {id} -> id) (\s@DateTimeDatasetParameter' {} a -> s {id = a} :: DateTimeDatasetParameter)

-- | The name of the date time parameter that is created in the dataset.
dateTimeDatasetParameter_name :: Lens.Lens' DateTimeDatasetParameter Prelude.Text
dateTimeDatasetParameter_name = Lens.lens (\DateTimeDatasetParameter' {name} -> name) (\s@DateTimeDatasetParameter' {} a -> s {name = a} :: DateTimeDatasetParameter)

-- | The value type of the dataset parameter. Valid values are @single value@
-- or @multi value@.
dateTimeDatasetParameter_valueType :: Lens.Lens' DateTimeDatasetParameter DatasetParameterValueType
dateTimeDatasetParameter_valueType = Lens.lens (\DateTimeDatasetParameter' {valueType} -> valueType) (\s@DateTimeDatasetParameter' {} a -> s {valueType = a} :: DateTimeDatasetParameter)

instance Data.FromJSON DateTimeDatasetParameter where
  parseJSON =
    Data.withObject
      "DateTimeDatasetParameter"
      ( \x ->
          DateTimeDatasetParameter'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ValueType")
      )

instance Prelude.Hashable DateTimeDatasetParameter where
  hashWithSalt _salt DateTimeDatasetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueType

instance Prelude.NFData DateTimeDatasetParameter where
  rnf DateTimeDatasetParameter' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueType

instance Data.ToJSON DateTimeDatasetParameter where
  toJSON DateTimeDatasetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ValueType" Data..= valueType)
          ]
      )
