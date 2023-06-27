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
-- Module      : Amazonka.TimeStreamWrite.Types.MeasureValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MeasureValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.MeasureValueType

-- | Represents the data attribute of the time series. For example, the CPU
-- utilization of an EC2 instance or the RPM of a wind turbine are
-- measures. MeasureValue has both name and value.
--
-- MeasureValue is only allowed for type @MULTI@. Using @MULTI@ type, you
-- can pass multiple data attributes associated with the same time series
-- in a single record
--
-- /See:/ 'newMeasureValue' smart constructor.
data MeasureValue = MeasureValue'
  { -- | The name of the MeasureValue.
    --
    -- For constraints on MeasureValue names, see
    -- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>
    -- in the Amazon Timestream Developer Guide.
    name :: Prelude.Text,
    -- | The value for the MeasureValue. For information, see
    -- <https://docs.aws.amazon.com/timestream/latest/developerguide/writes.html#writes.data-types Data types>.
    value :: Prelude.Text,
    -- | Contains the data type of the MeasureValue for the time-series data
    -- point.
    type' :: MeasureValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeasureValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'measureValue_name' - The name of the MeasureValue.
--
-- For constraints on MeasureValue names, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>
-- in the Amazon Timestream Developer Guide.
--
-- 'value', 'measureValue_value' - The value for the MeasureValue. For information, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/writes.html#writes.data-types Data types>.
--
-- 'type'', 'measureValue_type' - Contains the data type of the MeasureValue for the time-series data
-- point.
newMeasureValue ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  MeasureValueType ->
  MeasureValue
newMeasureValue pName_ pValue_ pType_ =
  MeasureValue'
    { name = pName_,
      value = pValue_,
      type' = pType_
    }

-- | The name of the MeasureValue.
--
-- For constraints on MeasureValue names, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>
-- in the Amazon Timestream Developer Guide.
measureValue_name :: Lens.Lens' MeasureValue Prelude.Text
measureValue_name = Lens.lens (\MeasureValue' {name} -> name) (\s@MeasureValue' {} a -> s {name = a} :: MeasureValue)

-- | The value for the MeasureValue. For information, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/writes.html#writes.data-types Data types>.
measureValue_value :: Lens.Lens' MeasureValue Prelude.Text
measureValue_value = Lens.lens (\MeasureValue' {value} -> value) (\s@MeasureValue' {} a -> s {value = a} :: MeasureValue)

-- | Contains the data type of the MeasureValue for the time-series data
-- point.
measureValue_type :: Lens.Lens' MeasureValue MeasureValueType
measureValue_type = Lens.lens (\MeasureValue' {type'} -> type') (\s@MeasureValue' {} a -> s {type' = a} :: MeasureValue)

instance Prelude.Hashable MeasureValue where
  hashWithSalt _salt MeasureValue' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData MeasureValue where
  rnf MeasureValue' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON MeasureValue where
  toJSON MeasureValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value),
            Prelude.Just ("Type" Data..= type')
          ]
      )
