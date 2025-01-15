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
-- Module      : Amazonka.QuickSight.Types.TimeRangeFilterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeRangeFilterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RollingDateConfiguration

-- | The value of a time range filter.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newTimeRangeFilterValue' smart constructor.
data TimeRangeFilterValue = TimeRangeFilterValue'
  { -- | The parameter type input value.
    parameter :: Prelude.Maybe Prelude.Text,
    -- | The rolling date input value.
    rollingDate :: Prelude.Maybe RollingDateConfiguration,
    -- | The static input value.
    staticValue :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameter', 'timeRangeFilterValue_parameter' - The parameter type input value.
--
-- 'rollingDate', 'timeRangeFilterValue_rollingDate' - The rolling date input value.
--
-- 'staticValue', 'timeRangeFilterValue_staticValue' - The static input value.
newTimeRangeFilterValue ::
  TimeRangeFilterValue
newTimeRangeFilterValue =
  TimeRangeFilterValue'
    { parameter = Prelude.Nothing,
      rollingDate = Prelude.Nothing,
      staticValue = Prelude.Nothing
    }

-- | The parameter type input value.
timeRangeFilterValue_parameter :: Lens.Lens' TimeRangeFilterValue (Prelude.Maybe Prelude.Text)
timeRangeFilterValue_parameter = Lens.lens (\TimeRangeFilterValue' {parameter} -> parameter) (\s@TimeRangeFilterValue' {} a -> s {parameter = a} :: TimeRangeFilterValue)

-- | The rolling date input value.
timeRangeFilterValue_rollingDate :: Lens.Lens' TimeRangeFilterValue (Prelude.Maybe RollingDateConfiguration)
timeRangeFilterValue_rollingDate = Lens.lens (\TimeRangeFilterValue' {rollingDate} -> rollingDate) (\s@TimeRangeFilterValue' {} a -> s {rollingDate = a} :: TimeRangeFilterValue)

-- | The static input value.
timeRangeFilterValue_staticValue :: Lens.Lens' TimeRangeFilterValue (Prelude.Maybe Prelude.UTCTime)
timeRangeFilterValue_staticValue = Lens.lens (\TimeRangeFilterValue' {staticValue} -> staticValue) (\s@TimeRangeFilterValue' {} a -> s {staticValue = a} :: TimeRangeFilterValue) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimeRangeFilterValue where
  parseJSON =
    Data.withObject
      "TimeRangeFilterValue"
      ( \x ->
          TimeRangeFilterValue'
            Prelude.<$> (x Data..:? "Parameter")
            Prelude.<*> (x Data..:? "RollingDate")
            Prelude.<*> (x Data..:? "StaticValue")
      )

instance Prelude.Hashable TimeRangeFilterValue where
  hashWithSalt _salt TimeRangeFilterValue' {..} =
    _salt
      `Prelude.hashWithSalt` parameter
      `Prelude.hashWithSalt` rollingDate
      `Prelude.hashWithSalt` staticValue

instance Prelude.NFData TimeRangeFilterValue where
  rnf TimeRangeFilterValue' {..} =
    Prelude.rnf parameter `Prelude.seq`
      Prelude.rnf rollingDate `Prelude.seq`
        Prelude.rnf staticValue

instance Data.ToJSON TimeRangeFilterValue where
  toJSON TimeRangeFilterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameter" Data..=) Prelude.<$> parameter,
            ("RollingDate" Data..=) Prelude.<$> rollingDate,
            ("StaticValue" Data..=) Prelude.<$> staticValue
          ]
      )
