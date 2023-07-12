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
-- Module      : Amazonka.QuickSight.Types.TimeEqualityFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeEqualityFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.TimeGranularity

-- | A @TimeEqualityFilter@ filters values that are equal to a given value.
--
-- /See:/ 'newTimeEqualityFilter' smart constructor.
data TimeEqualityFilter = TimeEqualityFilter'
  { -- | The parameter whose value should be used for the filter value.
    --
    -- This field is mutually exclusive to @Value@.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | The value of a @TimeEquality@ filter.
    --
    -- This field is mutually exclusive to @ParameterName@.
    value :: Prelude.Maybe Data.POSIX,
    -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeEqualityFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'timeEqualityFilter_parameterName' - The parameter whose value should be used for the filter value.
--
-- This field is mutually exclusive to @Value@.
--
-- 'timeGranularity', 'timeEqualityFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'value', 'timeEqualityFilter_value' - The value of a @TimeEquality@ filter.
--
-- This field is mutually exclusive to @ParameterName@.
--
-- 'filterId', 'timeEqualityFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'timeEqualityFilter_column' - The column that the filter is applied to.
newTimeEqualityFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  TimeEqualityFilter
newTimeEqualityFilter pFilterId_ pColumn_ =
  TimeEqualityFilter'
    { parameterName =
        Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      value = Prelude.Nothing,
      filterId = pFilterId_,
      column = pColumn_
    }

-- | The parameter whose value should be used for the filter value.
--
-- This field is mutually exclusive to @Value@.
timeEqualityFilter_parameterName :: Lens.Lens' TimeEqualityFilter (Prelude.Maybe Prelude.Text)
timeEqualityFilter_parameterName = Lens.lens (\TimeEqualityFilter' {parameterName} -> parameterName) (\s@TimeEqualityFilter' {} a -> s {parameterName = a} :: TimeEqualityFilter)

-- | The level of time precision that is used to aggregate @DateTime@ values.
timeEqualityFilter_timeGranularity :: Lens.Lens' TimeEqualityFilter (Prelude.Maybe TimeGranularity)
timeEqualityFilter_timeGranularity = Lens.lens (\TimeEqualityFilter' {timeGranularity} -> timeGranularity) (\s@TimeEqualityFilter' {} a -> s {timeGranularity = a} :: TimeEqualityFilter)

-- | The value of a @TimeEquality@ filter.
--
-- This field is mutually exclusive to @ParameterName@.
timeEqualityFilter_value :: Lens.Lens' TimeEqualityFilter (Prelude.Maybe Prelude.UTCTime)
timeEqualityFilter_value = Lens.lens (\TimeEqualityFilter' {value} -> value) (\s@TimeEqualityFilter' {} a -> s {value = a} :: TimeEqualityFilter) Prelude.. Lens.mapping Data._Time

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
timeEqualityFilter_filterId :: Lens.Lens' TimeEqualityFilter Prelude.Text
timeEqualityFilter_filterId = Lens.lens (\TimeEqualityFilter' {filterId} -> filterId) (\s@TimeEqualityFilter' {} a -> s {filterId = a} :: TimeEqualityFilter)

-- | The column that the filter is applied to.
timeEqualityFilter_column :: Lens.Lens' TimeEqualityFilter ColumnIdentifier
timeEqualityFilter_column = Lens.lens (\TimeEqualityFilter' {column} -> column) (\s@TimeEqualityFilter' {} a -> s {column = a} :: TimeEqualityFilter)

instance Data.FromJSON TimeEqualityFilter where
  parseJSON =
    Data.withObject
      "TimeEqualityFilter"
      ( \x ->
          TimeEqualityFilter'
            Prelude.<$> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable TimeEqualityFilter where
  hashWithSalt _salt TimeEqualityFilter' {..} =
    _salt
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column

instance Prelude.NFData TimeEqualityFilter where
  rnf TimeEqualityFilter' {..} =
    Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON TimeEqualityFilter where
  toJSON TimeEqualityFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
