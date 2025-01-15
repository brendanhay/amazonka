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
-- Module      : Amazonka.TimeStreamWrite.Types.Record
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.Dimension
import Amazonka.TimeStreamWrite.Types.MeasureValue
import Amazonka.TimeStreamWrite.Types.MeasureValueType
import Amazonka.TimeStreamWrite.Types.TimeUnit

-- | Record represents a time series data point being written into
-- Timestream. Each record contains an array of dimensions. Dimensions
-- represent the meta data attributes of a time series data point such as
-- the instance name or availability zone of an EC2 instance. A record also
-- contains the measure name which is the name of the measure being
-- collected for example the CPU utilization of an EC2 instance. A record
-- also contains the measure value and the value type which is the data
-- type of the measure value. In addition, the record contains the
-- timestamp when the measure was collected that the timestamp unit which
-- represents the granularity of the timestamp.
--
-- Records have a @Version@ field, which is a 64-bit @long@ that you can
-- use for updating data points. Writes of a duplicate record with the same
-- dimension, timestamp, and measure name but different measure value will
-- only succeed if the @Version@ attribute of the record in the write
-- request is higher than that of the existing record. Timestream defaults
-- to a @Version@ of @1@ for records without the @Version@ field.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | Contains the list of dimensions for time series data points.
    dimensions :: Prelude.Maybe [Dimension],
    -- | Measure represents the data attribute of the time series. For example,
    -- the CPU utilization of an EC2 instance or the RPM of a wind turbine are
    -- measures.
    measureName :: Prelude.Maybe Prelude.Text,
    -- | Contains the measure value for the time series data point.
    measureValue :: Prelude.Maybe Prelude.Text,
    -- | Contains the data type of the measure value for the time series data
    -- point. Default type is @DOUBLE@.
    measureValueType :: Prelude.Maybe MeasureValueType,
    -- | Contains the list of MeasureValue for time series data points.
    --
    -- This is only allowed for type @MULTI@. For scalar values, use
    -- @MeasureValue@ attribute of the Record directly.
    measureValues :: Prelude.Maybe [MeasureValue],
    -- | Contains the time at which the measure value for the data point was
    -- collected. The time value plus the unit provides the time elapsed since
    -- the epoch. For example, if the time value is @12345@ and the unit is
    -- @ms@, then @12345 ms@ have elapsed since the epoch.
    time :: Prelude.Maybe Prelude.Text,
    -- | The granularity of the timestamp unit. It indicates if the time value is
    -- in seconds, milliseconds, nanoseconds or other supported values. Default
    -- is @MILLISECONDS@.
    timeUnit :: Prelude.Maybe TimeUnit,
    -- | 64-bit attribute used for record updates. Write requests for duplicate
    -- data with a higher version number will update the existing measure value
    -- and version. In cases where the measure value is the same, @Version@
    -- will still be updated . Default value is @1@.
    --
    -- @Version@ must be @1@ or greater, or you will receive a
    -- @ValidationException@ error.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'record_dimensions' - Contains the list of dimensions for time series data points.
--
-- 'measureName', 'record_measureName' - Measure represents the data attribute of the time series. For example,
-- the CPU utilization of an EC2 instance or the RPM of a wind turbine are
-- measures.
--
-- 'measureValue', 'record_measureValue' - Contains the measure value for the time series data point.
--
-- 'measureValueType', 'record_measureValueType' - Contains the data type of the measure value for the time series data
-- point. Default type is @DOUBLE@.
--
-- 'measureValues', 'record_measureValues' - Contains the list of MeasureValue for time series data points.
--
-- This is only allowed for type @MULTI@. For scalar values, use
-- @MeasureValue@ attribute of the Record directly.
--
-- 'time', 'record_time' - Contains the time at which the measure value for the data point was
-- collected. The time value plus the unit provides the time elapsed since
-- the epoch. For example, if the time value is @12345@ and the unit is
-- @ms@, then @12345 ms@ have elapsed since the epoch.
--
-- 'timeUnit', 'record_timeUnit' - The granularity of the timestamp unit. It indicates if the time value is
-- in seconds, milliseconds, nanoseconds or other supported values. Default
-- is @MILLISECONDS@.
--
-- 'version', 'record_version' - 64-bit attribute used for record updates. Write requests for duplicate
-- data with a higher version number will update the existing measure value
-- and version. In cases where the measure value is the same, @Version@
-- will still be updated . Default value is @1@.
--
-- @Version@ must be @1@ or greater, or you will receive a
-- @ValidationException@ error.
newRecord ::
  Record
newRecord =
  Record'
    { dimensions = Prelude.Nothing,
      measureName = Prelude.Nothing,
      measureValue = Prelude.Nothing,
      measureValueType = Prelude.Nothing,
      measureValues = Prelude.Nothing,
      time = Prelude.Nothing,
      timeUnit = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Contains the list of dimensions for time series data points.
record_dimensions :: Lens.Lens' Record (Prelude.Maybe [Dimension])
record_dimensions = Lens.lens (\Record' {dimensions} -> dimensions) (\s@Record' {} a -> s {dimensions = a} :: Record) Prelude.. Lens.mapping Lens.coerced

-- | Measure represents the data attribute of the time series. For example,
-- the CPU utilization of an EC2 instance or the RPM of a wind turbine are
-- measures.
record_measureName :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_measureName = Lens.lens (\Record' {measureName} -> measureName) (\s@Record' {} a -> s {measureName = a} :: Record)

-- | Contains the measure value for the time series data point.
record_measureValue :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_measureValue = Lens.lens (\Record' {measureValue} -> measureValue) (\s@Record' {} a -> s {measureValue = a} :: Record)

-- | Contains the data type of the measure value for the time series data
-- point. Default type is @DOUBLE@.
record_measureValueType :: Lens.Lens' Record (Prelude.Maybe MeasureValueType)
record_measureValueType = Lens.lens (\Record' {measureValueType} -> measureValueType) (\s@Record' {} a -> s {measureValueType = a} :: Record)

-- | Contains the list of MeasureValue for time series data points.
--
-- This is only allowed for type @MULTI@. For scalar values, use
-- @MeasureValue@ attribute of the Record directly.
record_measureValues :: Lens.Lens' Record (Prelude.Maybe [MeasureValue])
record_measureValues = Lens.lens (\Record' {measureValues} -> measureValues) (\s@Record' {} a -> s {measureValues = a} :: Record) Prelude.. Lens.mapping Lens.coerced

-- | Contains the time at which the measure value for the data point was
-- collected. The time value plus the unit provides the time elapsed since
-- the epoch. For example, if the time value is @12345@ and the unit is
-- @ms@, then @12345 ms@ have elapsed since the epoch.
record_time :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_time = Lens.lens (\Record' {time} -> time) (\s@Record' {} a -> s {time = a} :: Record)

-- | The granularity of the timestamp unit. It indicates if the time value is
-- in seconds, milliseconds, nanoseconds or other supported values. Default
-- is @MILLISECONDS@.
record_timeUnit :: Lens.Lens' Record (Prelude.Maybe TimeUnit)
record_timeUnit = Lens.lens (\Record' {timeUnit} -> timeUnit) (\s@Record' {} a -> s {timeUnit = a} :: Record)

-- | 64-bit attribute used for record updates. Write requests for duplicate
-- data with a higher version number will update the existing measure value
-- and version. In cases where the measure value is the same, @Version@
-- will still be updated . Default value is @1@.
--
-- @Version@ must be @1@ or greater, or you will receive a
-- @ValidationException@ error.
record_version :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_version = Lens.lens (\Record' {version} -> version) (\s@Record' {} a -> s {version = a} :: Record)

instance Prelude.Hashable Record where
  hashWithSalt _salt Record' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` measureName
      `Prelude.hashWithSalt` measureValue
      `Prelude.hashWithSalt` measureValueType
      `Prelude.hashWithSalt` measureValues
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` timeUnit
      `Prelude.hashWithSalt` version

instance Prelude.NFData Record where
  rnf Record' {..} =
    Prelude.rnf dimensions `Prelude.seq`
      Prelude.rnf measureName `Prelude.seq`
        Prelude.rnf measureValue `Prelude.seq`
          Prelude.rnf measureValueType `Prelude.seq`
            Prelude.rnf measureValues `Prelude.seq`
              Prelude.rnf time `Prelude.seq`
                Prelude.rnf timeUnit `Prelude.seq`
                  Prelude.rnf version

instance Data.ToJSON Record where
  toJSON Record' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("MeasureName" Data..=) Prelude.<$> measureName,
            ("MeasureValue" Data..=) Prelude.<$> measureValue,
            ("MeasureValueType" Data..=)
              Prelude.<$> measureValueType,
            ("MeasureValues" Data..=) Prelude.<$> measureValues,
            ("Time" Data..=) Prelude.<$> time,
            ("TimeUnit" Data..=) Prelude.<$> timeUnit,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
