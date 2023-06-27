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
-- Module      : Amazonka.TimeStreamWrite.Types.DataModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DataModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.DimensionMapping
import Amazonka.TimeStreamWrite.Types.MixedMeasureMapping
import Amazonka.TimeStreamWrite.Types.MultiMeasureMappings
import Amazonka.TimeStreamWrite.Types.TimeUnit

-- | Data model for a batch load task.
--
-- /See:/ 'newDataModel' smart constructor.
data DataModel = DataModel'
  { measureNameColumn :: Prelude.Maybe Prelude.Text,
    -- | Source to target mappings for measures.
    mixedMeasureMappings :: Prelude.Maybe (Prelude.NonEmpty MixedMeasureMapping),
    -- | Source to target mappings for multi-measure records.
    multiMeasureMappings :: Prelude.Maybe MultiMeasureMappings,
    -- | Source column to be mapped to time.
    timeColumn :: Prelude.Maybe Prelude.Text,
    -- | The granularity of the timestamp unit. It indicates if the time value is
    -- in seconds, milliseconds, nanoseconds, or other supported values.
    -- Default is @MILLISECONDS@.
    timeUnit :: Prelude.Maybe TimeUnit,
    -- | Source to target mappings for dimensions.
    dimensionMappings :: Prelude.NonEmpty DimensionMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'measureNameColumn', 'dataModel_measureNameColumn' -
--
-- 'mixedMeasureMappings', 'dataModel_mixedMeasureMappings' - Source to target mappings for measures.
--
-- 'multiMeasureMappings', 'dataModel_multiMeasureMappings' - Source to target mappings for multi-measure records.
--
-- 'timeColumn', 'dataModel_timeColumn' - Source column to be mapped to time.
--
-- 'timeUnit', 'dataModel_timeUnit' - The granularity of the timestamp unit. It indicates if the time value is
-- in seconds, milliseconds, nanoseconds, or other supported values.
-- Default is @MILLISECONDS@.
--
-- 'dimensionMappings', 'dataModel_dimensionMappings' - Source to target mappings for dimensions.
newDataModel ::
  -- | 'dimensionMappings'
  Prelude.NonEmpty DimensionMapping ->
  DataModel
newDataModel pDimensionMappings_ =
  DataModel'
    { measureNameColumn = Prelude.Nothing,
      mixedMeasureMappings = Prelude.Nothing,
      multiMeasureMappings = Prelude.Nothing,
      timeColumn = Prelude.Nothing,
      timeUnit = Prelude.Nothing,
      dimensionMappings =
        Lens.coerced Lens.# pDimensionMappings_
    }

dataModel_measureNameColumn :: Lens.Lens' DataModel (Prelude.Maybe Prelude.Text)
dataModel_measureNameColumn = Lens.lens (\DataModel' {measureNameColumn} -> measureNameColumn) (\s@DataModel' {} a -> s {measureNameColumn = a} :: DataModel)

-- | Source to target mappings for measures.
dataModel_mixedMeasureMappings :: Lens.Lens' DataModel (Prelude.Maybe (Prelude.NonEmpty MixedMeasureMapping))
dataModel_mixedMeasureMappings = Lens.lens (\DataModel' {mixedMeasureMappings} -> mixedMeasureMappings) (\s@DataModel' {} a -> s {mixedMeasureMappings = a} :: DataModel) Prelude.. Lens.mapping Lens.coerced

-- | Source to target mappings for multi-measure records.
dataModel_multiMeasureMappings :: Lens.Lens' DataModel (Prelude.Maybe MultiMeasureMappings)
dataModel_multiMeasureMappings = Lens.lens (\DataModel' {multiMeasureMappings} -> multiMeasureMappings) (\s@DataModel' {} a -> s {multiMeasureMappings = a} :: DataModel)

-- | Source column to be mapped to time.
dataModel_timeColumn :: Lens.Lens' DataModel (Prelude.Maybe Prelude.Text)
dataModel_timeColumn = Lens.lens (\DataModel' {timeColumn} -> timeColumn) (\s@DataModel' {} a -> s {timeColumn = a} :: DataModel)

-- | The granularity of the timestamp unit. It indicates if the time value is
-- in seconds, milliseconds, nanoseconds, or other supported values.
-- Default is @MILLISECONDS@.
dataModel_timeUnit :: Lens.Lens' DataModel (Prelude.Maybe TimeUnit)
dataModel_timeUnit = Lens.lens (\DataModel' {timeUnit} -> timeUnit) (\s@DataModel' {} a -> s {timeUnit = a} :: DataModel)

-- | Source to target mappings for dimensions.
dataModel_dimensionMappings :: Lens.Lens' DataModel (Prelude.NonEmpty DimensionMapping)
dataModel_dimensionMappings = Lens.lens (\DataModel' {dimensionMappings} -> dimensionMappings) (\s@DataModel' {} a -> s {dimensionMappings = a} :: DataModel) Prelude.. Lens.coerced

instance Data.FromJSON DataModel where
  parseJSON =
    Data.withObject
      "DataModel"
      ( \x ->
          DataModel'
            Prelude.<$> (x Data..:? "MeasureNameColumn")
            Prelude.<*> (x Data..:? "MixedMeasureMappings")
            Prelude.<*> (x Data..:? "MultiMeasureMappings")
            Prelude.<*> (x Data..:? "TimeColumn")
            Prelude.<*> (x Data..:? "TimeUnit")
            Prelude.<*> (x Data..: "DimensionMappings")
      )

instance Prelude.Hashable DataModel where
  hashWithSalt _salt DataModel' {..} =
    _salt
      `Prelude.hashWithSalt` measureNameColumn
      `Prelude.hashWithSalt` mixedMeasureMappings
      `Prelude.hashWithSalt` multiMeasureMappings
      `Prelude.hashWithSalt` timeColumn
      `Prelude.hashWithSalt` timeUnit
      `Prelude.hashWithSalt` dimensionMappings

instance Prelude.NFData DataModel where
  rnf DataModel' {..} =
    Prelude.rnf measureNameColumn
      `Prelude.seq` Prelude.rnf mixedMeasureMappings
      `Prelude.seq` Prelude.rnf multiMeasureMappings
      `Prelude.seq` Prelude.rnf timeColumn
      `Prelude.seq` Prelude.rnf timeUnit
      `Prelude.seq` Prelude.rnf dimensionMappings

instance Data.ToJSON DataModel where
  toJSON DataModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MeasureNameColumn" Data..=)
              Prelude.<$> measureNameColumn,
            ("MixedMeasureMappings" Data..=)
              Prelude.<$> mixedMeasureMappings,
            ("MultiMeasureMappings" Data..=)
              Prelude.<$> multiMeasureMappings,
            ("TimeColumn" Data..=) Prelude.<$> timeColumn,
            ("TimeUnit" Data..=) Prelude.<$> timeUnit,
            Prelude.Just
              ("DimensionMappings" Data..= dimensionMappings)
          ]
      )
