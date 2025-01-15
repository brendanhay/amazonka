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
-- Module      : Amazonka.Location.Types.CalculateRouteSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.CalculateRouteSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.DistanceUnit
import qualified Amazonka.Prelude as Prelude

-- | A summary of the calculated route.
--
-- /See:/ 'newCalculateRouteSummary' smart constructor.
data CalculateRouteSummary = CalculateRouteSummary'
  { -- | The data provider of traffic and road network data used to calculate the
    -- route. Indicates one of the available providers:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The total distance covered by the route. The sum of the distance
    -- travelled between every stop on the route.
    --
    -- If Esri is the data source for the route calculator, the route distance
    -- can’t be greater than 400 km. If the route exceeds 400 km, the response
    -- is a @400 RoutesValidationException@ error.
    distance :: Prelude.Double,
    -- | The unit of measurement for route distances.
    distanceUnit :: DistanceUnit,
    -- | The total travel time for the route measured in seconds. The sum of the
    -- travel time between every stop on the route.
    durationSeconds :: Prelude.Double,
    -- | Specifies a geographical box surrounding a route. Used to zoom into a
    -- route when displaying it in a map. For example,
    -- @[min x, min y, max x, max y]@.
    --
    -- The first 2 @bbox@ parameters describe the lower southwest corner:
    --
    -- -   The first @bbox@ position is the X coordinate or longitude of the
    --     lower southwest corner.
    --
    -- -   The second @bbox@ position is the Y coordinate or latitude of the
    --     lower southwest corner.
    --
    -- The next 2 @bbox@ parameters describe the upper northeast corner:
    --
    -- -   The third @bbox@ position is the X coordinate, or longitude of the
    --     upper northeast corner.
    --
    -- -   The fourth @bbox@ position is the Y coordinate, or latitude of the
    --     upper northeast corner.
    routeBBox :: Data.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'calculateRouteSummary_dataSource' - The data provider of traffic and road network data used to calculate the
-- route. Indicates one of the available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'distance', 'calculateRouteSummary_distance' - The total distance covered by the route. The sum of the distance
-- travelled between every stop on the route.
--
-- If Esri is the data source for the route calculator, the route distance
-- can’t be greater than 400 km. If the route exceeds 400 km, the response
-- is a @400 RoutesValidationException@ error.
--
-- 'distanceUnit', 'calculateRouteSummary_distanceUnit' - The unit of measurement for route distances.
--
-- 'durationSeconds', 'calculateRouteSummary_durationSeconds' - The total travel time for the route measured in seconds. The sum of the
-- travel time between every stop on the route.
--
-- 'routeBBox', 'calculateRouteSummary_routeBBox' - Specifies a geographical box surrounding a route. Used to zoom into a
-- route when displaying it in a map. For example,
-- @[min x, min y, max x, max y]@.
--
-- The first 2 @bbox@ parameters describe the lower southwest corner:
--
-- -   The first @bbox@ position is the X coordinate or longitude of the
--     lower southwest corner.
--
-- -   The second @bbox@ position is the Y coordinate or latitude of the
--     lower southwest corner.
--
-- The next 2 @bbox@ parameters describe the upper northeast corner:
--
-- -   The third @bbox@ position is the X coordinate, or longitude of the
--     upper northeast corner.
--
-- -   The fourth @bbox@ position is the Y coordinate, or latitude of the
--     upper northeast corner.
newCalculateRouteSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'distance'
  Prelude.Double ->
  -- | 'distanceUnit'
  DistanceUnit ->
  -- | 'durationSeconds'
  Prelude.Double ->
  -- | 'routeBBox'
  Prelude.NonEmpty Prelude.Double ->
  CalculateRouteSummary
newCalculateRouteSummary
  pDataSource_
  pDistance_
  pDistanceUnit_
  pDurationSeconds_
  pRouteBBox_ =
    CalculateRouteSummary'
      { dataSource = pDataSource_,
        distance = pDistance_,
        distanceUnit = pDistanceUnit_,
        durationSeconds = pDurationSeconds_,
        routeBBox =
          Data._Sensitive
            Prelude.. Lens.coerced
            Lens.# pRouteBBox_
      }

-- | The data provider of traffic and road network data used to calculate the
-- route. Indicates one of the available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
calculateRouteSummary_dataSource :: Lens.Lens' CalculateRouteSummary Prelude.Text
calculateRouteSummary_dataSource = Lens.lens (\CalculateRouteSummary' {dataSource} -> dataSource) (\s@CalculateRouteSummary' {} a -> s {dataSource = a} :: CalculateRouteSummary)

-- | The total distance covered by the route. The sum of the distance
-- travelled between every stop on the route.
--
-- If Esri is the data source for the route calculator, the route distance
-- can’t be greater than 400 km. If the route exceeds 400 km, the response
-- is a @400 RoutesValidationException@ error.
calculateRouteSummary_distance :: Lens.Lens' CalculateRouteSummary Prelude.Double
calculateRouteSummary_distance = Lens.lens (\CalculateRouteSummary' {distance} -> distance) (\s@CalculateRouteSummary' {} a -> s {distance = a} :: CalculateRouteSummary)

-- | The unit of measurement for route distances.
calculateRouteSummary_distanceUnit :: Lens.Lens' CalculateRouteSummary DistanceUnit
calculateRouteSummary_distanceUnit = Lens.lens (\CalculateRouteSummary' {distanceUnit} -> distanceUnit) (\s@CalculateRouteSummary' {} a -> s {distanceUnit = a} :: CalculateRouteSummary)

-- | The total travel time for the route measured in seconds. The sum of the
-- travel time between every stop on the route.
calculateRouteSummary_durationSeconds :: Lens.Lens' CalculateRouteSummary Prelude.Double
calculateRouteSummary_durationSeconds = Lens.lens (\CalculateRouteSummary' {durationSeconds} -> durationSeconds) (\s@CalculateRouteSummary' {} a -> s {durationSeconds = a} :: CalculateRouteSummary)

-- | Specifies a geographical box surrounding a route. Used to zoom into a
-- route when displaying it in a map. For example,
-- @[min x, min y, max x, max y]@.
--
-- The first 2 @bbox@ parameters describe the lower southwest corner:
--
-- -   The first @bbox@ position is the X coordinate or longitude of the
--     lower southwest corner.
--
-- -   The second @bbox@ position is the Y coordinate or latitude of the
--     lower southwest corner.
--
-- The next 2 @bbox@ parameters describe the upper northeast corner:
--
-- -   The third @bbox@ position is the X coordinate, or longitude of the
--     upper northeast corner.
--
-- -   The fourth @bbox@ position is the Y coordinate, or latitude of the
--     upper northeast corner.
calculateRouteSummary_routeBBox :: Lens.Lens' CalculateRouteSummary (Prelude.NonEmpty Prelude.Double)
calculateRouteSummary_routeBBox = Lens.lens (\CalculateRouteSummary' {routeBBox} -> routeBBox) (\s@CalculateRouteSummary' {} a -> s {routeBBox = a} :: CalculateRouteSummary) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Data.FromJSON CalculateRouteSummary where
  parseJSON =
    Data.withObject
      "CalculateRouteSummary"
      ( \x ->
          CalculateRouteSummary'
            Prelude.<$> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Distance")
            Prelude.<*> (x Data..: "DistanceUnit")
            Prelude.<*> (x Data..: "DurationSeconds")
            Prelude.<*> (x Data..: "RouteBBox")
      )

instance Prelude.Hashable CalculateRouteSummary where
  hashWithSalt _salt CalculateRouteSummary' {..} =
    _salt
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` routeBBox

instance Prelude.NFData CalculateRouteSummary where
  rnf CalculateRouteSummary' {..} =
    Prelude.rnf dataSource `Prelude.seq`
      Prelude.rnf distance `Prelude.seq`
        Prelude.rnf distanceUnit `Prelude.seq`
          Prelude.rnf durationSeconds `Prelude.seq`
            Prelude.rnf routeBBox
