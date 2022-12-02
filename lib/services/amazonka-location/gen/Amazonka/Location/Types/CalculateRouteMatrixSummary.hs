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
-- Module      : Amazonka.Location.Types.CalculateRouteMatrixSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.CalculateRouteMatrixSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.DistanceUnit
import qualified Amazonka.Prelude as Prelude

-- | A summary of the calculated route matrix.
--
-- /See:/ 'newCalculateRouteMatrixSummary' smart constructor.
data CalculateRouteMatrixSummary = CalculateRouteMatrixSummary'
  { -- | The data provider of traffic and road network data used to calculate the
    -- routes. Indicates one of the available providers:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The unit of measurement for route distances.
    distanceUnit :: DistanceUnit,
    -- | The count of error results in the route matrix. If this number is 0, all
    -- routes were calculated successfully.
    errorCount :: Prelude.Natural,
    -- | The count of cells in the route matrix. Equal to the number of
    -- @DeparturePositions@ multiplied by the number of @DestinationPositions@.
    routeCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteMatrixSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'calculateRouteMatrixSummary_dataSource' - The data provider of traffic and road network data used to calculate the
-- routes. Indicates one of the available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'distanceUnit', 'calculateRouteMatrixSummary_distanceUnit' - The unit of measurement for route distances.
--
-- 'errorCount', 'calculateRouteMatrixSummary_errorCount' - The count of error results in the route matrix. If this number is 0, all
-- routes were calculated successfully.
--
-- 'routeCount', 'calculateRouteMatrixSummary_routeCount' - The count of cells in the route matrix. Equal to the number of
-- @DeparturePositions@ multiplied by the number of @DestinationPositions@.
newCalculateRouteMatrixSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'distanceUnit'
  DistanceUnit ->
  -- | 'errorCount'
  Prelude.Natural ->
  -- | 'routeCount'
  Prelude.Natural ->
  CalculateRouteMatrixSummary
newCalculateRouteMatrixSummary
  pDataSource_
  pDistanceUnit_
  pErrorCount_
  pRouteCount_ =
    CalculateRouteMatrixSummary'
      { dataSource =
          pDataSource_,
        distanceUnit = pDistanceUnit_,
        errorCount = pErrorCount_,
        routeCount = pRouteCount_
      }

-- | The data provider of traffic and road network data used to calculate the
-- routes. Indicates one of the available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
calculateRouteMatrixSummary_dataSource :: Lens.Lens' CalculateRouteMatrixSummary Prelude.Text
calculateRouteMatrixSummary_dataSource = Lens.lens (\CalculateRouteMatrixSummary' {dataSource} -> dataSource) (\s@CalculateRouteMatrixSummary' {} a -> s {dataSource = a} :: CalculateRouteMatrixSummary)

-- | The unit of measurement for route distances.
calculateRouteMatrixSummary_distanceUnit :: Lens.Lens' CalculateRouteMatrixSummary DistanceUnit
calculateRouteMatrixSummary_distanceUnit = Lens.lens (\CalculateRouteMatrixSummary' {distanceUnit} -> distanceUnit) (\s@CalculateRouteMatrixSummary' {} a -> s {distanceUnit = a} :: CalculateRouteMatrixSummary)

-- | The count of error results in the route matrix. If this number is 0, all
-- routes were calculated successfully.
calculateRouteMatrixSummary_errorCount :: Lens.Lens' CalculateRouteMatrixSummary Prelude.Natural
calculateRouteMatrixSummary_errorCount = Lens.lens (\CalculateRouteMatrixSummary' {errorCount} -> errorCount) (\s@CalculateRouteMatrixSummary' {} a -> s {errorCount = a} :: CalculateRouteMatrixSummary)

-- | The count of cells in the route matrix. Equal to the number of
-- @DeparturePositions@ multiplied by the number of @DestinationPositions@.
calculateRouteMatrixSummary_routeCount :: Lens.Lens' CalculateRouteMatrixSummary Prelude.Natural
calculateRouteMatrixSummary_routeCount = Lens.lens (\CalculateRouteMatrixSummary' {routeCount} -> routeCount) (\s@CalculateRouteMatrixSummary' {} a -> s {routeCount = a} :: CalculateRouteMatrixSummary)

instance Data.FromJSON CalculateRouteMatrixSummary where
  parseJSON =
    Data.withObject
      "CalculateRouteMatrixSummary"
      ( \x ->
          CalculateRouteMatrixSummary'
            Prelude.<$> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "DistanceUnit")
            Prelude.<*> (x Data..: "ErrorCount")
            Prelude.<*> (x Data..: "RouteCount")
      )

instance Prelude.Hashable CalculateRouteMatrixSummary where
  hashWithSalt _salt CalculateRouteMatrixSummary' {..} =
    _salt `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` errorCount
      `Prelude.hashWithSalt` routeCount

instance Prelude.NFData CalculateRouteMatrixSummary where
  rnf CalculateRouteMatrixSummary' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf errorCount
      `Prelude.seq` Prelude.rnf routeCount
