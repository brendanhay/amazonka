{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.CalculateRouteMatrix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html Calculates a route matrix>
-- given the following required parameters: @DeparturePositions@ and
-- @DestinationPositions@. @CalculateRouteMatrix@ calculates routes and
-- returns the travel time and travel distance from each departure position
-- to each destination position in the request. For example, given
-- departure positions A and B, and destination positions X and Y,
-- @CalculateRouteMatrix@ will return time and distance for routes from A
-- to X, A to Y, B to X, and B to Y (in that order). The number of results
-- returned (and routes calculated) will be the number of
-- @DeparturePositions@ times the number of @DestinationPositions@.
--
-- Your account is charged for each route calculated, not the number of
-- requests.
--
-- Requires that you first
-- <https://docs.aws.amazon.com/location-routes/latest/APIReference/API_CreateRouteCalculator.html create a route calculator resource>.
--
-- By default, a request that doesn\'t specify a departure time uses the
-- best time of day to travel with the best traffic conditions when
-- calculating routes.
--
-- Additional options include:
--
-- -   <https://docs.aws.amazon.com/location/latest/developerguide/departure-time.html Specifying a departure time>
--     using either @DepartureTime@ or @DepartNow@. This calculates routes
--     based on predictive traffic data at the given time.
--
--     You can\'t specify both @DepartureTime@ and @DepartNow@ in a single
--     request. Specifying both parameters returns a validation error.
--
-- -   <https://docs.aws.amazon.com/location/latest/developerguide/travel-mode.html Specifying a travel mode>
--     using TravelMode sets the transportation mode used to calculate the
--     routes. This also lets you specify additional route preferences in
--     @CarModeOptions@ if traveling by @Car@, or @TruckModeOptions@ if
--     traveling by @Truck@.
module Amazonka.Location.CalculateRouteMatrix
  ( -- * Creating a Request
    CalculateRouteMatrix (..),
    newCalculateRouteMatrix,

    -- * Request Lenses
    calculateRouteMatrix_carModeOptions,
    calculateRouteMatrix_departNow,
    calculateRouteMatrix_departureTime,
    calculateRouteMatrix_distanceUnit,
    calculateRouteMatrix_travelMode,
    calculateRouteMatrix_truckModeOptions,
    calculateRouteMatrix_calculatorName,
    calculateRouteMatrix_departurePositions,
    calculateRouteMatrix_destinationPositions,

    -- * Destructuring the Response
    CalculateRouteMatrixResponse (..),
    newCalculateRouteMatrixResponse,

    -- * Response Lenses
    calculateRouteMatrixResponse_snappedDeparturePositions,
    calculateRouteMatrixResponse_snappedDestinationPositions,
    calculateRouteMatrixResponse_httpStatus,
    calculateRouteMatrixResponse_routeMatrix,
    calculateRouteMatrixResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCalculateRouteMatrix' smart constructor.
data CalculateRouteMatrix = CalculateRouteMatrix'
  { -- | Specifies route preferences when traveling by @Car@, such as avoiding
    -- routes that use ferries or tolls.
    --
    -- Requirements: @TravelMode@ must be specified as @Car@.
    carModeOptions :: Prelude.Maybe CalculateRouteCarModeOptions,
    -- | Sets the time of departure as the current time. Uses the current time to
    -- calculate the route matrix. You can\'t set both @DepartureTime@ and
    -- @DepartNow@. If neither is set, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route matrix.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    departNow :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the desired time of departure. Uses the given time to
    -- calculate the route matrix. You can\'t set both @DepartureTime@ and
    -- @DepartNow@. If neither is set, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route matrix.
    --
    -- Setting a departure time in the past returns a @400 ValidationException@
    -- error.
    --
    -- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    --     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
    --     @2020–07-2T12:15:20.000Z+01:00@
    departureTime :: Prelude.Maybe Data.ISO8601,
    -- | Set the unit system to specify the distance.
    --
    -- Default Value: @Kilometers@
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | Specifies the mode of transport when calculating a route. Used in
    -- estimating the speed of travel and road compatibility.
    --
    -- The @TravelMode@ you specify also determines how you specify route
    -- preferences:
    --
    -- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
    --
    -- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
    --
    -- Default Value: @Car@
    travelMode :: Prelude.Maybe TravelMode,
    -- | Specifies route preferences when traveling by @Truck@, such as avoiding
    -- routes that use ferries or tolls, and truck specifications to consider
    -- when choosing an optimal road.
    --
    -- Requirements: @TravelMode@ must be specified as @Truck@.
    truckModeOptions :: Prelude.Maybe CalculateRouteTruckModeOptions,
    -- | The name of the route calculator resource that you want to use to
    -- calculate the route matrix.
    calculatorName :: Prelude.Text,
    -- | The list of departure (origin) positions for the route matrix. An array
    -- of points, each of which is itself a 2-value array defined in
    -- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
    -- @[longitude, latitude]@. For example, @[-123.115, 49.285]@.
    --
    -- Depending on the data provider selected in the route calculator resource
    -- there may be additional restrictions on the inputs you can choose. See
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
    -- in the /Amazon Location Service Developer Guide/.
    --
    -- For route calculators that use Esri as the data provider, if you specify
    -- a departure that\'s not located on a road, Amazon Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
    -- The snapped value is available in the result in
    -- @SnappedDeparturePositions@.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    departurePositions :: Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | The list of destination positions for the route matrix. An array of
    -- points, each of which is itself a 2-value array defined in
    -- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
    -- @[longitude, latitude]@. For example, @[-122.339, 47.615]@
    --
    -- Depending on the data provider selected in the route calculator resource
    -- there may be additional restrictions on the inputs you can choose. See
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
    -- in the /Amazon Location Service Developer Guide/.
    --
    -- For route calculators that use Esri as the data provider, if you specify
    -- a destination that\'s not located on a road, Amazon Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
    -- The snapped value is available in the result in
    -- @SnappedDestinationPositions@.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    destinationPositions :: Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteMatrix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carModeOptions', 'calculateRouteMatrix_carModeOptions' - Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
--
-- 'departNow', 'calculateRouteMatrix_departNow' - Sets the time of departure as the current time. Uses the current time to
-- calculate the route matrix. You can\'t set both @DepartureTime@ and
-- @DepartNow@. If neither is set, the best time of day to travel with the
-- best traffic conditions is used to calculate the route matrix.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'departureTime', 'calculateRouteMatrix_departureTime' - Specifies the desired time of departure. Uses the given time to
-- calculate the route matrix. You can\'t set both @DepartureTime@ and
-- @DepartNow@. If neither is set, the best time of day to travel with the
-- best traffic conditions is used to calculate the route matrix.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
--
-- 'distanceUnit', 'calculateRouteMatrix_distanceUnit' - Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
--
-- 'travelMode', 'calculateRouteMatrix_travelMode' - Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility.
--
-- The @TravelMode@ you specify also determines how you specify route
-- preferences:
--
-- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
--
-- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
--
-- Default Value: @Car@
--
-- 'truckModeOptions', 'calculateRouteMatrix_truckModeOptions' - Specifies route preferences when traveling by @Truck@, such as avoiding
-- routes that use ferries or tolls, and truck specifications to consider
-- when choosing an optimal road.
--
-- Requirements: @TravelMode@ must be specified as @Truck@.
--
-- 'calculatorName', 'calculateRouteMatrix_calculatorName' - The name of the route calculator resource that you want to use to
-- calculate the route matrix.
--
-- 'departurePositions', 'calculateRouteMatrix_departurePositions' - The list of departure (origin) positions for the route matrix. An array
-- of points, each of which is itself a 2-value array defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@. For example, @[-123.115, 49.285]@.
--
-- Depending on the data provider selected in the route calculator resource
-- there may be additional restrictions on the inputs you can choose. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
-- in the /Amazon Location Service Developer Guide/.
--
-- For route calculators that use Esri as the data provider, if you specify
-- a departure that\'s not located on a road, Amazon Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- The snapped value is available in the result in
-- @SnappedDeparturePositions@.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
--
-- 'destinationPositions', 'calculateRouteMatrix_destinationPositions' - The list of destination positions for the route matrix. An array of
-- points, each of which is itself a 2-value array defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@. For example, @[-122.339, 47.615]@
--
-- Depending on the data provider selected in the route calculator resource
-- there may be additional restrictions on the inputs you can choose. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
-- in the /Amazon Location Service Developer Guide/.
--
-- For route calculators that use Esri as the data provider, if you specify
-- a destination that\'s not located on a road, Amazon Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- The snapped value is available in the result in
-- @SnappedDestinationPositions@.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
newCalculateRouteMatrix ::
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'departurePositions'
  Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double) ->
  -- | 'destinationPositions'
  Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double) ->
  CalculateRouteMatrix
newCalculateRouteMatrix
  pCalculatorName_
  pDeparturePositions_
  pDestinationPositions_ =
    CalculateRouteMatrix'
      { carModeOptions =
          Prelude.Nothing,
        departNow = Prelude.Nothing,
        departureTime = Prelude.Nothing,
        distanceUnit = Prelude.Nothing,
        travelMode = Prelude.Nothing,
        truckModeOptions = Prelude.Nothing,
        calculatorName = pCalculatorName_,
        departurePositions =
          Lens.coerced Lens.# pDeparturePositions_,
        destinationPositions =
          Lens.coerced Lens.# pDestinationPositions_
      }

-- | Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
calculateRouteMatrix_carModeOptions :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe CalculateRouteCarModeOptions)
calculateRouteMatrix_carModeOptions = Lens.lens (\CalculateRouteMatrix' {carModeOptions} -> carModeOptions) (\s@CalculateRouteMatrix' {} a -> s {carModeOptions = a} :: CalculateRouteMatrix)

-- | Sets the time of departure as the current time. Uses the current time to
-- calculate the route matrix. You can\'t set both @DepartureTime@ and
-- @DepartNow@. If neither is set, the best time of day to travel with the
-- best traffic conditions is used to calculate the route matrix.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteMatrix_departNow :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe Prelude.Bool)
calculateRouteMatrix_departNow = Lens.lens (\CalculateRouteMatrix' {departNow} -> departNow) (\s@CalculateRouteMatrix' {} a -> s {departNow = a} :: CalculateRouteMatrix)

-- | Specifies the desired time of departure. Uses the given time to
-- calculate the route matrix. You can\'t set both @DepartureTime@ and
-- @DepartNow@. If neither is set, the best time of day to travel with the
-- best traffic conditions is used to calculate the route matrix.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
calculateRouteMatrix_departureTime :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe Prelude.UTCTime)
calculateRouteMatrix_departureTime = Lens.lens (\CalculateRouteMatrix' {departureTime} -> departureTime) (\s@CalculateRouteMatrix' {} a -> s {departureTime = a} :: CalculateRouteMatrix) Prelude.. Lens.mapping Data._Time

-- | Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
calculateRouteMatrix_distanceUnit :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe DistanceUnit)
calculateRouteMatrix_distanceUnit = Lens.lens (\CalculateRouteMatrix' {distanceUnit} -> distanceUnit) (\s@CalculateRouteMatrix' {} a -> s {distanceUnit = a} :: CalculateRouteMatrix)

-- | Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility.
--
-- The @TravelMode@ you specify also determines how you specify route
-- preferences:
--
-- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
--
-- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
--
-- Default Value: @Car@
calculateRouteMatrix_travelMode :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe TravelMode)
calculateRouteMatrix_travelMode = Lens.lens (\CalculateRouteMatrix' {travelMode} -> travelMode) (\s@CalculateRouteMatrix' {} a -> s {travelMode = a} :: CalculateRouteMatrix)

-- | Specifies route preferences when traveling by @Truck@, such as avoiding
-- routes that use ferries or tolls, and truck specifications to consider
-- when choosing an optimal road.
--
-- Requirements: @TravelMode@ must be specified as @Truck@.
calculateRouteMatrix_truckModeOptions :: Lens.Lens' CalculateRouteMatrix (Prelude.Maybe CalculateRouteTruckModeOptions)
calculateRouteMatrix_truckModeOptions = Lens.lens (\CalculateRouteMatrix' {truckModeOptions} -> truckModeOptions) (\s@CalculateRouteMatrix' {} a -> s {truckModeOptions = a} :: CalculateRouteMatrix)

-- | The name of the route calculator resource that you want to use to
-- calculate the route matrix.
calculateRouteMatrix_calculatorName :: Lens.Lens' CalculateRouteMatrix Prelude.Text
calculateRouteMatrix_calculatorName = Lens.lens (\CalculateRouteMatrix' {calculatorName} -> calculatorName) (\s@CalculateRouteMatrix' {} a -> s {calculatorName = a} :: CalculateRouteMatrix)

-- | The list of departure (origin) positions for the route matrix. An array
-- of points, each of which is itself a 2-value array defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@. For example, @[-123.115, 49.285]@.
--
-- Depending on the data provider selected in the route calculator resource
-- there may be additional restrictions on the inputs you can choose. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
-- in the /Amazon Location Service Developer Guide/.
--
-- For route calculators that use Esri as the data provider, if you specify
-- a departure that\'s not located on a road, Amazon Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- The snapped value is available in the result in
-- @SnappedDeparturePositions@.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRouteMatrix_departurePositions :: Lens.Lens' CalculateRouteMatrix (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double))
calculateRouteMatrix_departurePositions = Lens.lens (\CalculateRouteMatrix' {departurePositions} -> departurePositions) (\s@CalculateRouteMatrix' {} a -> s {departurePositions = a} :: CalculateRouteMatrix) Prelude.. Lens.coerced

-- | The list of destination positions for the route matrix. An array of
-- points, each of which is itself a 2-value array defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@. For example, @[-122.339, 47.615]@
--
-- Depending on the data provider selected in the route calculator resource
-- there may be additional restrictions on the inputs you can choose. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route-matrix.html#matrix-routing-position-limits Position restrictions>
-- in the /Amazon Location Service Developer Guide/.
--
-- For route calculators that use Esri as the data provider, if you specify
-- a destination that\'s not located on a road, Amazon Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- The snapped value is available in the result in
-- @SnappedDestinationPositions@.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRouteMatrix_destinationPositions :: Lens.Lens' CalculateRouteMatrix (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double))
calculateRouteMatrix_destinationPositions = Lens.lens (\CalculateRouteMatrix' {destinationPositions} -> destinationPositions) (\s@CalculateRouteMatrix' {} a -> s {destinationPositions = a} :: CalculateRouteMatrix) Prelude.. Lens.coerced

instance Core.AWSRequest CalculateRouteMatrix where
  type
    AWSResponse CalculateRouteMatrix =
      CalculateRouteMatrixResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CalculateRouteMatrixResponse'
            Prelude.<$> (x Data..?> "SnappedDeparturePositions")
            Prelude.<*> (x Data..?> "SnappedDestinationPositions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "RouteMatrix" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Summary")
      )

instance Prelude.Hashable CalculateRouteMatrix where
  hashWithSalt _salt CalculateRouteMatrix' {..} =
    _salt `Prelude.hashWithSalt` carModeOptions
      `Prelude.hashWithSalt` departNow
      `Prelude.hashWithSalt` departureTime
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` travelMode
      `Prelude.hashWithSalt` truckModeOptions
      `Prelude.hashWithSalt` calculatorName
      `Prelude.hashWithSalt` departurePositions
      `Prelude.hashWithSalt` destinationPositions

instance Prelude.NFData CalculateRouteMatrix where
  rnf CalculateRouteMatrix' {..} =
    Prelude.rnf carModeOptions
      `Prelude.seq` Prelude.rnf departNow
      `Prelude.seq` Prelude.rnf departureTime
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf travelMode
      `Prelude.seq` Prelude.rnf truckModeOptions
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf departurePositions
      `Prelude.seq` Prelude.rnf destinationPositions

instance Data.ToHeaders CalculateRouteMatrix where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CalculateRouteMatrix where
  toJSON CalculateRouteMatrix' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CarModeOptions" Data..=)
              Prelude.<$> carModeOptions,
            ("DepartNow" Data..=) Prelude.<$> departNow,
            ("DepartureTime" Data..=) Prelude.<$> departureTime,
            ("DistanceUnit" Data..=) Prelude.<$> distanceUnit,
            ("TravelMode" Data..=) Prelude.<$> travelMode,
            ("TruckModeOptions" Data..=)
              Prelude.<$> truckModeOptions,
            Prelude.Just
              ("DeparturePositions" Data..= departurePositions),
            Prelude.Just
              ( "DestinationPositions"
                  Data..= destinationPositions
              )
          ]
      )

instance Data.ToPath CalculateRouteMatrix where
  toPath CalculateRouteMatrix' {..} =
    Prelude.mconcat
      [ "/routes/v0/calculators/",
        Data.toBS calculatorName,
        "/calculate/route-matrix"
      ]

instance Data.ToQuery CalculateRouteMatrix where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the result of the route matrix calculation.
--
-- /See:/ 'newCalculateRouteMatrixResponse' smart constructor.
data CalculateRouteMatrixResponse = CalculateRouteMatrixResponse'
  { -- | For routes calculated using an Esri route calculator resource, departure
    -- positions are snapped to the closest road. For Esri route calculator
    -- resources, this returns the list of departure\/origin positions used for
    -- calculation of the @RouteMatrix@.
    snappedDeparturePositions :: Prelude.Maybe (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double))),
    -- | The list of destination positions for the route matrix used for
    -- calculation of the @RouteMatrix@.
    snappedDestinationPositions :: Prelude.Maybe (Prelude.NonEmpty (Data.Sensitive (Prelude.NonEmpty Prelude.Double))),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The calculated route matrix containing the results for all pairs of
    -- @DeparturePositions@ to @DestinationPositions@. Each row corresponds to
    -- one entry in @DeparturePositions@. Each entry in the row corresponds to
    -- the route from that entry in @DeparturePositions@ to an entry in
    -- @DestinationPositions@.
    routeMatrix :: [[RouteMatrixEntry]],
    -- | Contains information about the route matrix, @DataSource@,
    -- @DistanceUnit@, @RouteCount@ and @ErrorCount@.
    summary :: CalculateRouteMatrixSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteMatrixResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snappedDeparturePositions', 'calculateRouteMatrixResponse_snappedDeparturePositions' - For routes calculated using an Esri route calculator resource, departure
-- positions are snapped to the closest road. For Esri route calculator
-- resources, this returns the list of departure\/origin positions used for
-- calculation of the @RouteMatrix@.
--
-- 'snappedDestinationPositions', 'calculateRouteMatrixResponse_snappedDestinationPositions' - The list of destination positions for the route matrix used for
-- calculation of the @RouteMatrix@.
--
-- 'httpStatus', 'calculateRouteMatrixResponse_httpStatus' - The response's http status code.
--
-- 'routeMatrix', 'calculateRouteMatrixResponse_routeMatrix' - The calculated route matrix containing the results for all pairs of
-- @DeparturePositions@ to @DestinationPositions@. Each row corresponds to
-- one entry in @DeparturePositions@. Each entry in the row corresponds to
-- the route from that entry in @DeparturePositions@ to an entry in
-- @DestinationPositions@.
--
-- 'summary', 'calculateRouteMatrixResponse_summary' - Contains information about the route matrix, @DataSource@,
-- @DistanceUnit@, @RouteCount@ and @ErrorCount@.
newCalculateRouteMatrixResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  CalculateRouteMatrixSummary ->
  CalculateRouteMatrixResponse
newCalculateRouteMatrixResponse
  pHttpStatus_
  pSummary_ =
    CalculateRouteMatrixResponse'
      { snappedDeparturePositions =
          Prelude.Nothing,
        snappedDestinationPositions = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        routeMatrix = Prelude.mempty,
        summary = pSummary_
      }

-- | For routes calculated using an Esri route calculator resource, departure
-- positions are snapped to the closest road. For Esri route calculator
-- resources, this returns the list of departure\/origin positions used for
-- calculation of the @RouteMatrix@.
calculateRouteMatrixResponse_snappedDeparturePositions :: Lens.Lens' CalculateRouteMatrixResponse (Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)))
calculateRouteMatrixResponse_snappedDeparturePositions = Lens.lens (\CalculateRouteMatrixResponse' {snappedDeparturePositions} -> snappedDeparturePositions) (\s@CalculateRouteMatrixResponse' {} a -> s {snappedDeparturePositions = a} :: CalculateRouteMatrixResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of destination positions for the route matrix used for
-- calculation of the @RouteMatrix@.
calculateRouteMatrixResponse_snappedDestinationPositions :: Lens.Lens' CalculateRouteMatrixResponse (Prelude.Maybe (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Double)))
calculateRouteMatrixResponse_snappedDestinationPositions = Lens.lens (\CalculateRouteMatrixResponse' {snappedDestinationPositions} -> snappedDestinationPositions) (\s@CalculateRouteMatrixResponse' {} a -> s {snappedDestinationPositions = a} :: CalculateRouteMatrixResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
calculateRouteMatrixResponse_httpStatus :: Lens.Lens' CalculateRouteMatrixResponse Prelude.Int
calculateRouteMatrixResponse_httpStatus = Lens.lens (\CalculateRouteMatrixResponse' {httpStatus} -> httpStatus) (\s@CalculateRouteMatrixResponse' {} a -> s {httpStatus = a} :: CalculateRouteMatrixResponse)

-- | The calculated route matrix containing the results for all pairs of
-- @DeparturePositions@ to @DestinationPositions@. Each row corresponds to
-- one entry in @DeparturePositions@. Each entry in the row corresponds to
-- the route from that entry in @DeparturePositions@ to an entry in
-- @DestinationPositions@.
calculateRouteMatrixResponse_routeMatrix :: Lens.Lens' CalculateRouteMatrixResponse [[RouteMatrixEntry]]
calculateRouteMatrixResponse_routeMatrix = Lens.lens (\CalculateRouteMatrixResponse' {routeMatrix} -> routeMatrix) (\s@CalculateRouteMatrixResponse' {} a -> s {routeMatrix = a} :: CalculateRouteMatrixResponse) Prelude.. Lens.coerced

-- | Contains information about the route matrix, @DataSource@,
-- @DistanceUnit@, @RouteCount@ and @ErrorCount@.
calculateRouteMatrixResponse_summary :: Lens.Lens' CalculateRouteMatrixResponse CalculateRouteMatrixSummary
calculateRouteMatrixResponse_summary = Lens.lens (\CalculateRouteMatrixResponse' {summary} -> summary) (\s@CalculateRouteMatrixResponse' {} a -> s {summary = a} :: CalculateRouteMatrixResponse)

instance Prelude.NFData CalculateRouteMatrixResponse where
  rnf CalculateRouteMatrixResponse' {..} =
    Prelude.rnf snappedDeparturePositions
      `Prelude.seq` Prelude.rnf snappedDestinationPositions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf routeMatrix
      `Prelude.seq` Prelude.rnf summary
