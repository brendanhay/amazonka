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
-- Module      : Amazonka.Location.CalculateRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html Calculates a route>
-- given the following required parameters: @DeparturePosition@ and
-- @DestinationPosition@. Requires that you first
-- <https://docs.aws.amazon.com/location-routes/latest/APIReference/API_CreateRouteCalculator.html create a route calculator resource>.
--
-- By default, a request that doesn\'t specify a departure time uses the
-- best time of day to travel with the best traffic conditions when
-- calculating the route.
--
-- Additional options include:
--
-- -   <https://docs.aws.amazon.com/location/latest/developerguide/departure-time.html Specifying a departure time>
--     using either @DepartureTime@ or @DepartNow@. This calculates a route
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
--
--     If you specify @walking@ for the travel mode and your data provider
--     is Esri, the start and destination must be within 40km.
module Amazonka.Location.CalculateRoute
  ( -- * Creating a Request
    CalculateRoute (..),
    newCalculateRoute,

    -- * Request Lenses
    calculateRoute_carModeOptions,
    calculateRoute_departNow,
    calculateRoute_departureTime,
    calculateRoute_distanceUnit,
    calculateRoute_includeLegGeometry,
    calculateRoute_travelMode,
    calculateRoute_truckModeOptions,
    calculateRoute_waypointPositions,
    calculateRoute_calculatorName,
    calculateRoute_departurePosition,
    calculateRoute_destinationPosition,

    -- * Destructuring the Response
    CalculateRouteResponse (..),
    newCalculateRouteResponse,

    -- * Response Lenses
    calculateRouteResponse_httpStatus,
    calculateRouteResponse_legs,
    calculateRouteResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCalculateRoute' smart constructor.
data CalculateRoute = CalculateRoute'
  { -- | Specifies route preferences when traveling by @Car@, such as avoiding
    -- routes that use ferries or tolls.
    --
    -- Requirements: @TravelMode@ must be specified as @Car@.
    carModeOptions :: Prelude.Maybe CalculateRouteCarModeOptions,
    -- | Sets the time of departure as the current time. Uses the current time to
    -- calculate a route. Otherwise, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    departNow :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the desired time of departure. Uses the given time to
    -- calculate the route. Otherwise, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route.
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
    -- | Set to include the geometry details in the result for each path between
    -- a pair of positions.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    includeLegGeometry :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the mode of transport when calculating a route. Used in
    -- estimating the speed of travel and road compatibility. You can choose
    -- @Car@, @Truck@, or @Walking@ as options for the @TravelMode@.
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
    -- | Specifies an ordered list of up to 23 intermediate positions to include
    -- along a route between the departure position and destination position.
    --
    -- -   For example, from the @DeparturePosition@ @[-123.115, 49.285]@, the
    --     route follows the order that the waypoint positions are given
    --     @[[-122.757, 49.0021],[-122.349, 47.620]]@
    --
    -- If you specify a waypoint position that\'s not located on a road, Amazon
    -- Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
    --
    -- Specifying more than 23 waypoints returns a @400 ValidationException@
    -- error.
    --
    -- If Esri is the provider for your route calculator, specifying a route
    -- that is longer than 400 km returns a @400 RoutesValidationException@
    -- error.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    waypointPositions :: Prelude.Maybe [Data.Sensitive (Prelude.NonEmpty Prelude.Double)],
    -- | The name of the route calculator resource that you want to use to
    -- calculate the route.
    calculatorName :: Prelude.Text,
    -- | The start position for the route. Defined in
    -- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
    -- format: @[longitude, latitude]@.
    --
    -- -   For example, @[-123.115, 49.285]@
    --
    -- If you specify a departure that\'s not located on a road, Amazon
    -- Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
    -- If Esri is the provider for your route calculator, specifying a route
    -- that is longer than 400 km returns a @400 RoutesValidationException@
    -- error.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    departurePosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The finish position for the route. Defined in
    -- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
    -- format: @[longitude, latitude]@.
    --
    -- -   For example, @[-122.339, 47.615]@
    --
    -- If you specify a destination that\'s not located on a road, Amazon
    -- Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    destinationPosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carModeOptions', 'calculateRoute_carModeOptions' - Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
--
-- 'departNow', 'calculateRoute_departNow' - Sets the time of departure as the current time. Uses the current time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'departureTime', 'calculateRoute_departureTime' - Specifies the desired time of departure. Uses the given time to
-- calculate the route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
--
-- 'distanceUnit', 'calculateRoute_distanceUnit' - Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
--
-- 'includeLegGeometry', 'calculateRoute_includeLegGeometry' - Set to include the geometry details in the result for each path between
-- a pair of positions.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'travelMode', 'calculateRoute_travelMode' - Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility. You can choose
-- @Car@, @Truck@, or @Walking@ as options for the @TravelMode@.
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
-- 'truckModeOptions', 'calculateRoute_truckModeOptions' - Specifies route preferences when traveling by @Truck@, such as avoiding
-- routes that use ferries or tolls, and truck specifications to consider
-- when choosing an optimal road.
--
-- Requirements: @TravelMode@ must be specified as @Truck@.
--
-- 'waypointPositions', 'calculateRoute_waypointPositions' - Specifies an ordered list of up to 23 intermediate positions to include
-- along a route between the departure position and destination position.
--
-- -   For example, from the @DeparturePosition@ @[-123.115, 49.285]@, the
--     route follows the order that the waypoint positions are given
--     @[[-122.757, 49.0021],[-122.349, 47.620]]@
--
-- If you specify a waypoint position that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
--
-- Specifying more than 23 waypoints returns a @400 ValidationException@
-- error.
--
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
--
-- 'calculatorName', 'calculateRoute_calculatorName' - The name of the route calculator resource that you want to use to
-- calculate the route.
--
-- 'departurePosition', 'calculateRoute_departurePosition' - The start position for the route. Defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
-- format: @[longitude, latitude]@.
--
-- -   For example, @[-123.115, 49.285]@
--
-- If you specify a departure that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
--
-- 'destinationPosition', 'calculateRoute_destinationPosition' - The finish position for the route. Defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
-- format: @[longitude, latitude]@.
--
-- -   For example, @[-122.339, 47.615]@
--
-- If you specify a destination that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
newCalculateRoute ::
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'departurePosition'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'destinationPosition'
  Prelude.NonEmpty Prelude.Double ->
  CalculateRoute
newCalculateRoute
  pCalculatorName_
  pDeparturePosition_
  pDestinationPosition_ =
    CalculateRoute'
      { carModeOptions = Prelude.Nothing,
        departNow = Prelude.Nothing,
        departureTime = Prelude.Nothing,
        distanceUnit = Prelude.Nothing,
        includeLegGeometry = Prelude.Nothing,
        travelMode = Prelude.Nothing,
        truckModeOptions = Prelude.Nothing,
        waypointPositions = Prelude.Nothing,
        calculatorName = pCalculatorName_,
        departurePosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pDeparturePosition_,
        destinationPosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pDestinationPosition_
      }

-- | Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
calculateRoute_carModeOptions :: Lens.Lens' CalculateRoute (Prelude.Maybe CalculateRouteCarModeOptions)
calculateRoute_carModeOptions = Lens.lens (\CalculateRoute' {carModeOptions} -> carModeOptions) (\s@CalculateRoute' {} a -> s {carModeOptions = a} :: CalculateRoute)

-- | Sets the time of departure as the current time. Uses the current time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRoute_departNow :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.Bool)
calculateRoute_departNow = Lens.lens (\CalculateRoute' {departNow} -> departNow) (\s@CalculateRoute' {} a -> s {departNow = a} :: CalculateRoute)

-- | Specifies the desired time of departure. Uses the given time to
-- calculate the route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
calculateRoute_departureTime :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.UTCTime)
calculateRoute_departureTime = Lens.lens (\CalculateRoute' {departureTime} -> departureTime) (\s@CalculateRoute' {} a -> s {departureTime = a} :: CalculateRoute) Prelude.. Lens.mapping Data._Time

-- | Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
calculateRoute_distanceUnit :: Lens.Lens' CalculateRoute (Prelude.Maybe DistanceUnit)
calculateRoute_distanceUnit = Lens.lens (\CalculateRoute' {distanceUnit} -> distanceUnit) (\s@CalculateRoute' {} a -> s {distanceUnit = a} :: CalculateRoute)

-- | Set to include the geometry details in the result for each path between
-- a pair of positions.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRoute_includeLegGeometry :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.Bool)
calculateRoute_includeLegGeometry = Lens.lens (\CalculateRoute' {includeLegGeometry} -> includeLegGeometry) (\s@CalculateRoute' {} a -> s {includeLegGeometry = a} :: CalculateRoute)

-- | Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility. You can choose
-- @Car@, @Truck@, or @Walking@ as options for the @TravelMode@.
--
-- The @TravelMode@ you specify also determines how you specify route
-- preferences:
--
-- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
--
-- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
--
-- Default Value: @Car@
calculateRoute_travelMode :: Lens.Lens' CalculateRoute (Prelude.Maybe TravelMode)
calculateRoute_travelMode = Lens.lens (\CalculateRoute' {travelMode} -> travelMode) (\s@CalculateRoute' {} a -> s {travelMode = a} :: CalculateRoute)

-- | Specifies route preferences when traveling by @Truck@, such as avoiding
-- routes that use ferries or tolls, and truck specifications to consider
-- when choosing an optimal road.
--
-- Requirements: @TravelMode@ must be specified as @Truck@.
calculateRoute_truckModeOptions :: Lens.Lens' CalculateRoute (Prelude.Maybe CalculateRouteTruckModeOptions)
calculateRoute_truckModeOptions = Lens.lens (\CalculateRoute' {truckModeOptions} -> truckModeOptions) (\s@CalculateRoute' {} a -> s {truckModeOptions = a} :: CalculateRoute)

-- | Specifies an ordered list of up to 23 intermediate positions to include
-- along a route between the departure position and destination position.
--
-- -   For example, from the @DeparturePosition@ @[-123.115, 49.285]@, the
--     route follows the order that the waypoint positions are given
--     @[[-122.757, 49.0021],[-122.349, 47.620]]@
--
-- If you specify a waypoint position that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
--
-- Specifying more than 23 waypoints returns a @400 ValidationException@
-- error.
--
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRoute_waypointPositions :: Lens.Lens' CalculateRoute (Prelude.Maybe [Prelude.NonEmpty Prelude.Double])
calculateRoute_waypointPositions = Lens.lens (\CalculateRoute' {waypointPositions} -> waypointPositions) (\s@CalculateRoute' {} a -> s {waypointPositions = a} :: CalculateRoute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the route calculator resource that you want to use to
-- calculate the route.
calculateRoute_calculatorName :: Lens.Lens' CalculateRoute Prelude.Text
calculateRoute_calculatorName = Lens.lens (\CalculateRoute' {calculatorName} -> calculatorName) (\s@CalculateRoute' {} a -> s {calculatorName = a} :: CalculateRoute)

-- | The start position for the route. Defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
-- format: @[longitude, latitude]@.
--
-- -   For example, @[-123.115, 49.285]@
--
-- If you specify a departure that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRoute_departurePosition :: Lens.Lens' CalculateRoute (Prelude.NonEmpty Prelude.Double)
calculateRoute_departurePosition = Lens.lens (\CalculateRoute' {departurePosition} -> departurePosition) (\s@CalculateRoute' {} a -> s {departurePosition = a} :: CalculateRoute) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The finish position for the route. Defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 World Geodetic System (WGS 84)>
-- format: @[longitude, latitude]@.
--
-- -   For example, @[-122.339, 47.615]@
--
-- If you specify a destination that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html moves the position to the nearest road>.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRoute_destinationPosition :: Lens.Lens' CalculateRoute (Prelude.NonEmpty Prelude.Double)
calculateRoute_destinationPosition = Lens.lens (\CalculateRoute' {destinationPosition} -> destinationPosition) (\s@CalculateRoute' {} a -> s {destinationPosition = a} :: CalculateRoute) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Core.AWSRequest CalculateRoute where
  type
    AWSResponse CalculateRoute =
      CalculateRouteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CalculateRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Legs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Summary")
      )

instance Prelude.Hashable CalculateRoute where
  hashWithSalt _salt CalculateRoute' {..} =
    _salt `Prelude.hashWithSalt` carModeOptions
      `Prelude.hashWithSalt` departNow
      `Prelude.hashWithSalt` departureTime
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` includeLegGeometry
      `Prelude.hashWithSalt` travelMode
      `Prelude.hashWithSalt` truckModeOptions
      `Prelude.hashWithSalt` waypointPositions
      `Prelude.hashWithSalt` calculatorName
      `Prelude.hashWithSalt` departurePosition
      `Prelude.hashWithSalt` destinationPosition

instance Prelude.NFData CalculateRoute where
  rnf CalculateRoute' {..} =
    Prelude.rnf carModeOptions
      `Prelude.seq` Prelude.rnf departNow
      `Prelude.seq` Prelude.rnf departureTime
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf includeLegGeometry
      `Prelude.seq` Prelude.rnf travelMode
      `Prelude.seq` Prelude.rnf truckModeOptions
      `Prelude.seq` Prelude.rnf waypointPositions
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf departurePosition
      `Prelude.seq` Prelude.rnf destinationPosition

instance Data.ToHeaders CalculateRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CalculateRoute where
  toJSON CalculateRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CarModeOptions" Data..=)
              Prelude.<$> carModeOptions,
            ("DepartNow" Data..=) Prelude.<$> departNow,
            ("DepartureTime" Data..=) Prelude.<$> departureTime,
            ("DistanceUnit" Data..=) Prelude.<$> distanceUnit,
            ("IncludeLegGeometry" Data..=)
              Prelude.<$> includeLegGeometry,
            ("TravelMode" Data..=) Prelude.<$> travelMode,
            ("TruckModeOptions" Data..=)
              Prelude.<$> truckModeOptions,
            ("WaypointPositions" Data..=)
              Prelude.<$> waypointPositions,
            Prelude.Just
              ("DeparturePosition" Data..= departurePosition),
            Prelude.Just
              ("DestinationPosition" Data..= destinationPosition)
          ]
      )

instance Data.ToPath CalculateRoute where
  toPath CalculateRoute' {..} =
    Prelude.mconcat
      [ "/routes/v0/calculators/",
        Data.toBS calculatorName,
        "/calculate/route"
      ]

instance Data.ToQuery CalculateRoute where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the result of the route calculation. Metadata includes legs and
-- route summary.
--
-- /See:/ 'newCalculateRouteResponse' smart constructor.
data CalculateRouteResponse = CalculateRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains details about each path between a pair of positions included
    -- along a route such as: @StartPosition@, @EndPosition@, @Distance@,
    -- @DurationSeconds@, @Geometry@, and @Steps@. The number of legs returned
    -- corresponds to one fewer than the total number of positions in the
    -- request.
    --
    -- For example, a route with a departure position and destination position
    -- returns one leg with the positions
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>:
    --
    -- -   The @StartPosition@ is the departure position.
    --
    -- -   The @EndPosition@ is the destination position.
    --
    -- A route with a waypoint between the departure and destination position
    -- returns two legs with the positions snapped to a nearby road:
    --
    -- -   Leg 1: The @StartPosition@ is the departure position . The
    --     @EndPosition@ is the waypoint positon.
    --
    -- -   Leg 2: The @StartPosition@ is the waypoint position. The
    --     @EndPosition@ is the destination position.
    legs :: [Leg],
    -- | Contains information about the whole route, such as: @RouteBBox@,
    -- @DataSource@, @Distance@, @DistanceUnit@, and @DurationSeconds@.
    summary :: CalculateRouteSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'calculateRouteResponse_httpStatus' - The response's http status code.
--
-- 'legs', 'calculateRouteResponse_legs' - Contains details about each path between a pair of positions included
-- along a route such as: @StartPosition@, @EndPosition@, @Distance@,
-- @DurationSeconds@, @Geometry@, and @Steps@. The number of legs returned
-- corresponds to one fewer than the total number of positions in the
-- request.
--
-- For example, a route with a departure position and destination position
-- returns one leg with the positions
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>:
--
-- -   The @StartPosition@ is the departure position.
--
-- -   The @EndPosition@ is the destination position.
--
-- A route with a waypoint between the departure and destination position
-- returns two legs with the positions snapped to a nearby road:
--
-- -   Leg 1: The @StartPosition@ is the departure position . The
--     @EndPosition@ is the waypoint positon.
--
-- -   Leg 2: The @StartPosition@ is the waypoint position. The
--     @EndPosition@ is the destination position.
--
-- 'summary', 'calculateRouteResponse_summary' - Contains information about the whole route, such as: @RouteBBox@,
-- @DataSource@, @Distance@, @DistanceUnit@, and @DurationSeconds@.
newCalculateRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  CalculateRouteSummary ->
  CalculateRouteResponse
newCalculateRouteResponse pHttpStatus_ pSummary_ =
  CalculateRouteResponse'
    { httpStatus = pHttpStatus_,
      legs = Prelude.mempty,
      summary = pSummary_
    }

-- | The response's http status code.
calculateRouteResponse_httpStatus :: Lens.Lens' CalculateRouteResponse Prelude.Int
calculateRouteResponse_httpStatus = Lens.lens (\CalculateRouteResponse' {httpStatus} -> httpStatus) (\s@CalculateRouteResponse' {} a -> s {httpStatus = a} :: CalculateRouteResponse)

-- | Contains details about each path between a pair of positions included
-- along a route such as: @StartPosition@, @EndPosition@, @Distance@,
-- @DurationSeconds@, @Geometry@, and @Steps@. The number of legs returned
-- corresponds to one fewer than the total number of positions in the
-- request.
--
-- For example, a route with a departure position and destination position
-- returns one leg with the positions
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>:
--
-- -   The @StartPosition@ is the departure position.
--
-- -   The @EndPosition@ is the destination position.
--
-- A route with a waypoint between the departure and destination position
-- returns two legs with the positions snapped to a nearby road:
--
-- -   Leg 1: The @StartPosition@ is the departure position . The
--     @EndPosition@ is the waypoint positon.
--
-- -   Leg 2: The @StartPosition@ is the waypoint position. The
--     @EndPosition@ is the destination position.
calculateRouteResponse_legs :: Lens.Lens' CalculateRouteResponse [Leg]
calculateRouteResponse_legs = Lens.lens (\CalculateRouteResponse' {legs} -> legs) (\s@CalculateRouteResponse' {} a -> s {legs = a} :: CalculateRouteResponse) Prelude.. Lens.coerced

-- | Contains information about the whole route, such as: @RouteBBox@,
-- @DataSource@, @Distance@, @DistanceUnit@, and @DurationSeconds@.
calculateRouteResponse_summary :: Lens.Lens' CalculateRouteResponse CalculateRouteSummary
calculateRouteResponse_summary = Lens.lens (\CalculateRouteResponse' {summary} -> summary) (\s@CalculateRouteResponse' {} a -> s {summary = a} :: CalculateRouteResponse)

instance Prelude.NFData CalculateRouteResponse where
  rnf CalculateRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf legs
      `Prelude.seq` Prelude.rnf summary
