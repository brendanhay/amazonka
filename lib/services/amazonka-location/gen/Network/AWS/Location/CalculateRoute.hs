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
-- Module      : Network.AWS.Location.CalculateRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html Calculates a route>
-- given the following required parameters: @DeparturePostiton@ and
-- @DestinationPosition@. Requires that you first
-- <https://docs.aws.amazon.com/location-routes/latest/APIReference/API_CreateRouteCalculator.html create a route calculator resource>
--
-- By default, a request that doesn\'t specify a departure time uses the
-- best time of day to travel with the best traffic conditions when
-- calculating the route.
--
-- Additional options include:
--
-- -   <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#departure-time Specifying a departure time>
--     using either @DepartureTime@ or @DepartureNow@. This calculates a
--     route based on predictive traffic data at the given time.
--
--     You can\'t specify both @DepartureTime@ and @DepartureNow@ in a
--     single request. Specifying both parameters returns an error message.
--
-- -   <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#travel-mode Specifying a travel mode>
--     using TravelMode. This lets you specify an additional route
--     preference such as @CarModeOptions@ if traveling by @Car@, or
--     @TruckModeOptions@ if traveling by @Truck@.
module Network.AWS.Location.CalculateRoute
  ( -- * Creating a Request
    CalculateRoute (..),
    newCalculateRoute,

    -- * Request Lenses
    calculateRoute_distanceUnit,
    calculateRoute_truckModeOptions,
    calculateRoute_waypointPositions,
    calculateRoute_includeLegGeometry,
    calculateRoute_departNow,
    calculateRoute_travelMode,
    calculateRoute_carModeOptions,
    calculateRoute_departureTime,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCalculateRoute' smart constructor.
data CalculateRoute = CalculateRoute'
  { -- | Set the unit system to specify the distance.
    --
    -- Default Value: @Kilometers@
    distanceUnit :: Prelude.Maybe DistanceUnit,
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
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
    --
    -- Specifying more than 23 waypoints returns a @400 ValidationException@
    -- error.
    --
    -- If Esri is the provider for your route calculator, specifying a route
    -- that is longer than 400 km returns a @400 RoutesValidationException@
    -- error.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    waypointPositions :: Prelude.Maybe [Core.Sensitive (Prelude.NonEmpty Prelude.Double)],
    -- | Set to include the geometry details in the result for each path between
    -- a pair of positions.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    includeLegGeometry :: Prelude.Maybe Prelude.Bool,
    -- | Sets the time of departure as the current time. Uses the current time to
    -- calculate a route. Otherwise, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    departNow :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the mode of transport when calculating a route. Used in
    -- estimating the speed of travel and road compatibility.
    --
    -- The @TravelMode@ you specify determines how you specify route
    -- preferences:
    --
    -- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
    --
    -- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
    --
    -- Default Value: @Car@
    travelMode :: Prelude.Maybe TravelMode,
    -- | Specifies route preferences when traveling by @Car@, such as avoiding
    -- routes that use ferries or tolls.
    --
    -- Requirements: @TravelMode@ must be specified as @Car@.
    carModeOptions :: Prelude.Maybe CalculateRouteCarModeOptions,
    -- | Specifies the desired time of departure. Uses the given time to
    -- calculate a route. Otherwise, the best time of day to travel with the
    -- best traffic conditions is used to calculate the route.
    --
    -- Setting a departure time in the past returns a @400 ValidationException@
    -- error.
    --
    -- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    --     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
    --     @2020–07-2T12:15:20.000Z+01:00@
    departureTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the route calculator resource that you want to use to
    -- calculate a route.
    calculatorName :: Prelude.Text,
    -- | The start position for the route. Defined in
    -- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
    -- @[longitude, latitude]@.
    --
    -- -   For example, @[-123.115, 49.285]@
    --
    -- If you specify a departure that\'s not located on a road, Amazon
    -- Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
    -- If Esri is the provider for your route calculator, specifying a route
    -- that is longer than 400 km returns a @400 RoutesValidationException@
    -- error.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    departurePosition :: Core.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The finish position for the route. Defined in
    -- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
    -- @[longitude, latitude]@.
    --
    -- -   For example, @[-122.339, 47.615]@
    --
    -- If you specify a destination that\'s not located on a road, Amazon
    -- Location
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
    --
    -- Valid Values: @[-180 to 180,-90 to 90]@
    destinationPosition :: Core.Sensitive (Prelude.NonEmpty Prelude.Double)
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
-- 'distanceUnit', 'calculateRoute_distanceUnit' - Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
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
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
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
-- 'includeLegGeometry', 'calculateRoute_includeLegGeometry' - Set to include the geometry details in the result for each path between
-- a pair of positions.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'departNow', 'calculateRoute_departNow' - Sets the time of departure as the current time. Uses the current time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'travelMode', 'calculateRoute_travelMode' - Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility.
--
-- The @TravelMode@ you specify determines how you specify route
-- preferences:
--
-- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
--
-- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
--
-- Default Value: @Car@
--
-- 'carModeOptions', 'calculateRoute_carModeOptions' - Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
--
-- 'departureTime', 'calculateRoute_departureTime' - Specifies the desired time of departure. Uses the given time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
--
-- 'calculatorName', 'calculateRoute_calculatorName' - The name of the route calculator resource that you want to use to
-- calculate a route.
--
-- 'departurePosition', 'calculateRoute_departurePosition' - The start position for the route. Defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@.
--
-- -   For example, @[-123.115, 49.285]@
--
-- If you specify a departure that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
--
-- 'destinationPosition', 'calculateRoute_destinationPosition' - The finish position for the route. Defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@.
--
-- -   For example, @[-122.339, 47.615]@
--
-- If you specify a destination that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
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
      { distanceUnit = Prelude.Nothing,
        truckModeOptions = Prelude.Nothing,
        waypointPositions = Prelude.Nothing,
        includeLegGeometry = Prelude.Nothing,
        departNow = Prelude.Nothing,
        travelMode = Prelude.Nothing,
        carModeOptions = Prelude.Nothing,
        departureTime = Prelude.Nothing,
        calculatorName = pCalculatorName_,
        departurePosition =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pDeparturePosition_,
        destinationPosition =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pDestinationPosition_
      }

-- | Set the unit system to specify the distance.
--
-- Default Value: @Kilometers@
calculateRoute_distanceUnit :: Lens.Lens' CalculateRoute (Prelude.Maybe DistanceUnit)
calculateRoute_distanceUnit = Lens.lens (\CalculateRoute' {distanceUnit} -> distanceUnit) (\s@CalculateRoute' {} a -> s {distanceUnit = a} :: CalculateRoute)

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
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
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

-- | Set to include the geometry details in the result for each path between
-- a pair of positions.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRoute_includeLegGeometry :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.Bool)
calculateRoute_includeLegGeometry = Lens.lens (\CalculateRoute' {includeLegGeometry} -> includeLegGeometry) (\s@CalculateRoute' {} a -> s {includeLegGeometry = a} :: CalculateRoute)

-- | Sets the time of departure as the current time. Uses the current time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRoute_departNow :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.Bool)
calculateRoute_departNow = Lens.lens (\CalculateRoute' {departNow} -> departNow) (\s@CalculateRoute' {} a -> s {departNow = a} :: CalculateRoute)

-- | Specifies the mode of transport when calculating a route. Used in
-- estimating the speed of travel and road compatibility.
--
-- The @TravelMode@ you specify determines how you specify route
-- preferences:
--
-- -   If traveling by @Car@ use the @CarModeOptions@ parameter.
--
-- -   If traveling by @Truck@ use the @TruckModeOptions@ parameter.
--
-- Default Value: @Car@
calculateRoute_travelMode :: Lens.Lens' CalculateRoute (Prelude.Maybe TravelMode)
calculateRoute_travelMode = Lens.lens (\CalculateRoute' {travelMode} -> travelMode) (\s@CalculateRoute' {} a -> s {travelMode = a} :: CalculateRoute)

-- | Specifies route preferences when traveling by @Car@, such as avoiding
-- routes that use ferries or tolls.
--
-- Requirements: @TravelMode@ must be specified as @Car@.
calculateRoute_carModeOptions :: Lens.Lens' CalculateRoute (Prelude.Maybe CalculateRouteCarModeOptions)
calculateRoute_carModeOptions = Lens.lens (\CalculateRoute' {carModeOptions} -> carModeOptions) (\s@CalculateRoute' {} a -> s {carModeOptions = a} :: CalculateRoute)

-- | Specifies the desired time of departure. Uses the given time to
-- calculate a route. Otherwise, the best time of day to travel with the
-- best traffic conditions is used to calculate the route.
--
-- Setting a departure time in the past returns a @400 ValidationException@
-- error.
--
-- -   In <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
--     format: @YYYY-MM-DDThh:mm:ss.sssZ@. For example,
--     @2020–07-2T12:15:20.000Z+01:00@
calculateRoute_departureTime :: Lens.Lens' CalculateRoute (Prelude.Maybe Prelude.UTCTime)
calculateRoute_departureTime = Lens.lens (\CalculateRoute' {departureTime} -> departureTime) (\s@CalculateRoute' {} a -> s {departureTime = a} :: CalculateRoute) Prelude.. Lens.mapping Core._Time

-- | The name of the route calculator resource that you want to use to
-- calculate a route.
calculateRoute_calculatorName :: Lens.Lens' CalculateRoute Prelude.Text
calculateRoute_calculatorName = Lens.lens (\CalculateRoute' {calculatorName} -> calculatorName) (\s@CalculateRoute' {} a -> s {calculatorName = a} :: CalculateRoute)

-- | The start position for the route. Defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@.
--
-- -   For example, @[-123.115, 49.285]@
--
-- If you specify a departure that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
-- If Esri is the provider for your route calculator, specifying a route
-- that is longer than 400 km returns a @400 RoutesValidationException@
-- error.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRoute_departurePosition :: Lens.Lens' CalculateRoute (Prelude.NonEmpty Prelude.Double)
calculateRoute_departurePosition = Lens.lens (\CalculateRoute' {departurePosition} -> departurePosition) (\s@CalculateRoute' {} a -> s {departurePosition = a} :: CalculateRoute) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The finish position for the route. Defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[longitude, latitude]@.
--
-- -   For example, @[-122.339, 47.615]@
--
-- If you specify a destination that\'s not located on a road, Amazon
-- Location
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road moves the position to the nearest road>.
--
-- Valid Values: @[-180 to 180,-90 to 90]@
calculateRoute_destinationPosition :: Lens.Lens' CalculateRoute (Prelude.NonEmpty Prelude.Double)
calculateRoute_destinationPosition = Lens.lens (\CalculateRoute' {destinationPosition} -> destinationPosition) (\s@CalculateRoute' {} a -> s {destinationPosition = a} :: CalculateRoute) Prelude.. Core._Sensitive Prelude.. Lens.coerced

instance Core.AWSRequest CalculateRoute where
  type
    AWSResponse CalculateRoute =
      CalculateRouteResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CalculateRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Legs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "Summary")
      )

instance Prelude.Hashable CalculateRoute

instance Prelude.NFData CalculateRoute

instance Core.ToHeaders CalculateRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CalculateRoute where
  toJSON CalculateRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DistanceUnit" Core..=) Prelude.<$> distanceUnit,
            ("TruckModeOptions" Core..=)
              Prelude.<$> truckModeOptions,
            ("WaypointPositions" Core..=)
              Prelude.<$> waypointPositions,
            ("IncludeLegGeometry" Core..=)
              Prelude.<$> includeLegGeometry,
            ("DepartNow" Core..=) Prelude.<$> departNow,
            ("TravelMode" Core..=) Prelude.<$> travelMode,
            ("CarModeOptions" Core..=)
              Prelude.<$> carModeOptions,
            ("DepartureTime" Core..=) Prelude.<$> departureTime,
            Prelude.Just
              ("DeparturePosition" Core..= departurePosition),
            Prelude.Just
              ("DestinationPosition" Core..= destinationPosition)
          ]
      )

instance Core.ToPath CalculateRoute where
  toPath CalculateRoute' {..} =
    Prelude.mconcat
      [ "/routes/v0/calculators/",
        Core.toBS calculatorName,
        "/calculate/route"
      ]

instance Core.ToQuery CalculateRoute where
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
    -- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road snapped to a nearby road>:
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
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road snapped to a nearby road>:
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
-- <https://docs.aws.amazon.com/location/latest/developerguide/calculate-route.html#snap-to-nearby-road snapped to a nearby road>:
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

instance Prelude.NFData CalculateRouteResponse
