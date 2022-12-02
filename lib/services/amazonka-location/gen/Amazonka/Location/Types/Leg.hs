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
-- Module      : Amazonka.Location.Types.Leg
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.Leg where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.LegGeometry
import Amazonka.Location.Types.Step
import qualified Amazonka.Prelude as Prelude

-- | Contains the calculated route\'s details for each path between a pair of
-- positions. The number of legs returned corresponds to one fewer than the
-- total number of positions in the request.
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
-- /See:/ 'newLeg' smart constructor.
data Leg = Leg'
  { -- | Contains the calculated route\'s path as a linestring geometry.
    geometry :: Prelude.Maybe LegGeometry,
    -- | The distance between the leg\'s @StartPosition@ and @EndPosition@ along
    -- a calculated route.
    --
    -- -   The default measurement is @Kilometers@ unless the request specifies
    --     a @DistanceUnit@ of @Miles@.
    distance :: Prelude.Double,
    -- | The estimated travel time between the leg\'s @StartPosition@ and
    -- @EndPosition@. The travel mode and departure time that you specify in
    -- the request determines the calculated time.
    durationSeconds :: Prelude.Double,
    -- | The terminating position of the leg. Follows the format
    -- @[longitude,latitude]@.
    --
    -- If the @EndPosition@ isn\'t located on a road, it\'s
    -- <https://docs.aws.amazon.com/location/latest/developerguide/nap-to-nearby-road.html snapped to a nearby road>.
    endPosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The starting position of the leg. Follows the format
    -- @[longitude,latitude]@.
    --
    -- If the @StartPosition@ isn\'t located on a road, it\'s
    -- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>.
    startPosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | Contains a list of steps, which represent subsections of a leg. Each
    -- step provides instructions for how to move to the next step in the leg
    -- such as the step\'s start position, end position, travel distance,
    -- travel duration, and geometry offset.
    steps :: [Step]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Leg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geometry', 'leg_geometry' - Contains the calculated route\'s path as a linestring geometry.
--
-- 'distance', 'leg_distance' - The distance between the leg\'s @StartPosition@ and @EndPosition@ along
-- a calculated route.
--
-- -   The default measurement is @Kilometers@ unless the request specifies
--     a @DistanceUnit@ of @Miles@.
--
-- 'durationSeconds', 'leg_durationSeconds' - The estimated travel time between the leg\'s @StartPosition@ and
-- @EndPosition@. The travel mode and departure time that you specify in
-- the request determines the calculated time.
--
-- 'endPosition', 'leg_endPosition' - The terminating position of the leg. Follows the format
-- @[longitude,latitude]@.
--
-- If the @EndPosition@ isn\'t located on a road, it\'s
-- <https://docs.aws.amazon.com/location/latest/developerguide/nap-to-nearby-road.html snapped to a nearby road>.
--
-- 'startPosition', 'leg_startPosition' - The starting position of the leg. Follows the format
-- @[longitude,latitude]@.
--
-- If the @StartPosition@ isn\'t located on a road, it\'s
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>.
--
-- 'steps', 'leg_steps' - Contains a list of steps, which represent subsections of a leg. Each
-- step provides instructions for how to move to the next step in the leg
-- such as the step\'s start position, end position, travel distance,
-- travel duration, and geometry offset.
newLeg ::
  -- | 'distance'
  Prelude.Double ->
  -- | 'durationSeconds'
  Prelude.Double ->
  -- | 'endPosition'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'startPosition'
  Prelude.NonEmpty Prelude.Double ->
  Leg
newLeg
  pDistance_
  pDurationSeconds_
  pEndPosition_
  pStartPosition_ =
    Leg'
      { geometry = Prelude.Nothing,
        distance = pDistance_,
        durationSeconds = pDurationSeconds_,
        endPosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pEndPosition_,
        startPosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pStartPosition_,
        steps = Prelude.mempty
      }

-- | Contains the calculated route\'s path as a linestring geometry.
leg_geometry :: Lens.Lens' Leg (Prelude.Maybe LegGeometry)
leg_geometry = Lens.lens (\Leg' {geometry} -> geometry) (\s@Leg' {} a -> s {geometry = a} :: Leg)

-- | The distance between the leg\'s @StartPosition@ and @EndPosition@ along
-- a calculated route.
--
-- -   The default measurement is @Kilometers@ unless the request specifies
--     a @DistanceUnit@ of @Miles@.
leg_distance :: Lens.Lens' Leg Prelude.Double
leg_distance = Lens.lens (\Leg' {distance} -> distance) (\s@Leg' {} a -> s {distance = a} :: Leg)

-- | The estimated travel time between the leg\'s @StartPosition@ and
-- @EndPosition@. The travel mode and departure time that you specify in
-- the request determines the calculated time.
leg_durationSeconds :: Lens.Lens' Leg Prelude.Double
leg_durationSeconds = Lens.lens (\Leg' {durationSeconds} -> durationSeconds) (\s@Leg' {} a -> s {durationSeconds = a} :: Leg)

-- | The terminating position of the leg. Follows the format
-- @[longitude,latitude]@.
--
-- If the @EndPosition@ isn\'t located on a road, it\'s
-- <https://docs.aws.amazon.com/location/latest/developerguide/nap-to-nearby-road.html snapped to a nearby road>.
leg_endPosition :: Lens.Lens' Leg (Prelude.NonEmpty Prelude.Double)
leg_endPosition = Lens.lens (\Leg' {endPosition} -> endPosition) (\s@Leg' {} a -> s {endPosition = a} :: Leg) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The starting position of the leg. Follows the format
-- @[longitude,latitude]@.
--
-- If the @StartPosition@ isn\'t located on a road, it\'s
-- <https://docs.aws.amazon.com/location/latest/developerguide/snap-to-nearby-road.html snapped to a nearby road>.
leg_startPosition :: Lens.Lens' Leg (Prelude.NonEmpty Prelude.Double)
leg_startPosition = Lens.lens (\Leg' {startPosition} -> startPosition) (\s@Leg' {} a -> s {startPosition = a} :: Leg) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | Contains a list of steps, which represent subsections of a leg. Each
-- step provides instructions for how to move to the next step in the leg
-- such as the step\'s start position, end position, travel distance,
-- travel duration, and geometry offset.
leg_steps :: Lens.Lens' Leg [Step]
leg_steps = Lens.lens (\Leg' {steps} -> steps) (\s@Leg' {} a -> s {steps = a} :: Leg) Prelude.. Lens.coerced

instance Data.FromJSON Leg where
  parseJSON =
    Data.withObject
      "Leg"
      ( \x ->
          Leg'
            Prelude.<$> (x Data..:? "Geometry")
            Prelude.<*> (x Data..: "Distance")
            Prelude.<*> (x Data..: "DurationSeconds")
            Prelude.<*> (x Data..: "EndPosition")
            Prelude.<*> (x Data..: "StartPosition")
            Prelude.<*> (x Data..:? "Steps" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Leg where
  hashWithSalt _salt Leg' {..} =
    _salt `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` endPosition
      `Prelude.hashWithSalt` startPosition
      `Prelude.hashWithSalt` steps

instance Prelude.NFData Leg where
  rnf Leg' {..} =
    Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf distance
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf endPosition
      `Prelude.seq` Prelude.rnf startPosition
      `Prelude.seq` Prelude.rnf steps
