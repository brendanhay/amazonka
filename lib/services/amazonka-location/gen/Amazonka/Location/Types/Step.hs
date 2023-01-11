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
-- Module      : Amazonka.Location.Types.Step
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.Step where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an element of a leg within a route. A step contains
-- instructions for how to move to the next step in the leg.
--
-- /See:/ 'newStep' smart constructor.
data Step = Step'
  { -- | Represents the start position, or index, in a sequence of steps within
    -- the leg\'s line string geometry. For example, the index of the first
    -- step in a leg geometry is @0@.
    --
    -- Included in the response for queries that set @IncludeLegGeometry@ to
    -- @True@.
    geometryOffset :: Prelude.Maybe Prelude.Natural,
    -- | The travel distance between the step\'s @StartPosition@ and
    -- @EndPosition@.
    distance :: Prelude.Double,
    -- | The estimated travel time, in seconds, from the step\'s @StartPosition@
    -- to the @EndPosition@. . The travel mode and departure time that you
    -- specify in the request determines the calculated time.
    durationSeconds :: Prelude.Double,
    -- | The end position of a step. If the position the last step in the leg,
    -- this position is the same as the end position of the leg.
    endPosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The starting position of a step. If the position is the first step in
    -- the leg, this position is the same as the start position of the leg.
    startPosition :: Data.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Step' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geometryOffset', 'step_geometryOffset' - Represents the start position, or index, in a sequence of steps within
-- the leg\'s line string geometry. For example, the index of the first
-- step in a leg geometry is @0@.
--
-- Included in the response for queries that set @IncludeLegGeometry@ to
-- @True@.
--
-- 'distance', 'step_distance' - The travel distance between the step\'s @StartPosition@ and
-- @EndPosition@.
--
-- 'durationSeconds', 'step_durationSeconds' - The estimated travel time, in seconds, from the step\'s @StartPosition@
-- to the @EndPosition@. . The travel mode and departure time that you
-- specify in the request determines the calculated time.
--
-- 'endPosition', 'step_endPosition' - The end position of a step. If the position the last step in the leg,
-- this position is the same as the end position of the leg.
--
-- 'startPosition', 'step_startPosition' - The starting position of a step. If the position is the first step in
-- the leg, this position is the same as the start position of the leg.
newStep ::
  -- | 'distance'
  Prelude.Double ->
  -- | 'durationSeconds'
  Prelude.Double ->
  -- | 'endPosition'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'startPosition'
  Prelude.NonEmpty Prelude.Double ->
  Step
newStep
  pDistance_
  pDurationSeconds_
  pEndPosition_
  pStartPosition_ =
    Step'
      { geometryOffset = Prelude.Nothing,
        distance = pDistance_,
        durationSeconds = pDurationSeconds_,
        endPosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pEndPosition_,
        startPosition =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pStartPosition_
      }

-- | Represents the start position, or index, in a sequence of steps within
-- the leg\'s line string geometry. For example, the index of the first
-- step in a leg geometry is @0@.
--
-- Included in the response for queries that set @IncludeLegGeometry@ to
-- @True@.
step_geometryOffset :: Lens.Lens' Step (Prelude.Maybe Prelude.Natural)
step_geometryOffset = Lens.lens (\Step' {geometryOffset} -> geometryOffset) (\s@Step' {} a -> s {geometryOffset = a} :: Step)

-- | The travel distance between the step\'s @StartPosition@ and
-- @EndPosition@.
step_distance :: Lens.Lens' Step Prelude.Double
step_distance = Lens.lens (\Step' {distance} -> distance) (\s@Step' {} a -> s {distance = a} :: Step)

-- | The estimated travel time, in seconds, from the step\'s @StartPosition@
-- to the @EndPosition@. . The travel mode and departure time that you
-- specify in the request determines the calculated time.
step_durationSeconds :: Lens.Lens' Step Prelude.Double
step_durationSeconds = Lens.lens (\Step' {durationSeconds} -> durationSeconds) (\s@Step' {} a -> s {durationSeconds = a} :: Step)

-- | The end position of a step. If the position the last step in the leg,
-- this position is the same as the end position of the leg.
step_endPosition :: Lens.Lens' Step (Prelude.NonEmpty Prelude.Double)
step_endPosition = Lens.lens (\Step' {endPosition} -> endPosition) (\s@Step' {} a -> s {endPosition = a} :: Step) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The starting position of a step. If the position is the first step in
-- the leg, this position is the same as the start position of the leg.
step_startPosition :: Lens.Lens' Step (Prelude.NonEmpty Prelude.Double)
step_startPosition = Lens.lens (\Step' {startPosition} -> startPosition) (\s@Step' {} a -> s {startPosition = a} :: Step) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Data.FromJSON Step where
  parseJSON =
    Data.withObject
      "Step"
      ( \x ->
          Step'
            Prelude.<$> (x Data..:? "GeometryOffset")
            Prelude.<*> (x Data..: "Distance")
            Prelude.<*> (x Data..: "DurationSeconds")
            Prelude.<*> (x Data..: "EndPosition")
            Prelude.<*> (x Data..: "StartPosition")
      )

instance Prelude.Hashable Step where
  hashWithSalt _salt Step' {..} =
    _salt `Prelude.hashWithSalt` geometryOffset
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` endPosition
      `Prelude.hashWithSalt` startPosition

instance Prelude.NFData Step where
  rnf Step' {..} =
    Prelude.rnf geometryOffset
      `Prelude.seq` Prelude.rnf distance
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf endPosition
      `Prelude.seq` Prelude.rnf startPosition
