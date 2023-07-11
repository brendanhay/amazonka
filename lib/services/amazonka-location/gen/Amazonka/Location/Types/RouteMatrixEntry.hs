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
-- Module      : Amazonka.Location.Types.RouteMatrixEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.RouteMatrixEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.RouteMatrixEntryError
import qualified Amazonka.Prelude as Prelude

-- | The result for the calculated route of one @DeparturePosition@
-- @DestinationPosition@ pair.
--
-- /See:/ 'newRouteMatrixEntry' smart constructor.
data RouteMatrixEntry = RouteMatrixEntry'
  { -- | The total distance of travel for the route.
    distance :: Prelude.Maybe Prelude.Double,
    -- | The expected duration of travel for the route.
    durationSeconds :: Prelude.Maybe Prelude.Double,
    -- | An error corresponding to the calculation of a route between the
    -- @DeparturePosition@ and @DestinationPosition@.
    error :: Prelude.Maybe RouteMatrixEntryError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteMatrixEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distance', 'routeMatrixEntry_distance' - The total distance of travel for the route.
--
-- 'durationSeconds', 'routeMatrixEntry_durationSeconds' - The expected duration of travel for the route.
--
-- 'error', 'routeMatrixEntry_error' - An error corresponding to the calculation of a route between the
-- @DeparturePosition@ and @DestinationPosition@.
newRouteMatrixEntry ::
  RouteMatrixEntry
newRouteMatrixEntry =
  RouteMatrixEntry'
    { distance = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The total distance of travel for the route.
routeMatrixEntry_distance :: Lens.Lens' RouteMatrixEntry (Prelude.Maybe Prelude.Double)
routeMatrixEntry_distance = Lens.lens (\RouteMatrixEntry' {distance} -> distance) (\s@RouteMatrixEntry' {} a -> s {distance = a} :: RouteMatrixEntry)

-- | The expected duration of travel for the route.
routeMatrixEntry_durationSeconds :: Lens.Lens' RouteMatrixEntry (Prelude.Maybe Prelude.Double)
routeMatrixEntry_durationSeconds = Lens.lens (\RouteMatrixEntry' {durationSeconds} -> durationSeconds) (\s@RouteMatrixEntry' {} a -> s {durationSeconds = a} :: RouteMatrixEntry)

-- | An error corresponding to the calculation of a route between the
-- @DeparturePosition@ and @DestinationPosition@.
routeMatrixEntry_error :: Lens.Lens' RouteMatrixEntry (Prelude.Maybe RouteMatrixEntryError)
routeMatrixEntry_error = Lens.lens (\RouteMatrixEntry' {error} -> error) (\s@RouteMatrixEntry' {} a -> s {error = a} :: RouteMatrixEntry)

instance Data.FromJSON RouteMatrixEntry where
  parseJSON =
    Data.withObject
      "RouteMatrixEntry"
      ( \x ->
          RouteMatrixEntry'
            Prelude.<$> (x Data..:? "Distance")
            Prelude.<*> (x Data..:? "DurationSeconds")
            Prelude.<*> (x Data..:? "Error")
      )

instance Prelude.Hashable RouteMatrixEntry where
  hashWithSalt _salt RouteMatrixEntry' {..} =
    _salt
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` error

instance Prelude.NFData RouteMatrixEntry where
  rnf RouteMatrixEntry' {..} =
    Prelude.rnf distance
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf error
