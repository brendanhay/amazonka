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
-- Module      : Amazonka.Location.Types.SearchForPositionResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchForPositionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.Place
import qualified Amazonka.Prelude as Prelude

-- | Contains a search result from a position search query that is run on a
-- place index resource.
--
-- /See:/ 'newSearchForPositionResult' smart constructor.
data SearchForPositionResult = SearchForPositionResult'
  { -- | The unique identifier of the place. You can use this with the @GetPlace@
    -- operation to find the place again later.
    --
    -- For @SearchPlaceIndexForPosition@ operations, the @PlaceId@ is returned
    -- only by place indexes that use HERE as a data provider.
    placeId :: Prelude.Maybe Prelude.Text,
    -- | The distance in meters of a great-circle arc between the query position
    -- and the result.
    --
    -- A great-circle arc is the shortest path on a sphere, in this case the
    -- Earth. This returns the shortest distance between two locations.
    distance :: Prelude.Double,
    -- | Details about the search result, such as its address and position.
    place :: Place
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchForPositionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placeId', 'searchForPositionResult_placeId' - The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForPosition@ operations, the @PlaceId@ is returned
-- only by place indexes that use HERE as a data provider.
--
-- 'distance', 'searchForPositionResult_distance' - The distance in meters of a great-circle arc between the query position
-- and the result.
--
-- A great-circle arc is the shortest path on a sphere, in this case the
-- Earth. This returns the shortest distance between two locations.
--
-- 'place', 'searchForPositionResult_place' - Details about the search result, such as its address and position.
newSearchForPositionResult ::
  -- | 'distance'
  Prelude.Double ->
  -- | 'place'
  Place ->
  SearchForPositionResult
newSearchForPositionResult pDistance_ pPlace_ =
  SearchForPositionResult'
    { placeId = Prelude.Nothing,
      distance = pDistance_,
      place = pPlace_
    }

-- | The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForPosition@ operations, the @PlaceId@ is returned
-- only by place indexes that use HERE as a data provider.
searchForPositionResult_placeId :: Lens.Lens' SearchForPositionResult (Prelude.Maybe Prelude.Text)
searchForPositionResult_placeId = Lens.lens (\SearchForPositionResult' {placeId} -> placeId) (\s@SearchForPositionResult' {} a -> s {placeId = a} :: SearchForPositionResult)

-- | The distance in meters of a great-circle arc between the query position
-- and the result.
--
-- A great-circle arc is the shortest path on a sphere, in this case the
-- Earth. This returns the shortest distance between two locations.
searchForPositionResult_distance :: Lens.Lens' SearchForPositionResult Prelude.Double
searchForPositionResult_distance = Lens.lens (\SearchForPositionResult' {distance} -> distance) (\s@SearchForPositionResult' {} a -> s {distance = a} :: SearchForPositionResult)

-- | Details about the search result, such as its address and position.
searchForPositionResult_place :: Lens.Lens' SearchForPositionResult Place
searchForPositionResult_place = Lens.lens (\SearchForPositionResult' {place} -> place) (\s@SearchForPositionResult' {} a -> s {place = a} :: SearchForPositionResult)

instance Core.FromJSON SearchForPositionResult where
  parseJSON =
    Core.withObject
      "SearchForPositionResult"
      ( \x ->
          SearchForPositionResult'
            Prelude.<$> (x Core..:? "PlaceId")
            Prelude.<*> (x Core..: "Distance")
            Prelude.<*> (x Core..: "Place")
      )

instance Prelude.Hashable SearchForPositionResult where
  hashWithSalt _salt SearchForPositionResult' {..} =
    _salt `Prelude.hashWithSalt` placeId
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` place

instance Prelude.NFData SearchForPositionResult where
  rnf SearchForPositionResult' {..} =
    Prelude.rnf placeId
      `Prelude.seq` Prelude.rnf distance
      `Prelude.seq` Prelude.rnf place
