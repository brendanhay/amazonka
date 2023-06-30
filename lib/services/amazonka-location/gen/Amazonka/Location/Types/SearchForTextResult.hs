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
-- Module      : Amazonka.Location.Types.SearchForTextResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchForTextResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.Place
import qualified Amazonka.Prelude as Prelude

-- | Contains a search result from a text search query that is run on a place
-- index resource.
--
-- /See:/ 'newSearchForTextResult' smart constructor.
data SearchForTextResult = SearchForTextResult'
  { -- | The distance in meters of a great-circle arc between the bias position
    -- specified and the result. @Distance@ will be returned only if a bias
    -- position was specified in the query.
    --
    -- A great-circle arc is the shortest path on a sphere, in this case the
    -- Earth. This returns the shortest distance between two locations.
    distance :: Prelude.Maybe Prelude.Double,
    -- | The unique identifier of the place. You can use this with the @GetPlace@
    -- operation to find the place again later.
    --
    -- For @SearchPlaceIndexForText@ operations, the @PlaceId@ is returned only
    -- by place indexes that use HERE as a data provider.
    placeId :: Prelude.Maybe Prelude.Text,
    -- | The relative confidence in the match for a result among the results
    -- returned. For example, if more fields for an address match (including
    -- house number, street, city, country\/region, and postal code), the
    -- relevance score is closer to 1.
    --
    -- Returned only when the partner selected is Esri.
    relevance :: Prelude.Maybe Prelude.Double,
    -- | Details about the search result, such as its address and position.
    place :: Place
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchForTextResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distance', 'searchForTextResult_distance' - The distance in meters of a great-circle arc between the bias position
-- specified and the result. @Distance@ will be returned only if a bias
-- position was specified in the query.
--
-- A great-circle arc is the shortest path on a sphere, in this case the
-- Earth. This returns the shortest distance between two locations.
--
-- 'placeId', 'searchForTextResult_placeId' - The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForText@ operations, the @PlaceId@ is returned only
-- by place indexes that use HERE as a data provider.
--
-- 'relevance', 'searchForTextResult_relevance' - The relative confidence in the match for a result among the results
-- returned. For example, if more fields for an address match (including
-- house number, street, city, country\/region, and postal code), the
-- relevance score is closer to 1.
--
-- Returned only when the partner selected is Esri.
--
-- 'place', 'searchForTextResult_place' - Details about the search result, such as its address and position.
newSearchForTextResult ::
  -- | 'place'
  Place ->
  SearchForTextResult
newSearchForTextResult pPlace_ =
  SearchForTextResult'
    { distance = Prelude.Nothing,
      placeId = Prelude.Nothing,
      relevance = Prelude.Nothing,
      place = pPlace_
    }

-- | The distance in meters of a great-circle arc between the bias position
-- specified and the result. @Distance@ will be returned only if a bias
-- position was specified in the query.
--
-- A great-circle arc is the shortest path on a sphere, in this case the
-- Earth. This returns the shortest distance between two locations.
searchForTextResult_distance :: Lens.Lens' SearchForTextResult (Prelude.Maybe Prelude.Double)
searchForTextResult_distance = Lens.lens (\SearchForTextResult' {distance} -> distance) (\s@SearchForTextResult' {} a -> s {distance = a} :: SearchForTextResult)

-- | The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForText@ operations, the @PlaceId@ is returned only
-- by place indexes that use HERE as a data provider.
searchForTextResult_placeId :: Lens.Lens' SearchForTextResult (Prelude.Maybe Prelude.Text)
searchForTextResult_placeId = Lens.lens (\SearchForTextResult' {placeId} -> placeId) (\s@SearchForTextResult' {} a -> s {placeId = a} :: SearchForTextResult)

-- | The relative confidence in the match for a result among the results
-- returned. For example, if more fields for an address match (including
-- house number, street, city, country\/region, and postal code), the
-- relevance score is closer to 1.
--
-- Returned only when the partner selected is Esri.
searchForTextResult_relevance :: Lens.Lens' SearchForTextResult (Prelude.Maybe Prelude.Double)
searchForTextResult_relevance = Lens.lens (\SearchForTextResult' {relevance} -> relevance) (\s@SearchForTextResult' {} a -> s {relevance = a} :: SearchForTextResult)

-- | Details about the search result, such as its address and position.
searchForTextResult_place :: Lens.Lens' SearchForTextResult Place
searchForTextResult_place = Lens.lens (\SearchForTextResult' {place} -> place) (\s@SearchForTextResult' {} a -> s {place = a} :: SearchForTextResult)

instance Data.FromJSON SearchForTextResult where
  parseJSON =
    Data.withObject
      "SearchForTextResult"
      ( \x ->
          SearchForTextResult'
            Prelude.<$> (x Data..:? "Distance")
            Prelude.<*> (x Data..:? "PlaceId")
            Prelude.<*> (x Data..:? "Relevance")
            Prelude.<*> (x Data..: "Place")
      )

instance Prelude.Hashable SearchForTextResult where
  hashWithSalt _salt SearchForTextResult' {..} =
    _salt
      `Prelude.hashWithSalt` distance
      `Prelude.hashWithSalt` placeId
      `Prelude.hashWithSalt` relevance
      `Prelude.hashWithSalt` place

instance Prelude.NFData SearchForTextResult where
  rnf SearchForTextResult' {..} =
    Prelude.rnf distance
      `Prelude.seq` Prelude.rnf placeId
      `Prelude.seq` Prelude.rnf relevance
      `Prelude.seq` Prelude.rnf place
