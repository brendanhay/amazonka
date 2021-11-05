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
-- Module      : Network.AWS.Location.Types.SearchPlaceIndexForPositionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.SearchPlaceIndexForPositionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of the reverse geocoding request sent using
-- @SearchPlaceIndexForPosition@.
--
-- /See:/ 'newSearchPlaceIndexForPositionSummary' smart constructor.
data SearchPlaceIndexForPositionSummary = SearchPlaceIndexForPositionSummary'
  { -- | An optional parameter. The maximum number of results returned per
    -- request.
    --
    -- Default value: @50@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The data provider of geospatial data. Indicates one of the available
    -- providers:
    --
    -- -   Esri
    --
    -- -   HERE
    --
    -- For additional details on data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The position given in the reverse geocoding request.
    position :: Core.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForPositionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchPlaceIndexForPositionSummary_maxResults' - An optional parameter. The maximum number of results returned per
-- request.
--
-- Default value: @50@
--
-- 'dataSource', 'searchPlaceIndexForPositionSummary_dataSource' - The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   Esri
--
-- -   HERE
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'position', 'searchPlaceIndexForPositionSummary_position' - The position given in the reverse geocoding request.
newSearchPlaceIndexForPositionSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  SearchPlaceIndexForPositionSummary
newSearchPlaceIndexForPositionSummary
  pDataSource_
  pPosition_ =
    SearchPlaceIndexForPositionSummary'
      { maxResults =
          Prelude.Nothing,
        dataSource = pDataSource_,
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_
      }

-- | An optional parameter. The maximum number of results returned per
-- request.
--
-- Default value: @50@
searchPlaceIndexForPositionSummary_maxResults :: Lens.Lens' SearchPlaceIndexForPositionSummary (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForPositionSummary_maxResults = Lens.lens (\SearchPlaceIndexForPositionSummary' {maxResults} -> maxResults) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {maxResults = a} :: SearchPlaceIndexForPositionSummary)

-- | The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   Esri
--
-- -   HERE
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
searchPlaceIndexForPositionSummary_dataSource :: Lens.Lens' SearchPlaceIndexForPositionSummary Prelude.Text
searchPlaceIndexForPositionSummary_dataSource = Lens.lens (\SearchPlaceIndexForPositionSummary' {dataSource} -> dataSource) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {dataSource = a} :: SearchPlaceIndexForPositionSummary)

-- | The position given in the reverse geocoding request.
searchPlaceIndexForPositionSummary_position :: Lens.Lens' SearchPlaceIndexForPositionSummary (Prelude.NonEmpty Prelude.Double)
searchPlaceIndexForPositionSummary_position = Lens.lens (\SearchPlaceIndexForPositionSummary' {position} -> position) (\s@SearchPlaceIndexForPositionSummary' {} a -> s {position = a} :: SearchPlaceIndexForPositionSummary) Prelude.. Core._Sensitive Prelude.. Lens.coerced

instance
  Core.FromJSON
    SearchPlaceIndexForPositionSummary
  where
  parseJSON =
    Core.withObject
      "SearchPlaceIndexForPositionSummary"
      ( \x ->
          SearchPlaceIndexForPositionSummary'
            Prelude.<$> (x Core..:? "MaxResults")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "Position")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForPositionSummary

instance
  Prelude.NFData
    SearchPlaceIndexForPositionSummary
