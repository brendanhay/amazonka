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
-- Module      : Amazonka.Location.Types.SearchPlaceIndexForTextSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchPlaceIndexForTextSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of the geocoding request sent using @SearchPlaceIndexForText@.
--
-- /See:/ 'newSearchPlaceIndexForTextSummary' smart constructor.
data SearchPlaceIndexForTextSummary = SearchPlaceIndexForTextSummary'
  { -- | Contains the coordinates for the optional bounding box coordinated
    -- entered in the geocoding request.
    filterBBox :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | A bounding box that contains the search results within the specified
    -- area indicated by @FilterBBox@. A subset of bounding box specified using
    -- @FilterBBox@.
    resultBBox :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Contains the coordinates for the bias position entered in the geocoding
    -- request.
    biasPosition :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Contains the country filter entered in the geocoding request.
    filterCountries :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Contains the maximum number of results indicated for the request.
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
    -- | The address, name, city or region to be used in the geocoding request.
    -- In free-form text format. For example, @Vancouver@.
    text :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForTextSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterBBox', 'searchPlaceIndexForTextSummary_filterBBox' - Contains the coordinates for the optional bounding box coordinated
-- entered in the geocoding request.
--
-- 'resultBBox', 'searchPlaceIndexForTextSummary_resultBBox' - A bounding box that contains the search results within the specified
-- area indicated by @FilterBBox@. A subset of bounding box specified using
-- @FilterBBox@.
--
-- 'biasPosition', 'searchPlaceIndexForTextSummary_biasPosition' - Contains the coordinates for the bias position entered in the geocoding
-- request.
--
-- 'filterCountries', 'searchPlaceIndexForTextSummary_filterCountries' - Contains the country filter entered in the geocoding request.
--
-- 'maxResults', 'searchPlaceIndexForTextSummary_maxResults' - Contains the maximum number of results indicated for the request.
--
-- 'dataSource', 'searchPlaceIndexForTextSummary_dataSource' - The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   Esri
--
-- -   HERE
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'text', 'searchPlaceIndexForTextSummary_text' - The address, name, city or region to be used in the geocoding request.
-- In free-form text format. For example, @Vancouver@.
newSearchPlaceIndexForTextSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForTextSummary
newSearchPlaceIndexForTextSummary pDataSource_ pText_ =
  SearchPlaceIndexForTextSummary'
    { filterBBox =
        Prelude.Nothing,
      resultBBox = Prelude.Nothing,
      biasPosition = Prelude.Nothing,
      filterCountries = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dataSource = pDataSource_,
      text = Core._Sensitive Lens.# pText_
    }

-- | Contains the coordinates for the optional bounding box coordinated
-- entered in the geocoding request.
searchPlaceIndexForTextSummary_filterBBox :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_filterBBox = Lens.lens (\SearchPlaceIndexForTextSummary' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForTextSummary' {} a -> s {filterBBox = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | A bounding box that contains the search results within the specified
-- area indicated by @FilterBBox@. A subset of bounding box specified using
-- @FilterBBox@.
searchPlaceIndexForTextSummary_resultBBox :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_resultBBox = Lens.lens (\SearchPlaceIndexForTextSummary' {resultBBox} -> resultBBox) (\s@SearchPlaceIndexForTextSummary' {} a -> s {resultBBox = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Contains the coordinates for the bias position entered in the geocoding
-- request.
searchPlaceIndexForTextSummary_biasPosition :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_biasPosition = Lens.lens (\SearchPlaceIndexForTextSummary' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForTextSummary' {} a -> s {biasPosition = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Contains the country filter entered in the geocoding request.
searchPlaceIndexForTextSummary_filterCountries :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForTextSummary_filterCountries = Lens.lens (\SearchPlaceIndexForTextSummary' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForTextSummary' {} a -> s {filterCountries = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping Lens.coerced

-- | Contains the maximum number of results indicated for the request.
searchPlaceIndexForTextSummary_maxResults :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForTextSummary_maxResults = Lens.lens (\SearchPlaceIndexForTextSummary' {maxResults} -> maxResults) (\s@SearchPlaceIndexForTextSummary' {} a -> s {maxResults = a} :: SearchPlaceIndexForTextSummary)

-- | The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   Esri
--
-- -   HERE
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
searchPlaceIndexForTextSummary_dataSource :: Lens.Lens' SearchPlaceIndexForTextSummary Prelude.Text
searchPlaceIndexForTextSummary_dataSource = Lens.lens (\SearchPlaceIndexForTextSummary' {dataSource} -> dataSource) (\s@SearchPlaceIndexForTextSummary' {} a -> s {dataSource = a} :: SearchPlaceIndexForTextSummary)

-- | The address, name, city or region to be used in the geocoding request.
-- In free-form text format. For example, @Vancouver@.
searchPlaceIndexForTextSummary_text :: Lens.Lens' SearchPlaceIndexForTextSummary Prelude.Text
searchPlaceIndexForTextSummary_text = Lens.lens (\SearchPlaceIndexForTextSummary' {text} -> text) (\s@SearchPlaceIndexForTextSummary' {} a -> s {text = a} :: SearchPlaceIndexForTextSummary) Prelude.. Core._Sensitive

instance Core.FromJSON SearchPlaceIndexForTextSummary where
  parseJSON =
    Core.withObject
      "SearchPlaceIndexForTextSummary"
      ( \x ->
          SearchPlaceIndexForTextSummary'
            Prelude.<$> (x Core..:? "FilterBBox")
            Prelude.<*> (x Core..:? "ResultBBox")
            Prelude.<*> (x Core..:? "BiasPosition")
            Prelude.<*> (x Core..:? "FilterCountries")
            Prelude.<*> (x Core..:? "MaxResults")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "Text")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForTextSummary
  where
  hashWithSalt
    salt'
    SearchPlaceIndexForTextSummary' {..} =
      salt' `Prelude.hashWithSalt` text
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` filterCountries
        `Prelude.hashWithSalt` biasPosition
        `Prelude.hashWithSalt` resultBBox
        `Prelude.hashWithSalt` filterBBox

instance
  Prelude.NFData
    SearchPlaceIndexForTextSummary
  where
  rnf SearchPlaceIndexForTextSummary' {..} =
    Prelude.rnf filterBBox
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filterCountries
      `Prelude.seq` Prelude.rnf biasPosition
      `Prelude.seq` Prelude.rnf resultBBox
