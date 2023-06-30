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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchPlaceIndexForTextSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the request sent by using @SearchPlaceIndexForText@.
--
-- /See:/ 'newSearchPlaceIndexForTextSummary' smart constructor.
data SearchPlaceIndexForTextSummary = SearchPlaceIndexForTextSummary'
  { -- | Contains the coordinates for the optional bias position specified in the
    -- request.
    --
    -- This parameter contains a pair of numbers. The first number represents
    -- the X coordinate, or longitude; the second number represents the Y
    -- coordinate, or latitude.
    --
    -- For example, @[-123.1174, 49.2847]@ represents the position with
    -- longitude @-123.1174@ and latitude @49.2847@.
    biasPosition :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Contains the coordinates for the optional bounding box specified in the
    -- request.
    filterBBox :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Contains the optional country filter specified in the request.
    filterCountries :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The preferred language used to return results. Matches the language in
    -- the request. The value is a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    language :: Prelude.Maybe Prelude.Text,
    -- | Contains the optional result count limit specified in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The bounding box that fully contains all search results.
    --
    -- If you specified the optional @FilterBBox@ parameter in the request,
    -- @ResultBBox@ is contained within @FilterBBox@.
    resultBBox :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | The geospatial data provider attached to the place index resource
    -- specified in the request. Values can be one of the following:
    --
    -- -   Esri
    --
    -- -   Here
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The search text specified in the request.
    text :: Data.Sensitive Prelude.Text
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
-- 'biasPosition', 'searchPlaceIndexForTextSummary_biasPosition' - Contains the coordinates for the optional bias position specified in the
-- request.
--
-- This parameter contains a pair of numbers. The first number represents
-- the X coordinate, or longitude; the second number represents the Y
-- coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents the position with
-- longitude @-123.1174@ and latitude @49.2847@.
--
-- 'filterBBox', 'searchPlaceIndexForTextSummary_filterBBox' - Contains the coordinates for the optional bounding box specified in the
-- request.
--
-- 'filterCountries', 'searchPlaceIndexForTextSummary_filterCountries' - Contains the optional country filter specified in the request.
--
-- 'language', 'searchPlaceIndexForTextSummary_language' - The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- 'maxResults', 'searchPlaceIndexForTextSummary_maxResults' - Contains the optional result count limit specified in the request.
--
-- 'resultBBox', 'searchPlaceIndexForTextSummary_resultBBox' - The bounding box that fully contains all search results.
--
-- If you specified the optional @FilterBBox@ parameter in the request,
-- @ResultBBox@ is contained within @FilterBBox@.
--
-- 'dataSource', 'searchPlaceIndexForTextSummary_dataSource' - The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'text', 'searchPlaceIndexForTextSummary_text' - The search text specified in the request.
newSearchPlaceIndexForTextSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForTextSummary
newSearchPlaceIndexForTextSummary pDataSource_ pText_ =
  SearchPlaceIndexForTextSummary'
    { biasPosition =
        Prelude.Nothing,
      filterBBox = Prelude.Nothing,
      filterCountries = Prelude.Nothing,
      language = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resultBBox = Prelude.Nothing,
      dataSource = pDataSource_,
      text = Data._Sensitive Lens.# pText_
    }

-- | Contains the coordinates for the optional bias position specified in the
-- request.
--
-- This parameter contains a pair of numbers. The first number represents
-- the X coordinate, or longitude; the second number represents the Y
-- coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents the position with
-- longitude @-123.1174@ and latitude @49.2847@.
searchPlaceIndexForTextSummary_biasPosition :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_biasPosition = Lens.lens (\SearchPlaceIndexForTextSummary' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForTextSummary' {} a -> s {biasPosition = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Contains the coordinates for the optional bounding box specified in the
-- request.
searchPlaceIndexForTextSummary_filterBBox :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_filterBBox = Lens.lens (\SearchPlaceIndexForTextSummary' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForTextSummary' {} a -> s {filterBBox = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Contains the optional country filter specified in the request.
searchPlaceIndexForTextSummary_filterCountries :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForTextSummary_filterCountries = Lens.lens (\SearchPlaceIndexForTextSummary' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForTextSummary' {} a -> s {filterCountries = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping Lens.coerced

-- | The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
searchPlaceIndexForTextSummary_language :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe Prelude.Text)
searchPlaceIndexForTextSummary_language = Lens.lens (\SearchPlaceIndexForTextSummary' {language} -> language) (\s@SearchPlaceIndexForTextSummary' {} a -> s {language = a} :: SearchPlaceIndexForTextSummary)

-- | Contains the optional result count limit specified in the request.
searchPlaceIndexForTextSummary_maxResults :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForTextSummary_maxResults = Lens.lens (\SearchPlaceIndexForTextSummary' {maxResults} -> maxResults) (\s@SearchPlaceIndexForTextSummary' {} a -> s {maxResults = a} :: SearchPlaceIndexForTextSummary)

-- | The bounding box that fully contains all search results.
--
-- If you specified the optional @FilterBBox@ parameter in the request,
-- @ResultBBox@ is contained within @FilterBBox@.
searchPlaceIndexForTextSummary_resultBBox :: Lens.Lens' SearchPlaceIndexForTextSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForTextSummary_resultBBox = Lens.lens (\SearchPlaceIndexForTextSummary' {resultBBox} -> resultBBox) (\s@SearchPlaceIndexForTextSummary' {} a -> s {resultBBox = a} :: SearchPlaceIndexForTextSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
searchPlaceIndexForTextSummary_dataSource :: Lens.Lens' SearchPlaceIndexForTextSummary Prelude.Text
searchPlaceIndexForTextSummary_dataSource = Lens.lens (\SearchPlaceIndexForTextSummary' {dataSource} -> dataSource) (\s@SearchPlaceIndexForTextSummary' {} a -> s {dataSource = a} :: SearchPlaceIndexForTextSummary)

-- | The search text specified in the request.
searchPlaceIndexForTextSummary_text :: Lens.Lens' SearchPlaceIndexForTextSummary Prelude.Text
searchPlaceIndexForTextSummary_text = Lens.lens (\SearchPlaceIndexForTextSummary' {text} -> text) (\s@SearchPlaceIndexForTextSummary' {} a -> s {text = a} :: SearchPlaceIndexForTextSummary) Prelude.. Data._Sensitive

instance Data.FromJSON SearchPlaceIndexForTextSummary where
  parseJSON =
    Data.withObject
      "SearchPlaceIndexForTextSummary"
      ( \x ->
          SearchPlaceIndexForTextSummary'
            Prelude.<$> (x Data..:? "BiasPosition")
            Prelude.<*> (x Data..:? "FilterBBox")
            Prelude.<*> (x Data..:? "FilterCountries")
            Prelude.<*> (x Data..:? "Language")
            Prelude.<*> (x Data..:? "MaxResults")
            Prelude.<*> (x Data..:? "ResultBBox")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Text")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForTextSummary
  where
  hashWithSalt
    _salt
    SearchPlaceIndexForTextSummary' {..} =
      _salt
        `Prelude.hashWithSalt` biasPosition
        `Prelude.hashWithSalt` filterBBox
        `Prelude.hashWithSalt` filterCountries
        `Prelude.hashWithSalt` language
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` resultBBox
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` text

instance
  Prelude.NFData
    SearchPlaceIndexForTextSummary
  where
  rnf SearchPlaceIndexForTextSummary' {..} =
    Prelude.rnf biasPosition
      `Prelude.seq` Prelude.rnf filterBBox
      `Prelude.seq` Prelude.rnf filterCountries
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resultBBox
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf text
