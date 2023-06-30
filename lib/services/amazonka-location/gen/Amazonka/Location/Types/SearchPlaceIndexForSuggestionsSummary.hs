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
-- Module      : Amazonka.Location.Types.SearchPlaceIndexForSuggestionsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchPlaceIndexForSuggestionsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the request sent by using @SearchPlaceIndexForSuggestions@.
--
-- /See:/ 'newSearchPlaceIndexForSuggestionsSummary' smart constructor.
data SearchPlaceIndexForSuggestionsSummary = SearchPlaceIndexForSuggestionsSummary'
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
    maxResults :: Prelude.Maybe Prelude.Int,
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
    -- | The free-form partial text input specified in the request.
    text :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForSuggestionsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'biasPosition', 'searchPlaceIndexForSuggestionsSummary_biasPosition' - Contains the coordinates for the optional bias position specified in the
-- request.
--
-- This parameter contains a pair of numbers. The first number represents
-- the X coordinate, or longitude; the second number represents the Y
-- coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents the position with
-- longitude @-123.1174@ and latitude @49.2847@.
--
-- 'filterBBox', 'searchPlaceIndexForSuggestionsSummary_filterBBox' - Contains the coordinates for the optional bounding box specified in the
-- request.
--
-- 'filterCountries', 'searchPlaceIndexForSuggestionsSummary_filterCountries' - Contains the optional country filter specified in the request.
--
-- 'language', 'searchPlaceIndexForSuggestionsSummary_language' - The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- 'maxResults', 'searchPlaceIndexForSuggestionsSummary_maxResults' - Contains the optional result count limit specified in the request.
--
-- 'dataSource', 'searchPlaceIndexForSuggestionsSummary_dataSource' - The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'text', 'searchPlaceIndexForSuggestionsSummary_text' - The free-form partial text input specified in the request.
newSearchPlaceIndexForSuggestionsSummary ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForSuggestionsSummary
newSearchPlaceIndexForSuggestionsSummary
  pDataSource_
  pText_ =
    SearchPlaceIndexForSuggestionsSummary'
      { biasPosition =
          Prelude.Nothing,
        filterBBox = Prelude.Nothing,
        filterCountries = Prelude.Nothing,
        language = Prelude.Nothing,
        maxResults = Prelude.Nothing,
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
searchPlaceIndexForSuggestionsSummary_biasPosition :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForSuggestionsSummary_biasPosition = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {biasPosition = a} :: SearchPlaceIndexForSuggestionsSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Contains the coordinates for the optional bounding box specified in the
-- request.
searchPlaceIndexForSuggestionsSummary_filterBBox :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForSuggestionsSummary_filterBBox = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {filterBBox = a} :: SearchPlaceIndexForSuggestionsSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Contains the optional country filter specified in the request.
searchPlaceIndexForSuggestionsSummary_filterCountries :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForSuggestionsSummary_filterCountries = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {filterCountries = a} :: SearchPlaceIndexForSuggestionsSummary) Prelude.. Lens.mapping Lens.coerced

-- | The preferred language used to return results. Matches the language in
-- the request. The value is a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
searchPlaceIndexForSuggestionsSummary_language :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary (Prelude.Maybe Prelude.Text)
searchPlaceIndexForSuggestionsSummary_language = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {language} -> language) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {language = a} :: SearchPlaceIndexForSuggestionsSummary)

-- | Contains the optional result count limit specified in the request.
searchPlaceIndexForSuggestionsSummary_maxResults :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary (Prelude.Maybe Prelude.Int)
searchPlaceIndexForSuggestionsSummary_maxResults = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {maxResults} -> maxResults) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {maxResults = a} :: SearchPlaceIndexForSuggestionsSummary)

-- | The geospatial data provider attached to the place index resource
-- specified in the request. Values can be one of the following:
--
-- -   Esri
--
-- -   Here
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
searchPlaceIndexForSuggestionsSummary_dataSource :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary Prelude.Text
searchPlaceIndexForSuggestionsSummary_dataSource = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {dataSource} -> dataSource) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {dataSource = a} :: SearchPlaceIndexForSuggestionsSummary)

-- | The free-form partial text input specified in the request.
searchPlaceIndexForSuggestionsSummary_text :: Lens.Lens' SearchPlaceIndexForSuggestionsSummary Prelude.Text
searchPlaceIndexForSuggestionsSummary_text = Lens.lens (\SearchPlaceIndexForSuggestionsSummary' {text} -> text) (\s@SearchPlaceIndexForSuggestionsSummary' {} a -> s {text = a} :: SearchPlaceIndexForSuggestionsSummary) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    SearchPlaceIndexForSuggestionsSummary
  where
  parseJSON =
    Data.withObject
      "SearchPlaceIndexForSuggestionsSummary"
      ( \x ->
          SearchPlaceIndexForSuggestionsSummary'
            Prelude.<$> (x Data..:? "BiasPosition")
            Prelude.<*> (x Data..:? "FilterBBox")
            Prelude.<*> (x Data..:? "FilterCountries")
            Prelude.<*> (x Data..:? "Language")
            Prelude.<*> (x Data..:? "MaxResults")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Text")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForSuggestionsSummary
  where
  hashWithSalt
    _salt
    SearchPlaceIndexForSuggestionsSummary' {..} =
      _salt
        `Prelude.hashWithSalt` biasPosition
        `Prelude.hashWithSalt` filterBBox
        `Prelude.hashWithSalt` filterCountries
        `Prelude.hashWithSalt` language
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` text

instance
  Prelude.NFData
    SearchPlaceIndexForSuggestionsSummary
  where
  rnf SearchPlaceIndexForSuggestionsSummary' {..} =
    Prelude.rnf biasPosition
      `Prelude.seq` Prelude.rnf filterBBox
      `Prelude.seq` Prelude.rnf filterCountries
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf text
