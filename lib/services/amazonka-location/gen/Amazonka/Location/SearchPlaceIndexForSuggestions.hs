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
-- Module      : Amazonka.Location.SearchPlaceIndexForSuggestions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates suggestions for addresses and points of interest based on
-- partial or misspelled free-form text. This operation is also known as
-- autocomplete, autosuggest, or fuzzy matching.
--
-- Optional parameters let you narrow your search results by bounding box
-- or country, or bias your search toward a specific position on the globe.
--
-- You can search for suggested place names near a specified position by
-- using @BiasPosition@, or filter results within a bounding box by using
-- @FilterBBox@. These parameters are mutually exclusive; using both
-- @BiasPosition@ and @FilterBBox@ in the same command returns an error.
module Amazonka.Location.SearchPlaceIndexForSuggestions
  ( -- * Creating a Request
    SearchPlaceIndexForSuggestions (..),
    newSearchPlaceIndexForSuggestions,

    -- * Request Lenses
    searchPlaceIndexForSuggestions_filterBBox,
    searchPlaceIndexForSuggestions_biasPosition,
    searchPlaceIndexForSuggestions_filterCountries,
    searchPlaceIndexForSuggestions_maxResults,
    searchPlaceIndexForSuggestions_language,
    searchPlaceIndexForSuggestions_indexName,
    searchPlaceIndexForSuggestions_text,

    -- * Destructuring the Response
    SearchPlaceIndexForSuggestionsResponse (..),
    newSearchPlaceIndexForSuggestionsResponse,

    -- * Response Lenses
    searchPlaceIndexForSuggestionsResponse_httpStatus,
    searchPlaceIndexForSuggestionsResponse_results,
    searchPlaceIndexForSuggestionsResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchPlaceIndexForSuggestions' smart constructor.
data SearchPlaceIndexForSuggestions = SearchPlaceIndexForSuggestions'
  { -- | An optional parameter that limits the search results by returning only
    -- suggestions within a specified bounding box.
    --
    -- If provided, this parameter must contain a total of four consecutive
    -- numbers in two pairs. The first pair of numbers represents the X and Y
    -- coordinates (longitude and latitude, respectively) of the southwest
    -- corner of the bounding box; the second pair of numbers represents the X
    -- and Y coordinates (longitude and latitude, respectively) of the
    -- northeast corner of the bounding box.
    --
    -- For example, @[-12.7935, -37.4835, -12.0684, -36.9542]@ represents a
    -- bounding box where the southwest corner has longitude @-12.7935@ and
    -- latitude @-37.4835@, and the northeast corner has longitude @-12.0684@
    -- and latitude @-36.9542@.
    --
    -- @FilterBBox@ and @BiasPosition@ are mutually exclusive. Specifying both
    -- options results in an error.
    filterBBox :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | An optional parameter that indicates a preference for place suggestions
    -- that are closer to a specified position.
    --
    -- If provided, this parameter must contain a pair of numbers. The first
    -- number represents the X coordinate, or longitude; the second number
    -- represents the Y coordinate, or latitude.
    --
    -- For example, @[-123.1174, 49.2847]@ represents the position with
    -- longitude @-123.1174@ and latitude @49.2847@.
    --
    -- @BiasPosition@ and @FilterBBox@ are mutually exclusive. Specifying both
    -- options results in an error.
    biasPosition :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | An optional parameter that limits the search results by returning only
    -- suggestions within the provided list of countries.
    --
    -- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
    --     3-digit country code. For example, Australia uses three upper-case
    --     characters: @AUS@.
    filterCountries :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An optional parameter. The maximum number of results returned per
    -- request.
    --
    -- The default: @5@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The preferred language used to return results. The value must be a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    --
    -- This setting affects the languages used in the results. If no language
    -- is specified, or not supported for a particular result, the partner
    -- automatically chooses a language for the result.
    --
    -- For an example, we\'ll use the Greek language. You search for
    -- @Athens, Gr@ to get suggestions with the @language@ parameter set to
    -- @en@. The results found will most likely be returned as
    -- @Athens, Greece@.
    --
    -- If you set the @language@ parameter to @el@, for Greek, then the result
    -- found will more likely be returned as @Αθήνα, Ελλάδα@.
    --
    -- If the data provider does not have a value for Greek, the result will be
    -- in a language that the provider does support.
    language :: Prelude.Maybe Prelude.Text,
    -- | The name of the place index resource you want to use for the search.
    indexName :: Prelude.Text,
    -- | The free-form partial text to use to generate place suggestions. For
    -- example, @eiffel tow@.
    text :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForSuggestions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterBBox', 'searchPlaceIndexForSuggestions_filterBBox' - An optional parameter that limits the search results by returning only
-- suggestions within a specified bounding box.
--
-- If provided, this parameter must contain a total of four consecutive
-- numbers in two pairs. The first pair of numbers represents the X and Y
-- coordinates (longitude and latitude, respectively) of the southwest
-- corner of the bounding box; the second pair of numbers represents the X
-- and Y coordinates (longitude and latitude, respectively) of the
-- northeast corner of the bounding box.
--
-- For example, @[-12.7935, -37.4835, -12.0684, -36.9542]@ represents a
-- bounding box where the southwest corner has longitude @-12.7935@ and
-- latitude @-37.4835@, and the northeast corner has longitude @-12.0684@
-- and latitude @-36.9542@.
--
-- @FilterBBox@ and @BiasPosition@ are mutually exclusive. Specifying both
-- options results in an error.
--
-- 'biasPosition', 'searchPlaceIndexForSuggestions_biasPosition' - An optional parameter that indicates a preference for place suggestions
-- that are closer to a specified position.
--
-- If provided, this parameter must contain a pair of numbers. The first
-- number represents the X coordinate, or longitude; the second number
-- represents the Y coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents the position with
-- longitude @-123.1174@ and latitude @49.2847@.
--
-- @BiasPosition@ and @FilterBBox@ are mutually exclusive. Specifying both
-- options results in an error.
--
-- 'filterCountries', 'searchPlaceIndexForSuggestions_filterCountries' - An optional parameter that limits the search results by returning only
-- suggestions within the provided list of countries.
--
-- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
--     3-digit country code. For example, Australia uses three upper-case
--     characters: @AUS@.
--
-- 'maxResults', 'searchPlaceIndexForSuggestions_maxResults' - An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @5@
--
-- 'language', 'searchPlaceIndexForSuggestions_language' - The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results. If no language
-- is specified, or not supported for a particular result, the partner
-- automatically chooses a language for the result.
--
-- For an example, we\'ll use the Greek language. You search for
-- @Athens, Gr@ to get suggestions with the @language@ parameter set to
-- @en@. The results found will most likely be returned as
-- @Athens, Greece@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the result
-- found will more likely be returned as @Αθήνα, Ελλάδα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
--
-- 'indexName', 'searchPlaceIndexForSuggestions_indexName' - The name of the place index resource you want to use for the search.
--
-- 'text', 'searchPlaceIndexForSuggestions_text' - The free-form partial text to use to generate place suggestions. For
-- example, @eiffel tow@.
newSearchPlaceIndexForSuggestions ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForSuggestions
newSearchPlaceIndexForSuggestions pIndexName_ pText_ =
  SearchPlaceIndexForSuggestions'
    { filterBBox =
        Prelude.Nothing,
      biasPosition = Prelude.Nothing,
      filterCountries = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      language = Prelude.Nothing,
      indexName = pIndexName_,
      text = Data._Sensitive Lens.# pText_
    }

-- | An optional parameter that limits the search results by returning only
-- suggestions within a specified bounding box.
--
-- If provided, this parameter must contain a total of four consecutive
-- numbers in two pairs. The first pair of numbers represents the X and Y
-- coordinates (longitude and latitude, respectively) of the southwest
-- corner of the bounding box; the second pair of numbers represents the X
-- and Y coordinates (longitude and latitude, respectively) of the
-- northeast corner of the bounding box.
--
-- For example, @[-12.7935, -37.4835, -12.0684, -36.9542]@ represents a
-- bounding box where the southwest corner has longitude @-12.7935@ and
-- latitude @-37.4835@, and the northeast corner has longitude @-12.0684@
-- and latitude @-36.9542@.
--
-- @FilterBBox@ and @BiasPosition@ are mutually exclusive. Specifying both
-- options results in an error.
searchPlaceIndexForSuggestions_filterBBox :: Lens.Lens' SearchPlaceIndexForSuggestions (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForSuggestions_filterBBox = Lens.lens (\SearchPlaceIndexForSuggestions' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForSuggestions' {} a -> s {filterBBox = a} :: SearchPlaceIndexForSuggestions) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An optional parameter that indicates a preference for place suggestions
-- that are closer to a specified position.
--
-- If provided, this parameter must contain a pair of numbers. The first
-- number represents the X coordinate, or longitude; the second number
-- represents the Y coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents the position with
-- longitude @-123.1174@ and latitude @49.2847@.
--
-- @BiasPosition@ and @FilterBBox@ are mutually exclusive. Specifying both
-- options results in an error.
searchPlaceIndexForSuggestions_biasPosition :: Lens.Lens' SearchPlaceIndexForSuggestions (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForSuggestions_biasPosition = Lens.lens (\SearchPlaceIndexForSuggestions' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForSuggestions' {} a -> s {biasPosition = a} :: SearchPlaceIndexForSuggestions) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An optional parameter that limits the search results by returning only
-- suggestions within the provided list of countries.
--
-- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
--     3-digit country code. For example, Australia uses three upper-case
--     characters: @AUS@.
searchPlaceIndexForSuggestions_filterCountries :: Lens.Lens' SearchPlaceIndexForSuggestions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForSuggestions_filterCountries = Lens.lens (\SearchPlaceIndexForSuggestions' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForSuggestions' {} a -> s {filterCountries = a} :: SearchPlaceIndexForSuggestions) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @5@
searchPlaceIndexForSuggestions_maxResults :: Lens.Lens' SearchPlaceIndexForSuggestions (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForSuggestions_maxResults = Lens.lens (\SearchPlaceIndexForSuggestions' {maxResults} -> maxResults) (\s@SearchPlaceIndexForSuggestions' {} a -> s {maxResults = a} :: SearchPlaceIndexForSuggestions)

-- | The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results. If no language
-- is specified, or not supported for a particular result, the partner
-- automatically chooses a language for the result.
--
-- For an example, we\'ll use the Greek language. You search for
-- @Athens, Gr@ to get suggestions with the @language@ parameter set to
-- @en@. The results found will most likely be returned as
-- @Athens, Greece@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the result
-- found will more likely be returned as @Αθήνα, Ελλάδα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
searchPlaceIndexForSuggestions_language :: Lens.Lens' SearchPlaceIndexForSuggestions (Prelude.Maybe Prelude.Text)
searchPlaceIndexForSuggestions_language = Lens.lens (\SearchPlaceIndexForSuggestions' {language} -> language) (\s@SearchPlaceIndexForSuggestions' {} a -> s {language = a} :: SearchPlaceIndexForSuggestions)

-- | The name of the place index resource you want to use for the search.
searchPlaceIndexForSuggestions_indexName :: Lens.Lens' SearchPlaceIndexForSuggestions Prelude.Text
searchPlaceIndexForSuggestions_indexName = Lens.lens (\SearchPlaceIndexForSuggestions' {indexName} -> indexName) (\s@SearchPlaceIndexForSuggestions' {} a -> s {indexName = a} :: SearchPlaceIndexForSuggestions)

-- | The free-form partial text to use to generate place suggestions. For
-- example, @eiffel tow@.
searchPlaceIndexForSuggestions_text :: Lens.Lens' SearchPlaceIndexForSuggestions Prelude.Text
searchPlaceIndexForSuggestions_text = Lens.lens (\SearchPlaceIndexForSuggestions' {text} -> text) (\s@SearchPlaceIndexForSuggestions' {} a -> s {text = a} :: SearchPlaceIndexForSuggestions) Prelude.. Data._Sensitive

instance
  Core.AWSRequest
    SearchPlaceIndexForSuggestions
  where
  type
    AWSResponse SearchPlaceIndexForSuggestions =
      SearchPlaceIndexForSuggestionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchPlaceIndexForSuggestionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Summary")
      )

instance
  Prelude.Hashable
    SearchPlaceIndexForSuggestions
  where
  hashWithSalt
    _salt
    SearchPlaceIndexForSuggestions' {..} =
      _salt `Prelude.hashWithSalt` filterBBox
        `Prelude.hashWithSalt` biasPosition
        `Prelude.hashWithSalt` filterCountries
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` language
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` text

instance
  Prelude.NFData
    SearchPlaceIndexForSuggestions
  where
  rnf SearchPlaceIndexForSuggestions' {..} =
    Prelude.rnf filterBBox
      `Prelude.seq` Prelude.rnf biasPosition
      `Prelude.seq` Prelude.rnf filterCountries
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf text

instance
  Data.ToHeaders
    SearchPlaceIndexForSuggestions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchPlaceIndexForSuggestions where
  toJSON SearchPlaceIndexForSuggestions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterBBox" Data..=) Prelude.<$> filterBBox,
            ("BiasPosition" Data..=) Prelude.<$> biasPosition,
            ("FilterCountries" Data..=)
              Prelude.<$> filterCountries,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("Language" Data..=) Prelude.<$> language,
            Prelude.Just ("Text" Data..= text)
          ]
      )

instance Data.ToPath SearchPlaceIndexForSuggestions where
  toPath SearchPlaceIndexForSuggestions' {..} =
    Prelude.mconcat
      [ "/places/v0/indexes/",
        Data.toBS indexName,
        "/search/suggestions"
      ]

instance Data.ToQuery SearchPlaceIndexForSuggestions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchPlaceIndexForSuggestionsResponse' smart constructor.
data SearchPlaceIndexForSuggestionsResponse = SearchPlaceIndexForSuggestionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of place suggestions that best match the search text.
    results :: [SearchForSuggestionsResult],
    -- | Contains a summary of the request. Echoes the input values for
    -- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
    -- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
    -- index.
    summary :: SearchPlaceIndexForSuggestionsSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForSuggestionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'searchPlaceIndexForSuggestionsResponse_httpStatus' - The response's http status code.
--
-- 'results', 'searchPlaceIndexForSuggestionsResponse_results' - A list of place suggestions that best match the search text.
--
-- 'summary', 'searchPlaceIndexForSuggestionsResponse_summary' - Contains a summary of the request. Echoes the input values for
-- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
-- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
-- index.
newSearchPlaceIndexForSuggestionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  SearchPlaceIndexForSuggestionsSummary ->
  SearchPlaceIndexForSuggestionsResponse
newSearchPlaceIndexForSuggestionsResponse
  pHttpStatus_
  pSummary_ =
    SearchPlaceIndexForSuggestionsResponse'
      { httpStatus =
          pHttpStatus_,
        results = Prelude.mempty,
        summary = pSummary_
      }

-- | The response's http status code.
searchPlaceIndexForSuggestionsResponse_httpStatus :: Lens.Lens' SearchPlaceIndexForSuggestionsResponse Prelude.Int
searchPlaceIndexForSuggestionsResponse_httpStatus = Lens.lens (\SearchPlaceIndexForSuggestionsResponse' {httpStatus} -> httpStatus) (\s@SearchPlaceIndexForSuggestionsResponse' {} a -> s {httpStatus = a} :: SearchPlaceIndexForSuggestionsResponse)

-- | A list of place suggestions that best match the search text.
searchPlaceIndexForSuggestionsResponse_results :: Lens.Lens' SearchPlaceIndexForSuggestionsResponse [SearchForSuggestionsResult]
searchPlaceIndexForSuggestionsResponse_results = Lens.lens (\SearchPlaceIndexForSuggestionsResponse' {results} -> results) (\s@SearchPlaceIndexForSuggestionsResponse' {} a -> s {results = a} :: SearchPlaceIndexForSuggestionsResponse) Prelude.. Lens.coerced

-- | Contains a summary of the request. Echoes the input values for
-- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
-- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
-- index.
searchPlaceIndexForSuggestionsResponse_summary :: Lens.Lens' SearchPlaceIndexForSuggestionsResponse SearchPlaceIndexForSuggestionsSummary
searchPlaceIndexForSuggestionsResponse_summary = Lens.lens (\SearchPlaceIndexForSuggestionsResponse' {summary} -> summary) (\s@SearchPlaceIndexForSuggestionsResponse' {} a -> s {summary = a} :: SearchPlaceIndexForSuggestionsResponse)

instance
  Prelude.NFData
    SearchPlaceIndexForSuggestionsResponse
  where
  rnf SearchPlaceIndexForSuggestionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf summary
