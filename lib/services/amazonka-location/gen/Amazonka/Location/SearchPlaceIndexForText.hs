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
-- Module      : Amazonka.Location.SearchPlaceIndexForText
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Geocodes free-form text, such as an address, name, city, or region to
-- allow you to search for Places or points of interest.
--
-- Optional parameters let you narrow your search results by bounding box
-- or country, or bias your search toward a specific position on the globe.
--
-- You can search for places near a given position using @BiasPosition@, or
-- filter results within a bounding box using @FilterBBox@. Providing both
-- parameters simultaneously returns an error.
--
-- Search results are returned in order of highest to lowest relevance.
module Amazonka.Location.SearchPlaceIndexForText
  ( -- * Creating a Request
    SearchPlaceIndexForText (..),
    newSearchPlaceIndexForText,

    -- * Request Lenses
    searchPlaceIndexForText_biasPosition,
    searchPlaceIndexForText_filterBBox,
    searchPlaceIndexForText_filterCountries,
    searchPlaceIndexForText_language,
    searchPlaceIndexForText_maxResults,
    searchPlaceIndexForText_indexName,
    searchPlaceIndexForText_text,

    -- * Destructuring the Response
    SearchPlaceIndexForTextResponse (..),
    newSearchPlaceIndexForTextResponse,

    -- * Response Lenses
    searchPlaceIndexForTextResponse_httpStatus,
    searchPlaceIndexForTextResponse_results,
    searchPlaceIndexForTextResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchPlaceIndexForText' smart constructor.
data SearchPlaceIndexForText = SearchPlaceIndexForText'
  { -- | An optional parameter that indicates a preference for places that are
    -- closer to a specified position.
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
    -- places that are within the provided bounding box.
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
    -- | An optional parameter that limits the search results by returning only
    -- places that are in a specified list of countries.
    --
    -- -   Valid values include
    --     <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
    --     country codes. For example, Australia uses three upper-case
    --     characters: @AUS@.
    filterCountries :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The preferred language used to return results. The value must be a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    --
    -- This setting affects the languages used in the results, but not the
    -- results themselves. If no language is specified, or not supported for a
    -- particular result, the partner automatically chooses a language for the
    -- result.
    --
    -- For an example, we\'ll use the Greek language. You search for
    -- @Athens, Greece@, with the @language@ parameter set to @en@. The result
    -- found will most likely be returned as @Athens@.
    --
    -- If you set the @language@ parameter to @el@, for Greek, then the result
    -- found will more likely be returned as @Αθήνα@.
    --
    -- If the data provider does not have a value for Greek, the result will be
    -- in a language that the provider does support.
    language :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter. The maximum number of results returned per
    -- request.
    --
    -- The default: @50@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the place index resource you want to use for the search.
    indexName :: Prelude.Text,
    -- | The address, name, city, or region to be used in the search in free-form
    -- text format. For example, @123 Any Street@.
    text :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'biasPosition', 'searchPlaceIndexForText_biasPosition' - An optional parameter that indicates a preference for places that are
-- closer to a specified position.
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
-- 'filterBBox', 'searchPlaceIndexForText_filterBBox' - An optional parameter that limits the search results by returning only
-- places that are within the provided bounding box.
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
-- 'filterCountries', 'searchPlaceIndexForText_filterCountries' - An optional parameter that limits the search results by returning only
-- places that are in a specified list of countries.
--
-- -   Valid values include
--     <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
--     country codes. For example, Australia uses three upper-case
--     characters: @AUS@.
--
-- 'language', 'searchPlaceIndexForText_language' - The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for
-- @Athens, Greece@, with the @language@ parameter set to @en@. The result
-- found will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the result
-- found will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
--
-- 'maxResults', 'searchPlaceIndexForText_maxResults' - An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @50@
--
-- 'indexName', 'searchPlaceIndexForText_indexName' - The name of the place index resource you want to use for the search.
--
-- 'text', 'searchPlaceIndexForText_text' - The address, name, city, or region to be used in the search in free-form
-- text format. For example, @123 Any Street@.
newSearchPlaceIndexForText ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForText
newSearchPlaceIndexForText pIndexName_ pText_ =
  SearchPlaceIndexForText'
    { biasPosition =
        Prelude.Nothing,
      filterBBox = Prelude.Nothing,
      filterCountries = Prelude.Nothing,
      language = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexName = pIndexName_,
      text = Data._Sensitive Lens.# pText_
    }

-- | An optional parameter that indicates a preference for places that are
-- closer to a specified position.
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
searchPlaceIndexForText_biasPosition :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForText_biasPosition = Lens.lens (\SearchPlaceIndexForText' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForText' {} a -> s {biasPosition = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An optional parameter that limits the search results by returning only
-- places that are within the provided bounding box.
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
searchPlaceIndexForText_filterBBox :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForText_filterBBox = Lens.lens (\SearchPlaceIndexForText' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForText' {} a -> s {filterBBox = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An optional parameter that limits the search results by returning only
-- places that are in a specified list of countries.
--
-- -   Valid values include
--     <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
--     country codes. For example, Australia uses three upper-case
--     characters: @AUS@.
searchPlaceIndexForText_filterCountries :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForText_filterCountries = Lens.lens (\SearchPlaceIndexForText' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForText' {} a -> s {filterCountries = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping Lens.coerced

-- | The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for
-- @Athens, Greece@, with the @language@ parameter set to @en@. The result
-- found will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the result
-- found will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
searchPlaceIndexForText_language :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe Prelude.Text)
searchPlaceIndexForText_language = Lens.lens (\SearchPlaceIndexForText' {language} -> language) (\s@SearchPlaceIndexForText' {} a -> s {language = a} :: SearchPlaceIndexForText)

-- | An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @50@
searchPlaceIndexForText_maxResults :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForText_maxResults = Lens.lens (\SearchPlaceIndexForText' {maxResults} -> maxResults) (\s@SearchPlaceIndexForText' {} a -> s {maxResults = a} :: SearchPlaceIndexForText)

-- | The name of the place index resource you want to use for the search.
searchPlaceIndexForText_indexName :: Lens.Lens' SearchPlaceIndexForText Prelude.Text
searchPlaceIndexForText_indexName = Lens.lens (\SearchPlaceIndexForText' {indexName} -> indexName) (\s@SearchPlaceIndexForText' {} a -> s {indexName = a} :: SearchPlaceIndexForText)

-- | The address, name, city, or region to be used in the search in free-form
-- text format. For example, @123 Any Street@.
searchPlaceIndexForText_text :: Lens.Lens' SearchPlaceIndexForText Prelude.Text
searchPlaceIndexForText_text = Lens.lens (\SearchPlaceIndexForText' {text} -> text) (\s@SearchPlaceIndexForText' {} a -> s {text = a} :: SearchPlaceIndexForText) Prelude.. Data._Sensitive

instance Core.AWSRequest SearchPlaceIndexForText where
  type
    AWSResponse SearchPlaceIndexForText =
      SearchPlaceIndexForTextResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchPlaceIndexForTextResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Summary")
      )

instance Prelude.Hashable SearchPlaceIndexForText where
  hashWithSalt _salt SearchPlaceIndexForText' {..} =
    _salt `Prelude.hashWithSalt` biasPosition
      `Prelude.hashWithSalt` filterBBox
      `Prelude.hashWithSalt` filterCountries
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` text

instance Prelude.NFData SearchPlaceIndexForText where
  rnf SearchPlaceIndexForText' {..} =
    Prelude.rnf biasPosition
      `Prelude.seq` Prelude.rnf filterBBox
      `Prelude.seq` Prelude.rnf filterCountries
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf text

instance Data.ToHeaders SearchPlaceIndexForText where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchPlaceIndexForText where
  toJSON SearchPlaceIndexForText' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BiasPosition" Data..=) Prelude.<$> biasPosition,
            ("FilterBBox" Data..=) Prelude.<$> filterBBox,
            ("FilterCountries" Data..=)
              Prelude.<$> filterCountries,
            ("Language" Data..=) Prelude.<$> language,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Text" Data..= text)
          ]
      )

instance Data.ToPath SearchPlaceIndexForText where
  toPath SearchPlaceIndexForText' {..} =
    Prelude.mconcat
      [ "/places/v0/indexes/",
        Data.toBS indexName,
        "/search/text"
      ]

instance Data.ToQuery SearchPlaceIndexForText where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchPlaceIndexForTextResponse' smart constructor.
data SearchPlaceIndexForTextResponse = SearchPlaceIndexForTextResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of Places matching the input text. Each result contains
    -- additional information about the specific point of interest.
    --
    -- Not all response properties are included with all responses. Some
    -- properties may only be returned by specific data partners.
    results :: [SearchForTextResult],
    -- | Contains a summary of the request. Echoes the input values for
    -- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
    -- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
    -- index and the bounding box, @ResultBBox@, which surrounds the search
    -- results.
    summary :: SearchPlaceIndexForTextSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'searchPlaceIndexForTextResponse_httpStatus' - The response's http status code.
--
-- 'results', 'searchPlaceIndexForTextResponse_results' - A list of Places matching the input text. Each result contains
-- additional information about the specific point of interest.
--
-- Not all response properties are included with all responses. Some
-- properties may only be returned by specific data partners.
--
-- 'summary', 'searchPlaceIndexForTextResponse_summary' - Contains a summary of the request. Echoes the input values for
-- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
-- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
-- index and the bounding box, @ResultBBox@, which surrounds the search
-- results.
newSearchPlaceIndexForTextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  SearchPlaceIndexForTextSummary ->
  SearchPlaceIndexForTextResponse
newSearchPlaceIndexForTextResponse
  pHttpStatus_
  pSummary_ =
    SearchPlaceIndexForTextResponse'
      { httpStatus =
          pHttpStatus_,
        results = Prelude.mempty,
        summary = pSummary_
      }

-- | The response's http status code.
searchPlaceIndexForTextResponse_httpStatus :: Lens.Lens' SearchPlaceIndexForTextResponse Prelude.Int
searchPlaceIndexForTextResponse_httpStatus = Lens.lens (\SearchPlaceIndexForTextResponse' {httpStatus} -> httpStatus) (\s@SearchPlaceIndexForTextResponse' {} a -> s {httpStatus = a} :: SearchPlaceIndexForTextResponse)

-- | A list of Places matching the input text. Each result contains
-- additional information about the specific point of interest.
--
-- Not all response properties are included with all responses. Some
-- properties may only be returned by specific data partners.
searchPlaceIndexForTextResponse_results :: Lens.Lens' SearchPlaceIndexForTextResponse [SearchForTextResult]
searchPlaceIndexForTextResponse_results = Lens.lens (\SearchPlaceIndexForTextResponse' {results} -> results) (\s@SearchPlaceIndexForTextResponse' {} a -> s {results = a} :: SearchPlaceIndexForTextResponse) Prelude.. Lens.coerced

-- | Contains a summary of the request. Echoes the input values for
-- @BiasPosition@, @FilterBBox@, @FilterCountries@, @Language@,
-- @MaxResults@, and @Text@. Also includes the @DataSource@ of the place
-- index and the bounding box, @ResultBBox@, which surrounds the search
-- results.
searchPlaceIndexForTextResponse_summary :: Lens.Lens' SearchPlaceIndexForTextResponse SearchPlaceIndexForTextSummary
searchPlaceIndexForTextResponse_summary = Lens.lens (\SearchPlaceIndexForTextResponse' {summary} -> summary) (\s@SearchPlaceIndexForTextResponse' {} a -> s {summary = a} :: SearchPlaceIndexForTextResponse)

instance
  Prelude.NFData
    SearchPlaceIndexForTextResponse
  where
  rnf SearchPlaceIndexForTextResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf summary
