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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Geocodes free-form text, such as an address, name, city, or region to
-- allow you to search for Places or points of interest.
--
-- Includes the option to apply additional parameters to narrow your list
-- of results.
--
-- You can search for places near a given position using @BiasPosition@, or
-- filter results within a bounding box using @FilterBBox@. Providing both
-- parameters simultaneously returns an error.
module Amazonka.Location.SearchPlaceIndexForText
  ( -- * Creating a Request
    SearchPlaceIndexForText (..),
    newSearchPlaceIndexForText,

    -- * Request Lenses
    searchPlaceIndexForText_filterBBox,
    searchPlaceIndexForText_biasPosition,
    searchPlaceIndexForText_filterCountries,
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
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchPlaceIndexForText' smart constructor.
data SearchPlaceIndexForText = SearchPlaceIndexForText'
  { -- | Filters the results by returning only Places within the provided
    -- bounding box. An optional parameter.
    --
    -- The first 2 @bbox@ parameters describe the lower southwest corner:
    --
    -- -   The first @bbox@ position is the X coordinate or longitude of the
    --     lower southwest corner.
    --
    -- -   The second @bbox@ position is the Y coordinate or latitude of the
    --     lower southwest corner.
    --
    -- For example, @bbox=xLongitudeSW&bbox=yLatitudeSW@.
    --
    -- The next @bbox@ parameters describe the upper northeast corner:
    --
    -- -   The third @bbox@ position is the X coordinate, or longitude of the
    --     upper northeast corner.
    --
    -- -   The fourth @bbox@ position is the Y coordinate, or longitude of the
    --     upper northeast corner.
    --
    -- For example, @bbox=xLongitudeNE&bbox=yLatitudeNE@
    filterBBox :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Searches for results closest to the given position. An optional
    -- parameter defined by longitude, and latitude.
    --
    -- -   The first @bias@ position is the X coordinate, or longitude.
    --
    -- -   The second @bias@ position is the Y coordinate, or latitude.
    --
    -- For example, @bias=xLongitude&bias=yLatitude@.
    biasPosition :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty Prelude.Double)),
    -- | Limits the search to the given a list of countries\/regions. An optional
    -- parameter.
    --
    -- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
    --     3-digit country code. For example, Australia uses three upper-case
    --     characters: @AUS@.
    filterCountries :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An optional parameter. The maximum number of results returned per
    -- request.
    --
    -- The default: @50@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the place index resource you want to use for the search.
    indexName :: Prelude.Text,
    -- | The address, name, city, or region to be used in the search. In
    -- free-form text format. For example, @123 Any Street@.
    text :: Core.Sensitive Prelude.Text
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
-- 'filterBBox', 'searchPlaceIndexForText_filterBBox' - Filters the results by returning only Places within the provided
-- bounding box. An optional parameter.
--
-- The first 2 @bbox@ parameters describe the lower southwest corner:
--
-- -   The first @bbox@ position is the X coordinate or longitude of the
--     lower southwest corner.
--
-- -   The second @bbox@ position is the Y coordinate or latitude of the
--     lower southwest corner.
--
-- For example, @bbox=xLongitudeSW&bbox=yLatitudeSW@.
--
-- The next @bbox@ parameters describe the upper northeast corner:
--
-- -   The third @bbox@ position is the X coordinate, or longitude of the
--     upper northeast corner.
--
-- -   The fourth @bbox@ position is the Y coordinate, or longitude of the
--     upper northeast corner.
--
-- For example, @bbox=xLongitudeNE&bbox=yLatitudeNE@
--
-- 'biasPosition', 'searchPlaceIndexForText_biasPosition' - Searches for results closest to the given position. An optional
-- parameter defined by longitude, and latitude.
--
-- -   The first @bias@ position is the X coordinate, or longitude.
--
-- -   The second @bias@ position is the Y coordinate, or latitude.
--
-- For example, @bias=xLongitude&bias=yLatitude@.
--
-- 'filterCountries', 'searchPlaceIndexForText_filterCountries' - Limits the search to the given a list of countries\/regions. An optional
-- parameter.
--
-- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
--     3-digit country code. For example, Australia uses three upper-case
--     characters: @AUS@.
--
-- 'maxResults', 'searchPlaceIndexForText_maxResults' - An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @50@
--
-- 'indexName', 'searchPlaceIndexForText_indexName' - The name of the place index resource you want to use for the search.
--
-- 'text', 'searchPlaceIndexForText_text' - The address, name, city, or region to be used in the search. In
-- free-form text format. For example, @123 Any Street@.
newSearchPlaceIndexForText ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  SearchPlaceIndexForText
newSearchPlaceIndexForText pIndexName_ pText_ =
  SearchPlaceIndexForText'
    { filterBBox =
        Prelude.Nothing,
      biasPosition = Prelude.Nothing,
      filterCountries = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexName = pIndexName_,
      text = Core._Sensitive Lens.# pText_
    }

-- | Filters the results by returning only Places within the provided
-- bounding box. An optional parameter.
--
-- The first 2 @bbox@ parameters describe the lower southwest corner:
--
-- -   The first @bbox@ position is the X coordinate or longitude of the
--     lower southwest corner.
--
-- -   The second @bbox@ position is the Y coordinate or latitude of the
--     lower southwest corner.
--
-- For example, @bbox=xLongitudeSW&bbox=yLatitudeSW@.
--
-- The next @bbox@ parameters describe the upper northeast corner:
--
-- -   The third @bbox@ position is the X coordinate, or longitude of the
--     upper northeast corner.
--
-- -   The fourth @bbox@ position is the Y coordinate, or longitude of the
--     upper northeast corner.
--
-- For example, @bbox=xLongitudeNE&bbox=yLatitudeNE@
searchPlaceIndexForText_filterBBox :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForText_filterBBox = Lens.lens (\SearchPlaceIndexForText' {filterBBox} -> filterBBox) (\s@SearchPlaceIndexForText' {} a -> s {filterBBox = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Searches for results closest to the given position. An optional
-- parameter defined by longitude, and latitude.
--
-- -   The first @bias@ position is the X coordinate, or longitude.
--
-- -   The second @bias@ position is the Y coordinate, or latitude.
--
-- For example, @bias=xLongitude&bias=yLatitude@.
searchPlaceIndexForText_biasPosition :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
searchPlaceIndexForText_biasPosition = Lens.lens (\SearchPlaceIndexForText' {biasPosition} -> biasPosition) (\s@SearchPlaceIndexForText' {} a -> s {biasPosition = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Limits the search to the given a list of countries\/regions. An optional
-- parameter.
--
-- -   Use the <https://www.iso.org/iso-3166-country-codes.html ISO 3166>
--     3-digit country code. For example, Australia uses three upper-case
--     characters: @AUS@.
searchPlaceIndexForText_filterCountries :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
searchPlaceIndexForText_filterCountries = Lens.lens (\SearchPlaceIndexForText' {filterCountries} -> filterCountries) (\s@SearchPlaceIndexForText' {} a -> s {filterCountries = a} :: SearchPlaceIndexForText) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter. The maximum number of results returned per
-- request.
--
-- The default: @50@
searchPlaceIndexForText_maxResults :: Lens.Lens' SearchPlaceIndexForText (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForText_maxResults = Lens.lens (\SearchPlaceIndexForText' {maxResults} -> maxResults) (\s@SearchPlaceIndexForText' {} a -> s {maxResults = a} :: SearchPlaceIndexForText)

-- | The name of the place index resource you want to use for the search.
searchPlaceIndexForText_indexName :: Lens.Lens' SearchPlaceIndexForText Prelude.Text
searchPlaceIndexForText_indexName = Lens.lens (\SearchPlaceIndexForText' {indexName} -> indexName) (\s@SearchPlaceIndexForText' {} a -> s {indexName = a} :: SearchPlaceIndexForText)

-- | The address, name, city, or region to be used in the search. In
-- free-form text format. For example, @123 Any Street@.
searchPlaceIndexForText_text :: Lens.Lens' SearchPlaceIndexForText Prelude.Text
searchPlaceIndexForText_text = Lens.lens (\SearchPlaceIndexForText' {text} -> text) (\s@SearchPlaceIndexForText' {} a -> s {text = a} :: SearchPlaceIndexForText) Prelude.. Core._Sensitive

instance Core.AWSRequest SearchPlaceIndexForText where
  type
    AWSResponse SearchPlaceIndexForText =
      SearchPlaceIndexForTextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchPlaceIndexForTextResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "Summary")
      )

instance Prelude.Hashable SearchPlaceIndexForText

instance Prelude.NFData SearchPlaceIndexForText

instance Core.ToHeaders SearchPlaceIndexForText where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchPlaceIndexForText where
  toJSON SearchPlaceIndexForText' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FilterBBox" Core..=) Prelude.<$> filterBBox,
            ("BiasPosition" Core..=) Prelude.<$> biasPosition,
            ("FilterCountries" Core..=)
              Prelude.<$> filterCountries,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Text" Core..= text)
          ]
      )

instance Core.ToPath SearchPlaceIndexForText where
  toPath SearchPlaceIndexForText' {..} =
    Prelude.mconcat
      [ "/places/v0/indexes/",
        Core.toBS indexName,
        "/search/text"
      ]

instance Core.ToQuery SearchPlaceIndexForText where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchPlaceIndexForTextResponse' smart constructor.
data SearchPlaceIndexForTextResponse = SearchPlaceIndexForTextResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of Places closest to the specified position. Each result contains
    -- additional information about the specific point of interest.
    results :: [SearchForTextResult],
    -- | Contains a summary of the request. Contains the @BiasPosition@,
    -- @DataSource@, @FilterBBox@, @FilterCountries@, @MaxResults@,
    -- @ResultBBox@, and @Text@.
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
-- 'results', 'searchPlaceIndexForTextResponse_results' - A list of Places closest to the specified position. Each result contains
-- additional information about the specific point of interest.
--
-- 'summary', 'searchPlaceIndexForTextResponse_summary' - Contains a summary of the request. Contains the @BiasPosition@,
-- @DataSource@, @FilterBBox@, @FilterCountries@, @MaxResults@,
-- @ResultBBox@, and @Text@.
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

-- | A list of Places closest to the specified position. Each result contains
-- additional information about the specific point of interest.
searchPlaceIndexForTextResponse_results :: Lens.Lens' SearchPlaceIndexForTextResponse [SearchForTextResult]
searchPlaceIndexForTextResponse_results = Lens.lens (\SearchPlaceIndexForTextResponse' {results} -> results) (\s@SearchPlaceIndexForTextResponse' {} a -> s {results = a} :: SearchPlaceIndexForTextResponse) Prelude.. Lens.coerced

-- | Contains a summary of the request. Contains the @BiasPosition@,
-- @DataSource@, @FilterBBox@, @FilterCountries@, @MaxResults@,
-- @ResultBBox@, and @Text@.
searchPlaceIndexForTextResponse_summary :: Lens.Lens' SearchPlaceIndexForTextResponse SearchPlaceIndexForTextSummary
searchPlaceIndexForTextResponse_summary = Lens.lens (\SearchPlaceIndexForTextResponse' {summary} -> summary) (\s@SearchPlaceIndexForTextResponse' {} a -> s {summary = a} :: SearchPlaceIndexForTextResponse)

instance
  Prelude.NFData
    SearchPlaceIndexForTextResponse
