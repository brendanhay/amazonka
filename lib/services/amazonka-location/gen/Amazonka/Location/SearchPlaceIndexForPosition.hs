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
-- Module      : Amazonka.Location.SearchPlaceIndexForPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reverse geocodes a given coordinate and returns a legible address.
-- Allows you to search for Places or points of interest near a given
-- position.
module Amazonka.Location.SearchPlaceIndexForPosition
  ( -- * Creating a Request
    SearchPlaceIndexForPosition (..),
    newSearchPlaceIndexForPosition,

    -- * Request Lenses
    searchPlaceIndexForPosition_language,
    searchPlaceIndexForPosition_maxResults,
    searchPlaceIndexForPosition_indexName,
    searchPlaceIndexForPosition_position,

    -- * Destructuring the Response
    SearchPlaceIndexForPositionResponse (..),
    newSearchPlaceIndexForPositionResponse,

    -- * Response Lenses
    searchPlaceIndexForPositionResponse_httpStatus,
    searchPlaceIndexForPositionResponse_results,
    searchPlaceIndexForPositionResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchPlaceIndexForPosition' smart constructor.
data SearchPlaceIndexForPosition = SearchPlaceIndexForPosition'
  { -- | The preferred language used to return results. The value must be a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    --
    -- This setting affects the languages used in the results, but not the
    -- results themselves. If no language is specified, or not supported for a
    -- particular result, the partner automatically chooses a language for the
    -- result.
    --
    -- For an example, we\'ll use the Greek language. You search for a location
    -- around Athens, Greece, with the @language@ parameter set to @en@. The
    -- @city@ in the results will most likely be returned as @Athens@.
    --
    -- If you set the @language@ parameter to @el@, for Greek, then the @city@
    -- in the results will more likely be returned as @Αθήνα@.
    --
    -- If the data provider does not have a value for Greek, the result will be
    -- in a language that the provider does support.
    language :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter. The maximum number of results returned per
    -- request.
    --
    -- Default value: @50@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the place index resource you want to use for the search.
    indexName :: Prelude.Text,
    -- | Specifies the longitude and latitude of the position to query.
    --
    -- This parameter must contain a pair of numbers. The first number
    -- represents the X coordinate, or longitude; the second number represents
    -- the Y coordinate, or latitude.
    --
    -- For example, @[-123.1174, 49.2847]@ represents a position with longitude
    -- @-123.1174@ and latitude @49.2847@.
    position :: Data.Sensitive (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForPosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'searchPlaceIndexForPosition_language' - The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for a location
-- around Athens, Greece, with the @language@ parameter set to @en@. The
-- @city@ in the results will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the @city@
-- in the results will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
--
-- 'maxResults', 'searchPlaceIndexForPosition_maxResults' - An optional parameter. The maximum number of results returned per
-- request.
--
-- Default value: @50@
--
-- 'indexName', 'searchPlaceIndexForPosition_indexName' - The name of the place index resource you want to use for the search.
--
-- 'position', 'searchPlaceIndexForPosition_position' - Specifies the longitude and latitude of the position to query.
--
-- This parameter must contain a pair of numbers. The first number
-- represents the X coordinate, or longitude; the second number represents
-- the Y coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents a position with longitude
-- @-123.1174@ and latitude @49.2847@.
newSearchPlaceIndexForPosition ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  SearchPlaceIndexForPosition
newSearchPlaceIndexForPosition pIndexName_ pPosition_ =
  SearchPlaceIndexForPosition'
    { language =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexName = pIndexName_,
      position =
        Data._Sensitive Prelude.. Lens.coerced
          Lens.# pPosition_
    }

-- | The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for a location
-- around Athens, Greece, with the @language@ parameter set to @en@. The
-- @city@ in the results will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the @city@
-- in the results will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
searchPlaceIndexForPosition_language :: Lens.Lens' SearchPlaceIndexForPosition (Prelude.Maybe Prelude.Text)
searchPlaceIndexForPosition_language = Lens.lens (\SearchPlaceIndexForPosition' {language} -> language) (\s@SearchPlaceIndexForPosition' {} a -> s {language = a} :: SearchPlaceIndexForPosition)

-- | An optional parameter. The maximum number of results returned per
-- request.
--
-- Default value: @50@
searchPlaceIndexForPosition_maxResults :: Lens.Lens' SearchPlaceIndexForPosition (Prelude.Maybe Prelude.Natural)
searchPlaceIndexForPosition_maxResults = Lens.lens (\SearchPlaceIndexForPosition' {maxResults} -> maxResults) (\s@SearchPlaceIndexForPosition' {} a -> s {maxResults = a} :: SearchPlaceIndexForPosition)

-- | The name of the place index resource you want to use for the search.
searchPlaceIndexForPosition_indexName :: Lens.Lens' SearchPlaceIndexForPosition Prelude.Text
searchPlaceIndexForPosition_indexName = Lens.lens (\SearchPlaceIndexForPosition' {indexName} -> indexName) (\s@SearchPlaceIndexForPosition' {} a -> s {indexName = a} :: SearchPlaceIndexForPosition)

-- | Specifies the longitude and latitude of the position to query.
--
-- This parameter must contain a pair of numbers. The first number
-- represents the X coordinate, or longitude; the second number represents
-- the Y coordinate, or latitude.
--
-- For example, @[-123.1174, 49.2847]@ represents a position with longitude
-- @-123.1174@ and latitude @49.2847@.
searchPlaceIndexForPosition_position :: Lens.Lens' SearchPlaceIndexForPosition (Prelude.NonEmpty Prelude.Double)
searchPlaceIndexForPosition_position = Lens.lens (\SearchPlaceIndexForPosition' {position} -> position) (\s@SearchPlaceIndexForPosition' {} a -> s {position = a} :: SearchPlaceIndexForPosition) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Core.AWSRequest SearchPlaceIndexForPosition where
  type
    AWSResponse SearchPlaceIndexForPosition =
      SearchPlaceIndexForPositionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchPlaceIndexForPositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Summary")
      )

instance Prelude.Hashable SearchPlaceIndexForPosition where
  hashWithSalt _salt SearchPlaceIndexForPosition' {..} =
    _salt `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` position

instance Prelude.NFData SearchPlaceIndexForPosition where
  rnf SearchPlaceIndexForPosition' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf position

instance Data.ToHeaders SearchPlaceIndexForPosition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchPlaceIndexForPosition where
  toJSON SearchPlaceIndexForPosition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Language" Data..=) Prelude.<$> language,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Position" Data..= position)
          ]
      )

instance Data.ToPath SearchPlaceIndexForPosition where
  toPath SearchPlaceIndexForPosition' {..} =
    Prelude.mconcat
      [ "/places/v0/indexes/",
        Data.toBS indexName,
        "/search/position"
      ]

instance Data.ToQuery SearchPlaceIndexForPosition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchPlaceIndexForPositionResponse' smart constructor.
data SearchPlaceIndexForPositionResponse = SearchPlaceIndexForPositionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns a list of Places closest to the specified position. Each result
    -- contains additional information about the Places returned.
    results :: [SearchForPositionResult],
    -- | Contains a summary of the request. Echoes the input values for
    -- @Position@, @Language@, @MaxResults@, and the @DataSource@ of the place
    -- index.
    summary :: SearchPlaceIndexForPositionSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPlaceIndexForPositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'searchPlaceIndexForPositionResponse_httpStatus' - The response's http status code.
--
-- 'results', 'searchPlaceIndexForPositionResponse_results' - Returns a list of Places closest to the specified position. Each result
-- contains additional information about the Places returned.
--
-- 'summary', 'searchPlaceIndexForPositionResponse_summary' - Contains a summary of the request. Echoes the input values for
-- @Position@, @Language@, @MaxResults@, and the @DataSource@ of the place
-- index.
newSearchPlaceIndexForPositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  SearchPlaceIndexForPositionSummary ->
  SearchPlaceIndexForPositionResponse
newSearchPlaceIndexForPositionResponse
  pHttpStatus_
  pSummary_ =
    SearchPlaceIndexForPositionResponse'
      { httpStatus =
          pHttpStatus_,
        results = Prelude.mempty,
        summary = pSummary_
      }

-- | The response's http status code.
searchPlaceIndexForPositionResponse_httpStatus :: Lens.Lens' SearchPlaceIndexForPositionResponse Prelude.Int
searchPlaceIndexForPositionResponse_httpStatus = Lens.lens (\SearchPlaceIndexForPositionResponse' {httpStatus} -> httpStatus) (\s@SearchPlaceIndexForPositionResponse' {} a -> s {httpStatus = a} :: SearchPlaceIndexForPositionResponse)

-- | Returns a list of Places closest to the specified position. Each result
-- contains additional information about the Places returned.
searchPlaceIndexForPositionResponse_results :: Lens.Lens' SearchPlaceIndexForPositionResponse [SearchForPositionResult]
searchPlaceIndexForPositionResponse_results = Lens.lens (\SearchPlaceIndexForPositionResponse' {results} -> results) (\s@SearchPlaceIndexForPositionResponse' {} a -> s {results = a} :: SearchPlaceIndexForPositionResponse) Prelude.. Lens.coerced

-- | Contains a summary of the request. Echoes the input values for
-- @Position@, @Language@, @MaxResults@, and the @DataSource@ of the place
-- index.
searchPlaceIndexForPositionResponse_summary :: Lens.Lens' SearchPlaceIndexForPositionResponse SearchPlaceIndexForPositionSummary
searchPlaceIndexForPositionResponse_summary = Lens.lens (\SearchPlaceIndexForPositionResponse' {summary} -> summary) (\s@SearchPlaceIndexForPositionResponse' {} a -> s {summary = a} :: SearchPlaceIndexForPositionResponse)

instance
  Prelude.NFData
    SearchPlaceIndexForPositionResponse
  where
  rnf SearchPlaceIndexForPositionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf summary
