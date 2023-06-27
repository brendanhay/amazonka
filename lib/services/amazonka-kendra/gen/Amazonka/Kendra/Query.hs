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
-- Module      : Amazonka.Kendra.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches an index given an input query.
--
-- You can configure boosting or relevance tuning at the query level to
-- override boosting at the index level, filter based on document
-- fields\/attributes and faceted search, and filter based on the user or
-- their group access to documents. You can also include certain fields in
-- the response that might provide useful additional information.
--
-- A query response contains three types of results.
--
-- -   Relevant suggested answers. The answers can be either a text excerpt
--     or table excerpt. The answer can be highlighted in the excerpt.
--
-- -   Matching FAQs or questions-answer from your FAQ file.
--
-- -   Relevant documents. This result type includes an excerpt of the
--     document with the document title. The searched terms can be
--     highlighted in the excerpt.
--
-- You can specify that the query return only one type of result using the
-- @QueryResultTypeFilter@ parameter. Each query returns the 100 most
-- relevant results. If you filter result type to only question-answers, a
-- maximum of four results are returned. If you filter result type to only
-- answers, a maximum of three results are returned.
module Amazonka.Kendra.Query
  ( -- * Creating a Request
    Query (..),
    newQuery,

    -- * Request Lenses
    query_attributeFilter,
    query_documentRelevanceOverrideConfigurations,
    query_facets,
    query_pageNumber,
    query_pageSize,
    query_queryResultTypeFilter,
    query_queryText,
    query_requestedDocumentAttributes,
    query_sortingConfiguration,
    query_spellCorrectionConfiguration,
    query_userContext,
    query_visitorId,
    query_indexId,

    -- * Destructuring the Response
    QueryResponse (..),
    newQueryResponse,

    -- * Response Lenses
    queryResponse_facetResults,
    queryResponse_featuredResultsItems,
    queryResponse_queryId,
    queryResponse_resultItems,
    queryResponse_spellCorrectedQueries,
    queryResponse_totalNumberOfResults,
    queryResponse_warnings,
    queryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | Filters search results by document fields\/attributes. You can only
    -- provide one attribute filter; however, the @AndAllFilters@, @NotFilter@,
    -- and @OrAllFilters@ parameters contain a list of other filters.
    --
    -- The @AttributeFilter@ parameter means you can create a set of filtering
    -- rules that a document must satisfy to be included in the query results.
    attributeFilter :: Prelude.Maybe AttributeFilter,
    -- | Overrides relevance tuning configurations of fields\/attributes set at
    -- the index level.
    --
    -- If you use this API to override the relevance tuning configured at the
    -- index level, but there is no relevance tuning configured at the index
    -- level, then Amazon Kendra does not apply any relevance tuning.
    --
    -- If there is relevance tuning configured for fields at the index level,
    -- and you use this API to override only some of these fields, then for the
    -- fields you did not override, the importance is set to 1.
    documentRelevanceOverrideConfigurations :: Prelude.Maybe [DocumentRelevanceConfiguration],
    -- | An array of documents fields\/attributes for faceted search. Amazon
    -- Kendra returns a count for each field key specified. This helps your
    -- users narrow their search.
    facets :: Prelude.Maybe [Facet],
    -- | Query results are returned in pages the size of the @PageSize@
    -- parameter. By default, Amazon Kendra returns the first page of results.
    -- Use this parameter to get result pages after the first one.
    pageNumber :: Prelude.Maybe Prelude.Int,
    -- | Sets the number of results that are returned in each page of results.
    -- The default page size is 10. The maximum number of results returned is
    -- 100. If you ask for more than 100 results, only 100 are returned.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | Sets the type of query result or response. Only results for the
    -- specified type are returned.
    queryResultTypeFilter :: Prelude.Maybe QueryResultType,
    -- | The input query text for the search. Amazon Kendra truncates queries at
    -- 30 token words, which excludes punctuation and stop words. Truncation
    -- still applies if you use Boolean or more advanced, complex queries.
    queryText :: Prelude.Maybe Prelude.Text,
    -- | An array of document fields\/attributes to include in the response. You
    -- can limit the response to include certain document fields. By default,
    -- all document attributes are included in the response.
    requestedDocumentAttributes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Provides information that determines how the results of the query are
    -- sorted. You can set the field that Amazon Kendra should sort the results
    -- on, and specify whether the results should be sorted in ascending or
    -- descending order. In the case of ties in sorting the results, the
    -- results are sorted by relevance.
    --
    -- If you don\'t provide sorting configuration, the results are sorted by
    -- the relevance that Amazon Kendra determines for the result.
    sortingConfiguration :: Prelude.Maybe SortingConfiguration,
    -- | Enables suggested spell corrections for queries.
    spellCorrectionConfiguration :: Prelude.Maybe SpellCorrectionConfiguration,
    -- | The user context token or user and group information.
    userContext :: Prelude.Maybe UserContext,
    -- | Provides an identifier for a specific user. The @VisitorId@ should be a
    -- unique identifier, such as a GUID. Don\'t use personally identifiable
    -- information, such as the user\'s email address, as the @VisitorId@.
    visitorId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index for the search.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Query' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeFilter', 'query_attributeFilter' - Filters search results by document fields\/attributes. You can only
-- provide one attribute filter; however, the @AndAllFilters@, @NotFilter@,
-- and @OrAllFilters@ parameters contain a list of other filters.
--
-- The @AttributeFilter@ parameter means you can create a set of filtering
-- rules that a document must satisfy to be included in the query results.
--
-- 'documentRelevanceOverrideConfigurations', 'query_documentRelevanceOverrideConfigurations' - Overrides relevance tuning configurations of fields\/attributes set at
-- the index level.
--
-- If you use this API to override the relevance tuning configured at the
-- index level, but there is no relevance tuning configured at the index
-- level, then Amazon Kendra does not apply any relevance tuning.
--
-- If there is relevance tuning configured for fields at the index level,
-- and you use this API to override only some of these fields, then for the
-- fields you did not override, the importance is set to 1.
--
-- 'facets', 'query_facets' - An array of documents fields\/attributes for faceted search. Amazon
-- Kendra returns a count for each field key specified. This helps your
-- users narrow their search.
--
-- 'pageNumber', 'query_pageNumber' - Query results are returned in pages the size of the @PageSize@
-- parameter. By default, Amazon Kendra returns the first page of results.
-- Use this parameter to get result pages after the first one.
--
-- 'pageSize', 'query_pageSize' - Sets the number of results that are returned in each page of results.
-- The default page size is 10. The maximum number of results returned is
-- 100. If you ask for more than 100 results, only 100 are returned.
--
-- 'queryResultTypeFilter', 'query_queryResultTypeFilter' - Sets the type of query result or response. Only results for the
-- specified type are returned.
--
-- 'queryText', 'query_queryText' - The input query text for the search. Amazon Kendra truncates queries at
-- 30 token words, which excludes punctuation and stop words. Truncation
-- still applies if you use Boolean or more advanced, complex queries.
--
-- 'requestedDocumentAttributes', 'query_requestedDocumentAttributes' - An array of document fields\/attributes to include in the response. You
-- can limit the response to include certain document fields. By default,
-- all document attributes are included in the response.
--
-- 'sortingConfiguration', 'query_sortingConfiguration' - Provides information that determines how the results of the query are
-- sorted. You can set the field that Amazon Kendra should sort the results
-- on, and specify whether the results should be sorted in ascending or
-- descending order. In the case of ties in sorting the results, the
-- results are sorted by relevance.
--
-- If you don\'t provide sorting configuration, the results are sorted by
-- the relevance that Amazon Kendra determines for the result.
--
-- 'spellCorrectionConfiguration', 'query_spellCorrectionConfiguration' - Enables suggested spell corrections for queries.
--
-- 'userContext', 'query_userContext' - The user context token or user and group information.
--
-- 'visitorId', 'query_visitorId' - Provides an identifier for a specific user. The @VisitorId@ should be a
-- unique identifier, such as a GUID. Don\'t use personally identifiable
-- information, such as the user\'s email address, as the @VisitorId@.
--
-- 'indexId', 'query_indexId' - The identifier of the index for the search.
newQuery ::
  -- | 'indexId'
  Prelude.Text ->
  Query
newQuery pIndexId_ =
  Query'
    { attributeFilter = Prelude.Nothing,
      documentRelevanceOverrideConfigurations =
        Prelude.Nothing,
      facets = Prelude.Nothing,
      pageNumber = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      queryResultTypeFilter = Prelude.Nothing,
      queryText = Prelude.Nothing,
      requestedDocumentAttributes = Prelude.Nothing,
      sortingConfiguration = Prelude.Nothing,
      spellCorrectionConfiguration = Prelude.Nothing,
      userContext = Prelude.Nothing,
      visitorId = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | Filters search results by document fields\/attributes. You can only
-- provide one attribute filter; however, the @AndAllFilters@, @NotFilter@,
-- and @OrAllFilters@ parameters contain a list of other filters.
--
-- The @AttributeFilter@ parameter means you can create a set of filtering
-- rules that a document must satisfy to be included in the query results.
query_attributeFilter :: Lens.Lens' Query (Prelude.Maybe AttributeFilter)
query_attributeFilter = Lens.lens (\Query' {attributeFilter} -> attributeFilter) (\s@Query' {} a -> s {attributeFilter = a} :: Query)

-- | Overrides relevance tuning configurations of fields\/attributes set at
-- the index level.
--
-- If you use this API to override the relevance tuning configured at the
-- index level, but there is no relevance tuning configured at the index
-- level, then Amazon Kendra does not apply any relevance tuning.
--
-- If there is relevance tuning configured for fields at the index level,
-- and you use this API to override only some of these fields, then for the
-- fields you did not override, the importance is set to 1.
query_documentRelevanceOverrideConfigurations :: Lens.Lens' Query (Prelude.Maybe [DocumentRelevanceConfiguration])
query_documentRelevanceOverrideConfigurations = Lens.lens (\Query' {documentRelevanceOverrideConfigurations} -> documentRelevanceOverrideConfigurations) (\s@Query' {} a -> s {documentRelevanceOverrideConfigurations = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | An array of documents fields\/attributes for faceted search. Amazon
-- Kendra returns a count for each field key specified. This helps your
-- users narrow their search.
query_facets :: Lens.Lens' Query (Prelude.Maybe [Facet])
query_facets = Lens.lens (\Query' {facets} -> facets) (\s@Query' {} a -> s {facets = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | Query results are returned in pages the size of the @PageSize@
-- parameter. By default, Amazon Kendra returns the first page of results.
-- Use this parameter to get result pages after the first one.
query_pageNumber :: Lens.Lens' Query (Prelude.Maybe Prelude.Int)
query_pageNumber = Lens.lens (\Query' {pageNumber} -> pageNumber) (\s@Query' {} a -> s {pageNumber = a} :: Query)

-- | Sets the number of results that are returned in each page of results.
-- The default page size is 10. The maximum number of results returned is
-- 100. If you ask for more than 100 results, only 100 are returned.
query_pageSize :: Lens.Lens' Query (Prelude.Maybe Prelude.Int)
query_pageSize = Lens.lens (\Query' {pageSize} -> pageSize) (\s@Query' {} a -> s {pageSize = a} :: Query)

-- | Sets the type of query result or response. Only results for the
-- specified type are returned.
query_queryResultTypeFilter :: Lens.Lens' Query (Prelude.Maybe QueryResultType)
query_queryResultTypeFilter = Lens.lens (\Query' {queryResultTypeFilter} -> queryResultTypeFilter) (\s@Query' {} a -> s {queryResultTypeFilter = a} :: Query)

-- | The input query text for the search. Amazon Kendra truncates queries at
-- 30 token words, which excludes punctuation and stop words. Truncation
-- still applies if you use Boolean or more advanced, complex queries.
query_queryText :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_queryText = Lens.lens (\Query' {queryText} -> queryText) (\s@Query' {} a -> s {queryText = a} :: Query)

-- | An array of document fields\/attributes to include in the response. You
-- can limit the response to include certain document fields. By default,
-- all document attributes are included in the response.
query_requestedDocumentAttributes :: Lens.Lens' Query (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
query_requestedDocumentAttributes = Lens.lens (\Query' {requestedDocumentAttributes} -> requestedDocumentAttributes) (\s@Query' {} a -> s {requestedDocumentAttributes = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | Provides information that determines how the results of the query are
-- sorted. You can set the field that Amazon Kendra should sort the results
-- on, and specify whether the results should be sorted in ascending or
-- descending order. In the case of ties in sorting the results, the
-- results are sorted by relevance.
--
-- If you don\'t provide sorting configuration, the results are sorted by
-- the relevance that Amazon Kendra determines for the result.
query_sortingConfiguration :: Lens.Lens' Query (Prelude.Maybe SortingConfiguration)
query_sortingConfiguration = Lens.lens (\Query' {sortingConfiguration} -> sortingConfiguration) (\s@Query' {} a -> s {sortingConfiguration = a} :: Query)

-- | Enables suggested spell corrections for queries.
query_spellCorrectionConfiguration :: Lens.Lens' Query (Prelude.Maybe SpellCorrectionConfiguration)
query_spellCorrectionConfiguration = Lens.lens (\Query' {spellCorrectionConfiguration} -> spellCorrectionConfiguration) (\s@Query' {} a -> s {spellCorrectionConfiguration = a} :: Query)

-- | The user context token or user and group information.
query_userContext :: Lens.Lens' Query (Prelude.Maybe UserContext)
query_userContext = Lens.lens (\Query' {userContext} -> userContext) (\s@Query' {} a -> s {userContext = a} :: Query)

-- | Provides an identifier for a specific user. The @VisitorId@ should be a
-- unique identifier, such as a GUID. Don\'t use personally identifiable
-- information, such as the user\'s email address, as the @VisitorId@.
query_visitorId :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_visitorId = Lens.lens (\Query' {visitorId} -> visitorId) (\s@Query' {} a -> s {visitorId = a} :: Query)

-- | The identifier of the index for the search.
query_indexId :: Lens.Lens' Query Prelude.Text
query_indexId = Lens.lens (\Query' {indexId} -> indexId) (\s@Query' {} a -> s {indexId = a} :: Query)

instance Core.AWSRequest Query where
  type AWSResponse Query = QueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryResponse'
            Prelude.<$> (x Data..?> "FacetResults" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "FeaturedResultsItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "QueryId")
            Prelude.<*> (x Data..?> "ResultItems" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "SpellCorrectedQueries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TotalNumberOfResults")
            Prelude.<*> (x Data..?> "Warnings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt
      `Prelude.hashWithSalt` attributeFilter
      `Prelude.hashWithSalt` documentRelevanceOverrideConfigurations
      `Prelude.hashWithSalt` facets
      `Prelude.hashWithSalt` pageNumber
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` queryResultTypeFilter
      `Prelude.hashWithSalt` queryText
      `Prelude.hashWithSalt` requestedDocumentAttributes
      `Prelude.hashWithSalt` sortingConfiguration
      `Prelude.hashWithSalt` spellCorrectionConfiguration
      `Prelude.hashWithSalt` userContext
      `Prelude.hashWithSalt` visitorId
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData Query where
  rnf Query' {..} =
    Prelude.rnf attributeFilter
      `Prelude.seq` Prelude.rnf documentRelevanceOverrideConfigurations
      `Prelude.seq` Prelude.rnf facets
      `Prelude.seq` Prelude.rnf pageNumber
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf queryResultTypeFilter
      `Prelude.seq` Prelude.rnf queryText
      `Prelude.seq` Prelude.rnf requestedDocumentAttributes
      `Prelude.seq` Prelude.rnf sortingConfiguration
      `Prelude.seq` Prelude.rnf spellCorrectionConfiguration
      `Prelude.seq` Prelude.rnf userContext
      `Prelude.seq` Prelude.rnf visitorId
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders Query where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.Query" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Query where
  toJSON Query' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeFilter" Data..=)
              Prelude.<$> attributeFilter,
            ("DocumentRelevanceOverrideConfigurations" Data..=)
              Prelude.<$> documentRelevanceOverrideConfigurations,
            ("Facets" Data..=) Prelude.<$> facets,
            ("PageNumber" Data..=) Prelude.<$> pageNumber,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("QueryResultTypeFilter" Data..=)
              Prelude.<$> queryResultTypeFilter,
            ("QueryText" Data..=) Prelude.<$> queryText,
            ("RequestedDocumentAttributes" Data..=)
              Prelude.<$> requestedDocumentAttributes,
            ("SortingConfiguration" Data..=)
              Prelude.<$> sortingConfiguration,
            ("SpellCorrectionConfiguration" Data..=)
              Prelude.<$> spellCorrectionConfiguration,
            ("UserContext" Data..=) Prelude.<$> userContext,
            ("VisitorId" Data..=) Prelude.<$> visitorId,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath Query where
  toPath = Prelude.const "/"

instance Data.ToQuery Query where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryResponse' smart constructor.
data QueryResponse = QueryResponse'
  { -- | Contains the facet results. A @FacetResult@ contains the counts for each
    -- field\/attribute key that was specified in the @Facets@ input parameter.
    facetResults :: Prelude.Maybe [FacetResult],
    -- | The list of featured result items. Featured results are displayed at the
    -- top of the search results page, placed above all other results for
    -- certain queries. If there\'s an exact match of a query, then certain
    -- documents are featured in the search results.
    featuredResultsItems :: Prelude.Maybe [FeaturedResultsItem],
    -- | The identifier for the search. You also use @QueryId@ to identify the
    -- search when using the
    -- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html SubmitFeedback>
    -- API.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The results of the search.
    resultItems :: Prelude.Maybe [QueryResultItem],
    -- | A list of information related to suggested spell corrections for a
    -- query.
    spellCorrectedQueries :: Prelude.Maybe [SpellCorrectedQuery],
    -- | The total number of items found by the search. However, you can only
    -- retrieve up to 100 items. For example, if the search found 192 items,
    -- you can only retrieve the first 100 of the items.
    totalNumberOfResults :: Prelude.Maybe Prelude.Int,
    -- | A list of warning codes and their messages on problems with your query.
    --
    -- Amazon Kendra currently only supports one type of warning, which is a
    -- warning on invalid syntax used in the query. For examples of invalid
    -- query syntax, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/searching-example.html#searching-index-query-syntax Searching with advanced query syntax>.
    warnings :: Prelude.Maybe (Prelude.NonEmpty Warning),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetResults', 'queryResponse_facetResults' - Contains the facet results. A @FacetResult@ contains the counts for each
-- field\/attribute key that was specified in the @Facets@ input parameter.
--
-- 'featuredResultsItems', 'queryResponse_featuredResultsItems' - The list of featured result items. Featured results are displayed at the
-- top of the search results page, placed above all other results for
-- certain queries. If there\'s an exact match of a query, then certain
-- documents are featured in the search results.
--
-- 'queryId', 'queryResponse_queryId' - The identifier for the search. You also use @QueryId@ to identify the
-- search when using the
-- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html SubmitFeedback>
-- API.
--
-- 'resultItems', 'queryResponse_resultItems' - The results of the search.
--
-- 'spellCorrectedQueries', 'queryResponse_spellCorrectedQueries' - A list of information related to suggested spell corrections for a
-- query.
--
-- 'totalNumberOfResults', 'queryResponse_totalNumberOfResults' - The total number of items found by the search. However, you can only
-- retrieve up to 100 items. For example, if the search found 192 items,
-- you can only retrieve the first 100 of the items.
--
-- 'warnings', 'queryResponse_warnings' - A list of warning codes and their messages on problems with your query.
--
-- Amazon Kendra currently only supports one type of warning, which is a
-- warning on invalid syntax used in the query. For examples of invalid
-- query syntax, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/searching-example.html#searching-index-query-syntax Searching with advanced query syntax>.
--
-- 'httpStatus', 'queryResponse_httpStatus' - The response's http status code.
newQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryResponse
newQueryResponse pHttpStatus_ =
  QueryResponse'
    { facetResults = Prelude.Nothing,
      featuredResultsItems = Prelude.Nothing,
      queryId = Prelude.Nothing,
      resultItems = Prelude.Nothing,
      spellCorrectedQueries = Prelude.Nothing,
      totalNumberOfResults = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the facet results. A @FacetResult@ contains the counts for each
-- field\/attribute key that was specified in the @Facets@ input parameter.
queryResponse_facetResults :: Lens.Lens' QueryResponse (Prelude.Maybe [FacetResult])
queryResponse_facetResults = Lens.lens (\QueryResponse' {facetResults} -> facetResults) (\s@QueryResponse' {} a -> s {facetResults = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of featured result items. Featured results are displayed at the
-- top of the search results page, placed above all other results for
-- certain queries. If there\'s an exact match of a query, then certain
-- documents are featured in the search results.
queryResponse_featuredResultsItems :: Lens.Lens' QueryResponse (Prelude.Maybe [FeaturedResultsItem])
queryResponse_featuredResultsItems = Lens.lens (\QueryResponse' {featuredResultsItems} -> featuredResultsItems) (\s@QueryResponse' {} a -> s {featuredResultsItems = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the search. You also use @QueryId@ to identify the
-- search when using the
-- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html SubmitFeedback>
-- API.
queryResponse_queryId :: Lens.Lens' QueryResponse (Prelude.Maybe Prelude.Text)
queryResponse_queryId = Lens.lens (\QueryResponse' {queryId} -> queryId) (\s@QueryResponse' {} a -> s {queryId = a} :: QueryResponse)

-- | The results of the search.
queryResponse_resultItems :: Lens.Lens' QueryResponse (Prelude.Maybe [QueryResultItem])
queryResponse_resultItems = Lens.lens (\QueryResponse' {resultItems} -> resultItems) (\s@QueryResponse' {} a -> s {resultItems = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of information related to suggested spell corrections for a
-- query.
queryResponse_spellCorrectedQueries :: Lens.Lens' QueryResponse (Prelude.Maybe [SpellCorrectedQuery])
queryResponse_spellCorrectedQueries = Lens.lens (\QueryResponse' {spellCorrectedQueries} -> spellCorrectedQueries) (\s@QueryResponse' {} a -> s {spellCorrectedQueries = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of items found by the search. However, you can only
-- retrieve up to 100 items. For example, if the search found 192 items,
-- you can only retrieve the first 100 of the items.
queryResponse_totalNumberOfResults :: Lens.Lens' QueryResponse (Prelude.Maybe Prelude.Int)
queryResponse_totalNumberOfResults = Lens.lens (\QueryResponse' {totalNumberOfResults} -> totalNumberOfResults) (\s@QueryResponse' {} a -> s {totalNumberOfResults = a} :: QueryResponse)

-- | A list of warning codes and their messages on problems with your query.
--
-- Amazon Kendra currently only supports one type of warning, which is a
-- warning on invalid syntax used in the query. For examples of invalid
-- query syntax, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/searching-example.html#searching-index-query-syntax Searching with advanced query syntax>.
queryResponse_warnings :: Lens.Lens' QueryResponse (Prelude.Maybe (Prelude.NonEmpty Warning))
queryResponse_warnings = Lens.lens (\QueryResponse' {warnings} -> warnings) (\s@QueryResponse' {} a -> s {warnings = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
queryResponse_httpStatus :: Lens.Lens' QueryResponse Prelude.Int
queryResponse_httpStatus = Lens.lens (\QueryResponse' {httpStatus} -> httpStatus) (\s@QueryResponse' {} a -> s {httpStatus = a} :: QueryResponse)

instance Prelude.NFData QueryResponse where
  rnf QueryResponse' {..} =
    Prelude.rnf facetResults
      `Prelude.seq` Prelude.rnf featuredResultsItems
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf resultItems
      `Prelude.seq` Prelude.rnf spellCorrectedQueries
      `Prelude.seq` Prelude.rnf totalNumberOfResults
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
