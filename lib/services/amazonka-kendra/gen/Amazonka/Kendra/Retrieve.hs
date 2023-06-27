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
-- Module      : Amazonka.Kendra.Retrieve
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves relevant passages or text excerpts given an input query.
--
-- This API is similar to the
-- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_Query.html Query>
-- API. However, by default, the @Query@ API only returns excerpt passages
-- of up to 100 token words. With the @Retrieve@ API, you can retrieve
-- longer passages of up to 200 token words and up to 100 semantically
-- relevant passages. This doesn\'t include question-answer or FAQ type
-- responses from your index. The passages are text excerpts that can be
-- semantically extracted from multiple documents and multiple parts of the
-- same document. If in extreme cases your documents produce no relevant
-- passages using the @Retrieve@ API, you can alternatively use the @Query@
-- API.
--
-- You can also do the following:
--
-- -   Override boosting at the index level
--
-- -   Filter based on document fields or attributes
--
-- -   Filter based on the user or their group access to documents
--
-- You can also include certain fields in the response that might provide
-- useful additional information.
module Amazonka.Kendra.Retrieve
  ( -- * Creating a Request
    Retrieve (..),
    newRetrieve,

    -- * Request Lenses
    retrieve_attributeFilter,
    retrieve_documentRelevanceOverrideConfigurations,
    retrieve_pageNumber,
    retrieve_pageSize,
    retrieve_requestedDocumentAttributes,
    retrieve_userContext,
    retrieve_indexId,
    retrieve_queryText,

    -- * Destructuring the Response
    RetrieveResponse (..),
    newRetrieveResponse,

    -- * Response Lenses
    retrieveResponse_queryId,
    retrieveResponse_resultItems,
    retrieveResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRetrieve' smart constructor.
data Retrieve = Retrieve'
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
    -- | Retrieved relevant passages are returned in pages the size of the
    -- @PageSize@ parameter. By default, Amazon Kendra returns the first page
    -- of results. Use this parameter to get result pages after the first one.
    pageNumber :: Prelude.Maybe Prelude.Int,
    -- | Sets the number of retrieved relevant passages that are returned in each
    -- page of results. The default page size is 10. The maximum number of
    -- results returned is 100. If you ask for more than 100 results, only 100
    -- are returned.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | A list of document fields\/attributes to include in the response. You
    -- can limit the response to include certain document fields. By default,
    -- all document fields are included in the response.
    requestedDocumentAttributes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The user context token or user and group information.
    userContext :: Prelude.Maybe UserContext,
    -- | The identifier of the index to retrieve relevant passages for the
    -- search.
    indexId :: Prelude.Text,
    -- | The input query text to retrieve relevant passages for the search.
    -- Amazon Kendra truncates queries at 30 token words, which excludes
    -- punctuation and stop words. Truncation still applies if you use Boolean
    -- or more advanced, complex queries.
    queryText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Retrieve' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeFilter', 'retrieve_attributeFilter' - Filters search results by document fields\/attributes. You can only
-- provide one attribute filter; however, the @AndAllFilters@, @NotFilter@,
-- and @OrAllFilters@ parameters contain a list of other filters.
--
-- The @AttributeFilter@ parameter means you can create a set of filtering
-- rules that a document must satisfy to be included in the query results.
--
-- 'documentRelevanceOverrideConfigurations', 'retrieve_documentRelevanceOverrideConfigurations' - Overrides relevance tuning configurations of fields\/attributes set at
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
-- 'pageNumber', 'retrieve_pageNumber' - Retrieved relevant passages are returned in pages the size of the
-- @PageSize@ parameter. By default, Amazon Kendra returns the first page
-- of results. Use this parameter to get result pages after the first one.
--
-- 'pageSize', 'retrieve_pageSize' - Sets the number of retrieved relevant passages that are returned in each
-- page of results. The default page size is 10. The maximum number of
-- results returned is 100. If you ask for more than 100 results, only 100
-- are returned.
--
-- 'requestedDocumentAttributes', 'retrieve_requestedDocumentAttributes' - A list of document fields\/attributes to include in the response. You
-- can limit the response to include certain document fields. By default,
-- all document fields are included in the response.
--
-- 'userContext', 'retrieve_userContext' - The user context token or user and group information.
--
-- 'indexId', 'retrieve_indexId' - The identifier of the index to retrieve relevant passages for the
-- search.
--
-- 'queryText', 'retrieve_queryText' - The input query text to retrieve relevant passages for the search.
-- Amazon Kendra truncates queries at 30 token words, which excludes
-- punctuation and stop words. Truncation still applies if you use Boolean
-- or more advanced, complex queries.
newRetrieve ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'queryText'
  Prelude.Text ->
  Retrieve
newRetrieve pIndexId_ pQueryText_ =
  Retrieve'
    { attributeFilter = Prelude.Nothing,
      documentRelevanceOverrideConfigurations =
        Prelude.Nothing,
      pageNumber = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      requestedDocumentAttributes = Prelude.Nothing,
      userContext = Prelude.Nothing,
      indexId = pIndexId_,
      queryText = pQueryText_
    }

-- | Filters search results by document fields\/attributes. You can only
-- provide one attribute filter; however, the @AndAllFilters@, @NotFilter@,
-- and @OrAllFilters@ parameters contain a list of other filters.
--
-- The @AttributeFilter@ parameter means you can create a set of filtering
-- rules that a document must satisfy to be included in the query results.
retrieve_attributeFilter :: Lens.Lens' Retrieve (Prelude.Maybe AttributeFilter)
retrieve_attributeFilter = Lens.lens (\Retrieve' {attributeFilter} -> attributeFilter) (\s@Retrieve' {} a -> s {attributeFilter = a} :: Retrieve)

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
retrieve_documentRelevanceOverrideConfigurations :: Lens.Lens' Retrieve (Prelude.Maybe [DocumentRelevanceConfiguration])
retrieve_documentRelevanceOverrideConfigurations = Lens.lens (\Retrieve' {documentRelevanceOverrideConfigurations} -> documentRelevanceOverrideConfigurations) (\s@Retrieve' {} a -> s {documentRelevanceOverrideConfigurations = a} :: Retrieve) Prelude.. Lens.mapping Lens.coerced

-- | Retrieved relevant passages are returned in pages the size of the
-- @PageSize@ parameter. By default, Amazon Kendra returns the first page
-- of results. Use this parameter to get result pages after the first one.
retrieve_pageNumber :: Lens.Lens' Retrieve (Prelude.Maybe Prelude.Int)
retrieve_pageNumber = Lens.lens (\Retrieve' {pageNumber} -> pageNumber) (\s@Retrieve' {} a -> s {pageNumber = a} :: Retrieve)

-- | Sets the number of retrieved relevant passages that are returned in each
-- page of results. The default page size is 10. The maximum number of
-- results returned is 100. If you ask for more than 100 results, only 100
-- are returned.
retrieve_pageSize :: Lens.Lens' Retrieve (Prelude.Maybe Prelude.Int)
retrieve_pageSize = Lens.lens (\Retrieve' {pageSize} -> pageSize) (\s@Retrieve' {} a -> s {pageSize = a} :: Retrieve)

-- | A list of document fields\/attributes to include in the response. You
-- can limit the response to include certain document fields. By default,
-- all document fields are included in the response.
retrieve_requestedDocumentAttributes :: Lens.Lens' Retrieve (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
retrieve_requestedDocumentAttributes = Lens.lens (\Retrieve' {requestedDocumentAttributes} -> requestedDocumentAttributes) (\s@Retrieve' {} a -> s {requestedDocumentAttributes = a} :: Retrieve) Prelude.. Lens.mapping Lens.coerced

-- | The user context token or user and group information.
retrieve_userContext :: Lens.Lens' Retrieve (Prelude.Maybe UserContext)
retrieve_userContext = Lens.lens (\Retrieve' {userContext} -> userContext) (\s@Retrieve' {} a -> s {userContext = a} :: Retrieve)

-- | The identifier of the index to retrieve relevant passages for the
-- search.
retrieve_indexId :: Lens.Lens' Retrieve Prelude.Text
retrieve_indexId = Lens.lens (\Retrieve' {indexId} -> indexId) (\s@Retrieve' {} a -> s {indexId = a} :: Retrieve)

-- | The input query text to retrieve relevant passages for the search.
-- Amazon Kendra truncates queries at 30 token words, which excludes
-- punctuation and stop words. Truncation still applies if you use Boolean
-- or more advanced, complex queries.
retrieve_queryText :: Lens.Lens' Retrieve Prelude.Text
retrieve_queryText = Lens.lens (\Retrieve' {queryText} -> queryText) (\s@Retrieve' {} a -> s {queryText = a} :: Retrieve)

instance Core.AWSRequest Retrieve where
  type AWSResponse Retrieve = RetrieveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveResponse'
            Prelude.<$> (x Data..?> "QueryId")
            Prelude.<*> (x Data..?> "ResultItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Retrieve where
  hashWithSalt _salt Retrieve' {..} =
    _salt
      `Prelude.hashWithSalt` attributeFilter
      `Prelude.hashWithSalt` documentRelevanceOverrideConfigurations
      `Prelude.hashWithSalt` pageNumber
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` requestedDocumentAttributes
      `Prelude.hashWithSalt` userContext
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` queryText

instance Prelude.NFData Retrieve where
  rnf Retrieve' {..} =
    Prelude.rnf attributeFilter
      `Prelude.seq` Prelude.rnf documentRelevanceOverrideConfigurations
      `Prelude.seq` Prelude.rnf pageNumber
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf requestedDocumentAttributes
      `Prelude.seq` Prelude.rnf userContext
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf queryText

instance Data.ToHeaders Retrieve where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.Retrieve" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Retrieve where
  toJSON Retrieve' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeFilter" Data..=)
              Prelude.<$> attributeFilter,
            ("DocumentRelevanceOverrideConfigurations" Data..=)
              Prelude.<$> documentRelevanceOverrideConfigurations,
            ("PageNumber" Data..=) Prelude.<$> pageNumber,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("RequestedDocumentAttributes" Data..=)
              Prelude.<$> requestedDocumentAttributes,
            ("UserContext" Data..=) Prelude.<$> userContext,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("QueryText" Data..= queryText)
          ]
      )

instance Data.ToPath Retrieve where
  toPath = Prelude.const "/"

instance Data.ToQuery Retrieve where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetrieveResponse' smart constructor.
data RetrieveResponse = RetrieveResponse'
  { -- | The identifier of query used for the search. You also use @QueryId@ to
    -- identify the search when using the
    -- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html Submitfeedback>
    -- API.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The results of the retrieved relevant passages for the search.
    resultItems :: Prelude.Maybe [RetrieveResultItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetrieveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'retrieveResponse_queryId' - The identifier of query used for the search. You also use @QueryId@ to
-- identify the search when using the
-- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html Submitfeedback>
-- API.
--
-- 'resultItems', 'retrieveResponse_resultItems' - The results of the retrieved relevant passages for the search.
--
-- 'httpStatus', 'retrieveResponse_httpStatus' - The response's http status code.
newRetrieveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetrieveResponse
newRetrieveResponse pHttpStatus_ =
  RetrieveResponse'
    { queryId = Prelude.Nothing,
      resultItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of query used for the search. You also use @QueryId@ to
-- identify the search when using the
-- <https://docs.aws.amazon.com/kendra/latest/APIReference/API_SubmitFeedback.html Submitfeedback>
-- API.
retrieveResponse_queryId :: Lens.Lens' RetrieveResponse (Prelude.Maybe Prelude.Text)
retrieveResponse_queryId = Lens.lens (\RetrieveResponse' {queryId} -> queryId) (\s@RetrieveResponse' {} a -> s {queryId = a} :: RetrieveResponse)

-- | The results of the retrieved relevant passages for the search.
retrieveResponse_resultItems :: Lens.Lens' RetrieveResponse (Prelude.Maybe [RetrieveResultItem])
retrieveResponse_resultItems = Lens.lens (\RetrieveResponse' {resultItems} -> resultItems) (\s@RetrieveResponse' {} a -> s {resultItems = a} :: RetrieveResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
retrieveResponse_httpStatus :: Lens.Lens' RetrieveResponse Prelude.Int
retrieveResponse_httpStatus = Lens.lens (\RetrieveResponse' {httpStatus} -> httpStatus) (\s@RetrieveResponse' {} a -> s {httpStatus = a} :: RetrieveResponse)

instance Prelude.NFData RetrieveResponse where
  rnf RetrieveResponse' {..} =
    Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf resultItems
      `Prelude.seq` Prelude.rnf httpStatus
