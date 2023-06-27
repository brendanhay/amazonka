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
-- Module      : Amazonka.KendraRanking.Rescore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rescores or re-ranks search results from a search service such as
-- OpenSearch (self managed). You use the semantic search capabilities of
-- Amazon Kendra Intelligent Ranking to improve the search service\'s
-- results.
module Amazonka.KendraRanking.Rescore
  ( -- * Creating a Request
    Rescore (..),
    newRescore,

    -- * Request Lenses
    rescore_rescoreExecutionPlanId,
    rescore_searchQuery,
    rescore_documents,

    -- * Destructuring the Response
    RescoreResponse (..),
    newRescoreResponse,

    -- * Response Lenses
    rescoreResponse_rescoreId,
    rescoreResponse_resultItems,
    rescoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRescore' smart constructor.
data Rescore = Rescore'
  { -- | The identifier of the rescore execution plan. A rescore execution plan
    -- is an Amazon Kendra Intelligent Ranking resource used for provisioning
    -- the @Rescore@ API.
    rescoreExecutionPlanId :: Prelude.Text,
    -- | The input query from the search service.
    searchQuery :: Prelude.Text,
    -- | The list of documents for Amazon Kendra Intelligent Ranking to rescore
    -- or rank on.
    documents :: Prelude.NonEmpty Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rescore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescoreExecutionPlanId', 'rescore_rescoreExecutionPlanId' - The identifier of the rescore execution plan. A rescore execution plan
-- is an Amazon Kendra Intelligent Ranking resource used for provisioning
-- the @Rescore@ API.
--
-- 'searchQuery', 'rescore_searchQuery' - The input query from the search service.
--
-- 'documents', 'rescore_documents' - The list of documents for Amazon Kendra Intelligent Ranking to rescore
-- or rank on.
newRescore ::
  -- | 'rescoreExecutionPlanId'
  Prelude.Text ->
  -- | 'searchQuery'
  Prelude.Text ->
  -- | 'documents'
  Prelude.NonEmpty Document ->
  Rescore
newRescore
  pRescoreExecutionPlanId_
  pSearchQuery_
  pDocuments_ =
    Rescore'
      { rescoreExecutionPlanId =
          pRescoreExecutionPlanId_,
        searchQuery = pSearchQuery_,
        documents = Lens.coerced Lens.# pDocuments_
      }

-- | The identifier of the rescore execution plan. A rescore execution plan
-- is an Amazon Kendra Intelligent Ranking resource used for provisioning
-- the @Rescore@ API.
rescore_rescoreExecutionPlanId :: Lens.Lens' Rescore Prelude.Text
rescore_rescoreExecutionPlanId = Lens.lens (\Rescore' {rescoreExecutionPlanId} -> rescoreExecutionPlanId) (\s@Rescore' {} a -> s {rescoreExecutionPlanId = a} :: Rescore)

-- | The input query from the search service.
rescore_searchQuery :: Lens.Lens' Rescore Prelude.Text
rescore_searchQuery = Lens.lens (\Rescore' {searchQuery} -> searchQuery) (\s@Rescore' {} a -> s {searchQuery = a} :: Rescore)

-- | The list of documents for Amazon Kendra Intelligent Ranking to rescore
-- or rank on.
rescore_documents :: Lens.Lens' Rescore (Prelude.NonEmpty Document)
rescore_documents = Lens.lens (\Rescore' {documents} -> documents) (\s@Rescore' {} a -> s {documents = a} :: Rescore) Prelude.. Lens.coerced

instance Core.AWSRequest Rescore where
  type AWSResponse Rescore = RescoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RescoreResponse'
            Prelude.<$> (x Data..?> "RescoreId")
            Prelude.<*> (x Data..?> "ResultItems")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Rescore where
  hashWithSalt _salt Rescore' {..} =
    _salt
      `Prelude.hashWithSalt` rescoreExecutionPlanId
      `Prelude.hashWithSalt` searchQuery
      `Prelude.hashWithSalt` documents

instance Prelude.NFData Rescore where
  rnf Rescore' {..} =
    Prelude.rnf rescoreExecutionPlanId
      `Prelude.seq` Prelude.rnf searchQuery
      `Prelude.seq` Prelude.rnf documents

instance Data.ToHeaders Rescore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.Rescore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Rescore where
  toJSON Rescore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RescoreExecutionPlanId"
                  Data..= rescoreExecutionPlanId
              ),
            Prelude.Just ("SearchQuery" Data..= searchQuery),
            Prelude.Just ("Documents" Data..= documents)
          ]
      )

instance Data.ToPath Rescore where
  toPath = Prelude.const "/"

instance Data.ToQuery Rescore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRescoreResponse' smart constructor.
data RescoreResponse = RescoreResponse'
  { -- | The identifier associated with the scores that Amazon Kendra Intelligent
    -- Ranking gives to the results. Amazon Kendra Intelligent Ranking rescores
    -- or re-ranks the results for the search service.
    rescoreId :: Prelude.Maybe Prelude.Text,
    -- | A list of result items for documents with new relevancy scores. The
    -- results are in descending order.
    resultItems :: Prelude.Maybe (Prelude.NonEmpty RescoreResultItem),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RescoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescoreId', 'rescoreResponse_rescoreId' - The identifier associated with the scores that Amazon Kendra Intelligent
-- Ranking gives to the results. Amazon Kendra Intelligent Ranking rescores
-- or re-ranks the results for the search service.
--
-- 'resultItems', 'rescoreResponse_resultItems' - A list of result items for documents with new relevancy scores. The
-- results are in descending order.
--
-- 'httpStatus', 'rescoreResponse_httpStatus' - The response's http status code.
newRescoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RescoreResponse
newRescoreResponse pHttpStatus_ =
  RescoreResponse'
    { rescoreId = Prelude.Nothing,
      resultItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier associated with the scores that Amazon Kendra Intelligent
-- Ranking gives to the results. Amazon Kendra Intelligent Ranking rescores
-- or re-ranks the results for the search service.
rescoreResponse_rescoreId :: Lens.Lens' RescoreResponse (Prelude.Maybe Prelude.Text)
rescoreResponse_rescoreId = Lens.lens (\RescoreResponse' {rescoreId} -> rescoreId) (\s@RescoreResponse' {} a -> s {rescoreId = a} :: RescoreResponse)

-- | A list of result items for documents with new relevancy scores. The
-- results are in descending order.
rescoreResponse_resultItems :: Lens.Lens' RescoreResponse (Prelude.Maybe (Prelude.NonEmpty RescoreResultItem))
rescoreResponse_resultItems = Lens.lens (\RescoreResponse' {resultItems} -> resultItems) (\s@RescoreResponse' {} a -> s {resultItems = a} :: RescoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rescoreResponse_httpStatus :: Lens.Lens' RescoreResponse Prelude.Int
rescoreResponse_httpStatus = Lens.lens (\RescoreResponse' {httpStatus} -> httpStatus) (\s@RescoreResponse' {} a -> s {httpStatus = a} :: RescoreResponse)

instance Prelude.NFData RescoreResponse where
  rnf RescoreResponse' {..} =
    Prelude.rnf rescoreId
      `Prelude.seq` Prelude.rnf resultItems
      `Prelude.seq` Prelude.rnf httpStatus
