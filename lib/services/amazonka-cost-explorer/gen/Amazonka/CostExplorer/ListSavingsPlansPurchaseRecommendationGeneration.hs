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
-- Module      : Amazonka.CostExplorer.ListSavingsPlansPurchaseRecommendationGeneration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of your historical recommendation generations within
-- the past 30 days.
module Amazonka.CostExplorer.ListSavingsPlansPurchaseRecommendationGeneration
  ( -- * Creating a Request
    ListSavingsPlansPurchaseRecommendationGeneration (..),
    newListSavingsPlansPurchaseRecommendationGeneration,

    -- * Request Lenses
    listSavingsPlansPurchaseRecommendationGeneration_generationStatus,
    listSavingsPlansPurchaseRecommendationGeneration_nextPageToken,
    listSavingsPlansPurchaseRecommendationGeneration_pageSize,
    listSavingsPlansPurchaseRecommendationGeneration_recommendationIds,

    -- * Destructuring the Response
    ListSavingsPlansPurchaseRecommendationGenerationResponse (..),
    newListSavingsPlansPurchaseRecommendationGenerationResponse,

    -- * Response Lenses
    listSavingsPlansPurchaseRecommendationGenerationResponse_generationSummaryList,
    listSavingsPlansPurchaseRecommendationGenerationResponse_nextPageToken,
    listSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSavingsPlansPurchaseRecommendationGeneration' smart constructor.
data ListSavingsPlansPurchaseRecommendationGeneration = ListSavingsPlansPurchaseRecommendationGeneration'
  { -- | The status of the recommendation generation.
    generationStatus :: Prelude.Maybe GenerationStatus,
    -- | The token to retrieve the next set of results.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The IDs for each specific recommendation.
    recommendationIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSavingsPlansPurchaseRecommendationGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generationStatus', 'listSavingsPlansPurchaseRecommendationGeneration_generationStatus' - The status of the recommendation generation.
--
-- 'nextPageToken', 'listSavingsPlansPurchaseRecommendationGeneration_nextPageToken' - The token to retrieve the next set of results.
--
-- 'pageSize', 'listSavingsPlansPurchaseRecommendationGeneration_pageSize' - The number of recommendations that you want returned in a single
-- response object.
--
-- 'recommendationIds', 'listSavingsPlansPurchaseRecommendationGeneration_recommendationIds' - The IDs for each specific recommendation.
newListSavingsPlansPurchaseRecommendationGeneration ::
  ListSavingsPlansPurchaseRecommendationGeneration
newListSavingsPlansPurchaseRecommendationGeneration =
  ListSavingsPlansPurchaseRecommendationGeneration'
    { generationStatus =
        Prelude.Nothing,
      nextPageToken =
        Prelude.Nothing,
      pageSize =
        Prelude.Nothing,
      recommendationIds =
        Prelude.Nothing
    }

-- | The status of the recommendation generation.
listSavingsPlansPurchaseRecommendationGeneration_generationStatus :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGeneration (Prelude.Maybe GenerationStatus)
listSavingsPlansPurchaseRecommendationGeneration_generationStatus = Lens.lens (\ListSavingsPlansPurchaseRecommendationGeneration' {generationStatus} -> generationStatus) (\s@ListSavingsPlansPurchaseRecommendationGeneration' {} a -> s {generationStatus = a} :: ListSavingsPlansPurchaseRecommendationGeneration)

-- | The token to retrieve the next set of results.
listSavingsPlansPurchaseRecommendationGeneration_nextPageToken :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGeneration (Prelude.Maybe Prelude.Text)
listSavingsPlansPurchaseRecommendationGeneration_nextPageToken = Lens.lens (\ListSavingsPlansPurchaseRecommendationGeneration' {nextPageToken} -> nextPageToken) (\s@ListSavingsPlansPurchaseRecommendationGeneration' {} a -> s {nextPageToken = a} :: ListSavingsPlansPurchaseRecommendationGeneration)

-- | The number of recommendations that you want returned in a single
-- response object.
listSavingsPlansPurchaseRecommendationGeneration_pageSize :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGeneration (Prelude.Maybe Prelude.Natural)
listSavingsPlansPurchaseRecommendationGeneration_pageSize = Lens.lens (\ListSavingsPlansPurchaseRecommendationGeneration' {pageSize} -> pageSize) (\s@ListSavingsPlansPurchaseRecommendationGeneration' {} a -> s {pageSize = a} :: ListSavingsPlansPurchaseRecommendationGeneration)

-- | The IDs for each specific recommendation.
listSavingsPlansPurchaseRecommendationGeneration_recommendationIds :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGeneration (Prelude.Maybe [Prelude.Text])
listSavingsPlansPurchaseRecommendationGeneration_recommendationIds = Lens.lens (\ListSavingsPlansPurchaseRecommendationGeneration' {recommendationIds} -> recommendationIds) (\s@ListSavingsPlansPurchaseRecommendationGeneration' {} a -> s {recommendationIds = a} :: ListSavingsPlansPurchaseRecommendationGeneration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  type
    AWSResponse
      ListSavingsPlansPurchaseRecommendationGeneration =
      ListSavingsPlansPurchaseRecommendationGenerationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSavingsPlansPurchaseRecommendationGenerationResponse'
            Prelude.<$> ( x
                            Data..?> "GenerationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  hashWithSalt
    _salt
    ListSavingsPlansPurchaseRecommendationGeneration' {..} =
      _salt
        `Prelude.hashWithSalt` generationStatus
        `Prelude.hashWithSalt` nextPageToken
        `Prelude.hashWithSalt` pageSize
        `Prelude.hashWithSalt` recommendationIds

instance
  Prelude.NFData
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  rnf
    ListSavingsPlansPurchaseRecommendationGeneration' {..} =
      Prelude.rnf generationStatus `Prelude.seq`
        Prelude.rnf nextPageToken `Prelude.seq`
          Prelude.rnf pageSize `Prelude.seq`
            Prelude.rnf recommendationIds

instance
  Data.ToHeaders
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.ListSavingsPlansPurchaseRecommendationGeneration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  toJSON
    ListSavingsPlansPurchaseRecommendationGeneration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("GenerationStatus" Data..=)
                Prelude.<$> generationStatus,
              ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
              ("PageSize" Data..=) Prelude.<$> pageSize,
              ("RecommendationIds" Data..=)
                Prelude.<$> recommendationIds
            ]
        )

instance
  Data.ToPath
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListSavingsPlansPurchaseRecommendationGeneration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSavingsPlansPurchaseRecommendationGenerationResponse' smart constructor.
data ListSavingsPlansPurchaseRecommendationGenerationResponse = ListSavingsPlansPurchaseRecommendationGenerationResponse'
  { -- | The list of historical recommendation generations.
    generationSummaryList :: Prelude.Maybe [GenerationSummary],
    -- | The token to retrieve the next set of results.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSavingsPlansPurchaseRecommendationGenerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generationSummaryList', 'listSavingsPlansPurchaseRecommendationGenerationResponse_generationSummaryList' - The list of historical recommendation generations.
--
-- 'nextPageToken', 'listSavingsPlansPurchaseRecommendationGenerationResponse_nextPageToken' - The token to retrieve the next set of results.
--
-- 'httpStatus', 'listSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus' - The response's http status code.
newListSavingsPlansPurchaseRecommendationGenerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSavingsPlansPurchaseRecommendationGenerationResponse
newListSavingsPlansPurchaseRecommendationGenerationResponse
  pHttpStatus_ =
    ListSavingsPlansPurchaseRecommendationGenerationResponse'
      { generationSummaryList =
          Prelude.Nothing,
        nextPageToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The list of historical recommendation generations.
listSavingsPlansPurchaseRecommendationGenerationResponse_generationSummaryList :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGenerationResponse (Prelude.Maybe [GenerationSummary])
listSavingsPlansPurchaseRecommendationGenerationResponse_generationSummaryList = Lens.lens (\ListSavingsPlansPurchaseRecommendationGenerationResponse' {generationSummaryList} -> generationSummaryList) (\s@ListSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {generationSummaryList = a} :: ListSavingsPlansPurchaseRecommendationGenerationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results.
listSavingsPlansPurchaseRecommendationGenerationResponse_nextPageToken :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGenerationResponse (Prelude.Maybe Prelude.Text)
listSavingsPlansPurchaseRecommendationGenerationResponse_nextPageToken = Lens.lens (\ListSavingsPlansPurchaseRecommendationGenerationResponse' {nextPageToken} -> nextPageToken) (\s@ListSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {nextPageToken = a} :: ListSavingsPlansPurchaseRecommendationGenerationResponse)

-- | The response's http status code.
listSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus :: Lens.Lens' ListSavingsPlansPurchaseRecommendationGenerationResponse Prelude.Int
listSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus = Lens.lens (\ListSavingsPlansPurchaseRecommendationGenerationResponse' {httpStatus} -> httpStatus) (\s@ListSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {httpStatus = a} :: ListSavingsPlansPurchaseRecommendationGenerationResponse)

instance
  Prelude.NFData
    ListSavingsPlansPurchaseRecommendationGenerationResponse
  where
  rnf
    ListSavingsPlansPurchaseRecommendationGenerationResponse' {..} =
      Prelude.rnf generationSummaryList `Prelude.seq`
        Prelude.rnf nextPageToken `Prelude.seq`
          Prelude.rnf httpStatus
