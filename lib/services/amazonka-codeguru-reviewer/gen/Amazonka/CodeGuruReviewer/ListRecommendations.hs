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
-- Module      : Amazonka.CodeGuruReviewer.ListRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of all recommendations for a completed code review.
module Amazonka.CodeGuruReviewer.ListRecommendations
  ( -- * Creating a Request
    ListRecommendations (..),
    newListRecommendations,

    -- * Request Lenses
    listRecommendations_maxResults,
    listRecommendations_nextToken,
    listRecommendations_codeReviewArn,

    -- * Destructuring the Response
    ListRecommendationsResponse (..),
    newListRecommendationsResponse,

    -- * Response Lenses
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendationSummaries,
    listRecommendationsResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommendations' smart constructor.
data ListRecommendations = ListRecommendations'
  { -- | The maximum number of results that are returned per call. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecommendations_maxResults' - The maximum number of results that are returned per call. The default is
-- 100.
--
-- 'nextToken', 'listRecommendations_nextToken' - Pagination token.
--
-- 'codeReviewArn', 'listRecommendations_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
newListRecommendations ::
  -- | 'codeReviewArn'
  Prelude.Text ->
  ListRecommendations
newListRecommendations pCodeReviewArn_ =
  ListRecommendations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      codeReviewArn = pCodeReviewArn_
    }

-- | The maximum number of results that are returned per call. The default is
-- 100.
listRecommendations_maxResults :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Natural)
listRecommendations_maxResults = Lens.lens (\ListRecommendations' {maxResults} -> maxResults) (\s@ListRecommendations' {} a -> s {maxResults = a} :: ListRecommendations)

-- | Pagination token.
listRecommendations_nextToken :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Text)
listRecommendations_nextToken = Lens.lens (\ListRecommendations' {nextToken} -> nextToken) (\s@ListRecommendations' {} a -> s {nextToken = a} :: ListRecommendations)

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
listRecommendations_codeReviewArn :: Lens.Lens' ListRecommendations Prelude.Text
listRecommendations_codeReviewArn = Lens.lens (\ListRecommendations' {codeReviewArn} -> codeReviewArn) (\s@ListRecommendations' {} a -> s {codeReviewArn = a} :: ListRecommendations)

instance Core.AWSRequest ListRecommendations where
  type
    AWSResponse ListRecommendations =
      ListRecommendationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RecommendationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommendations where
  hashWithSalt _salt ListRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` codeReviewArn

instance Prelude.NFData ListRecommendations where
  rnf ListRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf codeReviewArn

instance Data.ToHeaders ListRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecommendations where
  toPath ListRecommendations' {..} =
    Prelude.mconcat
      [ "/codereviews/",
        Data.toBS codeReviewArn,
        "/Recommendations"
      ]

instance Data.ToQuery ListRecommendations where
  toQuery ListRecommendations' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRecommendationsResponse' smart constructor.
data ListRecommendationsResponse = ListRecommendationsResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of recommendations for the requested code review.
    recommendationSummaries :: Prelude.Maybe [RecommendationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommendationsResponse_nextToken' - Pagination token.
--
-- 'recommendationSummaries', 'listRecommendationsResponse_recommendationSummaries' - List of recommendations for the requested code review.
--
-- 'httpStatus', 'listRecommendationsResponse_httpStatus' - The response's http status code.
newListRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecommendationsResponse
newListRecommendationsResponse pHttpStatus_ =
  ListRecommendationsResponse'
    { nextToken =
        Prelude.Nothing,
      recommendationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listRecommendationsResponse_nextToken :: Lens.Lens' ListRecommendationsResponse (Prelude.Maybe Prelude.Text)
listRecommendationsResponse_nextToken = Lens.lens (\ListRecommendationsResponse' {nextToken} -> nextToken) (\s@ListRecommendationsResponse' {} a -> s {nextToken = a} :: ListRecommendationsResponse)

-- | List of recommendations for the requested code review.
listRecommendationsResponse_recommendationSummaries :: Lens.Lens' ListRecommendationsResponse (Prelude.Maybe [RecommendationSummary])
listRecommendationsResponse_recommendationSummaries = Lens.lens (\ListRecommendationsResponse' {recommendationSummaries} -> recommendationSummaries) (\s@ListRecommendationsResponse' {} a -> s {recommendationSummaries = a} :: ListRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendationsResponse_httpStatus :: Lens.Lens' ListRecommendationsResponse Prelude.Int
listRecommendationsResponse_httpStatus = Lens.lens (\ListRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListRecommendationsResponse' {} a -> s {httpStatus = a} :: ListRecommendationsResponse)

instance Prelude.NFData ListRecommendationsResponse where
  rnf ListRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
