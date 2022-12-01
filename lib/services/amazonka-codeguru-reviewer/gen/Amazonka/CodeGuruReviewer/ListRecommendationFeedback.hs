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
-- Module      : Amazonka.CodeGuruReviewer.ListRecommendationFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RecommendationFeedbackSummary.html RecommendationFeedbackSummary>
-- objects that contain customer recommendation feedback for all CodeGuru
-- Reviewer users.
module Amazonka.CodeGuruReviewer.ListRecommendationFeedback
  ( -- * Creating a Request
    ListRecommendationFeedback (..),
    newListRecommendationFeedback,

    -- * Request Lenses
    listRecommendationFeedback_nextToken,
    listRecommendationFeedback_recommendationIds,
    listRecommendationFeedback_maxResults,
    listRecommendationFeedback_userIds,
    listRecommendationFeedback_codeReviewArn,

    -- * Destructuring the Response
    ListRecommendationFeedbackResponse (..),
    newListRecommendationFeedbackResponse,

    -- * Response Lenses
    listRecommendationFeedbackResponse_nextToken,
    listRecommendationFeedbackResponse_recommendationFeedbackSummaries,
    listRecommendationFeedbackResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommendationFeedback' smart constructor.
data ListRecommendationFeedback = ListRecommendationFeedback'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Used to query the recommendation feedback for a given recommendation.
    recommendationIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of results that are returned per call. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An Amazon Web Services user\'s account ID or Amazon Resource Name (ARN).
    -- Use this ID to query the recommendation feedback for a code review from
    -- that user.
    --
    -- The @UserId@ is an IAM principal that can be specified as an Amazon Web
    -- Services account ID or an Amazon Resource Name (ARN). For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    userIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendationFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommendationFeedback_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'recommendationIds', 'listRecommendationFeedback_recommendationIds' - Used to query the recommendation feedback for a given recommendation.
--
-- 'maxResults', 'listRecommendationFeedback_maxResults' - The maximum number of results that are returned per call. The default is
-- 100.
--
-- 'userIds', 'listRecommendationFeedback_userIds' - An Amazon Web Services user\'s account ID or Amazon Resource Name (ARN).
-- Use this ID to query the recommendation feedback for a code review from
-- that user.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- 'codeReviewArn', 'listRecommendationFeedback_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
newListRecommendationFeedback ::
  -- | 'codeReviewArn'
  Prelude.Text ->
  ListRecommendationFeedback
newListRecommendationFeedback pCodeReviewArn_ =
  ListRecommendationFeedback'
    { nextToken =
        Prelude.Nothing,
      recommendationIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userIds = Prelude.Nothing,
      codeReviewArn = pCodeReviewArn_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
listRecommendationFeedback_nextToken :: Lens.Lens' ListRecommendationFeedback (Prelude.Maybe Prelude.Text)
listRecommendationFeedback_nextToken = Lens.lens (\ListRecommendationFeedback' {nextToken} -> nextToken) (\s@ListRecommendationFeedback' {} a -> s {nextToken = a} :: ListRecommendationFeedback)

-- | Used to query the recommendation feedback for a given recommendation.
listRecommendationFeedback_recommendationIds :: Lens.Lens' ListRecommendationFeedback (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listRecommendationFeedback_recommendationIds = Lens.lens (\ListRecommendationFeedback' {recommendationIds} -> recommendationIds) (\s@ListRecommendationFeedback' {} a -> s {recommendationIds = a} :: ListRecommendationFeedback) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results that are returned per call. The default is
-- 100.
listRecommendationFeedback_maxResults :: Lens.Lens' ListRecommendationFeedback (Prelude.Maybe Prelude.Natural)
listRecommendationFeedback_maxResults = Lens.lens (\ListRecommendationFeedback' {maxResults} -> maxResults) (\s@ListRecommendationFeedback' {} a -> s {maxResults = a} :: ListRecommendationFeedback)

-- | An Amazon Web Services user\'s account ID or Amazon Resource Name (ARN).
-- Use this ID to query the recommendation feedback for a code review from
-- that user.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
listRecommendationFeedback_userIds :: Lens.Lens' ListRecommendationFeedback (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listRecommendationFeedback_userIds = Lens.lens (\ListRecommendationFeedback' {userIds} -> userIds) (\s@ListRecommendationFeedback' {} a -> s {userIds = a} :: ListRecommendationFeedback) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
listRecommendationFeedback_codeReviewArn :: Lens.Lens' ListRecommendationFeedback Prelude.Text
listRecommendationFeedback_codeReviewArn = Lens.lens (\ListRecommendationFeedback' {codeReviewArn} -> codeReviewArn) (\s@ListRecommendationFeedback' {} a -> s {codeReviewArn = a} :: ListRecommendationFeedback)

instance Core.AWSRequest ListRecommendationFeedback where
  type
    AWSResponse ListRecommendationFeedback =
      ListRecommendationFeedbackResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendationFeedbackResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "RecommendationFeedbackSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommendationFeedback where
  hashWithSalt _salt ListRecommendationFeedback' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recommendationIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` codeReviewArn

instance Prelude.NFData ListRecommendationFeedback where
  rnf ListRecommendationFeedback' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf codeReviewArn

instance Core.ToHeaders ListRecommendationFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRecommendationFeedback where
  toPath ListRecommendationFeedback' {..} =
    Prelude.mconcat
      [ "/feedback/",
        Core.toBS codeReviewArn,
        "/RecommendationFeedback"
      ]

instance Core.ToQuery ListRecommendationFeedback where
  toQuery ListRecommendationFeedback' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "RecommendationIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> recommendationIds
            ),
        "MaxResults" Core.=: maxResults,
        "UserIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> userIds)
      ]

-- | /See:/ 'newListRecommendationFeedbackResponse' smart constructor.
data ListRecommendationFeedbackResponse = ListRecommendationFeedbackResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Recommendation feedback summaries corresponding to the code review ARN.
    recommendationFeedbackSummaries :: Prelude.Maybe [RecommendationFeedbackSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendationFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommendationFeedbackResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'recommendationFeedbackSummaries', 'listRecommendationFeedbackResponse_recommendationFeedbackSummaries' - Recommendation feedback summaries corresponding to the code review ARN.
--
-- 'httpStatus', 'listRecommendationFeedbackResponse_httpStatus' - The response's http status code.
newListRecommendationFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecommendationFeedbackResponse
newListRecommendationFeedbackResponse pHttpStatus_ =
  ListRecommendationFeedbackResponse'
    { nextToken =
        Prelude.Nothing,
      recommendationFeedbackSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
listRecommendationFeedbackResponse_nextToken :: Lens.Lens' ListRecommendationFeedbackResponse (Prelude.Maybe Prelude.Text)
listRecommendationFeedbackResponse_nextToken = Lens.lens (\ListRecommendationFeedbackResponse' {nextToken} -> nextToken) (\s@ListRecommendationFeedbackResponse' {} a -> s {nextToken = a} :: ListRecommendationFeedbackResponse)

-- | Recommendation feedback summaries corresponding to the code review ARN.
listRecommendationFeedbackResponse_recommendationFeedbackSummaries :: Lens.Lens' ListRecommendationFeedbackResponse (Prelude.Maybe [RecommendationFeedbackSummary])
listRecommendationFeedbackResponse_recommendationFeedbackSummaries = Lens.lens (\ListRecommendationFeedbackResponse' {recommendationFeedbackSummaries} -> recommendationFeedbackSummaries) (\s@ListRecommendationFeedbackResponse' {} a -> s {recommendationFeedbackSummaries = a} :: ListRecommendationFeedbackResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendationFeedbackResponse_httpStatus :: Lens.Lens' ListRecommendationFeedbackResponse Prelude.Int
listRecommendationFeedbackResponse_httpStatus = Lens.lens (\ListRecommendationFeedbackResponse' {httpStatus} -> httpStatus) (\s@ListRecommendationFeedbackResponse' {} a -> s {httpStatus = a} :: ListRecommendationFeedbackResponse)

instance
  Prelude.NFData
    ListRecommendationFeedbackResponse
  where
  rnf ListRecommendationFeedbackResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationFeedbackSummaries
      `Prelude.seq` Prelude.rnf httpStatus
