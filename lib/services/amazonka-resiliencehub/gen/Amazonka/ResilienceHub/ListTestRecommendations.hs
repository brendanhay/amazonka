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
-- Module      : Amazonka.ResilienceHub.ListTestRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the test recommendations for the Resilience Hub application.
module Amazonka.ResilienceHub.ListTestRecommendations
  ( -- * Creating a Request
    ListTestRecommendations (..),
    newListTestRecommendations,

    -- * Request Lenses
    listTestRecommendations_maxResults,
    listTestRecommendations_nextToken,
    listTestRecommendations_assessmentArn,

    -- * Destructuring the Response
    ListTestRecommendationsResponse (..),
    newListTestRecommendationsResponse,

    -- * Response Lenses
    listTestRecommendationsResponse_nextToken,
    listTestRecommendationsResponse_httpStatus,
    listTestRecommendationsResponse_testRecommendations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestRecommendations' smart constructor.
data ListTestRecommendations = ListTestRecommendations'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTestRecommendations_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listTestRecommendations_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'assessmentArn', 'listTestRecommendations_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListTestRecommendations ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListTestRecommendations
newListTestRecommendations pAssessmentArn_ =
  ListTestRecommendations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listTestRecommendations_maxResults :: Lens.Lens' ListTestRecommendations (Prelude.Maybe Prelude.Natural)
listTestRecommendations_maxResults = Lens.lens (\ListTestRecommendations' {maxResults} -> maxResults) (\s@ListTestRecommendations' {} a -> s {maxResults = a} :: ListTestRecommendations)

-- | Null, or the token from a previous call to get the next set of results.
listTestRecommendations_nextToken :: Lens.Lens' ListTestRecommendations (Prelude.Maybe Prelude.Text)
listTestRecommendations_nextToken = Lens.lens (\ListTestRecommendations' {nextToken} -> nextToken) (\s@ListTestRecommendations' {} a -> s {nextToken = a} :: ListTestRecommendations)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listTestRecommendations_assessmentArn :: Lens.Lens' ListTestRecommendations Prelude.Text
listTestRecommendations_assessmentArn = Lens.lens (\ListTestRecommendations' {assessmentArn} -> assessmentArn) (\s@ListTestRecommendations' {} a -> s {assessmentArn = a} :: ListTestRecommendations)

instance Core.AWSRequest ListTestRecommendations where
  type
    AWSResponse ListTestRecommendations =
      ListTestRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestRecommendationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "testRecommendations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTestRecommendations where
  hashWithSalt _salt ListTestRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData ListTestRecommendations where
  rnf ListTestRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders ListTestRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestRecommendations where
  toJSON ListTestRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath ListTestRecommendations where
  toPath = Prelude.const "/list-test-recommendations"

instance Data.ToQuery ListTestRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestRecommendationsResponse' smart constructor.
data ListTestRecommendationsResponse = ListTestRecommendationsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The test recommendations for the Resilience Hub application.
    testRecommendations :: [TestRecommendation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestRecommendationsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listTestRecommendationsResponse_httpStatus' - The response's http status code.
--
-- 'testRecommendations', 'listTestRecommendationsResponse_testRecommendations' - The test recommendations for the Resilience Hub application.
newListTestRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestRecommendationsResponse
newListTestRecommendationsResponse pHttpStatus_ =
  ListTestRecommendationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      testRecommendations = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listTestRecommendationsResponse_nextToken :: Lens.Lens' ListTestRecommendationsResponse (Prelude.Maybe Prelude.Text)
listTestRecommendationsResponse_nextToken = Lens.lens (\ListTestRecommendationsResponse' {nextToken} -> nextToken) (\s@ListTestRecommendationsResponse' {} a -> s {nextToken = a} :: ListTestRecommendationsResponse)

-- | The response's http status code.
listTestRecommendationsResponse_httpStatus :: Lens.Lens' ListTestRecommendationsResponse Prelude.Int
listTestRecommendationsResponse_httpStatus = Lens.lens (\ListTestRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListTestRecommendationsResponse' {} a -> s {httpStatus = a} :: ListTestRecommendationsResponse)

-- | The test recommendations for the Resilience Hub application.
listTestRecommendationsResponse_testRecommendations :: Lens.Lens' ListTestRecommendationsResponse [TestRecommendation]
listTestRecommendationsResponse_testRecommendations = Lens.lens (\ListTestRecommendationsResponse' {testRecommendations} -> testRecommendations) (\s@ListTestRecommendationsResponse' {} a -> s {testRecommendations = a} :: ListTestRecommendationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListTestRecommendationsResponse
  where
  rnf ListTestRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf testRecommendations
