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
-- Module      : Amazonka.ResilienceHub.ListAppComponentRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recommendations for an AWS Resilience Hub component.
module Amazonka.ResilienceHub.ListAppComponentRecommendations
  ( -- * Creating a Request
    ListAppComponentRecommendations (..),
    newListAppComponentRecommendations,

    -- * Request Lenses
    listAppComponentRecommendations_maxResults,
    listAppComponentRecommendations_nextToken,
    listAppComponentRecommendations_assessmentArn,

    -- * Destructuring the Response
    ListAppComponentRecommendationsResponse (..),
    newListAppComponentRecommendationsResponse,

    -- * Response Lenses
    listAppComponentRecommendationsResponse_nextToken,
    listAppComponentRecommendationsResponse_httpStatus,
    listAppComponentRecommendationsResponse_componentRecommendations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppComponentRecommendations' smart constructor.
data ListAppComponentRecommendations = ListAppComponentRecommendations'
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
-- Create a value of 'ListAppComponentRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppComponentRecommendations_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listAppComponentRecommendations_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'assessmentArn', 'listAppComponentRecommendations_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListAppComponentRecommendations ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListAppComponentRecommendations
newListAppComponentRecommendations pAssessmentArn_ =
  ListAppComponentRecommendations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAppComponentRecommendations_maxResults :: Lens.Lens' ListAppComponentRecommendations (Prelude.Maybe Prelude.Natural)
listAppComponentRecommendations_maxResults = Lens.lens (\ListAppComponentRecommendations' {maxResults} -> maxResults) (\s@ListAppComponentRecommendations' {} a -> s {maxResults = a} :: ListAppComponentRecommendations)

-- | Null, or the token from a previous call to get the next set of results.
listAppComponentRecommendations_nextToken :: Lens.Lens' ListAppComponentRecommendations (Prelude.Maybe Prelude.Text)
listAppComponentRecommendations_nextToken = Lens.lens (\ListAppComponentRecommendations' {nextToken} -> nextToken) (\s@ListAppComponentRecommendations' {} a -> s {nextToken = a} :: ListAppComponentRecommendations)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listAppComponentRecommendations_assessmentArn :: Lens.Lens' ListAppComponentRecommendations Prelude.Text
listAppComponentRecommendations_assessmentArn = Lens.lens (\ListAppComponentRecommendations' {assessmentArn} -> assessmentArn) (\s@ListAppComponentRecommendations' {} a -> s {assessmentArn = a} :: ListAppComponentRecommendations)

instance
  Core.AWSRequest
    ListAppComponentRecommendations
  where
  type
    AWSResponse ListAppComponentRecommendations =
      ListAppComponentRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppComponentRecommendationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "componentRecommendations"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListAppComponentRecommendations
  where
  hashWithSalt
    _salt
    ListAppComponentRecommendations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` assessmentArn

instance
  Prelude.NFData
    ListAppComponentRecommendations
  where
  rnf ListAppComponentRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance
  Data.ToHeaders
    ListAppComponentRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppComponentRecommendations where
  toJSON ListAppComponentRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath ListAppComponentRecommendations where
  toPath =
    Prelude.const "/list-app-component-recommendations"

instance Data.ToQuery ListAppComponentRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppComponentRecommendationsResponse' smart constructor.
data ListAppComponentRecommendationsResponse = ListAppComponentRecommendationsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The recommendations for an Resilience Hub application component,
    -- returned as an object. This object contains component names,
    -- configuration recommendations, and recommendation statuses.
    componentRecommendations :: [ComponentRecommendation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppComponentRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppComponentRecommendationsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppComponentRecommendationsResponse_httpStatus' - The response's http status code.
--
-- 'componentRecommendations', 'listAppComponentRecommendationsResponse_componentRecommendations' - The recommendations for an Resilience Hub application component,
-- returned as an object. This object contains component names,
-- configuration recommendations, and recommendation statuses.
newListAppComponentRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppComponentRecommendationsResponse
newListAppComponentRecommendationsResponse
  pHttpStatus_ =
    ListAppComponentRecommendationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        componentRecommendations =
          Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppComponentRecommendationsResponse_nextToken :: Lens.Lens' ListAppComponentRecommendationsResponse (Prelude.Maybe Prelude.Text)
listAppComponentRecommendationsResponse_nextToken = Lens.lens (\ListAppComponentRecommendationsResponse' {nextToken} -> nextToken) (\s@ListAppComponentRecommendationsResponse' {} a -> s {nextToken = a} :: ListAppComponentRecommendationsResponse)

-- | The response's http status code.
listAppComponentRecommendationsResponse_httpStatus :: Lens.Lens' ListAppComponentRecommendationsResponse Prelude.Int
listAppComponentRecommendationsResponse_httpStatus = Lens.lens (\ListAppComponentRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListAppComponentRecommendationsResponse' {} a -> s {httpStatus = a} :: ListAppComponentRecommendationsResponse)

-- | The recommendations for an Resilience Hub application component,
-- returned as an object. This object contains component names,
-- configuration recommendations, and recommendation statuses.
listAppComponentRecommendationsResponse_componentRecommendations :: Lens.Lens' ListAppComponentRecommendationsResponse [ComponentRecommendation]
listAppComponentRecommendationsResponse_componentRecommendations = Lens.lens (\ListAppComponentRecommendationsResponse' {componentRecommendations} -> componentRecommendations) (\s@ListAppComponentRecommendationsResponse' {} a -> s {componentRecommendations = a} :: ListAppComponentRecommendationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAppComponentRecommendationsResponse
  where
  rnf ListAppComponentRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf componentRecommendations
