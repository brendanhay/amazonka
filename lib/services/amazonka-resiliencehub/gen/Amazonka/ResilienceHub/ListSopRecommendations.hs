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
-- Module      : Amazonka.ResilienceHub.ListSopRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the standard operating procedure (SOP) recommendations for the
-- Resilience Hub applications.
module Amazonka.ResilienceHub.ListSopRecommendations
  ( -- * Creating a Request
    ListSopRecommendations (..),
    newListSopRecommendations,

    -- * Request Lenses
    listSopRecommendations_maxResults,
    listSopRecommendations_nextToken,
    listSopRecommendations_assessmentArn,

    -- * Destructuring the Response
    ListSopRecommendationsResponse (..),
    newListSopRecommendationsResponse,

    -- * Response Lenses
    listSopRecommendationsResponse_nextToken,
    listSopRecommendationsResponse_httpStatus,
    listSopRecommendationsResponse_sopRecommendations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSopRecommendations' smart constructor.
data ListSopRecommendations = ListSopRecommendations'
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
-- Create a value of 'ListSopRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSopRecommendations_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listSopRecommendations_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'assessmentArn', 'listSopRecommendations_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListSopRecommendations ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListSopRecommendations
newListSopRecommendations pAssessmentArn_ =
  ListSopRecommendations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listSopRecommendations_maxResults :: Lens.Lens' ListSopRecommendations (Prelude.Maybe Prelude.Natural)
listSopRecommendations_maxResults = Lens.lens (\ListSopRecommendations' {maxResults} -> maxResults) (\s@ListSopRecommendations' {} a -> s {maxResults = a} :: ListSopRecommendations)

-- | Null, or the token from a previous call to get the next set of results.
listSopRecommendations_nextToken :: Lens.Lens' ListSopRecommendations (Prelude.Maybe Prelude.Text)
listSopRecommendations_nextToken = Lens.lens (\ListSopRecommendations' {nextToken} -> nextToken) (\s@ListSopRecommendations' {} a -> s {nextToken = a} :: ListSopRecommendations)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listSopRecommendations_assessmentArn :: Lens.Lens' ListSopRecommendations Prelude.Text
listSopRecommendations_assessmentArn = Lens.lens (\ListSopRecommendations' {assessmentArn} -> assessmentArn) (\s@ListSopRecommendations' {} a -> s {assessmentArn = a} :: ListSopRecommendations)

instance Core.AWSRequest ListSopRecommendations where
  type
    AWSResponse ListSopRecommendations =
      ListSopRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSopRecommendationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "sopRecommendations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSopRecommendations where
  hashWithSalt _salt ListSopRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData ListSopRecommendations where
  rnf ListSopRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders ListSopRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSopRecommendations where
  toJSON ListSopRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath ListSopRecommendations where
  toPath = Prelude.const "/list-sop-recommendations"

instance Data.ToQuery ListSopRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSopRecommendationsResponse' smart constructor.
data ListSopRecommendationsResponse = ListSopRecommendationsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The standard operating procedure (SOP) recommendations for the
    -- Resilience Hub applications.
    sopRecommendations :: [SopRecommendation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSopRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSopRecommendationsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listSopRecommendationsResponse_httpStatus' - The response's http status code.
--
-- 'sopRecommendations', 'listSopRecommendationsResponse_sopRecommendations' - The standard operating procedure (SOP) recommendations for the
-- Resilience Hub applications.
newListSopRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSopRecommendationsResponse
newListSopRecommendationsResponse pHttpStatus_ =
  ListSopRecommendationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      sopRecommendations = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listSopRecommendationsResponse_nextToken :: Lens.Lens' ListSopRecommendationsResponse (Prelude.Maybe Prelude.Text)
listSopRecommendationsResponse_nextToken = Lens.lens (\ListSopRecommendationsResponse' {nextToken} -> nextToken) (\s@ListSopRecommendationsResponse' {} a -> s {nextToken = a} :: ListSopRecommendationsResponse)

-- | The response's http status code.
listSopRecommendationsResponse_httpStatus :: Lens.Lens' ListSopRecommendationsResponse Prelude.Int
listSopRecommendationsResponse_httpStatus = Lens.lens (\ListSopRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListSopRecommendationsResponse' {} a -> s {httpStatus = a} :: ListSopRecommendationsResponse)

-- | The standard operating procedure (SOP) recommendations for the
-- Resilience Hub applications.
listSopRecommendationsResponse_sopRecommendations :: Lens.Lens' ListSopRecommendationsResponse [SopRecommendation]
listSopRecommendationsResponse_sopRecommendations = Lens.lens (\ListSopRecommendationsResponse' {sopRecommendations} -> sopRecommendations) (\s@ListSopRecommendationsResponse' {} a -> s {sopRecommendations = a} :: ListSopRecommendationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSopRecommendationsResponse
  where
  rnf ListSopRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sopRecommendations
