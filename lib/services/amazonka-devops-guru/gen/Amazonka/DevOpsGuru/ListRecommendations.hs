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
-- Module      : Amazonka.DevOpsGuru.ListRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of a specified insight\'s recommendations. Each
-- recommendation includes a list of related metrics and a list of related
-- events.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListRecommendations
  ( -- * Creating a Request
    ListRecommendations (..),
    newListRecommendations,

    -- * Request Lenses
    listRecommendations_accountId,
    listRecommendations_locale,
    listRecommendations_nextToken,
    listRecommendations_insightId,

    -- * Destructuring the Response
    ListRecommendationsResponse (..),
    newListRecommendationsResponse,

    -- * Response Lenses
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendations,
    listRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommendations' smart constructor.
data ListRecommendations = ListRecommendations'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A locale that specifies the language to use for recommendations.
    locale :: Prelude.Maybe Locale,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the requested insight.
    insightId :: Prelude.Text
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
-- 'accountId', 'listRecommendations_accountId' - The ID of the Amazon Web Services account.
--
-- 'locale', 'listRecommendations_locale' - A locale that specifies the language to use for recommendations.
--
-- 'nextToken', 'listRecommendations_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'insightId', 'listRecommendations_insightId' - The ID of the requested insight.
newListRecommendations ::
  -- | 'insightId'
  Prelude.Text ->
  ListRecommendations
newListRecommendations pInsightId_ =
  ListRecommendations'
    { accountId = Prelude.Nothing,
      locale = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      insightId = pInsightId_
    }

-- | The ID of the Amazon Web Services account.
listRecommendations_accountId :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Text)
listRecommendations_accountId = Lens.lens (\ListRecommendations' {accountId} -> accountId) (\s@ListRecommendations' {} a -> s {accountId = a} :: ListRecommendations)

-- | A locale that specifies the language to use for recommendations.
listRecommendations_locale :: Lens.Lens' ListRecommendations (Prelude.Maybe Locale)
listRecommendations_locale = Lens.lens (\ListRecommendations' {locale} -> locale) (\s@ListRecommendations' {} a -> s {locale = a} :: ListRecommendations)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listRecommendations_nextToken :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Text)
listRecommendations_nextToken = Lens.lens (\ListRecommendations' {nextToken} -> nextToken) (\s@ListRecommendations' {} a -> s {nextToken = a} :: ListRecommendations)

-- | The ID of the requested insight.
listRecommendations_insightId :: Lens.Lens' ListRecommendations Prelude.Text
listRecommendations_insightId = Lens.lens (\ListRecommendations' {insightId} -> insightId) (\s@ListRecommendations' {} a -> s {insightId = a} :: ListRecommendations)

instance Core.AWSPager ListRecommendations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecommendationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecommendationsResponse_recommendations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecommendations_nextToken
          Lens..~ rs
          Lens.^? listRecommendationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRecommendations where
  type
    AWSResponse ListRecommendations =
      ListRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "Recommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommendations where
  hashWithSalt _salt ListRecommendations' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` insightId

instance Prelude.NFData ListRecommendations where
  rnf ListRecommendations' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf insightId

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

instance Data.ToJSON ListRecommendations where
  toJSON ListRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("Locale" Data..=) Prelude.<$> locale,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InsightId" Data..= insightId)
          ]
      )

instance Data.ToPath ListRecommendations where
  toPath = Prelude.const "/recommendations"

instance Data.ToQuery ListRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecommendationsResponse' smart constructor.
data ListRecommendationsResponse = ListRecommendationsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of the requested recommendations.
    recommendations :: Prelude.Maybe [Recommendation],
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
-- 'nextToken', 'listRecommendationsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'recommendations', 'listRecommendationsResponse_recommendations' - An array of the requested recommendations.
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
      recommendations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listRecommendationsResponse_nextToken :: Lens.Lens' ListRecommendationsResponse (Prelude.Maybe Prelude.Text)
listRecommendationsResponse_nextToken = Lens.lens (\ListRecommendationsResponse' {nextToken} -> nextToken) (\s@ListRecommendationsResponse' {} a -> s {nextToken = a} :: ListRecommendationsResponse)

-- | An array of the requested recommendations.
listRecommendationsResponse_recommendations :: Lens.Lens' ListRecommendationsResponse (Prelude.Maybe [Recommendation])
listRecommendationsResponse_recommendations = Lens.lens (\ListRecommendationsResponse' {recommendations} -> recommendations) (\s@ListRecommendationsResponse' {} a -> s {recommendations = a} :: ListRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendationsResponse_httpStatus :: Lens.Lens' ListRecommendationsResponse Prelude.Int
listRecommendationsResponse_httpStatus = Lens.lens (\ListRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListRecommendationsResponse' {} a -> s {httpStatus = a} :: ListRecommendationsResponse)

instance Prelude.NFData ListRecommendationsResponse where
  rnf ListRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf httpStatus
