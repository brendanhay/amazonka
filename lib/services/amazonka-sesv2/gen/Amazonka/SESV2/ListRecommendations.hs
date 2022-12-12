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
-- Module      : Amazonka.SESV2.ListRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recommendations present in your Amazon SES account in the
-- current Amazon Web Services Region.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.ListRecommendations
  ( -- * Creating a Request
    ListRecommendations (..),
    newListRecommendations,

    -- * Request Lenses
    listRecommendations_filter,
    listRecommendations_nextToken,
    listRecommendations_pageSize,

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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to list the existing recommendations for your
-- account.
--
-- /See:/ 'newListRecommendations' smart constructor.
data ListRecommendations = ListRecommendations'
  { -- | Filters applied when retrieving recommendations. Can eiter be an
    -- individual filter, or combinations of @STATUS@ and @IMPACT@ or @STATUS@
    -- and @TYPE@
    filter' :: Prelude.Maybe (Prelude.HashMap ListRecommendationsFilterKey Prelude.Text),
    -- | A token returned from a previous call to @ListRecommendations@ to
    -- indicate the position in the list of recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to @ListRecommendations@.
    -- If the number of results is larger than the number you specified in this
    -- parameter, then the response includes a @NextToken@ element, which you
    -- can use to obtain additional results.
    --
    -- The value you specify has to be at least 1, and can be no more than 100.
    pageSize :: Prelude.Maybe Prelude.Int
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
-- 'filter'', 'listRecommendations_filter' - Filters applied when retrieving recommendations. Can eiter be an
-- individual filter, or combinations of @STATUS@ and @IMPACT@ or @STATUS@
-- and @TYPE@
--
-- 'nextToken', 'listRecommendations_nextToken' - A token returned from a previous call to @ListRecommendations@ to
-- indicate the position in the list of recommendations.
--
-- 'pageSize', 'listRecommendations_pageSize' - The number of results to show in a single call to @ListRecommendations@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 1, and can be no more than 100.
newListRecommendations ::
  ListRecommendations
newListRecommendations =
  ListRecommendations'
    { filter' = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | Filters applied when retrieving recommendations. Can eiter be an
-- individual filter, or combinations of @STATUS@ and @IMPACT@ or @STATUS@
-- and @TYPE@
listRecommendations_filter :: Lens.Lens' ListRecommendations (Prelude.Maybe (Prelude.HashMap ListRecommendationsFilterKey Prelude.Text))
listRecommendations_filter = Lens.lens (\ListRecommendations' {filter'} -> filter') (\s@ListRecommendations' {} a -> s {filter' = a} :: ListRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | A token returned from a previous call to @ListRecommendations@ to
-- indicate the position in the list of recommendations.
listRecommendations_nextToken :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Text)
listRecommendations_nextToken = Lens.lens (\ListRecommendations' {nextToken} -> nextToken) (\s@ListRecommendations' {} a -> s {nextToken = a} :: ListRecommendations)

-- | The number of results to show in a single call to @ListRecommendations@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 1, and can be no more than 100.
listRecommendations_pageSize :: Lens.Lens' ListRecommendations (Prelude.Maybe Prelude.Int)
listRecommendations_pageSize = Lens.lens (\ListRecommendations' {pageSize} -> pageSize) (\s@ListRecommendations' {} a -> s {pageSize = a} :: ListRecommendations)

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
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListRecommendations where
  rnf ListRecommendations' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

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
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PageSize" Data..=) Prelude.<$> pageSize
          ]
      )

instance Data.ToPath ListRecommendations where
  toPath =
    Prelude.const "/v2/email/vdm/recommendations"

instance Data.ToQuery ListRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to your request to retrieve the list of
-- recommendations for your account.
--
-- /See:/ 'newListRecommendationsResponse' smart constructor.
data ListRecommendationsResponse = ListRecommendationsResponse'
  { -- | A string token indicating that there might be additional recommendations
    -- available to be listed. Use the token provided in the
    -- @ListRecommendationsResponse@ to use in the subsequent call to
    -- @ListRecommendations@ with the same parameters to retrieve the next page
    -- of recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The recommendations applicable to your account.
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
-- 'nextToken', 'listRecommendationsResponse_nextToken' - A string token indicating that there might be additional recommendations
-- available to be listed. Use the token provided in the
-- @ListRecommendationsResponse@ to use in the subsequent call to
-- @ListRecommendations@ with the same parameters to retrieve the next page
-- of recommendations.
--
-- 'recommendations', 'listRecommendationsResponse_recommendations' - The recommendations applicable to your account.
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

-- | A string token indicating that there might be additional recommendations
-- available to be listed. Use the token provided in the
-- @ListRecommendationsResponse@ to use in the subsequent call to
-- @ListRecommendations@ with the same parameters to retrieve the next page
-- of recommendations.
listRecommendationsResponse_nextToken :: Lens.Lens' ListRecommendationsResponse (Prelude.Maybe Prelude.Text)
listRecommendationsResponse_nextToken = Lens.lens (\ListRecommendationsResponse' {nextToken} -> nextToken) (\s@ListRecommendationsResponse' {} a -> s {nextToken = a} :: ListRecommendationsResponse)

-- | The recommendations applicable to your account.
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
