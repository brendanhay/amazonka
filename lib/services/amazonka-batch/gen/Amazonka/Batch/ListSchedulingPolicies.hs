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
-- Module      : Amazonka.Batch.ListSchedulingPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Batch scheduling policies.
--
-- This operation returns paginated results.
module Amazonka.Batch.ListSchedulingPolicies
  ( -- * Creating a Request
    ListSchedulingPolicies (..),
    newListSchedulingPolicies,

    -- * Request Lenses
    listSchedulingPolicies_maxResults,
    listSchedulingPolicies_nextToken,

    -- * Destructuring the Response
    ListSchedulingPoliciesResponse (..),
    newListSchedulingPoliciesResponse,

    -- * Response Lenses
    listSchedulingPoliciesResponse_nextToken,
    listSchedulingPoliciesResponse_schedulingPolicies,
    listSchedulingPoliciesResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @ListSchedulingPolicies@.
--
-- /See:/ 'newListSchedulingPolicies' smart constructor.
data ListSchedulingPolicies = ListSchedulingPolicies'
  { -- | The maximum number of results that\'s returned by
    -- @ListSchedulingPolicies@ in paginated output. When this parameter is
    -- used, @ListSchedulingPolicies@ only returns @maxResults@ results in a
    -- single page and a @nextToken@ response element. You can see the
    -- remaining results of the initial request by sending another
    -- @ListSchedulingPolicies@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter isn\'t used,
    -- @ListSchedulingPolicies@ returns up to 100 results and a @nextToken@
    -- value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @nextToken@ value that\'s returned from a previous paginated
    -- @ListSchedulingPolicies@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is @null@ when there are no more results to return.
    --
    -- Treat this token as an opaque identifier that\'s only used to retrieve
    -- the next items in a list and not for other programmatic purposes.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchedulingPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSchedulingPolicies_maxResults' - The maximum number of results that\'s returned by
-- @ListSchedulingPolicies@ in paginated output. When this parameter is
-- used, @ListSchedulingPolicies@ only returns @maxResults@ results in a
-- single page and a @nextToken@ response element. You can see the
-- remaining results of the initial request by sending another
-- @ListSchedulingPolicies@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used,
-- @ListSchedulingPolicies@ returns up to 100 results and a @nextToken@
-- value if applicable.
--
-- 'nextToken', 'listSchedulingPolicies_nextToken' - The @nextToken@ value that\'s returned from a previous paginated
-- @ListSchedulingPolicies@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
newListSchedulingPolicies ::
  ListSchedulingPolicies
newListSchedulingPolicies =
  ListSchedulingPolicies'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results that\'s returned by
-- @ListSchedulingPolicies@ in paginated output. When this parameter is
-- used, @ListSchedulingPolicies@ only returns @maxResults@ results in a
-- single page and a @nextToken@ response element. You can see the
-- remaining results of the initial request by sending another
-- @ListSchedulingPolicies@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used,
-- @ListSchedulingPolicies@ returns up to 100 results and a @nextToken@
-- value if applicable.
listSchedulingPolicies_maxResults :: Lens.Lens' ListSchedulingPolicies (Prelude.Maybe Prelude.Int)
listSchedulingPolicies_maxResults = Lens.lens (\ListSchedulingPolicies' {maxResults} -> maxResults) (\s@ListSchedulingPolicies' {} a -> s {maxResults = a} :: ListSchedulingPolicies)

-- | The @nextToken@ value that\'s returned from a previous paginated
-- @ListSchedulingPolicies@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
listSchedulingPolicies_nextToken :: Lens.Lens' ListSchedulingPolicies (Prelude.Maybe Prelude.Text)
listSchedulingPolicies_nextToken = Lens.lens (\ListSchedulingPolicies' {nextToken} -> nextToken) (\s@ListSchedulingPolicies' {} a -> s {nextToken = a} :: ListSchedulingPolicies)

instance Core.AWSPager ListSchedulingPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchedulingPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchedulingPoliciesResponse_schedulingPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchedulingPolicies_nextToken
          Lens..~ rs
          Lens.^? listSchedulingPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSchedulingPolicies where
  type
    AWSResponse ListSchedulingPolicies =
      ListSchedulingPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchedulingPoliciesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "schedulingPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchedulingPolicies where
  hashWithSalt _salt ListSchedulingPolicies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSchedulingPolicies where
  rnf ListSchedulingPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSchedulingPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSchedulingPolicies where
  toJSON ListSchedulingPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSchedulingPolicies where
  toPath = Prelude.const "/v1/listschedulingpolicies"

instance Data.ToQuery ListSchedulingPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchedulingPoliciesResponse' smart constructor.
data ListSchedulingPoliciesResponse = ListSchedulingPoliciesResponse'
  { -- | The @nextToken@ value to include in a future @ListSchedulingPolicies@
    -- request. When the results of a @ListSchedulingPolicies@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of scheduling policies that match the request.
    schedulingPolicies :: Prelude.Maybe [SchedulingPolicyListingDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchedulingPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchedulingPoliciesResponse_nextToken' - The @nextToken@ value to include in a future @ListSchedulingPolicies@
-- request. When the results of a @ListSchedulingPolicies@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'schedulingPolicies', 'listSchedulingPoliciesResponse_schedulingPolicies' - A list of scheduling policies that match the request.
--
-- 'httpStatus', 'listSchedulingPoliciesResponse_httpStatus' - The response's http status code.
newListSchedulingPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchedulingPoliciesResponse
newListSchedulingPoliciesResponse pHttpStatus_ =
  ListSchedulingPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      schedulingPolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListSchedulingPolicies@
-- request. When the results of a @ListSchedulingPolicies@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listSchedulingPoliciesResponse_nextToken :: Lens.Lens' ListSchedulingPoliciesResponse (Prelude.Maybe Prelude.Text)
listSchedulingPoliciesResponse_nextToken = Lens.lens (\ListSchedulingPoliciesResponse' {nextToken} -> nextToken) (\s@ListSchedulingPoliciesResponse' {} a -> s {nextToken = a} :: ListSchedulingPoliciesResponse)

-- | A list of scheduling policies that match the request.
listSchedulingPoliciesResponse_schedulingPolicies :: Lens.Lens' ListSchedulingPoliciesResponse (Prelude.Maybe [SchedulingPolicyListingDetail])
listSchedulingPoliciesResponse_schedulingPolicies = Lens.lens (\ListSchedulingPoliciesResponse' {schedulingPolicies} -> schedulingPolicies) (\s@ListSchedulingPoliciesResponse' {} a -> s {schedulingPolicies = a} :: ListSchedulingPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchedulingPoliciesResponse_httpStatus :: Lens.Lens' ListSchedulingPoliciesResponse Prelude.Int
listSchedulingPoliciesResponse_httpStatus = Lens.lens (\ListSchedulingPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListSchedulingPoliciesResponse' {} a -> s {httpStatus = a} :: ListSchedulingPoliciesResponse)

instance
  Prelude.NFData
    ListSchedulingPoliciesResponse
  where
  rnf ListSchedulingPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schedulingPolicies
      `Prelude.seq` Prelude.rnf httpStatus
