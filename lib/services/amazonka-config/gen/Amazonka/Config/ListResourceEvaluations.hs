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
-- Module      : Amazonka.Config.ListResourceEvaluations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of proactive resource evaluations.
--
-- This operation returns paginated results.
module Amazonka.Config.ListResourceEvaluations
  ( -- * Creating a Request
    ListResourceEvaluations (..),
    newListResourceEvaluations,

    -- * Request Lenses
    listResourceEvaluations_filters,
    listResourceEvaluations_limit,
    listResourceEvaluations_nextToken,

    -- * Destructuring the Response
    ListResourceEvaluationsResponse (..),
    newListResourceEvaluationsResponse,

    -- * Response Lenses
    listResourceEvaluationsResponse_nextToken,
    listResourceEvaluationsResponse_resourceEvaluations,
    listResourceEvaluationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceEvaluations' smart constructor.
data ListResourceEvaluations = ListResourceEvaluations'
  { -- | Returns a @ResourceEvaluationFilters@ object.
    filters :: Prelude.Maybe ResourceEvaluationFilters,
    -- | The maximum number of evaluations returned on each page. The default is
    -- 10. You cannot specify a number greater than 100. If you specify 0,
    -- Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listResourceEvaluations_filters' - Returns a @ResourceEvaluationFilters@ object.
--
-- 'limit', 'listResourceEvaluations_limit' - The maximum number of evaluations returned on each page. The default is
-- 10. You cannot specify a number greater than 100. If you specify 0,
-- Config uses the default.
--
-- 'nextToken', 'listResourceEvaluations_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newListResourceEvaluations ::
  ListResourceEvaluations
newListResourceEvaluations =
  ListResourceEvaluations'
    { filters = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns a @ResourceEvaluationFilters@ object.
listResourceEvaluations_filters :: Lens.Lens' ListResourceEvaluations (Prelude.Maybe ResourceEvaluationFilters)
listResourceEvaluations_filters = Lens.lens (\ListResourceEvaluations' {filters} -> filters) (\s@ListResourceEvaluations' {} a -> s {filters = a} :: ListResourceEvaluations)

-- | The maximum number of evaluations returned on each page. The default is
-- 10. You cannot specify a number greater than 100. If you specify 0,
-- Config uses the default.
listResourceEvaluations_limit :: Lens.Lens' ListResourceEvaluations (Prelude.Maybe Prelude.Natural)
listResourceEvaluations_limit = Lens.lens (\ListResourceEvaluations' {limit} -> limit) (\s@ListResourceEvaluations' {} a -> s {limit = a} :: ListResourceEvaluations)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listResourceEvaluations_nextToken :: Lens.Lens' ListResourceEvaluations (Prelude.Maybe Prelude.Text)
listResourceEvaluations_nextToken = Lens.lens (\ListResourceEvaluations' {nextToken} -> nextToken) (\s@ListResourceEvaluations' {} a -> s {nextToken = a} :: ListResourceEvaluations)

instance Core.AWSPager ListResourceEvaluations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceEvaluationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceEvaluationsResponse_resourceEvaluations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResourceEvaluations_nextToken
          Lens..~ rs
          Lens.^? listResourceEvaluationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListResourceEvaluations where
  type
    AWSResponse ListResourceEvaluations =
      ListResourceEvaluationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceEvaluationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ResourceEvaluations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceEvaluations where
  hashWithSalt _salt ListResourceEvaluations' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResourceEvaluations where
  rnf ListResourceEvaluations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListResourceEvaluations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.ListResourceEvaluations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceEvaluations where
  toJSON ListResourceEvaluations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListResourceEvaluations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourceEvaluations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceEvaluationsResponse' smart constructor.
data ListResourceEvaluationsResponse = ListResourceEvaluationsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a @ResourceEvaluations@ object.
    resourceEvaluations :: Prelude.Maybe [ResourceEvaluation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceEvaluationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceEvaluationsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceEvaluations', 'listResourceEvaluationsResponse_resourceEvaluations' - Returns a @ResourceEvaluations@ object.
--
-- 'httpStatus', 'listResourceEvaluationsResponse_httpStatus' - The response's http status code.
newListResourceEvaluationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceEvaluationsResponse
newListResourceEvaluationsResponse pHttpStatus_ =
  ListResourceEvaluationsResponse'
    { nextToken =
        Prelude.Nothing,
      resourceEvaluations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listResourceEvaluationsResponse_nextToken :: Lens.Lens' ListResourceEvaluationsResponse (Prelude.Maybe Prelude.Text)
listResourceEvaluationsResponse_nextToken = Lens.lens (\ListResourceEvaluationsResponse' {nextToken} -> nextToken) (\s@ListResourceEvaluationsResponse' {} a -> s {nextToken = a} :: ListResourceEvaluationsResponse)

-- | Returns a @ResourceEvaluations@ object.
listResourceEvaluationsResponse_resourceEvaluations :: Lens.Lens' ListResourceEvaluationsResponse (Prelude.Maybe [ResourceEvaluation])
listResourceEvaluationsResponse_resourceEvaluations = Lens.lens (\ListResourceEvaluationsResponse' {resourceEvaluations} -> resourceEvaluations) (\s@ListResourceEvaluationsResponse' {} a -> s {resourceEvaluations = a} :: ListResourceEvaluationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceEvaluationsResponse_httpStatus :: Lens.Lens' ListResourceEvaluationsResponse Prelude.Int
listResourceEvaluationsResponse_httpStatus = Lens.lens (\ListResourceEvaluationsResponse' {httpStatus} -> httpStatus) (\s@ListResourceEvaluationsResponse' {} a -> s {httpStatus = a} :: ListResourceEvaluationsResponse)

instance
  Prelude.NFData
    ListResourceEvaluationsResponse
  where
  rnf ListResourceEvaluationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceEvaluations
      `Prelude.seq` Prelude.rnf httpStatus
