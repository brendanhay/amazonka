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
-- Module      : Amazonka.IoT.ListBillingGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the billing groups you have created.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListBillingGroups>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListBillingGroups
  ( -- * Creating a Request
    ListBillingGroups (..),
    newListBillingGroups,

    -- * Request Lenses
    listBillingGroups_nextToken,
    listBillingGroups_namePrefixFilter,
    listBillingGroups_maxResults,

    -- * Destructuring the Response
    ListBillingGroupsResponse (..),
    newListBillingGroupsResponse,

    -- * Response Lenses
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit the results to billing groups whose names have the given prefix.
    namePrefixFilter :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBillingGroups_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'namePrefixFilter', 'listBillingGroups_namePrefixFilter' - Limit the results to billing groups whose names have the given prefix.
--
-- 'maxResults', 'listBillingGroups_maxResults' - The maximum number of results to return per request.
newListBillingGroups ::
  ListBillingGroups
newListBillingGroups =
  ListBillingGroups'
    { nextToken = Prelude.Nothing,
      namePrefixFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listBillingGroups_nextToken :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_nextToken = Lens.lens (\ListBillingGroups' {nextToken} -> nextToken) (\s@ListBillingGroups' {} a -> s {nextToken = a} :: ListBillingGroups)

-- | Limit the results to billing groups whose names have the given prefix.
listBillingGroups_namePrefixFilter :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_namePrefixFilter = Lens.lens (\ListBillingGroups' {namePrefixFilter} -> namePrefixFilter) (\s@ListBillingGroups' {} a -> s {namePrefixFilter = a} :: ListBillingGroups)

-- | The maximum number of results to return per request.
listBillingGroups_maxResults :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Natural)
listBillingGroups_maxResults = Lens.lens (\ListBillingGroups' {maxResults} -> maxResults) (\s@ListBillingGroups' {} a -> s {maxResults = a} :: ListBillingGroups)

instance Core.AWSPager ListBillingGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBillingGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBillingGroupsResponse_billingGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBillingGroups_nextToken
          Lens..~ rs
          Lens.^? listBillingGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBillingGroups where
  type
    AWSResponse ListBillingGroups =
      ListBillingGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "billingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBillingGroups where
  hashWithSalt _salt ListBillingGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namePrefixFilter
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListBillingGroups where
  rnf ListBillingGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namePrefixFilter
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListBillingGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListBillingGroups where
  toPath = Prelude.const "/billing-groups"

instance Core.ToQuery ListBillingGroups where
  toQuery ListBillingGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "namePrefixFilter" Core.=: namePrefixFilter,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of billing groups.
    billingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBillingGroupsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'billingGroups', 'listBillingGroupsResponse_billingGroups' - The list of billing groups.
--
-- 'httpStatus', 'listBillingGroupsResponse_httpStatus' - The response's http status code.
newListBillingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBillingGroupsResponse
newListBillingGroupsResponse pHttpStatus_ =
  ListBillingGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      billingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listBillingGroupsResponse_nextToken :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe Prelude.Text)
listBillingGroupsResponse_nextToken = Lens.lens (\ListBillingGroupsResponse' {nextToken} -> nextToken) (\s@ListBillingGroupsResponse' {} a -> s {nextToken = a} :: ListBillingGroupsResponse)

-- | The list of billing groups.
listBillingGroupsResponse_billingGroups :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe [GroupNameAndArn])
listBillingGroupsResponse_billingGroups = Lens.lens (\ListBillingGroupsResponse' {billingGroups} -> billingGroups) (\s@ListBillingGroupsResponse' {} a -> s {billingGroups = a} :: ListBillingGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBillingGroupsResponse_httpStatus :: Lens.Lens' ListBillingGroupsResponse Prelude.Int
listBillingGroupsResponse_httpStatus = Lens.lens (\ListBillingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListBillingGroupsResponse' {} a -> s {httpStatus = a} :: ListBillingGroupsResponse)

instance Prelude.NFData ListBillingGroupsResponse where
  rnf ListBillingGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingGroups
      `Prelude.seq` Prelude.rnf httpStatus
