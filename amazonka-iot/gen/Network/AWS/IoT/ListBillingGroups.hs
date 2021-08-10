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
-- Module      : Network.AWS.IoT.ListBillingGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the billing groups you have created.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListBillingGroups
  ( -- * Creating a Request
    ListBillingGroups (..),
    newListBillingGroups,

    -- * Request Lenses
    listBillingGroups_namePrefixFilter,
    listBillingGroups_nextToken,
    listBillingGroups_maxResults,

    -- * Destructuring the Response
    ListBillingGroupsResponse (..),
    newListBillingGroupsResponse,

    -- * Response Lenses
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { -- | Limit the results to billing groups whose names have the given prefix.
    namePrefixFilter :: Prelude.Maybe Prelude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'namePrefixFilter', 'listBillingGroups_namePrefixFilter' - Limit the results to billing groups whose names have the given prefix.
--
-- 'nextToken', 'listBillingGroups_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listBillingGroups_maxResults' - The maximum number of results to return per request.
newListBillingGroups ::
  ListBillingGroups
newListBillingGroups =
  ListBillingGroups'
    { namePrefixFilter =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Limit the results to billing groups whose names have the given prefix.
listBillingGroups_namePrefixFilter :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_namePrefixFilter = Lens.lens (\ListBillingGroups' {namePrefixFilter} -> namePrefixFilter) (\s@ListBillingGroups' {} a -> s {namePrefixFilter = a} :: ListBillingGroups)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listBillingGroups_nextToken :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_nextToken = Lens.lens (\ListBillingGroups' {nextToken} -> nextToken) (\s@ListBillingGroups' {} a -> s {nextToken = a} :: ListBillingGroups)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            Prelude.<$> (x Core..?> "billingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBillingGroups

instance Prelude.NFData ListBillingGroups

instance Core.ToHeaders ListBillingGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListBillingGroups where
  toPath = Prelude.const "/billing-groups"

instance Core.ToQuery ListBillingGroups where
  toQuery ListBillingGroups' {..} =
    Prelude.mconcat
      [ "namePrefixFilter" Core.=: namePrefixFilter,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { -- | The list of billing groups.
    billingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'billingGroups', 'listBillingGroupsResponse_billingGroups' - The list of billing groups.
--
-- 'nextToken', 'listBillingGroupsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listBillingGroupsResponse_httpStatus' - The response's http status code.
newListBillingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBillingGroupsResponse
newListBillingGroupsResponse pHttpStatus_ =
  ListBillingGroupsResponse'
    { billingGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of billing groups.
listBillingGroupsResponse_billingGroups :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe [GroupNameAndArn])
listBillingGroupsResponse_billingGroups = Lens.lens (\ListBillingGroupsResponse' {billingGroups} -> billingGroups) (\s@ListBillingGroupsResponse' {} a -> s {billingGroups = a} :: ListBillingGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listBillingGroupsResponse_nextToken :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe Prelude.Text)
listBillingGroupsResponse_nextToken = Lens.lens (\ListBillingGroupsResponse' {nextToken} -> nextToken) (\s@ListBillingGroupsResponse' {} a -> s {nextToken = a} :: ListBillingGroupsResponse)

-- | The response's http status code.
listBillingGroupsResponse_httpStatus :: Lens.Lens' ListBillingGroupsResponse Prelude.Int
listBillingGroupsResponse_httpStatus = Lens.lens (\ListBillingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListBillingGroupsResponse' {} a -> s {httpStatus = a} :: ListBillingGroupsResponse)

instance Prelude.NFData ListBillingGroupsResponse
