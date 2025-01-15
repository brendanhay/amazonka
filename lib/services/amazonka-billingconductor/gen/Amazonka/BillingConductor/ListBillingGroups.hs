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
-- Module      : Amazonka.BillingConductor.ListBillingGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A paginated call to retrieve a list of billing groups for the given
-- billing period. If you don\'t provide a billing group, the current
-- billing period is used.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListBillingGroups
  ( -- * Creating a Request
    ListBillingGroups (..),
    newListBillingGroups,

    -- * Request Lenses
    listBillingGroups_billingPeriod,
    listBillingGroups_filters,
    listBillingGroups_maxResults,
    listBillingGroups_nextToken,

    -- * Destructuring the Response
    ListBillingGroupsResponse (..),
    newListBillingGroupsResponse,

    -- * Response Lenses
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { -- | The preferred billing period to get billing groups.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | A @ListBillingGroupsFilter@ that specifies the billing group and pricing
    -- plan to retrieve billing group information.
    filters :: Prelude.Maybe ListBillingGroupsFilter,
    -- | The maximum number of billing groups to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used on subsequent calls to get billing
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'billingPeriod', 'listBillingGroups_billingPeriod' - The preferred billing period to get billing groups.
--
-- 'filters', 'listBillingGroups_filters' - A @ListBillingGroupsFilter@ that specifies the billing group and pricing
-- plan to retrieve billing group information.
--
-- 'maxResults', 'listBillingGroups_maxResults' - The maximum number of billing groups to retrieve.
--
-- 'nextToken', 'listBillingGroups_nextToken' - The pagination token that\'s used on subsequent calls to get billing
-- groups.
newListBillingGroups ::
  ListBillingGroups
newListBillingGroups =
  ListBillingGroups'
    { billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The preferred billing period to get billing groups.
listBillingGroups_billingPeriod :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_billingPeriod = Lens.lens (\ListBillingGroups' {billingPeriod} -> billingPeriod) (\s@ListBillingGroups' {} a -> s {billingPeriod = a} :: ListBillingGroups)

-- | A @ListBillingGroupsFilter@ that specifies the billing group and pricing
-- plan to retrieve billing group information.
listBillingGroups_filters :: Lens.Lens' ListBillingGroups (Prelude.Maybe ListBillingGroupsFilter)
listBillingGroups_filters = Lens.lens (\ListBillingGroups' {filters} -> filters) (\s@ListBillingGroups' {} a -> s {filters = a} :: ListBillingGroups)

-- | The maximum number of billing groups to retrieve.
listBillingGroups_maxResults :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Natural)
listBillingGroups_maxResults = Lens.lens (\ListBillingGroups' {maxResults} -> maxResults) (\s@ListBillingGroups' {} a -> s {maxResults = a} :: ListBillingGroups)

-- | The pagination token that\'s used on subsequent calls to get billing
-- groups.
listBillingGroups_nextToken :: Lens.Lens' ListBillingGroups (Prelude.Maybe Prelude.Text)
listBillingGroups_nextToken = Lens.lens (\ListBillingGroups' {nextToken} -> nextToken) (\s@ListBillingGroups' {} a -> s {nextToken = a} :: ListBillingGroups)

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
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            Prelude.<$> (x Data..?> "BillingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBillingGroups where
  hashWithSalt _salt ListBillingGroups' {..} =
    _salt
      `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBillingGroups where
  rnf ListBillingGroups' {..} =
    Prelude.rnf billingPeriod `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders ListBillingGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBillingGroups where
  toJSON ListBillingGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListBillingGroups where
  toPath = Prelude.const "/list-billing-groups"

instance Data.ToQuery ListBillingGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { -- | A list of @BillingGroupListElement@ retrieved.
    billingGroups :: Prelude.Maybe [BillingGroupListElement],
    -- | The pagination token that\'s used on subsequent calls to get billing
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroups', 'listBillingGroupsResponse_billingGroups' - A list of @BillingGroupListElement@ retrieved.
--
-- 'nextToken', 'listBillingGroupsResponse_nextToken' - The pagination token that\'s used on subsequent calls to get billing
-- groups.
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

-- | A list of @BillingGroupListElement@ retrieved.
listBillingGroupsResponse_billingGroups :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe [BillingGroupListElement])
listBillingGroupsResponse_billingGroups = Lens.lens (\ListBillingGroupsResponse' {billingGroups} -> billingGroups) (\s@ListBillingGroupsResponse' {} a -> s {billingGroups = a} :: ListBillingGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used on subsequent calls to get billing
-- groups.
listBillingGroupsResponse_nextToken :: Lens.Lens' ListBillingGroupsResponse (Prelude.Maybe Prelude.Text)
listBillingGroupsResponse_nextToken = Lens.lens (\ListBillingGroupsResponse' {nextToken} -> nextToken) (\s@ListBillingGroupsResponse' {} a -> s {nextToken = a} :: ListBillingGroupsResponse)

-- | The response's http status code.
listBillingGroupsResponse_httpStatus :: Lens.Lens' ListBillingGroupsResponse Prelude.Int
listBillingGroupsResponse_httpStatus = Lens.lens (\ListBillingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListBillingGroupsResponse' {} a -> s {httpStatus = a} :: ListBillingGroupsResponse)

instance Prelude.NFData ListBillingGroupsResponse where
  rnf ListBillingGroupsResponse' {..} =
    Prelude.rnf billingGroups `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
