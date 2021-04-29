{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.ListPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your policies.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPolicies
  ( -- * Creating a Request
    ListPolicies (..),
    newListPolicies,

    -- * Request Lenses
    listPolicies_pageSize,
    listPolicies_ascendingOrder,
    listPolicies_marker,

    -- * Destructuring the Response
    ListPoliciesResponse (..),
    newListPoliciesResponse,

    -- * Response Lenses
    listPoliciesResponse_policies,
    listPoliciesResponse_nextMarker,
    listPoliciesResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicies operation.
--
-- /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the order for results. If true, the results are returned in
    -- ascending creation order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listPolicies_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listPolicies_ascendingOrder' - Specifies the order for results. If true, the results are returned in
-- ascending creation order.
--
-- 'marker', 'listPolicies_marker' - The marker for the next set of results.
newListPolicies ::
  ListPolicies
newListPolicies =
  ListPolicies'
    { pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The result page size.
listPolicies_pageSize :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Natural)
listPolicies_pageSize = Lens.lens (\ListPolicies' {pageSize} -> pageSize) (\s@ListPolicies' {} a -> s {pageSize = a} :: ListPolicies)

-- | Specifies the order for results. If true, the results are returned in
-- ascending creation order.
listPolicies_ascendingOrder :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Bool)
listPolicies_ascendingOrder = Lens.lens (\ListPolicies' {ascendingOrder} -> ascendingOrder) (\s@ListPolicies' {} a -> s {ascendingOrder = a} :: ListPolicies)

-- | The marker for the next set of results.
listPolicies_marker :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Text)
listPolicies_marker = Lens.lens (\ListPolicies' {marker} -> marker) (\s@ListPolicies' {} a -> s {marker = a} :: ListPolicies)

instance Pager.AWSPager ListPolicies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPoliciesResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listPoliciesResponse_policies Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPolicies_marker
          Lens..~ rs
          Lens.^? listPoliciesResponse_nextMarker Prelude.. Lens._Just

instance Prelude.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Prelude.<$> (x Prelude..?> "policies" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicies

instance Prelude.NFData ListPolicies

instance Prelude.ToHeaders ListPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListPolicies where
  toPath = Prelude.const "/policies"

instance Prelude.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Prelude.mconcat
      [ "pageSize" Prelude.=: pageSize,
        "isAscendingOrder" Prelude.=: ascendingOrder,
        "marker" Prelude.=: marker
      ]

-- | The output from the ListPolicies operation.
--
-- /See:/ 'newListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | The descriptions of the policies.
    policies :: Prelude.Maybe [Policy],
    -- | The marker for the next set of results, or null if there are no
    -- additional results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'listPoliciesResponse_policies' - The descriptions of the policies.
--
-- 'nextMarker', 'listPoliciesResponse_nextMarker' - The marker for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listPoliciesResponse_httpStatus' - The response's http status code.
newListPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPoliciesResponse
newListPoliciesResponse pHttpStatus_ =
  ListPoliciesResponse'
    { policies = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The descriptions of the policies.
listPoliciesResponse_policies :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe [Policy])
listPoliciesResponse_policies = Lens.lens (\ListPoliciesResponse' {policies} -> policies) (\s@ListPoliciesResponse' {} a -> s {policies = a} :: ListPoliciesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The marker for the next set of results, or null if there are no
-- additional results.
listPoliciesResponse_nextMarker :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe Prelude.Text)
listPoliciesResponse_nextMarker = Lens.lens (\ListPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListPoliciesResponse' {} a -> s {nextMarker = a} :: ListPoliciesResponse)

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Prelude.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Prelude.NFData ListPoliciesResponse
