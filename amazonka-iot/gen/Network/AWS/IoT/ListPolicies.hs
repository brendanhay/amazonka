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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicies operation.
--
-- /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The result page size.
    pageSize :: Core.Maybe Core.Natural,
    -- | Specifies the order for results. If true, the results are returned in
    -- ascending creation order.
    ascendingOrder :: Core.Maybe Core.Bool,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { pageSize = Core.Nothing,
      ascendingOrder = Core.Nothing,
      marker = Core.Nothing
    }

-- | The result page size.
listPolicies_pageSize :: Lens.Lens' ListPolicies (Core.Maybe Core.Natural)
listPolicies_pageSize = Lens.lens (\ListPolicies' {pageSize} -> pageSize) (\s@ListPolicies' {} a -> s {pageSize = a} :: ListPolicies)

-- | Specifies the order for results. If true, the results are returned in
-- ascending creation order.
listPolicies_ascendingOrder :: Lens.Lens' ListPolicies (Core.Maybe Core.Bool)
listPolicies_ascendingOrder = Lens.lens (\ListPolicies' {ascendingOrder} -> ascendingOrder) (\s@ListPolicies' {} a -> s {ascendingOrder = a} :: ListPolicies)

-- | The marker for the next set of results.
listPolicies_marker :: Lens.Lens' ListPolicies (Core.Maybe Core.Text)
listPolicies_marker = Lens.lens (\ListPolicies' {marker} -> marker) (\s@ListPolicies' {} a -> s {marker = a} :: ListPolicies)

instance Core.AWSPager ListPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_policies Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPolicies_marker
          Lens..~ rs
          Lens.^? listPoliciesResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListPolicies where
  type AWSResponse ListPolicies = ListPoliciesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Core.<$> (x Core..?> "policies" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPolicies

instance Core.NFData ListPolicies

instance Core.ToHeaders ListPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPolicies where
  toPath = Core.const "/policies"

instance Core.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder,
        "marker" Core.=: marker
      ]

-- | The output from the ListPolicies operation.
--
-- /See:/ 'newListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | The descriptions of the policies.
    policies :: Core.Maybe [Policy],
    -- | The marker for the next set of results, or null if there are no
    -- additional results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListPoliciesResponse
newListPoliciesResponse pHttpStatus_ =
  ListPoliciesResponse'
    { policies = Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The descriptions of the policies.
listPoliciesResponse_policies :: Lens.Lens' ListPoliciesResponse (Core.Maybe [Policy])
listPoliciesResponse_policies = Lens.lens (\ListPoliciesResponse' {policies} -> policies) (\s@ListPoliciesResponse' {} a -> s {policies = a} :: ListPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results, or null if there are no
-- additional results.
listPoliciesResponse_nextMarker :: Lens.Lens' ListPoliciesResponse (Core.Maybe Core.Text)
listPoliciesResponse_nextMarker = Lens.lens (\ListPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListPoliciesResponse' {} a -> s {nextMarker = a} :: ListPoliciesResponse)

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Core.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Core.NFData ListPoliciesResponse
