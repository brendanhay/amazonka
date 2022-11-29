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
-- Module      : Amazonka.IoT.ListPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your policies.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListPolicies>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListPolicies
  ( -- * Creating a Request
    ListPolicies (..),
    newListPolicies,

    -- * Request Lenses
    listPolicies_marker,
    listPolicies_pageSize,
    listPolicies_ascendingOrder,

    -- * Destructuring the Response
    ListPoliciesResponse (..),
    newListPoliciesResponse,

    -- * Response Lenses
    listPoliciesResponse_policies,
    listPoliciesResponse_nextMarker,
    listPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListPolicies operation.
--
-- /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the order for results. If true, the results are returned in
    -- ascending creation order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPolicies_marker' - The marker for the next set of results.
--
-- 'pageSize', 'listPolicies_pageSize' - The result page size.
--
-- 'ascendingOrder', 'listPolicies_ascendingOrder' - Specifies the order for results. If true, the results are returned in
-- ascending creation order.
newListPolicies ::
  ListPolicies
newListPolicies =
  ListPolicies'
    { marker = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | The marker for the next set of results.
listPolicies_marker :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Text)
listPolicies_marker = Lens.lens (\ListPolicies' {marker} -> marker) (\s@ListPolicies' {} a -> s {marker = a} :: ListPolicies)

-- | The result page size.
listPolicies_pageSize :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Natural)
listPolicies_pageSize = Lens.lens (\ListPolicies' {pageSize} -> pageSize) (\s@ListPolicies' {} a -> s {pageSize = a} :: ListPolicies)

-- | Specifies the order for results. If true, the results are returned in
-- ascending creation order.
listPolicies_ascendingOrder :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Bool)
listPolicies_ascendingOrder = Lens.lens (\ListPolicies' {ascendingOrder} -> ascendingOrder) (\s@ListPolicies' {} a -> s {ascendingOrder = a} :: ListPolicies)

instance Core.AWSPager ListPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_policies Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicies_marker
          Lens..~ rs
          Lens.^? listPoliciesResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListPolicies where
  type AWSResponse ListPolicies = ListPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Prelude.<$> (x Core..?> "policies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicies where
  hashWithSalt _salt ListPolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` ascendingOrder

instance Prelude.NFData ListPolicies where
  rnf ListPolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf ascendingOrder

instance Core.ToHeaders ListPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPolicies where
  toPath = Prelude.const "/policies"

instance Core.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listPoliciesResponse_policies = Lens.lens (\ListPoliciesResponse' {policies} -> policies) (\s@ListPoliciesResponse' {} a -> s {policies = a} :: ListPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the next set of results, or null if there are no
-- additional results.
listPoliciesResponse_nextMarker :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe Prelude.Text)
listPoliciesResponse_nextMarker = Lens.lens (\ListPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListPoliciesResponse' {} a -> s {nextMarker = a} :: ListPoliciesResponse)

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Prelude.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Prelude.NFData ListPoliciesResponse where
  rnf ListPoliciesResponse' {..} =
    Prelude.rnf policies
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
