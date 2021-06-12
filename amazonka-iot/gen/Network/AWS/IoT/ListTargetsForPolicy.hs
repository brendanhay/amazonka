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
-- Module      : Network.AWS.IoT.ListTargetsForPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List targets for the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForPolicy
  ( -- * Creating a Request
    ListTargetsForPolicy (..),
    newListTargetsForPolicy,

    -- * Request Lenses
    listTargetsForPolicy_pageSize,
    listTargetsForPolicy_marker,
    listTargetsForPolicy_policyName,

    -- * Destructuring the Response
    ListTargetsForPolicyResponse (..),
    newListTargetsForPolicyResponse,

    -- * Response Lenses
    listTargetsForPolicyResponse_nextMarker,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The maximum number of results to return at one time.
    pageSize :: Core.Maybe Core.Natural,
    -- | A marker used to get the next set of results.
    marker :: Core.Maybe Core.Text,
    -- | The policy name.
    policyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTargetsForPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listTargetsForPolicy_pageSize' - The maximum number of results to return at one time.
--
-- 'marker', 'listTargetsForPolicy_marker' - A marker used to get the next set of results.
--
-- 'policyName', 'listTargetsForPolicy_policyName' - The policy name.
newListTargetsForPolicy ::
  -- | 'policyName'
  Core.Text ->
  ListTargetsForPolicy
newListTargetsForPolicy pPolicyName_ =
  ListTargetsForPolicy'
    { pageSize = Core.Nothing,
      marker = Core.Nothing,
      policyName = pPolicyName_
    }

-- | The maximum number of results to return at one time.
listTargetsForPolicy_pageSize :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Core.Natural)
listTargetsForPolicy_pageSize = Lens.lens (\ListTargetsForPolicy' {pageSize} -> pageSize) (\s@ListTargetsForPolicy' {} a -> s {pageSize = a} :: ListTargetsForPolicy)

-- | A marker used to get the next set of results.
listTargetsForPolicy_marker :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Core.Text)
listTargetsForPolicy_marker = Lens.lens (\ListTargetsForPolicy' {marker} -> marker) (\s@ListTargetsForPolicy' {} a -> s {marker = a} :: ListTargetsForPolicy)

-- | The policy name.
listTargetsForPolicy_policyName :: Lens.Lens' ListTargetsForPolicy Core.Text
listTargetsForPolicy_policyName = Lens.lens (\ListTargetsForPolicy' {policyName} -> policyName) (\s@ListTargetsForPolicy' {} a -> s {policyName = a} :: ListTargetsForPolicy)

instance Core.AWSPager ListTargetsForPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_targets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTargetsForPolicy_marker
          Lens..~ rs
          Lens.^? listTargetsForPolicyResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListTargetsForPolicy where
  type
    AWSResponse ListTargetsForPolicy =
      ListTargetsForPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Core.<$> (x Core..?> "nextMarker")
            Core.<*> (x Core..?> "targets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTargetsForPolicy

instance Core.NFData ListTargetsForPolicy

instance Core.ToHeaders ListTargetsForPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListTargetsForPolicy where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath ListTargetsForPolicy where
  toPath ListTargetsForPolicy' {..} =
    Core.mconcat
      ["/policy-targets/", Core.toBS policyName]

instance Core.ToQuery ListTargetsForPolicy where
  toQuery ListTargetsForPolicy' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The policy targets.
    targets :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTargetsForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listTargetsForPolicyResponse_nextMarker' - A marker used to get the next set of results.
--
-- 'targets', 'listTargetsForPolicyResponse_targets' - The policy targets.
--
-- 'httpStatus', 'listTargetsForPolicyResponse_httpStatus' - The response's http status code.
newListTargetsForPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTargetsForPolicyResponse
newListTargetsForPolicyResponse pHttpStatus_ =
  ListTargetsForPolicyResponse'
    { nextMarker =
        Core.Nothing,
      targets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A marker used to get the next set of results.
listTargetsForPolicyResponse_nextMarker :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe Core.Text)
listTargetsForPolicyResponse_nextMarker = Lens.lens (\ListTargetsForPolicyResponse' {nextMarker} -> nextMarker) (\s@ListTargetsForPolicyResponse' {} a -> s {nextMarker = a} :: ListTargetsForPolicyResponse)

-- | The policy targets.
listTargetsForPolicyResponse_targets :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe [Core.Text])
listTargetsForPolicyResponse_targets = Lens.lens (\ListTargetsForPolicyResponse' {targets} -> targets) (\s@ListTargetsForPolicyResponse' {} a -> s {targets = a} :: ListTargetsForPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTargetsForPolicyResponse_httpStatus :: Lens.Lens' ListTargetsForPolicyResponse Core.Int
listTargetsForPolicyResponse_httpStatus = Lens.lens (\ListTargetsForPolicyResponse' {httpStatus} -> httpStatus) (\s@ListTargetsForPolicyResponse' {} a -> s {httpStatus = a} :: ListTargetsForPolicyResponse)

instance Core.NFData ListTargetsForPolicyResponse
