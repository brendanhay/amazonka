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
-- Module      : Network.AWS.IoT.ListAttachedPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified thing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAttachedPolicies
  ( -- * Creating a Request
    ListAttachedPolicies (..),
    newListAttachedPolicies,

    -- * Request Lenses
    listAttachedPolicies_pageSize,
    listAttachedPolicies_recursive,
    listAttachedPolicies_marker,
    listAttachedPolicies_target,

    -- * Destructuring the Response
    ListAttachedPoliciesResponse (..),
    newListAttachedPoliciesResponse,

    -- * Response Lenses
    listAttachedPoliciesResponse_policies,
    listAttachedPoliciesResponse_nextMarker,
    listAttachedPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAttachedPolicies' smart constructor.
data ListAttachedPolicies = ListAttachedPolicies'
  { -- | The maximum number of results to be returned per request.
    pageSize :: Core.Maybe Core.Natural,
    -- | When true, recursively list attached policies.
    recursive :: Core.Maybe Core.Bool,
    -- | The token to retrieve the next set of results.
    marker :: Core.Maybe Core.Text,
    -- | The group or principal for which the policies will be listed. Valid
    -- principals are CertificateArn
    -- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
    -- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
    -- (/region/:/id/).
    target :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listAttachedPolicies_pageSize' - The maximum number of results to be returned per request.
--
-- 'recursive', 'listAttachedPolicies_recursive' - When true, recursively list attached policies.
--
-- 'marker', 'listAttachedPolicies_marker' - The token to retrieve the next set of results.
--
-- 'target', 'listAttachedPolicies_target' - The group or principal for which the policies will be listed. Valid
-- principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
newListAttachedPolicies ::
  -- | 'target'
  Core.Text ->
  ListAttachedPolicies
newListAttachedPolicies pTarget_ =
  ListAttachedPolicies'
    { pageSize = Core.Nothing,
      recursive = Core.Nothing,
      marker = Core.Nothing,
      target = pTarget_
    }

-- | The maximum number of results to be returned per request.
listAttachedPolicies_pageSize :: Lens.Lens' ListAttachedPolicies (Core.Maybe Core.Natural)
listAttachedPolicies_pageSize = Lens.lens (\ListAttachedPolicies' {pageSize} -> pageSize) (\s@ListAttachedPolicies' {} a -> s {pageSize = a} :: ListAttachedPolicies)

-- | When true, recursively list attached policies.
listAttachedPolicies_recursive :: Lens.Lens' ListAttachedPolicies (Core.Maybe Core.Bool)
listAttachedPolicies_recursive = Lens.lens (\ListAttachedPolicies' {recursive} -> recursive) (\s@ListAttachedPolicies' {} a -> s {recursive = a} :: ListAttachedPolicies)

-- | The token to retrieve the next set of results.
listAttachedPolicies_marker :: Lens.Lens' ListAttachedPolicies (Core.Maybe Core.Text)
listAttachedPolicies_marker = Lens.lens (\ListAttachedPolicies' {marker} -> marker) (\s@ListAttachedPolicies' {} a -> s {marker = a} :: ListAttachedPolicies)

-- | The group or principal for which the policies will be listed. Valid
-- principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
listAttachedPolicies_target :: Lens.Lens' ListAttachedPolicies Core.Text
listAttachedPolicies_target = Lens.lens (\ListAttachedPolicies' {target} -> target) (\s@ListAttachedPolicies' {} a -> s {target = a} :: ListAttachedPolicies)

instance Core.AWSPager ListAttachedPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedPoliciesResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttachedPoliciesResponse_policies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAttachedPolicies_marker
          Lens..~ rs
          Lens.^? listAttachedPoliciesResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListAttachedPolicies where
  type
    AWSResponse ListAttachedPolicies =
      ListAttachedPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedPoliciesResponse'
            Core.<$> (x Core..?> "policies" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAttachedPolicies

instance Core.NFData ListAttachedPolicies

instance Core.ToHeaders ListAttachedPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListAttachedPolicies where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath ListAttachedPolicies where
  toPath ListAttachedPolicies' {..} =
    Core.mconcat
      ["/attached-policies/", Core.toBS target]

instance Core.ToQuery ListAttachedPolicies where
  toQuery ListAttachedPolicies' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "recursive" Core.=: recursive,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListAttachedPoliciesResponse' smart constructor.
data ListAttachedPoliciesResponse = ListAttachedPoliciesResponse'
  { -- | The policies.
    policies :: Core.Maybe [Policy],
    -- | The token to retrieve the next set of results, or \`\`null\`\` if there
    -- are no more results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'listAttachedPoliciesResponse_policies' - The policies.
--
-- 'nextMarker', 'listAttachedPoliciesResponse_nextMarker' - The token to retrieve the next set of results, or \`\`null\`\` if there
-- are no more results.
--
-- 'httpStatus', 'listAttachedPoliciesResponse_httpStatus' - The response's http status code.
newListAttachedPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAttachedPoliciesResponse
newListAttachedPoliciesResponse pHttpStatus_ =
  ListAttachedPoliciesResponse'
    { policies =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policies.
listAttachedPoliciesResponse_policies :: Lens.Lens' ListAttachedPoliciesResponse (Core.Maybe [Policy])
listAttachedPoliciesResponse_policies = Lens.lens (\ListAttachedPoliciesResponse' {policies} -> policies) (\s@ListAttachedPoliciesResponse' {} a -> s {policies = a} :: ListAttachedPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to retrieve the next set of results, or \`\`null\`\` if there
-- are no more results.
listAttachedPoliciesResponse_nextMarker :: Lens.Lens' ListAttachedPoliciesResponse (Core.Maybe Core.Text)
listAttachedPoliciesResponse_nextMarker = Lens.lens (\ListAttachedPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListAttachedPoliciesResponse' {} a -> s {nextMarker = a} :: ListAttachedPoliciesResponse)

-- | The response's http status code.
listAttachedPoliciesResponse_httpStatus :: Lens.Lens' ListAttachedPoliciesResponse Core.Int
listAttachedPoliciesResponse_httpStatus = Lens.lens (\ListAttachedPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedPoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedPoliciesResponse)

instance Core.NFData ListAttachedPoliciesResponse
