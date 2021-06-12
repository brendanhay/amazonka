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
-- Module      : Network.AWS.CloudFront.ListCachePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of cache policies.
--
-- You can optionally apply a filter to return only the managed policies
-- created by AWS, or only the custom policies created in your AWS account.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListCachePolicies
  ( -- * Creating a Request
    ListCachePolicies (..),
    newListCachePolicies,

    -- * Request Lenses
    listCachePolicies_type,
    listCachePolicies_maxItems,
    listCachePolicies_marker,

    -- * Destructuring the Response
    ListCachePoliciesResponse (..),
    newListCachePoliciesResponse,

    -- * Response Lenses
    listCachePoliciesResponse_cachePolicyList,
    listCachePoliciesResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCachePolicies' smart constructor.
data ListCachePolicies = ListCachePolicies'
  { -- | A filter to return only the specified kinds of cache policies. Valid
    -- values are:
    --
    -- -   @managed@ – Returns only the managed policies created by AWS.
    --
    -- -   @custom@ – Returns only the custom policies created in your AWS
    --     account.
    type' :: Core.Maybe CachePolicyType,
    -- | The maximum number of cache policies that you want in the response.
    maxItems :: Core.Maybe Core.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of cache policies. The response includes cache policies in the
    -- list that occur after the marker. To get the next page of the list, set
    -- this field’s value to the value of @NextMarker@ from the current page’s
    -- response.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCachePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'listCachePolicies_type' - A filter to return only the specified kinds of cache policies. Valid
-- values are:
--
-- -   @managed@ – Returns only the managed policies created by AWS.
--
-- -   @custom@ – Returns only the custom policies created in your AWS
--     account.
--
-- 'maxItems', 'listCachePolicies_maxItems' - The maximum number of cache policies that you want in the response.
--
-- 'marker', 'listCachePolicies_marker' - Use this field when paginating results to indicate where to begin in
-- your list of cache policies. The response includes cache policies in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
newListCachePolicies ::
  ListCachePolicies
newListCachePolicies =
  ListCachePolicies'
    { type' = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | A filter to return only the specified kinds of cache policies. Valid
-- values are:
--
-- -   @managed@ – Returns only the managed policies created by AWS.
--
-- -   @custom@ – Returns only the custom policies created in your AWS
--     account.
listCachePolicies_type :: Lens.Lens' ListCachePolicies (Core.Maybe CachePolicyType)
listCachePolicies_type = Lens.lens (\ListCachePolicies' {type'} -> type') (\s@ListCachePolicies' {} a -> s {type' = a} :: ListCachePolicies)

-- | The maximum number of cache policies that you want in the response.
listCachePolicies_maxItems :: Lens.Lens' ListCachePolicies (Core.Maybe Core.Text)
listCachePolicies_maxItems = Lens.lens (\ListCachePolicies' {maxItems} -> maxItems) (\s@ListCachePolicies' {} a -> s {maxItems = a} :: ListCachePolicies)

-- | Use this field when paginating results to indicate where to begin in
-- your list of cache policies. The response includes cache policies in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
listCachePolicies_marker :: Lens.Lens' ListCachePolicies (Core.Maybe Core.Text)
listCachePolicies_marker = Lens.lens (\ListCachePolicies' {marker} -> marker) (\s@ListCachePolicies' {} a -> s {marker = a} :: ListCachePolicies)

instance Core.AWSRequest ListCachePolicies where
  type
    AWSResponse ListCachePolicies =
      ListCachePoliciesResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListCachePoliciesResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCachePolicies

instance Core.NFData ListCachePolicies

instance Core.ToHeaders ListCachePolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListCachePolicies where
  toPath = Core.const "/2020-05-31/cache-policy"

instance Core.ToQuery ListCachePolicies where
  toQuery ListCachePolicies' {..} =
    Core.mconcat
      [ "Type" Core.=: type',
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListCachePoliciesResponse' smart constructor.
data ListCachePoliciesResponse = ListCachePoliciesResponse'
  { -- | A list of cache policies.
    cachePolicyList :: Core.Maybe CachePolicyList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCachePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicyList', 'listCachePoliciesResponse_cachePolicyList' - A list of cache policies.
--
-- 'httpStatus', 'listCachePoliciesResponse_httpStatus' - The response's http status code.
newListCachePoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCachePoliciesResponse
newListCachePoliciesResponse pHttpStatus_ =
  ListCachePoliciesResponse'
    { cachePolicyList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache policies.
listCachePoliciesResponse_cachePolicyList :: Lens.Lens' ListCachePoliciesResponse (Core.Maybe CachePolicyList)
listCachePoliciesResponse_cachePolicyList = Lens.lens (\ListCachePoliciesResponse' {cachePolicyList} -> cachePolicyList) (\s@ListCachePoliciesResponse' {} a -> s {cachePolicyList = a} :: ListCachePoliciesResponse)

-- | The response's http status code.
listCachePoliciesResponse_httpStatus :: Lens.Lens' ListCachePoliciesResponse Core.Int
listCachePoliciesResponse_httpStatus = Lens.lens (\ListCachePoliciesResponse' {httpStatus} -> httpStatus) (\s@ListCachePoliciesResponse' {} a -> s {httpStatus = a} :: ListCachePoliciesResponse)

instance Core.NFData ListCachePoliciesResponse
