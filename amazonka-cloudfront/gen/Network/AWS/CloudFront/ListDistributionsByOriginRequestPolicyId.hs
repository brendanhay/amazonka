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
-- Module      : Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that’s associated with the specified origin request policy.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
  ( -- * Creating a Request
    ListDistributionsByOriginRequestPolicyId (..),
    newListDistributionsByOriginRequestPolicyId,

    -- * Request Lenses
    listDistributionsByOriginRequestPolicyId_maxItems,
    listDistributionsByOriginRequestPolicyId_marker,
    listDistributionsByOriginRequestPolicyId_originRequestPolicyId,

    -- * Destructuring the Response
    ListDistributionsByOriginRequestPolicyIdResponse (..),
    newListDistributionsByOriginRequestPolicyIdResponse,

    -- * Response Lenses
    listDistributionsByOriginRequestPolicyIdResponse_distributionIdList,
    listDistributionsByOriginRequestPolicyIdResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDistributionsByOriginRequestPolicyId' smart constructor.
data ListDistributionsByOriginRequestPolicyId = ListDistributionsByOriginRequestPolicyId'
  { -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Core.Maybe Core.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field’s value to the value of @NextMarker@ from the current
    -- page’s response.
    marker :: Core.Maybe Core.Text,
    -- | The ID of the origin request policy whose associated distribution IDs
    -- you want to list.
    originRequestPolicyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDistributionsByOriginRequestPolicyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listDistributionsByOriginRequestPolicyId_maxItems' - The maximum number of distribution IDs that you want in the response.
--
-- 'marker', 'listDistributionsByOriginRequestPolicyId_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
--
-- 'originRequestPolicyId', 'listDistributionsByOriginRequestPolicyId_originRequestPolicyId' - The ID of the origin request policy whose associated distribution IDs
-- you want to list.
newListDistributionsByOriginRequestPolicyId ::
  -- | 'originRequestPolicyId'
  Core.Text ->
  ListDistributionsByOriginRequestPolicyId
newListDistributionsByOriginRequestPolicyId
  pOriginRequestPolicyId_ =
    ListDistributionsByOriginRequestPolicyId'
      { maxItems =
          Core.Nothing,
        marker = Core.Nothing,
        originRequestPolicyId =
          pOriginRequestPolicyId_
      }

-- | The maximum number of distribution IDs that you want in the response.
listDistributionsByOriginRequestPolicyId_maxItems :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Core.Maybe Core.Text)
listDistributionsByOriginRequestPolicyId_maxItems = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {maxItems} -> maxItems) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {maxItems = a} :: ListDistributionsByOriginRequestPolicyId)

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
listDistributionsByOriginRequestPolicyId_marker :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Core.Maybe Core.Text)
listDistributionsByOriginRequestPolicyId_marker = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {marker} -> marker) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {marker = a} :: ListDistributionsByOriginRequestPolicyId)

-- | The ID of the origin request policy whose associated distribution IDs
-- you want to list.
listDistributionsByOriginRequestPolicyId_originRequestPolicyId :: Lens.Lens' ListDistributionsByOriginRequestPolicyId Core.Text
listDistributionsByOriginRequestPolicyId_originRequestPolicyId = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {originRequestPolicyId} -> originRequestPolicyId) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {originRequestPolicyId = a} :: ListDistributionsByOriginRequestPolicyId)

instance
  Core.AWSRequest
    ListDistributionsByOriginRequestPolicyId
  where
  type
    AWSResponse
      ListDistributionsByOriginRequestPolicyId =
      ListDistributionsByOriginRequestPolicyIdResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByOriginRequestPolicyIdResponse'
            Core.<$> (Core.parseXML x)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListDistributionsByOriginRequestPolicyId

instance
  Core.NFData
    ListDistributionsByOriginRequestPolicyId

instance
  Core.ToHeaders
    ListDistributionsByOriginRequestPolicyId
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ListDistributionsByOriginRequestPolicyId
  where
  toPath ListDistributionsByOriginRequestPolicyId' {..} =
    Core.mconcat
      [ "/2020-05-31/distributionsByOriginRequestPolicyId/",
        Core.toBS originRequestPolicyId
      ]

instance
  Core.ToQuery
    ListDistributionsByOriginRequestPolicyId
  where
  toQuery ListDistributionsByOriginRequestPolicyId' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListDistributionsByOriginRequestPolicyIdResponse' smart constructor.
data ListDistributionsByOriginRequestPolicyIdResponse = ListDistributionsByOriginRequestPolicyIdResponse'
  { -- | A list of distribution IDs.
    distributionIdList :: Core.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDistributionsByOriginRequestPolicyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByOriginRequestPolicyIdResponse_distributionIdList' - A list of distribution IDs.
--
-- 'httpStatus', 'listDistributionsByOriginRequestPolicyIdResponse_httpStatus' - The response's http status code.
newListDistributionsByOriginRequestPolicyIdResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDistributionsByOriginRequestPolicyIdResponse
newListDistributionsByOriginRequestPolicyIdResponse
  pHttpStatus_ =
    ListDistributionsByOriginRequestPolicyIdResponse'
      { distributionIdList =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of distribution IDs.
listDistributionsByOriginRequestPolicyIdResponse_distributionIdList :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse (Core.Maybe DistributionIdList)
listDistributionsByOriginRequestPolicyIdResponse_distributionIdList = Lens.lens (\ListDistributionsByOriginRequestPolicyIdResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByOriginRequestPolicyIdResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByOriginRequestPolicyIdResponse)

-- | The response's http status code.
listDistributionsByOriginRequestPolicyIdResponse_httpStatus :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse Core.Int
listDistributionsByOriginRequestPolicyIdResponse_httpStatus = Lens.lens (\ListDistributionsByOriginRequestPolicyIdResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByOriginRequestPolicyIdResponse' {} a -> s {httpStatus = a} :: ListDistributionsByOriginRequestPolicyIdResponse)

instance
  Core.NFData
    ListDistributionsByOriginRequestPolicyIdResponse
