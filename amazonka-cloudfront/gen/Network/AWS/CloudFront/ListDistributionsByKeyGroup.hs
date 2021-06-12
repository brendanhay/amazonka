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
-- Module      : Network.AWS.CloudFront.ListDistributionsByKeyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that references the specified key group.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByKeyGroup
  ( -- * Creating a Request
    ListDistributionsByKeyGroup (..),
    newListDistributionsByKeyGroup,

    -- * Request Lenses
    listDistributionsByKeyGroup_maxItems,
    listDistributionsByKeyGroup_marker,
    listDistributionsByKeyGroup_keyGroupId,

    -- * Destructuring the Response
    ListDistributionsByKeyGroupResponse (..),
    newListDistributionsByKeyGroupResponse,

    -- * Response Lenses
    listDistributionsByKeyGroupResponse_distributionIdList,
    listDistributionsByKeyGroupResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDistributionsByKeyGroup' smart constructor.
data ListDistributionsByKeyGroup = ListDistributionsByKeyGroup'
  { -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Core.Maybe Core.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field’s value to the value of @NextMarker@ from the current
    -- page’s response.
    marker :: Core.Maybe Core.Text,
    -- | The ID of the key group whose associated distribution IDs you are
    -- listing.
    keyGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDistributionsByKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listDistributionsByKeyGroup_maxItems' - The maximum number of distribution IDs that you want in the response.
--
-- 'marker', 'listDistributionsByKeyGroup_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
--
-- 'keyGroupId', 'listDistributionsByKeyGroup_keyGroupId' - The ID of the key group whose associated distribution IDs you are
-- listing.
newListDistributionsByKeyGroup ::
  -- | 'keyGroupId'
  Core.Text ->
  ListDistributionsByKeyGroup
newListDistributionsByKeyGroup pKeyGroupId_ =
  ListDistributionsByKeyGroup'
    { maxItems =
        Core.Nothing,
      marker = Core.Nothing,
      keyGroupId = pKeyGroupId_
    }

-- | The maximum number of distribution IDs that you want in the response.
listDistributionsByKeyGroup_maxItems :: Lens.Lens' ListDistributionsByKeyGroup (Core.Maybe Core.Text)
listDistributionsByKeyGroup_maxItems = Lens.lens (\ListDistributionsByKeyGroup' {maxItems} -> maxItems) (\s@ListDistributionsByKeyGroup' {} a -> s {maxItems = a} :: ListDistributionsByKeyGroup)

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
listDistributionsByKeyGroup_marker :: Lens.Lens' ListDistributionsByKeyGroup (Core.Maybe Core.Text)
listDistributionsByKeyGroup_marker = Lens.lens (\ListDistributionsByKeyGroup' {marker} -> marker) (\s@ListDistributionsByKeyGroup' {} a -> s {marker = a} :: ListDistributionsByKeyGroup)

-- | The ID of the key group whose associated distribution IDs you are
-- listing.
listDistributionsByKeyGroup_keyGroupId :: Lens.Lens' ListDistributionsByKeyGroup Core.Text
listDistributionsByKeyGroup_keyGroupId = Lens.lens (\ListDistributionsByKeyGroup' {keyGroupId} -> keyGroupId) (\s@ListDistributionsByKeyGroup' {} a -> s {keyGroupId = a} :: ListDistributionsByKeyGroup)

instance Core.AWSRequest ListDistributionsByKeyGroup where
  type
    AWSResponse ListDistributionsByKeyGroup =
      ListDistributionsByKeyGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByKeyGroupResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDistributionsByKeyGroup

instance Core.NFData ListDistributionsByKeyGroup

instance Core.ToHeaders ListDistributionsByKeyGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDistributionsByKeyGroup where
  toPath ListDistributionsByKeyGroup' {..} =
    Core.mconcat
      [ "/2020-05-31/distributionsByKeyGroupId/",
        Core.toBS keyGroupId
      ]

instance Core.ToQuery ListDistributionsByKeyGroup where
  toQuery ListDistributionsByKeyGroup' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListDistributionsByKeyGroupResponse' smart constructor.
data ListDistributionsByKeyGroupResponse = ListDistributionsByKeyGroupResponse'
  { distributionIdList :: Core.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDistributionsByKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByKeyGroupResponse_distributionIdList' - Undocumented member.
--
-- 'httpStatus', 'listDistributionsByKeyGroupResponse_httpStatus' - The response's http status code.
newListDistributionsByKeyGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDistributionsByKeyGroupResponse
newListDistributionsByKeyGroupResponse pHttpStatus_ =
  ListDistributionsByKeyGroupResponse'
    { distributionIdList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listDistributionsByKeyGroupResponse_distributionIdList :: Lens.Lens' ListDistributionsByKeyGroupResponse (Core.Maybe DistributionIdList)
listDistributionsByKeyGroupResponse_distributionIdList = Lens.lens (\ListDistributionsByKeyGroupResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByKeyGroupResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByKeyGroupResponse)

-- | The response's http status code.
listDistributionsByKeyGroupResponse_httpStatus :: Lens.Lens' ListDistributionsByKeyGroupResponse Core.Int
listDistributionsByKeyGroupResponse_httpStatus = Lens.lens (\ListDistributionsByKeyGroupResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByKeyGroupResponse' {} a -> s {httpStatus = a} :: ListDistributionsByKeyGroupResponse)

instance
  Core.NFData
    ListDistributionsByKeyGroupResponse
