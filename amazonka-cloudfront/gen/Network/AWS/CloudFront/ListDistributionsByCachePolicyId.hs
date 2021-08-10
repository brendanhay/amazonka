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
-- Module      : Network.AWS.CloudFront.ListDistributionsByCachePolicyId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that’s associated with the specified cache policy.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByCachePolicyId
  ( -- * Creating a Request
    ListDistributionsByCachePolicyId (..),
    newListDistributionsByCachePolicyId,

    -- * Request Lenses
    listDistributionsByCachePolicyId_maxItems,
    listDistributionsByCachePolicyId_marker,
    listDistributionsByCachePolicyId_cachePolicyId,

    -- * Destructuring the Response
    ListDistributionsByCachePolicyIdResponse (..),
    newListDistributionsByCachePolicyIdResponse,

    -- * Response Lenses
    listDistributionsByCachePolicyIdResponse_distributionIdList,
    listDistributionsByCachePolicyIdResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDistributionsByCachePolicyId' smart constructor.
data ListDistributionsByCachePolicyId = ListDistributionsByCachePolicyId'
  { -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field’s value to the value of @NextMarker@ from the current
    -- page’s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the cache policy whose associated distribution IDs you want to
    -- list.
    cachePolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByCachePolicyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listDistributionsByCachePolicyId_maxItems' - The maximum number of distribution IDs that you want in the response.
--
-- 'marker', 'listDistributionsByCachePolicyId_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
--
-- 'cachePolicyId', 'listDistributionsByCachePolicyId_cachePolicyId' - The ID of the cache policy whose associated distribution IDs you want to
-- list.
newListDistributionsByCachePolicyId ::
  -- | 'cachePolicyId'
  Prelude.Text ->
  ListDistributionsByCachePolicyId
newListDistributionsByCachePolicyId pCachePolicyId_ =
  ListDistributionsByCachePolicyId'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      cachePolicyId = pCachePolicyId_
    }

-- | The maximum number of distribution IDs that you want in the response.
listDistributionsByCachePolicyId_maxItems :: Lens.Lens' ListDistributionsByCachePolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByCachePolicyId_maxItems = Lens.lens (\ListDistributionsByCachePolicyId' {maxItems} -> maxItems) (\s@ListDistributionsByCachePolicyId' {} a -> s {maxItems = a} :: ListDistributionsByCachePolicyId)

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
listDistributionsByCachePolicyId_marker :: Lens.Lens' ListDistributionsByCachePolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByCachePolicyId_marker = Lens.lens (\ListDistributionsByCachePolicyId' {marker} -> marker) (\s@ListDistributionsByCachePolicyId' {} a -> s {marker = a} :: ListDistributionsByCachePolicyId)

-- | The ID of the cache policy whose associated distribution IDs you want to
-- list.
listDistributionsByCachePolicyId_cachePolicyId :: Lens.Lens' ListDistributionsByCachePolicyId Prelude.Text
listDistributionsByCachePolicyId_cachePolicyId = Lens.lens (\ListDistributionsByCachePolicyId' {cachePolicyId} -> cachePolicyId) (\s@ListDistributionsByCachePolicyId' {} a -> s {cachePolicyId = a} :: ListDistributionsByCachePolicyId)

instance
  Core.AWSRequest
    ListDistributionsByCachePolicyId
  where
  type
    AWSResponse ListDistributionsByCachePolicyId =
      ListDistributionsByCachePolicyIdResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByCachePolicyIdResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionsByCachePolicyId

instance
  Prelude.NFData
    ListDistributionsByCachePolicyId

instance
  Core.ToHeaders
    ListDistributionsByCachePolicyId
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDistributionsByCachePolicyId where
  toPath ListDistributionsByCachePolicyId' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributionsByCachePolicyId/",
        Core.toBS cachePolicyId
      ]

instance
  Core.ToQuery
    ListDistributionsByCachePolicyId
  where
  toQuery ListDistributionsByCachePolicyId' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListDistributionsByCachePolicyIdResponse' smart constructor.
data ListDistributionsByCachePolicyIdResponse = ListDistributionsByCachePolicyIdResponse'
  { -- | A list of distribution IDs.
    distributionIdList :: Prelude.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByCachePolicyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByCachePolicyIdResponse_distributionIdList' - A list of distribution IDs.
--
-- 'httpStatus', 'listDistributionsByCachePolicyIdResponse_httpStatus' - The response's http status code.
newListDistributionsByCachePolicyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByCachePolicyIdResponse
newListDistributionsByCachePolicyIdResponse
  pHttpStatus_ =
    ListDistributionsByCachePolicyIdResponse'
      { distributionIdList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of distribution IDs.
listDistributionsByCachePolicyIdResponse_distributionIdList :: Lens.Lens' ListDistributionsByCachePolicyIdResponse (Prelude.Maybe DistributionIdList)
listDistributionsByCachePolicyIdResponse_distributionIdList = Lens.lens (\ListDistributionsByCachePolicyIdResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByCachePolicyIdResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByCachePolicyIdResponse)

-- | The response's http status code.
listDistributionsByCachePolicyIdResponse_httpStatus :: Lens.Lens' ListDistributionsByCachePolicyIdResponse Prelude.Int
listDistributionsByCachePolicyIdResponse_httpStatus = Lens.lens (\ListDistributionsByCachePolicyIdResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByCachePolicyIdResponse' {} a -> s {httpStatus = a} :: ListDistributionsByCachePolicyIdResponse)

instance
  Prelude.NFData
    ListDistributionsByCachePolicyIdResponse
