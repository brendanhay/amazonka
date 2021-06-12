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
-- Module      : Network.AWS.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceFleets
  ( -- * Creating a Request
    ListInstanceFleets (..),
    newListInstanceFleets,

    -- * Request Lenses
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,

    -- * Destructuring the Response
    ListInstanceFleetsResponse (..),
    newListInstanceFleetsResponse,

    -- * Response Lenses
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The unique identifier of the cluster.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceFleets_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listInstanceFleets_clusterId' - The unique identifier of the cluster.
newListInstanceFleets ::
  -- | 'clusterId'
  Core.Text ->
  ListInstanceFleets
newListInstanceFleets pClusterId_ =
  ListInstanceFleets'
    { marker = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceFleets_marker :: Lens.Lens' ListInstanceFleets (Core.Maybe Core.Text)
listInstanceFleets_marker = Lens.lens (\ListInstanceFleets' {marker} -> marker) (\s@ListInstanceFleets' {} a -> s {marker = a} :: ListInstanceFleets)

-- | The unique identifier of the cluster.
listInstanceFleets_clusterId :: Lens.Lens' ListInstanceFleets Core.Text
listInstanceFleets_clusterId = Lens.lens (\ListInstanceFleets' {clusterId} -> clusterId) (\s@ListInstanceFleets' {} a -> s {clusterId = a} :: ListInstanceFleets)

instance Core.AWSPager ListInstanceFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceFleetsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceFleetsResponse_instanceFleets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstanceFleets_marker
          Lens..~ rs
          Lens.^? listInstanceFleetsResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListInstanceFleets where
  type
    AWSResponse ListInstanceFleets =
      ListInstanceFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceFleetsResponse'
            Core.<$> (x Core..?> "InstanceFleets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstanceFleets

instance Core.NFData ListInstanceFleets

instance Core.ToHeaders ListInstanceFleets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListInstanceFleets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInstanceFleets where
  toJSON ListInstanceFleets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListInstanceFleets where
  toPath = Core.const "/"

instance Core.ToQuery ListInstanceFleets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { -- | The list of instance fleets for the cluster and given filters.
    instanceFleets :: Core.Maybe [InstanceFleet],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFleets', 'listInstanceFleetsResponse_instanceFleets' - The list of instance fleets for the cluster and given filters.
--
-- 'marker', 'listInstanceFleetsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listInstanceFleetsResponse_httpStatus' - The response's http status code.
newListInstanceFleetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInstanceFleetsResponse
newListInstanceFleetsResponse pHttpStatus_ =
  ListInstanceFleetsResponse'
    { instanceFleets =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of instance fleets for the cluster and given filters.
listInstanceFleetsResponse_instanceFleets :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe [InstanceFleet])
listInstanceFleetsResponse_instanceFleets = Lens.lens (\ListInstanceFleetsResponse' {instanceFleets} -> instanceFleets) (\s@ListInstanceFleetsResponse' {} a -> s {instanceFleets = a} :: ListInstanceFleetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceFleetsResponse_marker :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe Core.Text)
listInstanceFleetsResponse_marker = Lens.lens (\ListInstanceFleetsResponse' {marker} -> marker) (\s@ListInstanceFleetsResponse' {} a -> s {marker = a} :: ListInstanceFleetsResponse)

-- | The response's http status code.
listInstanceFleetsResponse_httpStatus :: Lens.Lens' ListInstanceFleetsResponse Core.Int
listInstanceFleetsResponse_httpStatus = Lens.lens (\ListInstanceFleetsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceFleetsResponse' {} a -> s {httpStatus = a} :: ListInstanceFleetsResponse)

instance Core.NFData ListInstanceFleetsResponse
