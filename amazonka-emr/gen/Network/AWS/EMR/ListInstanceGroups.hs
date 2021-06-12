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
-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceGroups
  ( -- * Creating a Request
    ListInstanceGroups (..),
    newListInstanceGroups,

    -- * Request Lenses
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,

    -- * Destructuring the Response
    ListInstanceGroupsResponse (..),
    newListInstanceGroupsResponse,

    -- * Response Lenses
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroups' smart constructor.
data ListInstanceGroups = ListInstanceGroups'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The identifier of the cluster for which to list the instance groups.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceGroups_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listInstanceGroups_clusterId' - The identifier of the cluster for which to list the instance groups.
newListInstanceGroups ::
  -- | 'clusterId'
  Core.Text ->
  ListInstanceGroups
newListInstanceGroups pClusterId_ =
  ListInstanceGroups'
    { marker = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceGroups_marker :: Lens.Lens' ListInstanceGroups (Core.Maybe Core.Text)
listInstanceGroups_marker = Lens.lens (\ListInstanceGroups' {marker} -> marker) (\s@ListInstanceGroups' {} a -> s {marker = a} :: ListInstanceGroups)

-- | The identifier of the cluster for which to list the instance groups.
listInstanceGroups_clusterId :: Lens.Lens' ListInstanceGroups Core.Text
listInstanceGroups_clusterId = Lens.lens (\ListInstanceGroups' {clusterId} -> clusterId) (\s@ListInstanceGroups' {} a -> s {clusterId = a} :: ListInstanceGroups)

instance Core.AWSPager ListInstanceGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceGroupsResponse_instanceGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstanceGroups_marker
          Lens..~ rs
          Lens.^? listInstanceGroupsResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListInstanceGroups where
  type
    AWSResponse ListInstanceGroups =
      ListInstanceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceGroupsResponse'
            Core.<$> (x Core..?> "InstanceGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstanceGroups

instance Core.NFData ListInstanceGroups

instance Core.ToHeaders ListInstanceGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListInstanceGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInstanceGroups where
  toJSON ListInstanceGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListInstanceGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListInstanceGroups where
  toQuery = Core.const Core.mempty

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'newListInstanceGroupsResponse' smart constructor.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
  { -- | The list of instance groups for the cluster and given filters.
    instanceGroups :: Core.Maybe [InstanceGroup],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceGroups', 'listInstanceGroupsResponse_instanceGroups' - The list of instance groups for the cluster and given filters.
--
-- 'marker', 'listInstanceGroupsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listInstanceGroupsResponse_httpStatus' - The response's http status code.
newListInstanceGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInstanceGroupsResponse
newListInstanceGroupsResponse pHttpStatus_ =
  ListInstanceGroupsResponse'
    { instanceGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of instance groups for the cluster and given filters.
listInstanceGroupsResponse_instanceGroups :: Lens.Lens' ListInstanceGroupsResponse (Core.Maybe [InstanceGroup])
listInstanceGroupsResponse_instanceGroups = Lens.lens (\ListInstanceGroupsResponse' {instanceGroups} -> instanceGroups) (\s@ListInstanceGroupsResponse' {} a -> s {instanceGroups = a} :: ListInstanceGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceGroupsResponse_marker :: Lens.Lens' ListInstanceGroupsResponse (Core.Maybe Core.Text)
listInstanceGroupsResponse_marker = Lens.lens (\ListInstanceGroupsResponse' {marker} -> marker) (\s@ListInstanceGroupsResponse' {} a -> s {marker = a} :: ListInstanceGroupsResponse)

-- | The response's http status code.
listInstanceGroupsResponse_httpStatus :: Lens.Lens' ListInstanceGroupsResponse Core.Int
listInstanceGroupsResponse_httpStatus = Lens.lens (\ListInstanceGroupsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceGroupsResponse' {} a -> s {httpStatus = a} :: ListInstanceGroupsResponse)

instance Core.NFData ListInstanceGroupsResponse
