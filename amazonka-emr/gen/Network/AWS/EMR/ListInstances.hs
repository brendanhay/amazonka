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
-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information for all active EC2 instances and EC2 instances
-- terminated in the last 30 days, up to a maximum of 2,000. EC2 instances
-- in any of the following states are considered active:
-- AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_instanceFleetType,
    listInstances_instanceGroupId,
    listInstances_instanceStates,
    listInstances_instanceFleetId,
    listInstances_instanceGroupTypes,
    listInstances_marker,
    listInstances_clusterId,

    -- * Destructuring the Response
    ListInstancesResponse (..),
    newListInstancesResponse,

    -- * Response Lenses
    listInstancesResponse_instances,
    listInstancesResponse_marker,
    listInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which instances to list.
--
-- /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
    instanceFleetType :: Core.Maybe InstanceFleetType,
    -- | The identifier of the instance group for which to list the instances.
    instanceGroupId :: Core.Maybe Core.Text,
    -- | A list of instance states that will filter the instances returned with
    -- this request.
    instanceStates :: Core.Maybe [InstanceState],
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Core.Maybe Core.Text,
    -- | The type of instance group for which to list the instances.
    instanceGroupTypes :: Core.Maybe [InstanceGroupType],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The identifier of the cluster for which to list the instances.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFleetType', 'listInstances_instanceFleetType' - The node type of the instance fleet. For example MASTER, CORE, or TASK.
--
-- 'instanceGroupId', 'listInstances_instanceGroupId' - The identifier of the instance group for which to list the instances.
--
-- 'instanceStates', 'listInstances_instanceStates' - A list of instance states that will filter the instances returned with
-- this request.
--
-- 'instanceFleetId', 'listInstances_instanceFleetId' - The unique identifier of the instance fleet.
--
-- 'instanceGroupTypes', 'listInstances_instanceGroupTypes' - The type of instance group for which to list the instances.
--
-- 'marker', 'listInstances_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listInstances_clusterId' - The identifier of the cluster for which to list the instances.
newListInstances ::
  -- | 'clusterId'
  Core.Text ->
  ListInstances
newListInstances pClusterId_ =
  ListInstances'
    { instanceFleetType = Core.Nothing,
      instanceGroupId = Core.Nothing,
      instanceStates = Core.Nothing,
      instanceFleetId = Core.Nothing,
      instanceGroupTypes = Core.Nothing,
      marker = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
listInstances_instanceFleetType :: Lens.Lens' ListInstances (Core.Maybe InstanceFleetType)
listInstances_instanceFleetType = Lens.lens (\ListInstances' {instanceFleetType} -> instanceFleetType) (\s@ListInstances' {} a -> s {instanceFleetType = a} :: ListInstances)

-- | The identifier of the instance group for which to list the instances.
listInstances_instanceGroupId :: Lens.Lens' ListInstances (Core.Maybe Core.Text)
listInstances_instanceGroupId = Lens.lens (\ListInstances' {instanceGroupId} -> instanceGroupId) (\s@ListInstances' {} a -> s {instanceGroupId = a} :: ListInstances)

-- | A list of instance states that will filter the instances returned with
-- this request.
listInstances_instanceStates :: Lens.Lens' ListInstances (Core.Maybe [InstanceState])
listInstances_instanceStates = Lens.lens (\ListInstances' {instanceStates} -> instanceStates) (\s@ListInstances' {} a -> s {instanceStates = a} :: ListInstances) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier of the instance fleet.
listInstances_instanceFleetId :: Lens.Lens' ListInstances (Core.Maybe Core.Text)
listInstances_instanceFleetId = Lens.lens (\ListInstances' {instanceFleetId} -> instanceFleetId) (\s@ListInstances' {} a -> s {instanceFleetId = a} :: ListInstances)

-- | The type of instance group for which to list the instances.
listInstances_instanceGroupTypes :: Lens.Lens' ListInstances (Core.Maybe [InstanceGroupType])
listInstances_instanceGroupTypes = Lens.lens (\ListInstances' {instanceGroupTypes} -> instanceGroupTypes) (\s@ListInstances' {} a -> s {instanceGroupTypes = a} :: ListInstances) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listInstances_marker :: Lens.Lens' ListInstances (Core.Maybe Core.Text)
listInstances_marker = Lens.lens (\ListInstances' {marker} -> marker) (\s@ListInstances' {} a -> s {marker = a} :: ListInstances)

-- | The identifier of the cluster for which to list the instances.
listInstances_clusterId :: Lens.Lens' ListInstances Core.Text
listInstances_clusterId = Lens.lens (\ListInstances' {clusterId} -> clusterId) (\s@ListInstances' {} a -> s {clusterId = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instances Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstances_marker
          Lens..~ rs
          Lens.^? listInstancesResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Core.<$> (x Core..?> "Instances" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstances

instance Core.NFData ListInstances

instance Core.ToHeaders ListInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceFleetType" Core..=)
              Core.<$> instanceFleetType,
            ("InstanceGroupId" Core..=) Core.<$> instanceGroupId,
            ("InstanceStates" Core..=) Core.<$> instanceStates,
            ("InstanceFleetId" Core..=) Core.<$> instanceFleetId,
            ("InstanceGroupTypes" Core..=)
              Core.<$> instanceGroupTypes,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListInstances where
  toQuery = Core.const Core.mempty

-- | This output contains the list of instances.
--
-- /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | The list of instances for the cluster and given filters.
    instances :: Core.Maybe [Instance],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'listInstancesResponse_instances' - The list of instances for the cluster and given filters.
--
-- 'marker', 'listInstancesResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listInstancesResponse_httpStatus' - The response's http status code.
newListInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { instances = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of instances for the cluster and given filters.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Core.Maybe [Instance])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listInstancesResponse_marker :: Lens.Lens' ListInstancesResponse (Core.Maybe Core.Text)
listInstancesResponse_marker = Lens.lens (\ListInstancesResponse' {marker} -> marker) (\s@ListInstancesResponse' {} a -> s {marker = a} :: ListInstancesResponse)

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Core.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Core.NFData ListInstancesResponse
