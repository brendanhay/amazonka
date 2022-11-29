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
-- Module      : Amazonka.EMR.ListInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EMR.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_marker,
    listInstances_instanceFleetId,
    listInstances_instanceFleetType,
    listInstances_instanceGroupTypes,
    listInstances_instanceGroupId,
    listInstances_instanceStates,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input determines which instances to list.
--
-- /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Prelude.Maybe Prelude.Text,
    -- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
    instanceFleetType :: Prelude.Maybe InstanceFleetType,
    -- | The type of instance group for which to list the instances.
    instanceGroupTypes :: Prelude.Maybe [InstanceGroupType],
    -- | The identifier of the instance group for which to list the instances.
    instanceGroupId :: Prelude.Maybe Prelude.Text,
    -- | A list of instance states that will filter the instances returned with
    -- this request.
    instanceStates :: Prelude.Maybe [InstanceState],
    -- | The identifier of the cluster for which to list the instances.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstances_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'instanceFleetId', 'listInstances_instanceFleetId' - The unique identifier of the instance fleet.
--
-- 'instanceFleetType', 'listInstances_instanceFleetType' - The node type of the instance fleet. For example MASTER, CORE, or TASK.
--
-- 'instanceGroupTypes', 'listInstances_instanceGroupTypes' - The type of instance group for which to list the instances.
--
-- 'instanceGroupId', 'listInstances_instanceGroupId' - The identifier of the instance group for which to list the instances.
--
-- 'instanceStates', 'listInstances_instanceStates' - A list of instance states that will filter the instances returned with
-- this request.
--
-- 'clusterId', 'listInstances_clusterId' - The identifier of the cluster for which to list the instances.
newListInstances ::
  -- | 'clusterId'
  Prelude.Text ->
  ListInstances
newListInstances pClusterId_ =
  ListInstances'
    { marker = Prelude.Nothing,
      instanceFleetId = Prelude.Nothing,
      instanceFleetType = Prelude.Nothing,
      instanceGroupTypes = Prelude.Nothing,
      instanceGroupId = Prelude.Nothing,
      instanceStates = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstances_marker :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_marker = Lens.lens (\ListInstances' {marker} -> marker) (\s@ListInstances' {} a -> s {marker = a} :: ListInstances)

-- | The unique identifier of the instance fleet.
listInstances_instanceFleetId :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_instanceFleetId = Lens.lens (\ListInstances' {instanceFleetId} -> instanceFleetId) (\s@ListInstances' {} a -> s {instanceFleetId = a} :: ListInstances)

-- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
listInstances_instanceFleetType :: Lens.Lens' ListInstances (Prelude.Maybe InstanceFleetType)
listInstances_instanceFleetType = Lens.lens (\ListInstances' {instanceFleetType} -> instanceFleetType) (\s@ListInstances' {} a -> s {instanceFleetType = a} :: ListInstances)

-- | The type of instance group for which to list the instances.
listInstances_instanceGroupTypes :: Lens.Lens' ListInstances (Prelude.Maybe [InstanceGroupType])
listInstances_instanceGroupTypes = Lens.lens (\ListInstances' {instanceGroupTypes} -> instanceGroupTypes) (\s@ListInstances' {} a -> s {instanceGroupTypes = a} :: ListInstances) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the instance group for which to list the instances.
listInstances_instanceGroupId :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_instanceGroupId = Lens.lens (\ListInstances' {instanceGroupId} -> instanceGroupId) (\s@ListInstances' {} a -> s {instanceGroupId = a} :: ListInstances)

-- | A list of instance states that will filter the instances returned with
-- this request.
listInstances_instanceStates :: Lens.Lens' ListInstances (Prelude.Maybe [InstanceState])
listInstances_instanceStates = Lens.lens (\ListInstances' {instanceStates} -> instanceStates) (\s@ListInstances' {} a -> s {instanceStates = a} :: ListInstances) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the cluster for which to list the instances.
listInstances_clusterId :: Lens.Lens' ListInstances Prelude.Text
listInstances_clusterId = Lens.lens (\ListInstances' {clusterId} -> clusterId) (\s@ListInstances' {} a -> s {clusterId = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instances Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstances_marker
          Lens..~ rs
          Lens.^? listInstancesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Prelude.<$> (x Core..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstances where
  hashWithSalt _salt ListInstances' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` instanceFleetId
      `Prelude.hashWithSalt` instanceFleetType
      `Prelude.hashWithSalt` instanceGroupTypes
      `Prelude.hashWithSalt` instanceGroupId
      `Prelude.hashWithSalt` instanceStates
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListInstances where
  rnf ListInstances' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf instanceFleetId
      `Prelude.seq` Prelude.rnf instanceFleetType
      `Prelude.seq` Prelude.rnf instanceGroupTypes
      `Prelude.seq` Prelude.rnf instanceGroupId
      `Prelude.seq` Prelude.rnf instanceStates
      `Prelude.seq` Prelude.rnf clusterId

instance Core.ToHeaders ListInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("InstanceFleetId" Core..=)
              Prelude.<$> instanceFleetId,
            ("InstanceFleetType" Core..=)
              Prelude.<$> instanceFleetType,
            ("InstanceGroupTypes" Core..=)
              Prelude.<$> instanceGroupTypes,
            ("InstanceGroupId" Core..=)
              Prelude.<$> instanceGroupId,
            ("InstanceStates" Core..=)
              Prelude.<$> instanceStates,
            Prelude.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInstances where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the list of instances.
--
-- /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | The list of instances for the cluster and given filters.
    instances :: Prelude.Maybe [Instance],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { instances = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of instances for the cluster and given filters.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Prelude.Maybe [Instance])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
listInstancesResponse_marker :: Lens.Lens' ListInstancesResponse (Prelude.Maybe Prelude.Text)
listInstancesResponse_marker = Lens.lens (\ListInstancesResponse' {marker} -> marker) (\s@ListInstancesResponse' {} a -> s {marker = a} :: ListInstancesResponse)

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Prelude.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Prelude.NFData ListInstancesResponse where
  rnf ListInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
