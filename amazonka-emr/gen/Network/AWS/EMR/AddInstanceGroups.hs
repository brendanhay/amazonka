{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more instance groups to a running cluster.
module Network.AWS.EMR.AddInstanceGroups
  ( -- * Creating a Request
    AddInstanceGroups (..),
    newAddInstanceGroups,

    -- * Request Lenses
    addInstanceGroups_instanceGroups,
    addInstanceGroups_jobFlowId,

    -- * Destructuring the Response
    AddInstanceGroupsResponse (..),
    newAddInstanceGroupsResponse,

    -- * Response Lenses
    addInstanceGroupsResponse_clusterArn,
    addInstanceGroupsResponse_instanceGroupIds,
    addInstanceGroupsResponse_jobFlowId,
    addInstanceGroupsResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to an AddInstanceGroups call.
--
-- /See:/ 'newAddInstanceGroups' smart constructor.
data AddInstanceGroups = AddInstanceGroups'
  { -- | Instance groups to add.
    instanceGroups :: [InstanceGroupConfig],
    -- | Job flow in which to add the instance groups.
    jobFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddInstanceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceGroups', 'addInstanceGroups_instanceGroups' - Instance groups to add.
--
-- 'jobFlowId', 'addInstanceGroups_jobFlowId' - Job flow in which to add the instance groups.
newAddInstanceGroups ::
  -- | 'jobFlowId'
  Prelude.Text ->
  AddInstanceGroups
newAddInstanceGroups pJobFlowId_ =
  AddInstanceGroups'
    { instanceGroups = Prelude.mempty,
      jobFlowId = pJobFlowId_
    }

-- | Instance groups to add.
addInstanceGroups_instanceGroups :: Lens.Lens' AddInstanceGroups [InstanceGroupConfig]
addInstanceGroups_instanceGroups = Lens.lens (\AddInstanceGroups' {instanceGroups} -> instanceGroups) (\s@AddInstanceGroups' {} a -> s {instanceGroups = a} :: AddInstanceGroups) Prelude.. Prelude._Coerce

-- | Job flow in which to add the instance groups.
addInstanceGroups_jobFlowId :: Lens.Lens' AddInstanceGroups Prelude.Text
addInstanceGroups_jobFlowId = Lens.lens (\AddInstanceGroups' {jobFlowId} -> jobFlowId) (\s@AddInstanceGroups' {} a -> s {jobFlowId = a} :: AddInstanceGroups)

instance Prelude.AWSRequest AddInstanceGroups where
  type Rs AddInstanceGroups = AddInstanceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceGroupsResponse'
            Prelude.<$> (x Prelude..?> "ClusterArn")
            Prelude.<*> ( x Prelude..?> "InstanceGroupIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "JobFlowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddInstanceGroups

instance Prelude.NFData AddInstanceGroups

instance Prelude.ToHeaders AddInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.AddInstanceGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddInstanceGroups where
  toJSON AddInstanceGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InstanceGroups" Prelude..= instanceGroups),
            Prelude.Just ("JobFlowId" Prelude..= jobFlowId)
          ]
      )

instance Prelude.ToPath AddInstanceGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddInstanceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | Output from an AddInstanceGroups call.
--
-- /See:/ 'newAddInstanceGroupsResponse' smart constructor.
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | Instance group IDs of the newly created instance groups.
    instanceGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The job flow ID in which the instance groups are added.
    jobFlowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'addInstanceGroupsResponse_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'instanceGroupIds', 'addInstanceGroupsResponse_instanceGroupIds' - Instance group IDs of the newly created instance groups.
--
-- 'jobFlowId', 'addInstanceGroupsResponse_jobFlowId' - The job flow ID in which the instance groups are added.
--
-- 'httpStatus', 'addInstanceGroupsResponse_httpStatus' - The response's http status code.
newAddInstanceGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddInstanceGroupsResponse
newAddInstanceGroupsResponse pHttpStatus_ =
  AddInstanceGroupsResponse'
    { clusterArn =
        Prelude.Nothing,
      instanceGroupIds = Prelude.Nothing,
      jobFlowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the cluster.
addInstanceGroupsResponse_clusterArn :: Lens.Lens' AddInstanceGroupsResponse (Prelude.Maybe Prelude.Text)
addInstanceGroupsResponse_clusterArn = Lens.lens (\AddInstanceGroupsResponse' {clusterArn} -> clusterArn) (\s@AddInstanceGroupsResponse' {} a -> s {clusterArn = a} :: AddInstanceGroupsResponse)

-- | Instance group IDs of the newly created instance groups.
addInstanceGroupsResponse_instanceGroupIds :: Lens.Lens' AddInstanceGroupsResponse (Prelude.Maybe [Prelude.Text])
addInstanceGroupsResponse_instanceGroupIds = Lens.lens (\AddInstanceGroupsResponse' {instanceGroupIds} -> instanceGroupIds) (\s@AddInstanceGroupsResponse' {} a -> s {instanceGroupIds = a} :: AddInstanceGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The job flow ID in which the instance groups are added.
addInstanceGroupsResponse_jobFlowId :: Lens.Lens' AddInstanceGroupsResponse (Prelude.Maybe Prelude.Text)
addInstanceGroupsResponse_jobFlowId = Lens.lens (\AddInstanceGroupsResponse' {jobFlowId} -> jobFlowId) (\s@AddInstanceGroupsResponse' {} a -> s {jobFlowId = a} :: AddInstanceGroupsResponse)

-- | The response's http status code.
addInstanceGroupsResponse_httpStatus :: Lens.Lens' AddInstanceGroupsResponse Prelude.Int
addInstanceGroupsResponse_httpStatus = Lens.lens (\AddInstanceGroupsResponse' {httpStatus} -> httpStatus) (\s@AddInstanceGroupsResponse' {} a -> s {httpStatus = a} :: AddInstanceGroupsResponse)

instance Prelude.NFData AddInstanceGroupsResponse
