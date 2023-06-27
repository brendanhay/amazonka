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
-- Module      : Amazonka.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more instance groups to a running cluster.
module Amazonka.EMR.AddInstanceGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to an AddInstanceGroups call.
--
-- /See:/ 'newAddInstanceGroups' smart constructor.
data AddInstanceGroups = AddInstanceGroups'
  { -- | Instance groups to add.
    instanceGroups :: [InstanceGroupConfig],
    -- | Job flow in which to add the instance groups.
    jobFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
addInstanceGroups_instanceGroups = Lens.lens (\AddInstanceGroups' {instanceGroups} -> instanceGroups) (\s@AddInstanceGroups' {} a -> s {instanceGroups = a} :: AddInstanceGroups) Prelude.. Lens.coerced

-- | Job flow in which to add the instance groups.
addInstanceGroups_jobFlowId :: Lens.Lens' AddInstanceGroups Prelude.Text
addInstanceGroups_jobFlowId = Lens.lens (\AddInstanceGroups' {jobFlowId} -> jobFlowId) (\s@AddInstanceGroups' {} a -> s {jobFlowId = a} :: AddInstanceGroups)

instance Core.AWSRequest AddInstanceGroups where
  type
    AWSResponse AddInstanceGroups =
      AddInstanceGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceGroupsResponse'
            Prelude.<$> (x Data..?> "ClusterArn")
            Prelude.<*> ( x
                            Data..?> "InstanceGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "JobFlowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddInstanceGroups where
  hashWithSalt _salt AddInstanceGroups' {..} =
    _salt
      `Prelude.hashWithSalt` instanceGroups
      `Prelude.hashWithSalt` jobFlowId

instance Prelude.NFData AddInstanceGroups where
  rnf AddInstanceGroups' {..} =
    Prelude.rnf instanceGroups
      `Prelude.seq` Prelude.rnf jobFlowId

instance Data.ToHeaders AddInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.AddInstanceGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddInstanceGroups where
  toJSON AddInstanceGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InstanceGroups" Data..= instanceGroups),
            Prelude.Just ("JobFlowId" Data..= jobFlowId)
          ]
      )

instance Data.ToPath AddInstanceGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery AddInstanceGroups where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
addInstanceGroupsResponse_instanceGroupIds = Lens.lens (\AddInstanceGroupsResponse' {instanceGroupIds} -> instanceGroupIds) (\s@AddInstanceGroupsResponse' {} a -> s {instanceGroupIds = a} :: AddInstanceGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The job flow ID in which the instance groups are added.
addInstanceGroupsResponse_jobFlowId :: Lens.Lens' AddInstanceGroupsResponse (Prelude.Maybe Prelude.Text)
addInstanceGroupsResponse_jobFlowId = Lens.lens (\AddInstanceGroupsResponse' {jobFlowId} -> jobFlowId) (\s@AddInstanceGroupsResponse' {} a -> s {jobFlowId = a} :: AddInstanceGroupsResponse)

-- | The response's http status code.
addInstanceGroupsResponse_httpStatus :: Lens.Lens' AddInstanceGroupsResponse Prelude.Int
addInstanceGroupsResponse_httpStatus = Lens.lens (\AddInstanceGroupsResponse' {httpStatus} -> httpStatus) (\s@AddInstanceGroupsResponse' {} a -> s {httpStatus = a} :: AddInstanceGroupsResponse)

instance Prelude.NFData AddInstanceGroupsResponse where
  rnf AddInstanceGroupsResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf instanceGroupIds
      `Prelude.seq` Prelude.rnf jobFlowId
      `Prelude.seq` Prelude.rnf httpStatus
