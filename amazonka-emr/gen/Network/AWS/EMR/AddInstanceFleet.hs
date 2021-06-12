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
-- Module      : Network.AWS.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x.
module Network.AWS.EMR.AddInstanceFleet
  ( -- * Creating a Request
    AddInstanceFleet (..),
    newAddInstanceFleet,

    -- * Request Lenses
    addInstanceFleet_clusterId,
    addInstanceFleet_instanceFleet,

    -- * Destructuring the Response
    AddInstanceFleetResponse (..),
    newAddInstanceFleetResponse,

    -- * Response Lenses
    addInstanceFleetResponse_clusterArn,
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { -- | The unique identifier of the cluster.
    clusterId :: Core.Text,
    -- | Specifies the configuration of the instance fleet.
    instanceFleet :: InstanceFleetConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddInstanceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'addInstanceFleet_clusterId' - The unique identifier of the cluster.
--
-- 'instanceFleet', 'addInstanceFleet_instanceFleet' - Specifies the configuration of the instance fleet.
newAddInstanceFleet ::
  -- | 'clusterId'
  Core.Text ->
  -- | 'instanceFleet'
  InstanceFleetConfig ->
  AddInstanceFleet
newAddInstanceFleet pClusterId_ pInstanceFleet_ =
  AddInstanceFleet'
    { clusterId = pClusterId_,
      instanceFleet = pInstanceFleet_
    }

-- | The unique identifier of the cluster.
addInstanceFleet_clusterId :: Lens.Lens' AddInstanceFleet Core.Text
addInstanceFleet_clusterId = Lens.lens (\AddInstanceFleet' {clusterId} -> clusterId) (\s@AddInstanceFleet' {} a -> s {clusterId = a} :: AddInstanceFleet)

-- | Specifies the configuration of the instance fleet.
addInstanceFleet_instanceFleet :: Lens.Lens' AddInstanceFleet InstanceFleetConfig
addInstanceFleet_instanceFleet = Lens.lens (\AddInstanceFleet' {instanceFleet} -> instanceFleet) (\s@AddInstanceFleet' {} a -> s {instanceFleet = a} :: AddInstanceFleet)

instance Core.AWSRequest AddInstanceFleet where
  type
    AWSResponse AddInstanceFleet =
      AddInstanceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceFleetResponse'
            Core.<$> (x Core..?> "ClusterArn")
            Core.<*> (x Core..?> "ClusterId")
            Core.<*> (x Core..?> "InstanceFleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddInstanceFleet

instance Core.NFData AddInstanceFleet

instance Core.ToHeaders AddInstanceFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.AddInstanceFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddInstanceFleet where
  toJSON AddInstanceFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("InstanceFleet" Core..= instanceFleet)
          ]
      )

instance Core.ToPath AddInstanceFleet where
  toPath = Core.const "/"

instance Core.ToQuery AddInstanceFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Core.Maybe Core.Text,
    -- | The unique identifier of the cluster.
    clusterId :: Core.Maybe Core.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddInstanceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'addInstanceFleetResponse_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'clusterId', 'addInstanceFleetResponse_clusterId' - The unique identifier of the cluster.
--
-- 'instanceFleetId', 'addInstanceFleetResponse_instanceFleetId' - The unique identifier of the instance fleet.
--
-- 'httpStatus', 'addInstanceFleetResponse_httpStatus' - The response's http status code.
newAddInstanceFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddInstanceFleetResponse
newAddInstanceFleetResponse pHttpStatus_ =
  AddInstanceFleetResponse'
    { clusterArn =
        Core.Nothing,
      clusterId = Core.Nothing,
      instanceFleetId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the cluster.
addInstanceFleetResponse_clusterArn :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Core.Text)
addInstanceFleetResponse_clusterArn = Lens.lens (\AddInstanceFleetResponse' {clusterArn} -> clusterArn) (\s@AddInstanceFleetResponse' {} a -> s {clusterArn = a} :: AddInstanceFleetResponse)

-- | The unique identifier of the cluster.
addInstanceFleetResponse_clusterId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Core.Text)
addInstanceFleetResponse_clusterId = Lens.lens (\AddInstanceFleetResponse' {clusterId} -> clusterId) (\s@AddInstanceFleetResponse' {} a -> s {clusterId = a} :: AddInstanceFleetResponse)

-- | The unique identifier of the instance fleet.
addInstanceFleetResponse_instanceFleetId :: Lens.Lens' AddInstanceFleetResponse (Core.Maybe Core.Text)
addInstanceFleetResponse_instanceFleetId = Lens.lens (\AddInstanceFleetResponse' {instanceFleetId} -> instanceFleetId) (\s@AddInstanceFleetResponse' {} a -> s {instanceFleetId = a} :: AddInstanceFleetResponse)

-- | The response's http status code.
addInstanceFleetResponse_httpStatus :: Lens.Lens' AddInstanceFleetResponse Core.Int
addInstanceFleetResponse_httpStatus = Lens.lens (\AddInstanceFleetResponse' {httpStatus} -> httpStatus) (\s@AddInstanceFleetResponse' {} a -> s {httpStatus = a} :: AddInstanceFleetResponse)

instance Core.NFData AddInstanceFleetResponse
