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
-- Module      : Network.AWS.DAX.DecreaseReplicationFactor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more nodes from a DAX cluster.
--
-- You cannot use @DecreaseReplicationFactor@ to remove the last node in a
-- DAX cluster. If you need to do this, use @DeleteCluster@ instead.
module Network.AWS.DAX.DecreaseReplicationFactor
  ( -- * Creating a Request
    DecreaseReplicationFactor (..),
    newDecreaseReplicationFactor,

    -- * Request Lenses
    decreaseReplicationFactor_availabilityZones,
    decreaseReplicationFactor_nodeIdsToRemove,
    decreaseReplicationFactor_clusterName,
    decreaseReplicationFactor_newReplicationFactor,

    -- * Destructuring the Response
    DecreaseReplicationFactorResponse (..),
    newDecreaseReplicationFactorResponse,

    -- * Response Lenses
    decreaseReplicationFactorResponse_cluster,
    decreaseReplicationFactorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDecreaseReplicationFactor' smart constructor.
data DecreaseReplicationFactor = DecreaseReplicationFactor'
  { -- | The Availability Zone(s) from which to remove nodes.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The unique identifiers of the nodes to be removed from the cluster.
    nodeIdsToRemove :: Core.Maybe [Core.Text],
    -- | The name of the DAX cluster from which you want to remove nodes.
    clusterName :: Core.Text,
    -- | The new number of nodes for the DAX cluster.
    newReplicationFactor' :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseReplicationFactor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'decreaseReplicationFactor_availabilityZones' - The Availability Zone(s) from which to remove nodes.
--
-- 'nodeIdsToRemove', 'decreaseReplicationFactor_nodeIdsToRemove' - The unique identifiers of the nodes to be removed from the cluster.
--
-- 'clusterName', 'decreaseReplicationFactor_clusterName' - The name of the DAX cluster from which you want to remove nodes.
--
-- 'newReplicationFactor'', 'decreaseReplicationFactor_newReplicationFactor' - The new number of nodes for the DAX cluster.
newDecreaseReplicationFactor ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'newReplicationFactor''
  Core.Int ->
  DecreaseReplicationFactor
newDecreaseReplicationFactor
  pClusterName_
  pNewReplicationFactor_ =
    DecreaseReplicationFactor'
      { availabilityZones =
          Core.Nothing,
        nodeIdsToRemove = Core.Nothing,
        clusterName = pClusterName_,
        newReplicationFactor' = pNewReplicationFactor_
      }

-- | The Availability Zone(s) from which to remove nodes.
decreaseReplicationFactor_availabilityZones :: Lens.Lens' DecreaseReplicationFactor (Core.Maybe [Core.Text])
decreaseReplicationFactor_availabilityZones = Lens.lens (\DecreaseReplicationFactor' {availabilityZones} -> availabilityZones) (\s@DecreaseReplicationFactor' {} a -> s {availabilityZones = a} :: DecreaseReplicationFactor) Core.. Lens.mapping Lens._Coerce

-- | The unique identifiers of the nodes to be removed from the cluster.
decreaseReplicationFactor_nodeIdsToRemove :: Lens.Lens' DecreaseReplicationFactor (Core.Maybe [Core.Text])
decreaseReplicationFactor_nodeIdsToRemove = Lens.lens (\DecreaseReplicationFactor' {nodeIdsToRemove} -> nodeIdsToRemove) (\s@DecreaseReplicationFactor' {} a -> s {nodeIdsToRemove = a} :: DecreaseReplicationFactor) Core.. Lens.mapping Lens._Coerce

-- | The name of the DAX cluster from which you want to remove nodes.
decreaseReplicationFactor_clusterName :: Lens.Lens' DecreaseReplicationFactor Core.Text
decreaseReplicationFactor_clusterName = Lens.lens (\DecreaseReplicationFactor' {clusterName} -> clusterName) (\s@DecreaseReplicationFactor' {} a -> s {clusterName = a} :: DecreaseReplicationFactor)

-- | The new number of nodes for the DAX cluster.
decreaseReplicationFactor_newReplicationFactor :: Lens.Lens' DecreaseReplicationFactor Core.Int
decreaseReplicationFactor_newReplicationFactor = Lens.lens (\DecreaseReplicationFactor' {newReplicationFactor'} -> newReplicationFactor') (\s@DecreaseReplicationFactor' {} a -> s {newReplicationFactor' = a} :: DecreaseReplicationFactor)

instance Core.AWSRequest DecreaseReplicationFactor where
  type
    AWSResponse DecreaseReplicationFactor =
      DecreaseReplicationFactorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DecreaseReplicationFactorResponse'
            Core.<$> (x Core..?> "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DecreaseReplicationFactor

instance Core.NFData DecreaseReplicationFactor

instance Core.ToHeaders DecreaseReplicationFactor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DecreaseReplicationFactor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DecreaseReplicationFactor where
  toJSON DecreaseReplicationFactor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AvailabilityZones" Core..=)
              Core.<$> availabilityZones,
            ("NodeIdsToRemove" Core..=) Core.<$> nodeIdsToRemove,
            Core.Just ("ClusterName" Core..= clusterName),
            Core.Just
              ( "NewReplicationFactor"
                  Core..= newReplicationFactor'
              )
          ]
      )

instance Core.ToPath DecreaseReplicationFactor where
  toPath = Core.const "/"

instance Core.ToQuery DecreaseReplicationFactor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDecreaseReplicationFactorResponse' smart constructor.
data DecreaseReplicationFactorResponse = DecreaseReplicationFactorResponse'
  { -- | A description of the DAX cluster, after you have decreased its
    -- replication factor.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseReplicationFactorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'decreaseReplicationFactorResponse_cluster' - A description of the DAX cluster, after you have decreased its
-- replication factor.
--
-- 'httpStatus', 'decreaseReplicationFactorResponse_httpStatus' - The response's http status code.
newDecreaseReplicationFactorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DecreaseReplicationFactorResponse
newDecreaseReplicationFactorResponse pHttpStatus_ =
  DecreaseReplicationFactorResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster, after you have decreased its
-- replication factor.
decreaseReplicationFactorResponse_cluster :: Lens.Lens' DecreaseReplicationFactorResponse (Core.Maybe Cluster)
decreaseReplicationFactorResponse_cluster = Lens.lens (\DecreaseReplicationFactorResponse' {cluster} -> cluster) (\s@DecreaseReplicationFactorResponse' {} a -> s {cluster = a} :: DecreaseReplicationFactorResponse)

-- | The response's http status code.
decreaseReplicationFactorResponse_httpStatus :: Lens.Lens' DecreaseReplicationFactorResponse Core.Int
decreaseReplicationFactorResponse_httpStatus = Lens.lens (\DecreaseReplicationFactorResponse' {httpStatus} -> httpStatus) (\s@DecreaseReplicationFactorResponse' {} a -> s {httpStatus = a} :: DecreaseReplicationFactorResponse)

instance
  Core.NFData
    DecreaseReplicationFactorResponse
