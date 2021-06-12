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
-- Module      : Network.AWS.DAX.IncreaseReplicationFactor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more nodes to a DAX cluster.
module Network.AWS.DAX.IncreaseReplicationFactor
  ( -- * Creating a Request
    IncreaseReplicationFactor (..),
    newIncreaseReplicationFactor,

    -- * Request Lenses
    increaseReplicationFactor_availabilityZones,
    increaseReplicationFactor_clusterName,
    increaseReplicationFactor_newReplicationFactor,

    -- * Destructuring the Response
    IncreaseReplicationFactorResponse (..),
    newIncreaseReplicationFactorResponse,

    -- * Response Lenses
    increaseReplicationFactorResponse_cluster,
    increaseReplicationFactorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newIncreaseReplicationFactor' smart constructor.
data IncreaseReplicationFactor = IncreaseReplicationFactor'
  { -- | The Availability Zones (AZs) in which the cluster nodes will be created.
    -- All nodes belonging to the cluster are placed in these Availability
    -- Zones. Use this parameter if you want to distribute the nodes across
    -- multiple AZs.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The name of the DAX cluster that will receive additional nodes.
    clusterName :: Core.Text,
    -- | The new number of nodes for the DAX cluster.
    newReplicationFactor' :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IncreaseReplicationFactor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'increaseReplicationFactor_availabilityZones' - The Availability Zones (AZs) in which the cluster nodes will be created.
-- All nodes belonging to the cluster are placed in these Availability
-- Zones. Use this parameter if you want to distribute the nodes across
-- multiple AZs.
--
-- 'clusterName', 'increaseReplicationFactor_clusterName' - The name of the DAX cluster that will receive additional nodes.
--
-- 'newReplicationFactor'', 'increaseReplicationFactor_newReplicationFactor' - The new number of nodes for the DAX cluster.
newIncreaseReplicationFactor ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'newReplicationFactor''
  Core.Int ->
  IncreaseReplicationFactor
newIncreaseReplicationFactor
  pClusterName_
  pNewReplicationFactor_ =
    IncreaseReplicationFactor'
      { availabilityZones =
          Core.Nothing,
        clusterName = pClusterName_,
        newReplicationFactor' = pNewReplicationFactor_
      }

-- | The Availability Zones (AZs) in which the cluster nodes will be created.
-- All nodes belonging to the cluster are placed in these Availability
-- Zones. Use this parameter if you want to distribute the nodes across
-- multiple AZs.
increaseReplicationFactor_availabilityZones :: Lens.Lens' IncreaseReplicationFactor (Core.Maybe [Core.Text])
increaseReplicationFactor_availabilityZones = Lens.lens (\IncreaseReplicationFactor' {availabilityZones} -> availabilityZones) (\s@IncreaseReplicationFactor' {} a -> s {availabilityZones = a} :: IncreaseReplicationFactor) Core.. Lens.mapping Lens._Coerce

-- | The name of the DAX cluster that will receive additional nodes.
increaseReplicationFactor_clusterName :: Lens.Lens' IncreaseReplicationFactor Core.Text
increaseReplicationFactor_clusterName = Lens.lens (\IncreaseReplicationFactor' {clusterName} -> clusterName) (\s@IncreaseReplicationFactor' {} a -> s {clusterName = a} :: IncreaseReplicationFactor)

-- | The new number of nodes for the DAX cluster.
increaseReplicationFactor_newReplicationFactor :: Lens.Lens' IncreaseReplicationFactor Core.Int
increaseReplicationFactor_newReplicationFactor = Lens.lens (\IncreaseReplicationFactor' {newReplicationFactor'} -> newReplicationFactor') (\s@IncreaseReplicationFactor' {} a -> s {newReplicationFactor' = a} :: IncreaseReplicationFactor)

instance Core.AWSRequest IncreaseReplicationFactor where
  type
    AWSResponse IncreaseReplicationFactor =
      IncreaseReplicationFactorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          IncreaseReplicationFactorResponse'
            Core.<$> (x Core..?> "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable IncreaseReplicationFactor

instance Core.NFData IncreaseReplicationFactor

instance Core.ToHeaders IncreaseReplicationFactor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.IncreaseReplicationFactor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON IncreaseReplicationFactor where
  toJSON IncreaseReplicationFactor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AvailabilityZones" Core..=)
              Core.<$> availabilityZones,
            Core.Just ("ClusterName" Core..= clusterName),
            Core.Just
              ( "NewReplicationFactor"
                  Core..= newReplicationFactor'
              )
          ]
      )

instance Core.ToPath IncreaseReplicationFactor where
  toPath = Core.const "/"

instance Core.ToQuery IncreaseReplicationFactor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newIncreaseReplicationFactorResponse' smart constructor.
data IncreaseReplicationFactorResponse = IncreaseReplicationFactorResponse'
  { -- | A description of the DAX cluster. with its new replication factor.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IncreaseReplicationFactorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'increaseReplicationFactorResponse_cluster' - A description of the DAX cluster. with its new replication factor.
--
-- 'httpStatus', 'increaseReplicationFactorResponse_httpStatus' - The response's http status code.
newIncreaseReplicationFactorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  IncreaseReplicationFactorResponse
newIncreaseReplicationFactorResponse pHttpStatus_ =
  IncreaseReplicationFactorResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster. with its new replication factor.
increaseReplicationFactorResponse_cluster :: Lens.Lens' IncreaseReplicationFactorResponse (Core.Maybe Cluster)
increaseReplicationFactorResponse_cluster = Lens.lens (\IncreaseReplicationFactorResponse' {cluster} -> cluster) (\s@IncreaseReplicationFactorResponse' {} a -> s {cluster = a} :: IncreaseReplicationFactorResponse)

-- | The response's http status code.
increaseReplicationFactorResponse_httpStatus :: Lens.Lens' IncreaseReplicationFactorResponse Core.Int
increaseReplicationFactorResponse_httpStatus = Lens.lens (\IncreaseReplicationFactorResponse' {httpStatus} -> httpStatus) (\s@IncreaseReplicationFactorResponse' {} a -> s {httpStatus = a} :: IncreaseReplicationFactorResponse)

instance
  Core.NFData
    IncreaseReplicationFactorResponse
