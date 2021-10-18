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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newIncreaseReplicationFactor' smart constructor.
data IncreaseReplicationFactor = IncreaseReplicationFactor'
  { -- | The Availability Zones (AZs) in which the cluster nodes will be created.
    -- All nodes belonging to the cluster are placed in these Availability
    -- Zones. Use this parameter if you want to distribute the nodes across
    -- multiple AZs.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DAX cluster that will receive additional nodes.
    clusterName :: Prelude.Text,
    -- | The new number of nodes for the DAX cluster.
    newReplicationFactor' :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'newReplicationFactor''
  Prelude.Int ->
  IncreaseReplicationFactor
newIncreaseReplicationFactor
  pClusterName_
  pNewReplicationFactor_ =
    IncreaseReplicationFactor'
      { availabilityZones =
          Prelude.Nothing,
        clusterName = pClusterName_,
        newReplicationFactor' = pNewReplicationFactor_
      }

-- | The Availability Zones (AZs) in which the cluster nodes will be created.
-- All nodes belonging to the cluster are placed in these Availability
-- Zones. Use this parameter if you want to distribute the nodes across
-- multiple AZs.
increaseReplicationFactor_availabilityZones :: Lens.Lens' IncreaseReplicationFactor (Prelude.Maybe [Prelude.Text])
increaseReplicationFactor_availabilityZones = Lens.lens (\IncreaseReplicationFactor' {availabilityZones} -> availabilityZones) (\s@IncreaseReplicationFactor' {} a -> s {availabilityZones = a} :: IncreaseReplicationFactor) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the DAX cluster that will receive additional nodes.
increaseReplicationFactor_clusterName :: Lens.Lens' IncreaseReplicationFactor Prelude.Text
increaseReplicationFactor_clusterName = Lens.lens (\IncreaseReplicationFactor' {clusterName} -> clusterName) (\s@IncreaseReplicationFactor' {} a -> s {clusterName = a} :: IncreaseReplicationFactor)

-- | The new number of nodes for the DAX cluster.
increaseReplicationFactor_newReplicationFactor :: Lens.Lens' IncreaseReplicationFactor Prelude.Int
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
            Prelude.<$> (x Core..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IncreaseReplicationFactor

instance Prelude.NFData IncreaseReplicationFactor

instance Core.ToHeaders IncreaseReplicationFactor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.IncreaseReplicationFactor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON IncreaseReplicationFactor where
  toJSON IncreaseReplicationFactor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            Prelude.Just ("ClusterName" Core..= clusterName),
            Prelude.Just
              ( "NewReplicationFactor"
                  Core..= newReplicationFactor'
              )
          ]
      )

instance Core.ToPath IncreaseReplicationFactor where
  toPath = Prelude.const "/"

instance Core.ToQuery IncreaseReplicationFactor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIncreaseReplicationFactorResponse' smart constructor.
data IncreaseReplicationFactorResponse = IncreaseReplicationFactorResponse'
  { -- | A description of the DAX cluster. with its new replication factor.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  IncreaseReplicationFactorResponse
newIncreaseReplicationFactorResponse pHttpStatus_ =
  IncreaseReplicationFactorResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster. with its new replication factor.
increaseReplicationFactorResponse_cluster :: Lens.Lens' IncreaseReplicationFactorResponse (Prelude.Maybe Cluster)
increaseReplicationFactorResponse_cluster = Lens.lens (\IncreaseReplicationFactorResponse' {cluster} -> cluster) (\s@IncreaseReplicationFactorResponse' {} a -> s {cluster = a} :: IncreaseReplicationFactorResponse)

-- | The response's http status code.
increaseReplicationFactorResponse_httpStatus :: Lens.Lens' IncreaseReplicationFactorResponse Prelude.Int
increaseReplicationFactorResponse_httpStatus = Lens.lens (\IncreaseReplicationFactorResponse' {httpStatus} -> httpStatus) (\s@IncreaseReplicationFactorResponse' {} a -> s {httpStatus = a} :: IncreaseReplicationFactorResponse)

instance
  Prelude.NFData
    IncreaseReplicationFactorResponse
