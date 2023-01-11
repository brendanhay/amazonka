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
-- Module      : Amazonka.DAX.DecreaseReplicationFactor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more nodes from a DAX cluster.
--
-- You cannot use @DecreaseReplicationFactor@ to remove the last node in a
-- DAX cluster. If you need to do this, use @DeleteCluster@ instead.
module Amazonka.DAX.DecreaseReplicationFactor
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecreaseReplicationFactor' smart constructor.
data DecreaseReplicationFactor = DecreaseReplicationFactor'
  { -- | The Availability Zone(s) from which to remove nodes.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifiers of the nodes to be removed from the cluster.
    nodeIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DAX cluster from which you want to remove nodes.
    clusterName :: Prelude.Text,
    -- | The new number of nodes for the DAX cluster.
    newReplicationFactor' :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'newReplicationFactor''
  Prelude.Int ->
  DecreaseReplicationFactor
newDecreaseReplicationFactor
  pClusterName_
  pNewReplicationFactor_ =
    DecreaseReplicationFactor'
      { availabilityZones =
          Prelude.Nothing,
        nodeIdsToRemove = Prelude.Nothing,
        clusterName = pClusterName_,
        newReplicationFactor' = pNewReplicationFactor_
      }

-- | The Availability Zone(s) from which to remove nodes.
decreaseReplicationFactor_availabilityZones :: Lens.Lens' DecreaseReplicationFactor (Prelude.Maybe [Prelude.Text])
decreaseReplicationFactor_availabilityZones = Lens.lens (\DecreaseReplicationFactor' {availabilityZones} -> availabilityZones) (\s@DecreaseReplicationFactor' {} a -> s {availabilityZones = a} :: DecreaseReplicationFactor) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifiers of the nodes to be removed from the cluster.
decreaseReplicationFactor_nodeIdsToRemove :: Lens.Lens' DecreaseReplicationFactor (Prelude.Maybe [Prelude.Text])
decreaseReplicationFactor_nodeIdsToRemove = Lens.lens (\DecreaseReplicationFactor' {nodeIdsToRemove} -> nodeIdsToRemove) (\s@DecreaseReplicationFactor' {} a -> s {nodeIdsToRemove = a} :: DecreaseReplicationFactor) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DAX cluster from which you want to remove nodes.
decreaseReplicationFactor_clusterName :: Lens.Lens' DecreaseReplicationFactor Prelude.Text
decreaseReplicationFactor_clusterName = Lens.lens (\DecreaseReplicationFactor' {clusterName} -> clusterName) (\s@DecreaseReplicationFactor' {} a -> s {clusterName = a} :: DecreaseReplicationFactor)

-- | The new number of nodes for the DAX cluster.
decreaseReplicationFactor_newReplicationFactor :: Lens.Lens' DecreaseReplicationFactor Prelude.Int
decreaseReplicationFactor_newReplicationFactor = Lens.lens (\DecreaseReplicationFactor' {newReplicationFactor'} -> newReplicationFactor') (\s@DecreaseReplicationFactor' {} a -> s {newReplicationFactor' = a} :: DecreaseReplicationFactor)

instance Core.AWSRequest DecreaseReplicationFactor where
  type
    AWSResponse DecreaseReplicationFactor =
      DecreaseReplicationFactorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DecreaseReplicationFactorResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DecreaseReplicationFactor where
  hashWithSalt _salt DecreaseReplicationFactor' {..} =
    _salt `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` nodeIdsToRemove
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` newReplicationFactor'

instance Prelude.NFData DecreaseReplicationFactor where
  rnf DecreaseReplicationFactor' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf nodeIdsToRemove
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf newReplicationFactor'

instance Data.ToHeaders DecreaseReplicationFactor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDAXV3.DecreaseReplicationFactor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DecreaseReplicationFactor where
  toJSON DecreaseReplicationFactor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("NodeIdsToRemove" Data..=)
              Prelude.<$> nodeIdsToRemove,
            Prelude.Just ("ClusterName" Data..= clusterName),
            Prelude.Just
              ( "NewReplicationFactor"
                  Data..= newReplicationFactor'
              )
          ]
      )

instance Data.ToPath DecreaseReplicationFactor where
  toPath = Prelude.const "/"

instance Data.ToQuery DecreaseReplicationFactor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecreaseReplicationFactorResponse' smart constructor.
data DecreaseReplicationFactorResponse = DecreaseReplicationFactorResponse'
  { -- | A description of the DAX cluster, after you have decreased its
    -- replication factor.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DecreaseReplicationFactorResponse
newDecreaseReplicationFactorResponse pHttpStatus_ =
  DecreaseReplicationFactorResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster, after you have decreased its
-- replication factor.
decreaseReplicationFactorResponse_cluster :: Lens.Lens' DecreaseReplicationFactorResponse (Prelude.Maybe Cluster)
decreaseReplicationFactorResponse_cluster = Lens.lens (\DecreaseReplicationFactorResponse' {cluster} -> cluster) (\s@DecreaseReplicationFactorResponse' {} a -> s {cluster = a} :: DecreaseReplicationFactorResponse)

-- | The response's http status code.
decreaseReplicationFactorResponse_httpStatus :: Lens.Lens' DecreaseReplicationFactorResponse Prelude.Int
decreaseReplicationFactorResponse_httpStatus = Lens.lens (\DecreaseReplicationFactorResponse' {httpStatus} -> httpStatus) (\s@DecreaseReplicationFactorResponse' {} a -> s {httpStatus = a} :: DecreaseReplicationFactorResponse)

instance
  Prelude.NFData
    DecreaseReplicationFactorResponse
  where
  rnf DecreaseReplicationFactorResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
