{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.ClusterEndpoint
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | A cluster is a set of five consensus-forming Regional endpoints that
-- represent the infrastructure that hosts your routing controls.
-- Typically, you host together on one cluster all of the routing controls
-- for your applications.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | Deployment status of a resource. Status can be one of the following:
    -- PENDING, DEPLOYED, PENDING_DELETION.
    status :: Prelude.Maybe Status,
    -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | Endpoints for a cluster. Specify one of these endpoints when you want to
    -- set or retrieve a routing control state in the cluster.
    --
    -- To get or update the routing control state, see the Amazon Route 53
    -- Application Recovery Controller Cluster (Data Plane) Actions.
    clusterEndpoints :: Prelude.Maybe [ClusterEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'cluster_status' - Deployment status of a resource. Status can be one of the following:
-- PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'name', 'cluster_name' - The name of the cluster.
--
-- 'clusterEndpoints', 'cluster_clusterEndpoints' - Endpoints for a cluster. Specify one of these endpoints when you want to
-- set or retrieve a routing control state in the cluster.
--
-- To get or update the routing control state, see the Amazon Route 53
-- Application Recovery Controller Cluster (Data Plane) Actions.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { status = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      name = Prelude.Nothing,
      clusterEndpoints = Prelude.Nothing
    }

-- | Deployment status of a resource. Status can be one of the following:
-- PENDING, DEPLOYED, PENDING_DELETION.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe Status)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the cluster.
cluster_clusterArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The name of the cluster.
cluster_name :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | Endpoints for a cluster. Specify one of these endpoints when you want to
-- set or retrieve a routing control state in the cluster.
--
-- To get or update the routing control state, see the Amazon Route 53
-- Application Recovery Controller Cluster (Data Plane) Actions.
cluster_clusterEndpoints :: Lens.Lens' Cluster (Prelude.Maybe [ClusterEndpoint])
cluster_clusterEndpoints = Lens.lens (\Cluster' {clusterEndpoints} -> clusterEndpoints) (\s@Cluster' {} a -> s {clusterEndpoints = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ClusterArn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> ( x Core..:? "ClusterEndpoints"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Cluster where
  hashWithSalt salt' Cluster' {..} =
    salt' `Prelude.hashWithSalt` clusterEndpoints
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf clusterEndpoints
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clusterArn
