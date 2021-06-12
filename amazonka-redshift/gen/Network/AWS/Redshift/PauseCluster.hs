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
-- Module      : Network.AWS.Redshift.PauseCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Network.AWS.Redshift.PauseCluster
  ( -- * Creating a Request
    PauseCluster (..),
    newPauseCluster,

    -- * Request Lenses
    pauseCluster_clusterIdentifier,

    -- * Destructuring the Response
    PauseClusterResponse (..),
    newPauseClusterResponse,

    -- * Response Lenses
    pauseClusterResponse_cluster,
    pauseClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a pause cluster operation. For example, a scheduled action to
-- run the @PauseCluster@ API operation.
--
-- /See:/ 'newPauseCluster' smart constructor.
data PauseCluster = PauseCluster'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PauseCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'pauseCluster_clusterIdentifier' - The identifier of the cluster to be paused.
newPauseCluster ::
  -- | 'clusterIdentifier'
  Core.Text ->
  PauseCluster
newPauseCluster pClusterIdentifier_ =
  PauseCluster'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be paused.
pauseCluster_clusterIdentifier :: Lens.Lens' PauseCluster Core.Text
pauseCluster_clusterIdentifier = Lens.lens (\PauseCluster' {clusterIdentifier} -> clusterIdentifier) (\s@PauseCluster' {} a -> s {clusterIdentifier = a} :: PauseCluster)

instance Core.AWSRequest PauseCluster where
  type AWSResponse PauseCluster = PauseClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PauseCluster

instance Core.NFData PauseCluster

instance Core.ToHeaders PauseCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath PauseCluster where
  toPath = Core.const "/"

instance Core.ToQuery PauseCluster where
  toQuery PauseCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("PauseCluster" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PauseClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'pauseClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'pauseClusterResponse_httpStatus' - The response's http status code.
newPauseClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PauseClusterResponse
newPauseClusterResponse pHttpStatus_ =
  PauseClusterResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
pauseClusterResponse_cluster :: Lens.Lens' PauseClusterResponse (Core.Maybe Cluster)
pauseClusterResponse_cluster = Lens.lens (\PauseClusterResponse' {cluster} -> cluster) (\s@PauseClusterResponse' {} a -> s {cluster = a} :: PauseClusterResponse)

-- | The response's http status code.
pauseClusterResponse_httpStatus :: Lens.Lens' PauseClusterResponse Core.Int
pauseClusterResponse_httpStatus = Lens.lens (\PauseClusterResponse' {httpStatus} -> httpStatus) (\s@PauseClusterResponse' {} a -> s {httpStatus = a} :: PauseClusterResponse)

instance Core.NFData PauseClusterResponse
