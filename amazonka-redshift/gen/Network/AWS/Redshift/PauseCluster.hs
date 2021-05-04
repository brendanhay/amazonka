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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a pause cluster operation. For example, a scheduled action to
-- run the @PauseCluster@ API operation.
--
-- /See:/ 'newPauseCluster' smart constructor.
data PauseCluster = PauseCluster'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  PauseCluster
newPauseCluster pClusterIdentifier_ =
  PauseCluster'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be paused.
pauseCluster_clusterIdentifier :: Lens.Lens' PauseCluster Prelude.Text
pauseCluster_clusterIdentifier = Lens.lens (\PauseCluster' {clusterIdentifier} -> clusterIdentifier) (\s@PauseCluster' {} a -> s {clusterIdentifier = a} :: PauseCluster)

instance Prelude.AWSRequest PauseCluster where
  type Rs PauseCluster = PauseClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            Prelude.<$> (x Prelude..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PauseCluster

instance Prelude.NFData PauseCluster

instance Prelude.ToHeaders PauseCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PauseCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PauseCluster where
  toQuery PauseCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PauseCluster" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]

-- | /See:/ 'newPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PauseClusterResponse
newPauseClusterResponse pHttpStatus_ =
  PauseClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
pauseClusterResponse_cluster :: Lens.Lens' PauseClusterResponse (Prelude.Maybe Cluster)
pauseClusterResponse_cluster = Lens.lens (\PauseClusterResponse' {cluster} -> cluster) (\s@PauseClusterResponse' {} a -> s {cluster = a} :: PauseClusterResponse)

-- | The response's http status code.
pauseClusterResponse_httpStatus :: Lens.Lens' PauseClusterResponse Prelude.Int
pauseClusterResponse_httpStatus = Lens.lens (\PauseClusterResponse' {httpStatus} -> httpStatus) (\s@PauseClusterResponse' {} a -> s {httpStatus = a} :: PauseClusterResponse)

instance Prelude.NFData PauseClusterResponse
