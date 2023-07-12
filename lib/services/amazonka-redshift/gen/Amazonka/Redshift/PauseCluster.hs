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
-- Module      : Amazonka.Redshift.PauseCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Amazonka.Redshift.PauseCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Describes a pause cluster operation. For example, a scheduled action to
-- run the @PauseCluster@ API operation.
--
-- /See:/ 'newPauseCluster' smart constructor.
data PauseCluster = PauseCluster'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PauseCluster where
  type AWSResponse PauseCluster = PauseClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            Prelude.<$> (x Data..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PauseCluster where
  hashWithSalt _salt PauseCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData PauseCluster where
  rnf PauseCluster' {..} = Prelude.rnf clusterIdentifier

instance Data.ToHeaders PauseCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PauseCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery PauseCluster where
  toQuery PauseCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PauseCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData PauseClusterResponse where
  rnf PauseClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
