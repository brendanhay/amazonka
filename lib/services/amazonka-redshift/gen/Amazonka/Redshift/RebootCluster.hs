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
-- Module      : Amazonka.Redshift.RebootCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a cluster. This action is taken as soon as possible. It results
-- in a momentary outage to the cluster, during which the cluster status is
-- set to @rebooting@. A cluster event is created when the reboot is
-- completed. Any pending cluster modifications (see ModifyCluster) are
-- applied at this reboot. For more information about managing clusters, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.RebootCluster
  ( -- * Creating a Request
    RebootCluster (..),
    newRebootCluster,

    -- * Request Lenses
    rebootCluster_clusterIdentifier,

    -- * Destructuring the Response
    RebootClusterResponse (..),
    newRebootClusterResponse,

    -- * Response Lenses
    rebootClusterResponse_cluster,
    rebootClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRebootCluster' smart constructor.
data RebootCluster = RebootCluster'
  { -- | The cluster identifier.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'rebootCluster_clusterIdentifier' - The cluster identifier.
newRebootCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  RebootCluster
newRebootCluster pClusterIdentifier_ =
  RebootCluster'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The cluster identifier.
rebootCluster_clusterIdentifier :: Lens.Lens' RebootCluster Prelude.Text
rebootCluster_clusterIdentifier = Lens.lens (\RebootCluster' {clusterIdentifier} -> clusterIdentifier) (\s@RebootCluster' {} a -> s {clusterIdentifier = a} :: RebootCluster)

instance Core.AWSRequest RebootCluster where
  type
    AWSResponse RebootCluster =
      RebootClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RebootClusterResult"
      ( \s h x ->
          RebootClusterResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootCluster where
  hashWithSalt _salt RebootCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData RebootCluster where
  rnf RebootCluster' {..} =
    Prelude.rnf clusterIdentifier

instance Core.ToHeaders RebootCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RebootCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery RebootCluster where
  toQuery RebootCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RebootCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newRebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'rebootClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'rebootClusterResponse_httpStatus' - The response's http status code.
newRebootClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootClusterResponse
newRebootClusterResponse pHttpStatus_ =
  RebootClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rebootClusterResponse_cluster :: Lens.Lens' RebootClusterResponse (Prelude.Maybe Cluster)
rebootClusterResponse_cluster = Lens.lens (\RebootClusterResponse' {cluster} -> cluster) (\s@RebootClusterResponse' {} a -> s {cluster = a} :: RebootClusterResponse)

-- | The response's http status code.
rebootClusterResponse_httpStatus :: Lens.Lens' RebootClusterResponse Prelude.Int
rebootClusterResponse_httpStatus = Lens.lens (\RebootClusterResponse' {httpStatus} -> httpStatus) (\s@RebootClusterResponse' {} a -> s {httpStatus = a} :: RebootClusterResponse)

instance Prelude.NFData RebootClusterResponse where
  rnf RebootClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
