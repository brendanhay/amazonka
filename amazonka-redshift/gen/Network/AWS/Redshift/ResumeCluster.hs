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
-- Module      : Network.AWS.Redshift.ResumeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes a paused cluster.
module Network.AWS.Redshift.ResumeCluster
  ( -- * Creating a Request
    ResumeCluster (..),
    newResumeCluster,

    -- * Request Lenses
    resumeCluster_clusterIdentifier,

    -- * Destructuring the Response
    ResumeClusterResponse (..),
    newResumeClusterResponse,

    -- * Response Lenses
    resumeClusterResponse_cluster,
    resumeClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a resume cluster operation. For example, a scheduled action to
-- run the @ResumeCluster@ API operation.
--
-- /See:/ 'newResumeCluster' smart constructor.
data ResumeCluster = ResumeCluster'
  { -- | The identifier of the cluster to be resumed.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'resumeCluster_clusterIdentifier' - The identifier of the cluster to be resumed.
newResumeCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ResumeCluster
newResumeCluster pClusterIdentifier_ =
  ResumeCluster'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be resumed.
resumeCluster_clusterIdentifier :: Lens.Lens' ResumeCluster Prelude.Text
resumeCluster_clusterIdentifier = Lens.lens (\ResumeCluster' {clusterIdentifier} -> clusterIdentifier) (\s@ResumeCluster' {} a -> s {clusterIdentifier = a} :: ResumeCluster)

instance Core.AWSRequest ResumeCluster where
  type
    AWSResponse ResumeCluster =
      ResumeClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ResumeClusterResult"
      ( \s h x ->
          ResumeClusterResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeCluster

instance Prelude.NFData ResumeCluster

instance Core.ToHeaders ResumeCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResumeCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery ResumeCluster where
  toQuery ResumeCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ResumeCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newResumeClusterResponse' smart constructor.
data ResumeClusterResponse = ResumeClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'resumeClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'resumeClusterResponse_httpStatus' - The response's http status code.
newResumeClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeClusterResponse
newResumeClusterResponse pHttpStatus_ =
  ResumeClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
resumeClusterResponse_cluster :: Lens.Lens' ResumeClusterResponse (Prelude.Maybe Cluster)
resumeClusterResponse_cluster = Lens.lens (\ResumeClusterResponse' {cluster} -> cluster) (\s@ResumeClusterResponse' {} a -> s {cluster = a} :: ResumeClusterResponse)

-- | The response's http status code.
resumeClusterResponse_httpStatus :: Lens.Lens' ResumeClusterResponse Prelude.Int
resumeClusterResponse_httpStatus = Lens.lens (\ResumeClusterResponse' {httpStatus} -> httpStatus) (\s@ResumeClusterResponse' {} a -> s {httpStatus = a} :: ResumeClusterResponse)

instance Prelude.NFData ResumeClusterResponse
