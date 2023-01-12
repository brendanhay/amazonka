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
-- Module      : Amazonka.Redshift.ResumeCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes a paused cluster.
module Amazonka.Redshift.ResumeCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResumeClusterResult"
      ( \s h x ->
          ResumeClusterResponse'
            Prelude.<$> (x Data..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeCluster where
  hashWithSalt _salt ResumeCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ResumeCluster where
  rnf ResumeCluster' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToHeaders ResumeCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResumeCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeCluster where
  toQuery ResumeCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResumeCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
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

instance Prelude.NFData ResumeClusterResponse where
  rnf ResumeClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
