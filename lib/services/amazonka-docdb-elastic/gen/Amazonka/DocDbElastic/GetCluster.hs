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
-- Module      : Amazonka.DocDbElastic.GetCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Elastic DocumentDB cluster.
module Amazonka.DocDbElastic.GetCluster
  ( -- * Creating a Request
    GetCluster (..),
    newGetCluster,

    -- * Request Lenses
    getCluster_clusterArn,

    -- * Destructuring the Response
    GetClusterResponse (..),
    newGetClusterResponse,

    -- * Response Lenses
    getClusterResponse_httpStatus,
    getClusterResponse_cluster,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCluster' smart constructor.
data GetCluster = GetCluster'
  { -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'getCluster_clusterArn' - The arn of the Elastic DocumentDB cluster.
newGetCluster ::
  -- | 'clusterArn'
  Prelude.Text ->
  GetCluster
newGetCluster pClusterArn_ =
  GetCluster' {clusterArn = pClusterArn_}

-- | The arn of the Elastic DocumentDB cluster.
getCluster_clusterArn :: Lens.Lens' GetCluster Prelude.Text
getCluster_clusterArn = Lens.lens (\GetCluster' {clusterArn} -> clusterArn) (\s@GetCluster' {} a -> s {clusterArn = a} :: GetCluster)

instance Core.AWSRequest GetCluster where
  type AWSResponse GetCluster = GetClusterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "cluster")
      )

instance Prelude.Hashable GetCluster where
  hashWithSalt _salt GetCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData GetCluster where
  rnf GetCluster' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders GetCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCluster where
  toPath GetCluster' {..} =
    Prelude.mconcat ["/cluster/", Data.toBS clusterArn]

instance Data.ToQuery GetCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClusterResponse' smart constructor.
data GetClusterResponse = GetClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about a specific Elastic DocumentDB cluster.
    cluster :: Cluster
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getClusterResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'getClusterResponse_cluster' - Returns information about a specific Elastic DocumentDB cluster.
newGetClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  GetClusterResponse
newGetClusterResponse pHttpStatus_ pCluster_ =
  GetClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
getClusterResponse_httpStatus :: Lens.Lens' GetClusterResponse Prelude.Int
getClusterResponse_httpStatus = Lens.lens (\GetClusterResponse' {httpStatus} -> httpStatus) (\s@GetClusterResponse' {} a -> s {httpStatus = a} :: GetClusterResponse)

-- | Returns information about a specific Elastic DocumentDB cluster.
getClusterResponse_cluster :: Lens.Lens' GetClusterResponse Cluster
getClusterResponse_cluster = Lens.lens (\GetClusterResponse' {cluster} -> cluster) (\s@GetClusterResponse' {} a -> s {cluster = a} :: GetClusterResponse)

instance Prelude.NFData GetClusterResponse where
  rnf GetClusterResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf cluster
