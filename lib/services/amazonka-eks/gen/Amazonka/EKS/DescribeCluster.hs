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
-- Module      : Amazonka.EKS.DescribeCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an Amazon EKS cluster.
--
-- The API server endpoint and certificate authority data returned by this
-- operation are required for @kubelet@ and @kubectl@ to communicate with
-- your Kubernetes API server. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/create-kubeconfig.html Create a kubeconfig for Amazon EKS>.
--
-- The API server endpoint and certificate authority data aren\'t available
-- until the cluster reaches the @ACTIVE@ state.
module Amazonka.EKS.DescribeCluster
  ( -- * Creating a Request
    DescribeCluster (..),
    newDescribeCluster,

    -- * Request Lenses
    describeCluster_name,

    -- * Destructuring the Response
    DescribeClusterResponse (..),
    newDescribeClusterResponse,

    -- * Response Lenses
    describeClusterResponse_cluster,
    describeClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The name of the cluster to describe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeCluster_name' - The name of the cluster to describe.
newDescribeCluster ::
  -- | 'name'
  Prelude.Text ->
  DescribeCluster
newDescribeCluster pName_ =
  DescribeCluster' {name = pName_}

-- | The name of the cluster to describe.
describeCluster_name :: Lens.Lens' DescribeCluster Prelude.Text
describeCluster_name = Lens.lens (\DescribeCluster' {name} -> name) (\s@DescribeCluster' {} a -> s {name = a} :: DescribeCluster)

instance Core.AWSRequest DescribeCluster where
  type
    AWSResponse DescribeCluster =
      DescribeClusterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Prelude.<$> (x Data..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCluster where
  hashWithSalt _salt DescribeCluster' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeCluster where
  rnf DescribeCluster' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCluster where
  toPath DescribeCluster' {..} =
    Prelude.mconcat ["/clusters/", Data.toBS name]

instance Data.ToQuery DescribeCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | The full description of your specified cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'describeClusterResponse_cluster' - The full description of your specified cluster.
--
-- 'httpStatus', 'describeClusterResponse_httpStatus' - The response's http status code.
newDescribeClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterResponse
newDescribeClusterResponse pHttpStatus_ =
  DescribeClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your specified cluster.
describeClusterResponse_cluster :: Lens.Lens' DescribeClusterResponse (Prelude.Maybe Cluster)
describeClusterResponse_cluster = Lens.lens (\DescribeClusterResponse' {cluster} -> cluster) (\s@DescribeClusterResponse' {} a -> s {cluster = a} :: DescribeClusterResponse)

-- | The response's http status code.
describeClusterResponse_httpStatus :: Lens.Lens' DescribeClusterResponse Prelude.Int
describeClusterResponse_httpStatus = Lens.lens (\DescribeClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterResponse' {} a -> s {httpStatus = a} :: DescribeClusterResponse)

instance Prelude.NFData DescribeClusterResponse where
  rnf DescribeClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
