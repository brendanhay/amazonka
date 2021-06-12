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
-- Module      : Network.AWS.EKS.DescribeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EKS.DescribeCluster
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

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The name of the cluster to describe.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeCluster
newDescribeCluster pName_ =
  DescribeCluster' {name = pName_}

-- | The name of the cluster to describe.
describeCluster_name :: Lens.Lens' DescribeCluster Core.Text
describeCluster_name = Lens.lens (\DescribeCluster' {name} -> name) (\s@DescribeCluster' {} a -> s {name = a} :: DescribeCluster)

instance Core.AWSRequest DescribeCluster where
  type
    AWSResponse DescribeCluster =
      DescribeClusterResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Core.<$> (x Core..?> "cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCluster

instance Core.NFData DescribeCluster

instance Core.ToHeaders DescribeCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeCluster where
  toPath DescribeCluster' {..} =
    Core.mconcat ["/clusters/", Core.toBS name]

instance Core.ToQuery DescribeCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | The full description of your specified cluster.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeClusterResponse
newDescribeClusterResponse pHttpStatus_ =
  DescribeClusterResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your specified cluster.
describeClusterResponse_cluster :: Lens.Lens' DescribeClusterResponse (Core.Maybe Cluster)
describeClusterResponse_cluster = Lens.lens (\DescribeClusterResponse' {cluster} -> cluster) (\s@DescribeClusterResponse' {} a -> s {cluster = a} :: DescribeClusterResponse)

-- | The response's http status code.
describeClusterResponse_httpStatus :: Lens.Lens' DescribeClusterResponse Core.Int
describeClusterResponse_httpStatus = Lens.lens (\DescribeClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterResponse' {} a -> s {httpStatus = a} :: DescribeClusterResponse)

instance Core.NFData DescribeClusterResponse
