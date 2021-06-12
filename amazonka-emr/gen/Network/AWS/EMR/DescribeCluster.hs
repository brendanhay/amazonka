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
-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on.
module Network.AWS.EMR.DescribeCluster
  ( -- * Creating a Request
    DescribeCluster (..),
    newDescribeCluster,

    -- * Request Lenses
    describeCluster_clusterId,

    -- * Destructuring the Response
    DescribeClusterResponse (..),
    newDescribeClusterResponse,

    -- * Response Lenses
    describeClusterResponse_httpStatus,
    describeClusterResponse_cluster,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which cluster to describe.
--
-- /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The identifier of the cluster to describe.
    clusterId :: Core.Text
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
-- 'clusterId', 'describeCluster_clusterId' - The identifier of the cluster to describe.
newDescribeCluster ::
  -- | 'clusterId'
  Core.Text ->
  DescribeCluster
newDescribeCluster pClusterId_ =
  DescribeCluster' {clusterId = pClusterId_}

-- | The identifier of the cluster to describe.
describeCluster_clusterId :: Lens.Lens' DescribeCluster Core.Text
describeCluster_clusterId = Lens.lens (\DescribeCluster' {clusterId} -> clusterId) (\s@DescribeCluster' {} a -> s {clusterId = a} :: DescribeCluster)

instance Core.AWSRequest DescribeCluster where
  type
    AWSResponse DescribeCluster =
      DescribeClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Cluster")
      )

instance Core.Hashable DescribeCluster

instance Core.NFData DescribeCluster

instance Core.ToHeaders DescribeCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCluster where
  toJSON DescribeCluster' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ClusterId" Core..= clusterId)]
      )

instance Core.ToPath DescribeCluster where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCluster where
  toQuery = Core.const Core.mempty

-- | This output contains the description of the cluster.
--
-- /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | This output contains the details for the requested cluster.
    cluster :: Cluster
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
-- 'httpStatus', 'describeClusterResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'describeClusterResponse_cluster' - This output contains the details for the requested cluster.
newDescribeClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'cluster'
  Cluster ->
  DescribeClusterResponse
newDescribeClusterResponse pHttpStatus_ pCluster_ =
  DescribeClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
describeClusterResponse_httpStatus :: Lens.Lens' DescribeClusterResponse Core.Int
describeClusterResponse_httpStatus = Lens.lens (\DescribeClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterResponse' {} a -> s {httpStatus = a} :: DescribeClusterResponse)

-- | This output contains the details for the requested cluster.
describeClusterResponse_cluster :: Lens.Lens' DescribeClusterResponse Cluster
describeClusterResponse_cluster = Lens.lens (\DescribeClusterResponse' {cluster} -> cluster) (\s@DescribeClusterResponse' {} a -> s {cluster = a} :: DescribeClusterResponse)

instance Core.NFData DescribeClusterResponse
