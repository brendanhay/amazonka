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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which cluster to describe.
--
-- /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The identifier of the cluster to describe.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeCluster
newDescribeCluster pClusterId_ =
  DescribeCluster' {clusterId = pClusterId_}

-- | The identifier of the cluster to describe.
describeCluster_clusterId :: Lens.Lens' DescribeCluster Prelude.Text
describeCluster_clusterId = Lens.lens (\DescribeCluster' {clusterId} -> clusterId) (\s@DescribeCluster' {} a -> s {clusterId = a} :: DescribeCluster)

instance Prelude.AWSRequest DescribeCluster where
  type Rs DescribeCluster = DescribeClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Cluster")
      )

instance Prelude.Hashable DescribeCluster

instance Prelude.NFData DescribeCluster

instance Prelude.ToHeaders DescribeCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.DescribeCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeCluster where
  toJSON DescribeCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Prelude..= clusterId)]
      )

instance Prelude.ToPath DescribeCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeCluster where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the description of the cluster.
--
-- /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | This output contains the details for the requested cluster.
    cluster :: Cluster
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  DescribeClusterResponse
newDescribeClusterResponse pHttpStatus_ pCluster_ =
  DescribeClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
describeClusterResponse_httpStatus :: Lens.Lens' DescribeClusterResponse Prelude.Int
describeClusterResponse_httpStatus = Lens.lens (\DescribeClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterResponse' {} a -> s {httpStatus = a} :: DescribeClusterResponse)

-- | This output contains the details for the requested cluster.
describeClusterResponse_cluster :: Lens.Lens' DescribeClusterResponse Cluster
describeClusterResponse_cluster = Lens.lens (\DescribeClusterResponse' {cluster} -> cluster) (\s@DescribeClusterResponse' {} a -> s {cluster = a} :: DescribeClusterResponse)

instance Prelude.NFData DescribeClusterResponse
