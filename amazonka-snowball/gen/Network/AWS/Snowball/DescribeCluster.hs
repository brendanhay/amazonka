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
-- Module      : Network.AWS.Snowball.DescribeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific cluster including shipping
-- information, cluster status, and other important metadata.
module Network.AWS.Snowball.DescribeCluster
  ( -- * Creating a Request
    DescribeCluster (..),
    newDescribeCluster,

    -- * Request Lenses
    describeCluster_clusterId,

    -- * Destructuring the Response
    DescribeClusterResponse (..),
    newDescribeClusterResponse,

    -- * Response Lenses
    describeClusterResponse_clusterMetadata,
    describeClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The automatically generated ID for a cluster.
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
-- 'clusterId', 'describeCluster_clusterId' - The automatically generated ID for a cluster.
newDescribeCluster ::
  -- | 'clusterId'
  Prelude.Text ->
  DescribeCluster
newDescribeCluster pClusterId_ =
  DescribeCluster' {clusterId = pClusterId_}

-- | The automatically generated ID for a cluster.
describeCluster_clusterId :: Lens.Lens' DescribeCluster Prelude.Text
describeCluster_clusterId = Lens.lens (\DescribeCluster' {clusterId} -> clusterId) (\s@DescribeCluster' {} a -> s {clusterId = a} :: DescribeCluster)

instance Prelude.AWSRequest DescribeCluster where
  type Rs DescribeCluster = DescribeClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Prelude.<$> (x Prelude..?> "ClusterMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCluster

instance Prelude.NFData DescribeCluster

instance Prelude.ToHeaders DescribeCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.DescribeCluster" ::
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

-- | /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | Information about a specific cluster, including shipping information,
    -- cluster status, and other important metadata.
    clusterMetadata :: Prelude.Maybe ClusterMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'clusterMetadata', 'describeClusterResponse_clusterMetadata' - Information about a specific cluster, including shipping information,
-- cluster status, and other important metadata.
--
-- 'httpStatus', 'describeClusterResponse_httpStatus' - The response's http status code.
newDescribeClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterResponse
newDescribeClusterResponse pHttpStatus_ =
  DescribeClusterResponse'
    { clusterMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a specific cluster, including shipping information,
-- cluster status, and other important metadata.
describeClusterResponse_clusterMetadata :: Lens.Lens' DescribeClusterResponse (Prelude.Maybe ClusterMetadata)
describeClusterResponse_clusterMetadata = Lens.lens (\DescribeClusterResponse' {clusterMetadata} -> clusterMetadata) (\s@DescribeClusterResponse' {} a -> s {clusterMetadata = a} :: DescribeClusterResponse)

-- | The response's http status code.
describeClusterResponse_httpStatus :: Lens.Lens' DescribeClusterResponse Prelude.Int
describeClusterResponse_httpStatus = Lens.lens (\DescribeClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterResponse' {} a -> s {httpStatus = a} :: DescribeClusterResponse)

instance Prelude.NFData DescribeClusterResponse
