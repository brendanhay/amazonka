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
-- Module      : Amazonka.Snowball.DescribeCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific cluster including shipping
-- information, cluster status, and other important metadata.
module Amazonka.Snowball.DescribeCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newDescribeCluster' smart constructor.
data DescribeCluster = DescribeCluster'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Text
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

instance Core.AWSRequest DescribeCluster where
  type
    AWSResponse DescribeCluster =
      DescribeClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Prelude.<$> (x Data..?> "ClusterMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCluster where
  hashWithSalt _salt DescribeCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData DescribeCluster where
  rnf DescribeCluster' {..} = Prelude.rnf clusterId

instance Data.ToHeaders DescribeCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.DescribeCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCluster where
  toJSON DescribeCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath DescribeCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | Information about a specific cluster, including shipping information,
    -- cluster status, and other important metadata.
    clusterMetadata :: Prelude.Maybe ClusterMetadata,
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

instance Prelude.NFData DescribeClusterResponse where
  rnf DescribeClusterResponse' {..} =
    Prelude.rnf clusterMetadata
      `Prelude.seq` Prelude.rnf httpStatus
