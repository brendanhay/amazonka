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
-- Module      : Amazonka.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots some, or all, of the cache nodes within a provisioned cluster.
-- This operation applies any modified cache parameter groups to the
-- cluster. The reboot operation takes place as soon as possible, and
-- results in a momentary outage to the cluster. During the reboot, the
-- cluster status is set to REBOOTING.
--
-- The reboot causes the contents of the cache (for each cache node being
-- rebooted) to be lost.
--
-- When the reboot is complete, a cluster event is created.
--
-- Rebooting a cluster is currently supported on Memcached and Redis
-- (cluster mode disabled) clusters. Rebooting is not supported on Redis
-- (cluster mode enabled) clusters.
--
-- If you make changes to parameters that require a Redis (cluster mode
-- enabled) cluster reboot for the changes to be applied, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/nodes.rebooting.html Rebooting a Cluster>
-- for an alternate process.
module Amazonka.ElastiCache.RebootCacheCluster
  ( -- * Creating a Request
    RebootCacheCluster (..),
    newRebootCacheCluster,

    -- * Request Lenses
    rebootCacheCluster_cacheClusterId,
    rebootCacheCluster_cacheNodeIdsToReboot,

    -- * Destructuring the Response
    RebootCacheClusterResponse (..),
    newRebootCacheClusterResponse,

    -- * Response Lenses
    rebootCacheClusterResponse_cacheCluster,
    rebootCacheClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @RebootCacheCluster@ operation.
--
-- /See:/ 'newRebootCacheCluster' smart constructor.
data RebootCacheCluster = RebootCacheCluster'
  { -- | The cluster identifier. This parameter is stored as a lowercase string.
    cacheClusterId :: Prelude.Text,
    -- | A list of cache node IDs to reboot. A node ID is a numeric identifier
    -- (0001, 0002, etc.). To reboot an entire cluster, specify all of the
    -- cache node IDs.
    cacheNodeIdsToReboot :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootCacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'rebootCacheCluster_cacheClusterId' - The cluster identifier. This parameter is stored as a lowercase string.
--
-- 'cacheNodeIdsToReboot', 'rebootCacheCluster_cacheNodeIdsToReboot' - A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cluster, specify all of the
-- cache node IDs.
newRebootCacheCluster ::
  -- | 'cacheClusterId'
  Prelude.Text ->
  RebootCacheCluster
newRebootCacheCluster pCacheClusterId_ =
  RebootCacheCluster'
    { cacheClusterId =
        pCacheClusterId_,
      cacheNodeIdsToReboot = Prelude.mempty
    }

-- | The cluster identifier. This parameter is stored as a lowercase string.
rebootCacheCluster_cacheClusterId :: Lens.Lens' RebootCacheCluster Prelude.Text
rebootCacheCluster_cacheClusterId = Lens.lens (\RebootCacheCluster' {cacheClusterId} -> cacheClusterId) (\s@RebootCacheCluster' {} a -> s {cacheClusterId = a} :: RebootCacheCluster)

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cluster, specify all of the
-- cache node IDs.
rebootCacheCluster_cacheNodeIdsToReboot :: Lens.Lens' RebootCacheCluster [Prelude.Text]
rebootCacheCluster_cacheNodeIdsToReboot = Lens.lens (\RebootCacheCluster' {cacheNodeIdsToReboot} -> cacheNodeIdsToReboot) (\s@RebootCacheCluster' {} a -> s {cacheNodeIdsToReboot = a} :: RebootCacheCluster) Prelude.. Lens.coerced

instance Core.AWSRequest RebootCacheCluster where
  type
    AWSResponse RebootCacheCluster =
      RebootCacheClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RebootCacheClusterResult"
      ( \s h x ->
          RebootCacheClusterResponse'
            Prelude.<$> (x Core..@? "CacheCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootCacheCluster where
  hashWithSalt _salt RebootCacheCluster' {..} =
    _salt `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` cacheNodeIdsToReboot

instance Prelude.NFData RebootCacheCluster where
  rnf RebootCacheCluster' {..} =
    Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf cacheNodeIdsToReboot

instance Core.ToHeaders RebootCacheCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RebootCacheCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery RebootCacheCluster where
  toQuery RebootCacheCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RebootCacheCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterId" Core.=: cacheClusterId,
        "CacheNodeIdsToReboot"
          Core.=: Core.toQueryList "CacheNodeId" cacheNodeIdsToReboot
      ]

-- | /See:/ 'newRebootCacheClusterResponse' smart constructor.
data RebootCacheClusterResponse = RebootCacheClusterResponse'
  { cacheCluster :: Prelude.Maybe CacheCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootCacheClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheCluster', 'rebootCacheClusterResponse_cacheCluster' - Undocumented member.
--
-- 'httpStatus', 'rebootCacheClusterResponse_httpStatus' - The response's http status code.
newRebootCacheClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootCacheClusterResponse
newRebootCacheClusterResponse pHttpStatus_ =
  RebootCacheClusterResponse'
    { cacheCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rebootCacheClusterResponse_cacheCluster :: Lens.Lens' RebootCacheClusterResponse (Prelude.Maybe CacheCluster)
rebootCacheClusterResponse_cacheCluster = Lens.lens (\RebootCacheClusterResponse' {cacheCluster} -> cacheCluster) (\s@RebootCacheClusterResponse' {} a -> s {cacheCluster = a} :: RebootCacheClusterResponse)

-- | The response's http status code.
rebootCacheClusterResponse_httpStatus :: Lens.Lens' RebootCacheClusterResponse Prelude.Int
rebootCacheClusterResponse_httpStatus = Lens.lens (\RebootCacheClusterResponse' {httpStatus} -> httpStatus) (\s@RebootCacheClusterResponse' {} a -> s {httpStatus = a} :: RebootCacheClusterResponse)

instance Prelude.NFData RebootCacheClusterResponse where
  rnf RebootCacheClusterResponse' {..} =
    Prelude.rnf cacheCluster
      `Prelude.seq` Prelude.rnf httpStatus
