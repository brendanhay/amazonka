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
-- Module      : Network.AWS.RDS.RemoveFromGlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Aurora secondary cluster from an Aurora global database
-- cluster. The cluster becomes a standalone cluster with read-write
-- capability instead of being read-only and receiving data from a primary
-- cluster in a different region.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.RemoveFromGlobalCluster
  ( -- * Creating a Request
    RemoveFromGlobalCluster (..),
    newRemoveFromGlobalCluster,

    -- * Request Lenses
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalCluster_globalClusterIdentifier,

    -- * Destructuring the Response
    RemoveFromGlobalClusterResponse (..),
    newRemoveFromGlobalClusterResponse,

    -- * Response Lenses
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveFromGlobalCluster' smart constructor.
data RemoveFromGlobalCluster = RemoveFromGlobalCluster'
  { -- | The Amazon Resource Name (ARN) identifying the cluster that was detached
    -- from the Aurora global database cluster.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | The cluster identifier to detach from the Aurora global database
    -- cluster.
    globalClusterIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveFromGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'removeFromGlobalCluster_dbClusterIdentifier' - The Amazon Resource Name (ARN) identifying the cluster that was detached
-- from the Aurora global database cluster.
--
-- 'globalClusterIdentifier', 'removeFromGlobalCluster_globalClusterIdentifier' - The cluster identifier to detach from the Aurora global database
-- cluster.
newRemoveFromGlobalCluster ::
  RemoveFromGlobalCluster
newRemoveFromGlobalCluster =
  RemoveFromGlobalCluster'
    { dbClusterIdentifier =
        Core.Nothing,
      globalClusterIdentifier = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) identifying the cluster that was detached
-- from the Aurora global database cluster.
removeFromGlobalCluster_dbClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Core.Text)
removeFromGlobalCluster_dbClusterIdentifier = Lens.lens (\RemoveFromGlobalCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RemoveFromGlobalCluster' {} a -> s {dbClusterIdentifier = a} :: RemoveFromGlobalCluster)

-- | The cluster identifier to detach from the Aurora global database
-- cluster.
removeFromGlobalCluster_globalClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Core.Text)
removeFromGlobalCluster_globalClusterIdentifier = Lens.lens (\RemoveFromGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@RemoveFromGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: RemoveFromGlobalCluster)

instance Core.AWSRequest RemoveFromGlobalCluster where
  type
    AWSResponse RemoveFromGlobalCluster =
      RemoveFromGlobalClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RemoveFromGlobalClusterResult"
      ( \s h x ->
          RemoveFromGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveFromGlobalCluster

instance Core.NFData RemoveFromGlobalCluster

instance Core.ToHeaders RemoveFromGlobalCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RemoveFromGlobalCluster where
  toPath = Core.const "/"

instance Core.ToQuery RemoveFromGlobalCluster where
  toQuery RemoveFromGlobalCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RemoveFromGlobalCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DbClusterIdentifier" Core.=: dbClusterIdentifier,
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier
      ]

-- | /See:/ 'newRemoveFromGlobalClusterResponse' smart constructor.
data RemoveFromGlobalClusterResponse = RemoveFromGlobalClusterResponse'
  { globalCluster :: Core.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveFromGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'removeFromGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'removeFromGlobalClusterResponse_httpStatus' - The response's http status code.
newRemoveFromGlobalClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveFromGlobalClusterResponse
newRemoveFromGlobalClusterResponse pHttpStatus_ =
  RemoveFromGlobalClusterResponse'
    { globalCluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
removeFromGlobalClusterResponse_globalCluster :: Lens.Lens' RemoveFromGlobalClusterResponse (Core.Maybe GlobalCluster)
removeFromGlobalClusterResponse_globalCluster = Lens.lens (\RemoveFromGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@RemoveFromGlobalClusterResponse' {} a -> s {globalCluster = a} :: RemoveFromGlobalClusterResponse)

-- | The response's http status code.
removeFromGlobalClusterResponse_httpStatus :: Lens.Lens' RemoveFromGlobalClusterResponse Core.Int
removeFromGlobalClusterResponse_httpStatus = Lens.lens (\RemoveFromGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@RemoveFromGlobalClusterResponse' {} a -> s {httpStatus = a} :: RemoveFromGlobalClusterResponse)

instance Core.NFData RemoveFromGlobalClusterResponse
