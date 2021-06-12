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
-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
--
-- If your cluster and its snapshots are encrypted using a customer master
-- key (CMK) from AWS KMS, use DeleteSnapshotCopyGrant to delete the grant
-- that grants Amazon Redshift permission to the CMK in the destination
-- region.
module Network.AWS.Redshift.DisableSnapshotCopy
  ( -- * Creating a Request
    DisableSnapshotCopy (..),
    newDisableSnapshotCopy,

    -- * Request Lenses
    disableSnapshotCopy_clusterIdentifier,

    -- * Destructuring the Response
    DisableSnapshotCopyResponse (..),
    newDisableSnapshotCopyResponse,

    -- * Response Lenses
    disableSnapshotCopyResponse_cluster,
    disableSnapshotCopyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDisableSnapshotCopy' smart constructor.
data DisableSnapshotCopy = DisableSnapshotCopy'
  { -- | The unique identifier of the source cluster that you want to disable
    -- copying of snapshots to a destination region.
    --
    -- Constraints: Must be the valid name of an existing cluster that has
    -- cross-region snapshot copy enabled.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableSnapshotCopy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'disableSnapshotCopy_clusterIdentifier' - The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
newDisableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Core.Text ->
  DisableSnapshotCopy
newDisableSnapshotCopy pClusterIdentifier_ =
  DisableSnapshotCopy'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
disableSnapshotCopy_clusterIdentifier :: Lens.Lens' DisableSnapshotCopy Core.Text
disableSnapshotCopy_clusterIdentifier = Lens.lens (\DisableSnapshotCopy' {clusterIdentifier} -> clusterIdentifier) (\s@DisableSnapshotCopy' {} a -> s {clusterIdentifier = a} :: DisableSnapshotCopy)

instance Core.AWSRequest DisableSnapshotCopy where
  type
    AWSResponse DisableSnapshotCopy =
      DisableSnapshotCopyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisableSnapshotCopyResult"
      ( \s h x ->
          DisableSnapshotCopyResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableSnapshotCopy

instance Core.NFData DisableSnapshotCopy

instance Core.ToHeaders DisableSnapshotCopy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisableSnapshotCopy where
  toPath = Core.const "/"

instance Core.ToQuery DisableSnapshotCopy where
  toQuery DisableSnapshotCopy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DisableSnapshotCopy" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newDisableSnapshotCopyResponse' smart constructor.
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableSnapshotCopyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'disableSnapshotCopyResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'disableSnapshotCopyResponse_httpStatus' - The response's http status code.
newDisableSnapshotCopyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableSnapshotCopyResponse
newDisableSnapshotCopyResponse pHttpStatus_ =
  DisableSnapshotCopyResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
disableSnapshotCopyResponse_cluster :: Lens.Lens' DisableSnapshotCopyResponse (Core.Maybe Cluster)
disableSnapshotCopyResponse_cluster = Lens.lens (\DisableSnapshotCopyResponse' {cluster} -> cluster) (\s@DisableSnapshotCopyResponse' {} a -> s {cluster = a} :: DisableSnapshotCopyResponse)

-- | The response's http status code.
disableSnapshotCopyResponse_httpStatus :: Lens.Lens' DisableSnapshotCopyResponse Core.Int
disableSnapshotCopyResponse_httpStatus = Lens.lens (\DisableSnapshotCopyResponse' {httpStatus} -> httpStatus) (\s@DisableSnapshotCopyResponse' {} a -> s {httpStatus = a} :: DisableSnapshotCopyResponse)

instance Core.NFData DisableSnapshotCopyResponse
