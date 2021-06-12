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
-- Module      : Network.AWS.Redshift.ModifyClusterDbRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the database revision of a cluster. The database revision is a
-- unique revision of the database running in a cluster.
module Network.AWS.Redshift.ModifyClusterDbRevision
  ( -- * Creating a Request
    ModifyClusterDbRevision (..),
    newModifyClusterDbRevision,

    -- * Request Lenses
    modifyClusterDbRevision_clusterIdentifier,
    modifyClusterDbRevision_revisionTarget,

    -- * Destructuring the Response
    ModifyClusterDbRevisionResponse (..),
    newModifyClusterDbRevisionResponse,

    -- * Response Lenses
    modifyClusterDbRevisionResponse_cluster,
    modifyClusterDbRevisionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyClusterDbRevision' smart constructor.
data ModifyClusterDbRevision = ModifyClusterDbRevision'
  { -- | The unique identifier of a cluster whose database revision you want to
    -- modify.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Core.Text,
    -- | The identifier of the database revision. You can retrieve this value
    -- from the response to the DescribeClusterDbRevisions request.
    revisionTarget :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterDbRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'modifyClusterDbRevision_clusterIdentifier' - The unique identifier of a cluster whose database revision you want to
-- modify.
--
-- Example: @examplecluster@
--
-- 'revisionTarget', 'modifyClusterDbRevision_revisionTarget' - The identifier of the database revision. You can retrieve this value
-- from the response to the DescribeClusterDbRevisions request.
newModifyClusterDbRevision ::
  -- | 'clusterIdentifier'
  Core.Text ->
  -- | 'revisionTarget'
  Core.Text ->
  ModifyClusterDbRevision
newModifyClusterDbRevision
  pClusterIdentifier_
  pRevisionTarget_ =
    ModifyClusterDbRevision'
      { clusterIdentifier =
          pClusterIdentifier_,
        revisionTarget = pRevisionTarget_
      }

-- | The unique identifier of a cluster whose database revision you want to
-- modify.
--
-- Example: @examplecluster@
modifyClusterDbRevision_clusterIdentifier :: Lens.Lens' ModifyClusterDbRevision Core.Text
modifyClusterDbRevision_clusterIdentifier = Lens.lens (\ModifyClusterDbRevision' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyClusterDbRevision' {} a -> s {clusterIdentifier = a} :: ModifyClusterDbRevision)

-- | The identifier of the database revision. You can retrieve this value
-- from the response to the DescribeClusterDbRevisions request.
modifyClusterDbRevision_revisionTarget :: Lens.Lens' ModifyClusterDbRevision Core.Text
modifyClusterDbRevision_revisionTarget = Lens.lens (\ModifyClusterDbRevision' {revisionTarget} -> revisionTarget) (\s@ModifyClusterDbRevision' {} a -> s {revisionTarget = a} :: ModifyClusterDbRevision)

instance Core.AWSRequest ModifyClusterDbRevision where
  type
    AWSResponse ModifyClusterDbRevision =
      ModifyClusterDbRevisionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterDbRevisionResult"
      ( \s h x ->
          ModifyClusterDbRevisionResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClusterDbRevision

instance Core.NFData ModifyClusterDbRevision

instance Core.ToHeaders ModifyClusterDbRevision where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClusterDbRevision where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClusterDbRevision where
  toQuery ModifyClusterDbRevision' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClusterDbRevision" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "RevisionTarget" Core.=: revisionTarget
      ]

-- | /See:/ 'newModifyClusterDbRevisionResponse' smart constructor.
data ModifyClusterDbRevisionResponse = ModifyClusterDbRevisionResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterDbRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifyClusterDbRevisionResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterDbRevisionResponse_httpStatus' - The response's http status code.
newModifyClusterDbRevisionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClusterDbRevisionResponse
newModifyClusterDbRevisionResponse pHttpStatus_ =
  ModifyClusterDbRevisionResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterDbRevisionResponse_cluster :: Lens.Lens' ModifyClusterDbRevisionResponse (Core.Maybe Cluster)
modifyClusterDbRevisionResponse_cluster = Lens.lens (\ModifyClusterDbRevisionResponse' {cluster} -> cluster) (\s@ModifyClusterDbRevisionResponse' {} a -> s {cluster = a} :: ModifyClusterDbRevisionResponse)

-- | The response's http status code.
modifyClusterDbRevisionResponse_httpStatus :: Lens.Lens' ModifyClusterDbRevisionResponse Core.Int
modifyClusterDbRevisionResponse_httpStatus = Lens.lens (\ModifyClusterDbRevisionResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterDbRevisionResponse' {} a -> s {httpStatus = a} :: ModifyClusterDbRevisionResponse)

instance Core.NFData ModifyClusterDbRevisionResponse
