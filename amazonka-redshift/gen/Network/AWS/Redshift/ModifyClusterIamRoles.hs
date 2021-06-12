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
-- Module      : Network.AWS.Redshift.ModifyClusterIamRoles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the list of AWS Identity and Access Management (IAM) roles that
-- can be used by the cluster to access other AWS services.
--
-- A cluster can have up to 10 IAM roles associated at any time.
module Network.AWS.Redshift.ModifyClusterIamRoles
  ( -- * Creating a Request
    ModifyClusterIamRoles (..),
    newModifyClusterIamRoles,

    -- * Request Lenses
    modifyClusterIamRoles_removeIamRoles,
    modifyClusterIamRoles_addIamRoles,
    modifyClusterIamRoles_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterIamRolesResponse (..),
    newModifyClusterIamRolesResponse,

    -- * Response Lenses
    modifyClusterIamRolesResponse_cluster,
    modifyClusterIamRolesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyClusterIamRoles' smart constructor.
data ModifyClusterIamRoles = ModifyClusterIamRoles'
  { -- | Zero or more IAM roles in ARN format to disassociate from the cluster.
    -- You can disassociate up to 10 IAM roles from a single cluster in a
    -- single request.
    removeIamRoles :: Core.Maybe [Core.Text],
    -- | Zero or more IAM roles to associate with the cluster. The roles must be
    -- in their Amazon Resource Name (ARN) format. You can associate up to 10
    -- IAM roles with a single cluster in a single request.
    addIamRoles :: Core.Maybe [Core.Text],
    -- | The unique identifier of the cluster for which you want to associate or
    -- disassociate IAM roles.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterIamRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeIamRoles', 'modifyClusterIamRoles_removeIamRoles' - Zero or more IAM roles in ARN format to disassociate from the cluster.
-- You can disassociate up to 10 IAM roles from a single cluster in a
-- single request.
--
-- 'addIamRoles', 'modifyClusterIamRoles_addIamRoles' - Zero or more IAM roles to associate with the cluster. The roles must be
-- in their Amazon Resource Name (ARN) format. You can associate up to 10
-- IAM roles with a single cluster in a single request.
--
-- 'clusterIdentifier', 'modifyClusterIamRoles_clusterIdentifier' - The unique identifier of the cluster for which you want to associate or
-- disassociate IAM roles.
newModifyClusterIamRoles ::
  -- | 'clusterIdentifier'
  Core.Text ->
  ModifyClusterIamRoles
newModifyClusterIamRoles pClusterIdentifier_ =
  ModifyClusterIamRoles'
    { removeIamRoles =
        Core.Nothing,
      addIamRoles = Core.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | Zero or more IAM roles in ARN format to disassociate from the cluster.
-- You can disassociate up to 10 IAM roles from a single cluster in a
-- single request.
modifyClusterIamRoles_removeIamRoles :: Lens.Lens' ModifyClusterIamRoles (Core.Maybe [Core.Text])
modifyClusterIamRoles_removeIamRoles = Lens.lens (\ModifyClusterIamRoles' {removeIamRoles} -> removeIamRoles) (\s@ModifyClusterIamRoles' {} a -> s {removeIamRoles = a} :: ModifyClusterIamRoles) Core.. Lens.mapping Lens._Coerce

-- | Zero or more IAM roles to associate with the cluster. The roles must be
-- in their Amazon Resource Name (ARN) format. You can associate up to 10
-- IAM roles with a single cluster in a single request.
modifyClusterIamRoles_addIamRoles :: Lens.Lens' ModifyClusterIamRoles (Core.Maybe [Core.Text])
modifyClusterIamRoles_addIamRoles = Lens.lens (\ModifyClusterIamRoles' {addIamRoles} -> addIamRoles) (\s@ModifyClusterIamRoles' {} a -> s {addIamRoles = a} :: ModifyClusterIamRoles) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier of the cluster for which you want to associate or
-- disassociate IAM roles.
modifyClusterIamRoles_clusterIdentifier :: Lens.Lens' ModifyClusterIamRoles Core.Text
modifyClusterIamRoles_clusterIdentifier = Lens.lens (\ModifyClusterIamRoles' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyClusterIamRoles' {} a -> s {clusterIdentifier = a} :: ModifyClusterIamRoles)

instance Core.AWSRequest ModifyClusterIamRoles where
  type
    AWSResponse ModifyClusterIamRoles =
      ModifyClusterIamRolesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterIamRolesResult"
      ( \s h x ->
          ModifyClusterIamRolesResponse'
            Core.<$> (x Core..@? "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClusterIamRoles

instance Core.NFData ModifyClusterIamRoles

instance Core.ToHeaders ModifyClusterIamRoles where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClusterIamRoles where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClusterIamRoles where
  toQuery ModifyClusterIamRoles' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClusterIamRoles" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "RemoveIamRoles"
          Core.=: Core.toQuery
            ( Core.toQueryList "IamRoleArn"
                Core.<$> removeIamRoles
            ),
        "AddIamRoles"
          Core.=: Core.toQuery
            (Core.toQueryList "IamRoleArn" Core.<$> addIamRoles),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterIamRolesResponse' smart constructor.
data ModifyClusterIamRolesResponse = ModifyClusterIamRolesResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterIamRolesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifyClusterIamRolesResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterIamRolesResponse_httpStatus' - The response's http status code.
newModifyClusterIamRolesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClusterIamRolesResponse
newModifyClusterIamRolesResponse pHttpStatus_ =
  ModifyClusterIamRolesResponse'
    { cluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterIamRolesResponse_cluster :: Lens.Lens' ModifyClusterIamRolesResponse (Core.Maybe Cluster)
modifyClusterIamRolesResponse_cluster = Lens.lens (\ModifyClusterIamRolesResponse' {cluster} -> cluster) (\s@ModifyClusterIamRolesResponse' {} a -> s {cluster = a} :: ModifyClusterIamRolesResponse)

-- | The response's http status code.
modifyClusterIamRolesResponse_httpStatus :: Lens.Lens' ModifyClusterIamRolesResponse Core.Int
modifyClusterIamRolesResponse_httpStatus = Lens.lens (\ModifyClusterIamRolesResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterIamRolesResponse' {} a -> s {httpStatus = a} :: ModifyClusterIamRolesResponse)

instance Core.NFData ModifyClusterIamRolesResponse
