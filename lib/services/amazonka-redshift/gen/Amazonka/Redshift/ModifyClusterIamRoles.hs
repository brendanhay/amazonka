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
-- Module      : Amazonka.Redshift.ModifyClusterIamRoles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the list of Identity and Access Management (IAM) roles that can
-- be used by the cluster to access other Amazon Web Services services.
--
-- The maximum number of IAM roles that you can associate is subject to a
-- quota. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.ModifyClusterIamRoles
  ( -- * Creating a Request
    ModifyClusterIamRoles (..),
    newModifyClusterIamRoles,

    -- * Request Lenses
    modifyClusterIamRoles_removeIamRoles,
    modifyClusterIamRoles_addIamRoles,
    modifyClusterIamRoles_defaultIamRoleArn,
    modifyClusterIamRoles_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterIamRolesResponse (..),
    newModifyClusterIamRolesResponse,

    -- * Response Lenses
    modifyClusterIamRolesResponse_cluster,
    modifyClusterIamRolesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyClusterIamRoles' smart constructor.
data ModifyClusterIamRoles = ModifyClusterIamRoles'
  { -- | Zero or more IAM roles in ARN format to disassociate from the cluster.
    removeIamRoles :: Prelude.Maybe [Prelude.Text],
    -- | Zero or more IAM roles to associate with the cluster. The roles must be
    -- in their Amazon Resource Name (ARN) format.
    addIamRoles :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) for the IAM role that was set as default
    -- for the cluster when the cluster was last modified.
    defaultIamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster for which you want to associate or
    -- disassociate IAM roles.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterIamRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeIamRoles', 'modifyClusterIamRoles_removeIamRoles' - Zero or more IAM roles in ARN format to disassociate from the cluster.
--
-- 'addIamRoles', 'modifyClusterIamRoles_addIamRoles' - Zero or more IAM roles to associate with the cluster. The roles must be
-- in their Amazon Resource Name (ARN) format.
--
-- 'defaultIamRoleArn', 'modifyClusterIamRoles_defaultIamRoleArn' - The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was last modified.
--
-- 'clusterIdentifier', 'modifyClusterIamRoles_clusterIdentifier' - The unique identifier of the cluster for which you want to associate or
-- disassociate IAM roles.
newModifyClusterIamRoles ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyClusterIamRoles
newModifyClusterIamRoles pClusterIdentifier_ =
  ModifyClusterIamRoles'
    { removeIamRoles =
        Prelude.Nothing,
      addIamRoles = Prelude.Nothing,
      defaultIamRoleArn = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | Zero or more IAM roles in ARN format to disassociate from the cluster.
modifyClusterIamRoles_removeIamRoles :: Lens.Lens' ModifyClusterIamRoles (Prelude.Maybe [Prelude.Text])
modifyClusterIamRoles_removeIamRoles = Lens.lens (\ModifyClusterIamRoles' {removeIamRoles} -> removeIamRoles) (\s@ModifyClusterIamRoles' {} a -> s {removeIamRoles = a} :: ModifyClusterIamRoles) Prelude.. Lens.mapping Lens.coerced

-- | Zero or more IAM roles to associate with the cluster. The roles must be
-- in their Amazon Resource Name (ARN) format.
modifyClusterIamRoles_addIamRoles :: Lens.Lens' ModifyClusterIamRoles (Prelude.Maybe [Prelude.Text])
modifyClusterIamRoles_addIamRoles = Lens.lens (\ModifyClusterIamRoles' {addIamRoles} -> addIamRoles) (\s@ModifyClusterIamRoles' {} a -> s {addIamRoles = a} :: ModifyClusterIamRoles) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was last modified.
modifyClusterIamRoles_defaultIamRoleArn :: Lens.Lens' ModifyClusterIamRoles (Prelude.Maybe Prelude.Text)
modifyClusterIamRoles_defaultIamRoleArn = Lens.lens (\ModifyClusterIamRoles' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@ModifyClusterIamRoles' {} a -> s {defaultIamRoleArn = a} :: ModifyClusterIamRoles)

-- | The unique identifier of the cluster for which you want to associate or
-- disassociate IAM roles.
modifyClusterIamRoles_clusterIdentifier :: Lens.Lens' ModifyClusterIamRoles Prelude.Text
modifyClusterIamRoles_clusterIdentifier = Lens.lens (\ModifyClusterIamRoles' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyClusterIamRoles' {} a -> s {clusterIdentifier = a} :: ModifyClusterIamRoles)

instance Core.AWSRequest ModifyClusterIamRoles where
  type
    AWSResponse ModifyClusterIamRoles =
      ModifyClusterIamRolesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyClusterIamRolesResult"
      ( \s h x ->
          ModifyClusterIamRolesResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClusterIamRoles where
  hashWithSalt _salt ModifyClusterIamRoles' {..} =
    _salt `Prelude.hashWithSalt` removeIamRoles
      `Prelude.hashWithSalt` addIamRoles
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ModifyClusterIamRoles where
  rnf ModifyClusterIamRoles' {..} =
    Prelude.rnf removeIamRoles
      `Prelude.seq` Prelude.rnf addIamRoles
      `Prelude.seq` Prelude.rnf defaultIamRoleArn
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Core.ToHeaders ModifyClusterIamRoles where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyClusterIamRoles where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyClusterIamRoles where
  toQuery ModifyClusterIamRoles' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyClusterIamRoles" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "RemoveIamRoles"
          Core.=: Core.toQuery
            ( Core.toQueryList "IamRoleArn"
                Prelude.<$> removeIamRoles
            ),
        "AddIamRoles"
          Core.=: Core.toQuery
            ( Core.toQueryList "IamRoleArn"
                Prelude.<$> addIamRoles
            ),
        "DefaultIamRoleArn" Core.=: defaultIamRoleArn,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterIamRolesResponse' smart constructor.
data ModifyClusterIamRolesResponse = ModifyClusterIamRolesResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyClusterIamRolesResponse
newModifyClusterIamRolesResponse pHttpStatus_ =
  ModifyClusterIamRolesResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterIamRolesResponse_cluster :: Lens.Lens' ModifyClusterIamRolesResponse (Prelude.Maybe Cluster)
modifyClusterIamRolesResponse_cluster = Lens.lens (\ModifyClusterIamRolesResponse' {cluster} -> cluster) (\s@ModifyClusterIamRolesResponse' {} a -> s {cluster = a} :: ModifyClusterIamRolesResponse)

-- | The response's http status code.
modifyClusterIamRolesResponse_httpStatus :: Lens.Lens' ModifyClusterIamRolesResponse Prelude.Int
modifyClusterIamRolesResponse_httpStatus = Lens.lens (\ModifyClusterIamRolesResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterIamRolesResponse' {} a -> s {httpStatus = a} :: ModifyClusterIamRolesResponse)

instance Prelude.NFData ModifyClusterIamRolesResponse where
  rnf ModifyClusterIamRolesResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
