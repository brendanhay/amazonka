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
-- Module      : Amazonka.EKS.DeleteNodegroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon EKS node group for a cluster.
module Amazonka.EKS.DeleteNodegroup
  ( -- * Creating a Request
    DeleteNodegroup (..),
    newDeleteNodegroup,

    -- * Request Lenses
    deleteNodegroup_clusterName,
    deleteNodegroup_nodegroupName,

    -- * Destructuring the Response
    DeleteNodegroupResponse (..),
    newDeleteNodegroupResponse,

    -- * Response Lenses
    deleteNodegroupResponse_nodegroup,
    deleteNodegroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNodegroup' smart constructor.
data DeleteNodegroup = DeleteNodegroup'
  { -- | The name of the Amazon EKS cluster that is associated with your node
    -- group.
    clusterName :: Prelude.Text,
    -- | The name of the node group to delete.
    nodegroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNodegroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'deleteNodegroup_clusterName' - The name of the Amazon EKS cluster that is associated with your node
-- group.
--
-- 'nodegroupName', 'deleteNodegroup_nodegroupName' - The name of the node group to delete.
newDeleteNodegroup ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'nodegroupName'
  Prelude.Text ->
  DeleteNodegroup
newDeleteNodegroup pClusterName_ pNodegroupName_ =
  DeleteNodegroup'
    { clusterName = pClusterName_,
      nodegroupName = pNodegroupName_
    }

-- | The name of the Amazon EKS cluster that is associated with your node
-- group.
deleteNodegroup_clusterName :: Lens.Lens' DeleteNodegroup Prelude.Text
deleteNodegroup_clusterName = Lens.lens (\DeleteNodegroup' {clusterName} -> clusterName) (\s@DeleteNodegroup' {} a -> s {clusterName = a} :: DeleteNodegroup)

-- | The name of the node group to delete.
deleteNodegroup_nodegroupName :: Lens.Lens' DeleteNodegroup Prelude.Text
deleteNodegroup_nodegroupName = Lens.lens (\DeleteNodegroup' {nodegroupName} -> nodegroupName) (\s@DeleteNodegroup' {} a -> s {nodegroupName = a} :: DeleteNodegroup)

instance Core.AWSRequest DeleteNodegroup where
  type
    AWSResponse DeleteNodegroup =
      DeleteNodegroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNodegroupResponse'
            Prelude.<$> (x Data..?> "nodegroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNodegroup where
  hashWithSalt _salt DeleteNodegroup' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodegroupName

instance Prelude.NFData DeleteNodegroup where
  rnf DeleteNodegroup' {..} =
    Prelude.rnf clusterName `Prelude.seq`
      Prelude.rnf nodegroupName

instance Data.ToHeaders DeleteNodegroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteNodegroup where
  toPath DeleteNodegroup' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/node-groups/",
        Data.toBS nodegroupName
      ]

instance Data.ToQuery DeleteNodegroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNodegroupResponse' smart constructor.
data DeleteNodegroupResponse = DeleteNodegroupResponse'
  { -- | The full description of your deleted node group.
    nodegroup :: Prelude.Maybe Nodegroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNodegroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodegroup', 'deleteNodegroupResponse_nodegroup' - The full description of your deleted node group.
--
-- 'httpStatus', 'deleteNodegroupResponse_httpStatus' - The response's http status code.
newDeleteNodegroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNodegroupResponse
newDeleteNodegroupResponse pHttpStatus_ =
  DeleteNodegroupResponse'
    { nodegroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your deleted node group.
deleteNodegroupResponse_nodegroup :: Lens.Lens' DeleteNodegroupResponse (Prelude.Maybe Nodegroup)
deleteNodegroupResponse_nodegroup = Lens.lens (\DeleteNodegroupResponse' {nodegroup} -> nodegroup) (\s@DeleteNodegroupResponse' {} a -> s {nodegroup = a} :: DeleteNodegroupResponse)

-- | The response's http status code.
deleteNodegroupResponse_httpStatus :: Lens.Lens' DeleteNodegroupResponse Prelude.Int
deleteNodegroupResponse_httpStatus = Lens.lens (\DeleteNodegroupResponse' {httpStatus} -> httpStatus) (\s@DeleteNodegroupResponse' {} a -> s {httpStatus = a} :: DeleteNodegroupResponse)

instance Prelude.NFData DeleteNodegroupResponse where
  rnf DeleteNodegroupResponse' {..} =
    Prelude.rnf nodegroup `Prelude.seq`
      Prelude.rnf httpStatus
