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
-- Module      : Amazonka.AppMesh.DeleteVirtualNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual node.
--
-- You must delete any virtual services that list a virtual node as a
-- service provider before you can delete the virtual node itself.
module Amazonka.AppMesh.DeleteVirtualNode
  ( -- * Creating a Request
    DeleteVirtualNode (..),
    newDeleteVirtualNode,

    -- * Request Lenses
    deleteVirtualNode_meshOwner,
    deleteVirtualNode_meshName,
    deleteVirtualNode_virtualNodeName,

    -- * Destructuring the Response
    DeleteVirtualNodeResponse (..),
    newDeleteVirtualNodeResponse,

    -- * Response Lenses
    deleteVirtualNodeResponse_httpStatus,
    deleteVirtualNodeResponse_virtualNode,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes a virtual node input.
--
-- /See:/ 'newDeleteVirtualNode' smart constructor.
data DeleteVirtualNode = DeleteVirtualNode'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to delete the virtual node in.
    meshName :: Prelude.Text,
    -- | The name of the virtual node to delete.
    virtualNodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteVirtualNode_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'deleteVirtualNode_meshName' - The name of the service mesh to delete the virtual node in.
--
-- 'virtualNodeName', 'deleteVirtualNode_virtualNodeName' - The name of the virtual node to delete.
newDeleteVirtualNode ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualNodeName'
  Prelude.Text ->
  DeleteVirtualNode
newDeleteVirtualNode pMeshName_ pVirtualNodeName_ =
  DeleteVirtualNode'
    { meshOwner = Prelude.Nothing,
      meshName = pMeshName_,
      virtualNodeName = pVirtualNodeName_
    }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteVirtualNode_meshOwner :: Lens.Lens' DeleteVirtualNode (Prelude.Maybe Prelude.Text)
deleteVirtualNode_meshOwner = Lens.lens (\DeleteVirtualNode' {meshOwner} -> meshOwner) (\s@DeleteVirtualNode' {} a -> s {meshOwner = a} :: DeleteVirtualNode)

-- | The name of the service mesh to delete the virtual node in.
deleteVirtualNode_meshName :: Lens.Lens' DeleteVirtualNode Prelude.Text
deleteVirtualNode_meshName = Lens.lens (\DeleteVirtualNode' {meshName} -> meshName) (\s@DeleteVirtualNode' {} a -> s {meshName = a} :: DeleteVirtualNode)

-- | The name of the virtual node to delete.
deleteVirtualNode_virtualNodeName :: Lens.Lens' DeleteVirtualNode Prelude.Text
deleteVirtualNode_virtualNodeName = Lens.lens (\DeleteVirtualNode' {virtualNodeName} -> virtualNodeName) (\s@DeleteVirtualNode' {} a -> s {virtualNodeName = a} :: DeleteVirtualNode)

instance Core.AWSRequest DeleteVirtualNode where
  type
    AWSResponse DeleteVirtualNode =
      DeleteVirtualNodeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualNodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVirtualNode where
  hashWithSalt _salt DeleteVirtualNode' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualNodeName

instance Prelude.NFData DeleteVirtualNode where
  rnf DeleteVirtualNode' {..} =
    Prelude.rnf meshOwner `Prelude.seq`
      Prelude.rnf meshName `Prelude.seq`
        Prelude.rnf virtualNodeName

instance Data.ToHeaders DeleteVirtualNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVirtualNode where
  toPath DeleteVirtualNode' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualNodes/",
        Data.toBS virtualNodeName
      ]

instance Data.ToQuery DeleteVirtualNode where
  toQuery DeleteVirtualNode' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDeleteVirtualNodeResponse' smart constructor.
data DeleteVirtualNodeResponse = DeleteVirtualNodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The virtual node that was deleted.
    virtualNode :: VirtualNodeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVirtualNodeResponse_httpStatus' - The response's http status code.
--
-- 'virtualNode', 'deleteVirtualNodeResponse_virtualNode' - The virtual node that was deleted.
newDeleteVirtualNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualNode'
  VirtualNodeData ->
  DeleteVirtualNodeResponse
newDeleteVirtualNodeResponse
  pHttpStatus_
  pVirtualNode_ =
    DeleteVirtualNodeResponse'
      { httpStatus =
          pHttpStatus_,
        virtualNode = pVirtualNode_
      }

-- | The response's http status code.
deleteVirtualNodeResponse_httpStatus :: Lens.Lens' DeleteVirtualNodeResponse Prelude.Int
deleteVirtualNodeResponse_httpStatus = Lens.lens (\DeleteVirtualNodeResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualNodeResponse' {} a -> s {httpStatus = a} :: DeleteVirtualNodeResponse)

-- | The virtual node that was deleted.
deleteVirtualNodeResponse_virtualNode :: Lens.Lens' DeleteVirtualNodeResponse VirtualNodeData
deleteVirtualNodeResponse_virtualNode = Lens.lens (\DeleteVirtualNodeResponse' {virtualNode} -> virtualNode) (\s@DeleteVirtualNodeResponse' {} a -> s {virtualNode = a} :: DeleteVirtualNodeResponse)

instance Prelude.NFData DeleteVirtualNodeResponse where
  rnf DeleteVirtualNodeResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf virtualNode
