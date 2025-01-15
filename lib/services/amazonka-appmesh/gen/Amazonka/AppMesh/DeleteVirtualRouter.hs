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
-- Module      : Amazonka.AppMesh.DeleteVirtualRouter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual router.
--
-- You must delete any routes associated with the virtual router before you
-- can delete the router itself.
module Amazonka.AppMesh.DeleteVirtualRouter
  ( -- * Creating a Request
    DeleteVirtualRouter (..),
    newDeleteVirtualRouter,

    -- * Request Lenses
    deleteVirtualRouter_meshOwner,
    deleteVirtualRouter_meshName,
    deleteVirtualRouter_virtualRouterName,

    -- * Destructuring the Response
    DeleteVirtualRouterResponse (..),
    newDeleteVirtualRouterResponse,

    -- * Response Lenses
    deleteVirtualRouterResponse_httpStatus,
    deleteVirtualRouterResponse_virtualRouter,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteVirtualRouter' smart constructor.
data DeleteVirtualRouter = DeleteVirtualRouter'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to delete the virtual router in.
    meshName :: Prelude.Text,
    -- | The name of the virtual router to delete.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualRouter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteVirtualRouter_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'deleteVirtualRouter_meshName' - The name of the service mesh to delete the virtual router in.
--
-- 'virtualRouterName', 'deleteVirtualRouter_virtualRouterName' - The name of the virtual router to delete.
newDeleteVirtualRouter ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  DeleteVirtualRouter
newDeleteVirtualRouter pMeshName_ pVirtualRouterName_ =
  DeleteVirtualRouter'
    { meshOwner = Prelude.Nothing,
      meshName = pMeshName_,
      virtualRouterName = pVirtualRouterName_
    }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteVirtualRouter_meshOwner :: Lens.Lens' DeleteVirtualRouter (Prelude.Maybe Prelude.Text)
deleteVirtualRouter_meshOwner = Lens.lens (\DeleteVirtualRouter' {meshOwner} -> meshOwner) (\s@DeleteVirtualRouter' {} a -> s {meshOwner = a} :: DeleteVirtualRouter)

-- | The name of the service mesh to delete the virtual router in.
deleteVirtualRouter_meshName :: Lens.Lens' DeleteVirtualRouter Prelude.Text
deleteVirtualRouter_meshName = Lens.lens (\DeleteVirtualRouter' {meshName} -> meshName) (\s@DeleteVirtualRouter' {} a -> s {meshName = a} :: DeleteVirtualRouter)

-- | The name of the virtual router to delete.
deleteVirtualRouter_virtualRouterName :: Lens.Lens' DeleteVirtualRouter Prelude.Text
deleteVirtualRouter_virtualRouterName = Lens.lens (\DeleteVirtualRouter' {virtualRouterName} -> virtualRouterName) (\s@DeleteVirtualRouter' {} a -> s {virtualRouterName = a} :: DeleteVirtualRouter)

instance Core.AWSRequest DeleteVirtualRouter where
  type
    AWSResponse DeleteVirtualRouter =
      DeleteVirtualRouterResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualRouterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVirtualRouter where
  hashWithSalt _salt DeleteVirtualRouter' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData DeleteVirtualRouter where
  rnf DeleteVirtualRouter' {..} =
    Prelude.rnf meshOwner `Prelude.seq`
      Prelude.rnf meshName `Prelude.seq`
        Prelude.rnf virtualRouterName

instance Data.ToHeaders DeleteVirtualRouter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVirtualRouter where
  toPath DeleteVirtualRouter' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouters/",
        Data.toBS virtualRouterName
      ]

instance Data.ToQuery DeleteVirtualRouter where
  toQuery DeleteVirtualRouter' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDeleteVirtualRouterResponse' smart constructor.
data DeleteVirtualRouterResponse = DeleteVirtualRouterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The virtual router that was deleted.
    virtualRouter :: VirtualRouterData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualRouterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVirtualRouterResponse_httpStatus' - The response's http status code.
--
-- 'virtualRouter', 'deleteVirtualRouterResponse_virtualRouter' - The virtual router that was deleted.
newDeleteVirtualRouterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualRouter'
  VirtualRouterData ->
  DeleteVirtualRouterResponse
newDeleteVirtualRouterResponse
  pHttpStatus_
  pVirtualRouter_ =
    DeleteVirtualRouterResponse'
      { httpStatus =
          pHttpStatus_,
        virtualRouter = pVirtualRouter_
      }

-- | The response's http status code.
deleteVirtualRouterResponse_httpStatus :: Lens.Lens' DeleteVirtualRouterResponse Prelude.Int
deleteVirtualRouterResponse_httpStatus = Lens.lens (\DeleteVirtualRouterResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualRouterResponse' {} a -> s {httpStatus = a} :: DeleteVirtualRouterResponse)

-- | The virtual router that was deleted.
deleteVirtualRouterResponse_virtualRouter :: Lens.Lens' DeleteVirtualRouterResponse VirtualRouterData
deleteVirtualRouterResponse_virtualRouter = Lens.lens (\DeleteVirtualRouterResponse' {virtualRouter} -> virtualRouter) (\s@DeleteVirtualRouterResponse' {} a -> s {virtualRouter = a} :: DeleteVirtualRouterResponse)

instance Prelude.NFData DeleteVirtualRouterResponse where
  rnf DeleteVirtualRouterResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf virtualRouter
