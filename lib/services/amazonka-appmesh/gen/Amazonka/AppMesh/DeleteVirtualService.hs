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
-- Module      : Amazonka.AppMesh.DeleteVirtualService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual service.
module Amazonka.AppMesh.DeleteVirtualService
  ( -- * Creating a Request
    DeleteVirtualService (..),
    newDeleteVirtualService,

    -- * Request Lenses
    deleteVirtualService_meshOwner,
    deleteVirtualService_meshName,
    deleteVirtualService_virtualServiceName,

    -- * Destructuring the Response
    DeleteVirtualServiceResponse (..),
    newDeleteVirtualServiceResponse,

    -- * Response Lenses
    deleteVirtualServiceResponse_httpStatus,
    deleteVirtualServiceResponse_virtualService,
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
-- /See:/ 'newDeleteVirtualService' smart constructor.
data DeleteVirtualService = DeleteVirtualService'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to delete the virtual service in.
    meshName :: Prelude.Text,
    -- | The name of the virtual service to delete.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteVirtualService_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'deleteVirtualService_meshName' - The name of the service mesh to delete the virtual service in.
--
-- 'virtualServiceName', 'deleteVirtualService_virtualServiceName' - The name of the virtual service to delete.
newDeleteVirtualService ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualServiceName'
  Prelude.Text ->
  DeleteVirtualService
newDeleteVirtualService
  pMeshName_
  pVirtualServiceName_ =
    DeleteVirtualService'
      { meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        virtualServiceName = pVirtualServiceName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteVirtualService_meshOwner :: Lens.Lens' DeleteVirtualService (Prelude.Maybe Prelude.Text)
deleteVirtualService_meshOwner = Lens.lens (\DeleteVirtualService' {meshOwner} -> meshOwner) (\s@DeleteVirtualService' {} a -> s {meshOwner = a} :: DeleteVirtualService)

-- | The name of the service mesh to delete the virtual service in.
deleteVirtualService_meshName :: Lens.Lens' DeleteVirtualService Prelude.Text
deleteVirtualService_meshName = Lens.lens (\DeleteVirtualService' {meshName} -> meshName) (\s@DeleteVirtualService' {} a -> s {meshName = a} :: DeleteVirtualService)

-- | The name of the virtual service to delete.
deleteVirtualService_virtualServiceName :: Lens.Lens' DeleteVirtualService Prelude.Text
deleteVirtualService_virtualServiceName = Lens.lens (\DeleteVirtualService' {virtualServiceName} -> virtualServiceName) (\s@DeleteVirtualService' {} a -> s {virtualServiceName = a} :: DeleteVirtualService)

instance Core.AWSRequest DeleteVirtualService where
  type
    AWSResponse DeleteVirtualService =
      DeleteVirtualServiceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVirtualService where
  hashWithSalt _salt DeleteVirtualService' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData DeleteVirtualService where
  rnf DeleteVirtualService' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualServiceName

instance Data.ToHeaders DeleteVirtualService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVirtualService where
  toPath DeleteVirtualService' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualServices/",
        Data.toBS virtualServiceName
      ]

instance Data.ToQuery DeleteVirtualService where
  toQuery DeleteVirtualService' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDeleteVirtualServiceResponse' smart constructor.
data DeleteVirtualServiceResponse = DeleteVirtualServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The virtual service that was deleted.
    virtualService :: VirtualServiceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVirtualServiceResponse_httpStatus' - The response's http status code.
--
-- 'virtualService', 'deleteVirtualServiceResponse_virtualService' - The virtual service that was deleted.
newDeleteVirtualServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualService'
  VirtualServiceData ->
  DeleteVirtualServiceResponse
newDeleteVirtualServiceResponse
  pHttpStatus_
  pVirtualService_ =
    DeleteVirtualServiceResponse'
      { httpStatus =
          pHttpStatus_,
        virtualService = pVirtualService_
      }

-- | The response's http status code.
deleteVirtualServiceResponse_httpStatus :: Lens.Lens' DeleteVirtualServiceResponse Prelude.Int
deleteVirtualServiceResponse_httpStatus = Lens.lens (\DeleteVirtualServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualServiceResponse' {} a -> s {httpStatus = a} :: DeleteVirtualServiceResponse)

-- | The virtual service that was deleted.
deleteVirtualServiceResponse_virtualService :: Lens.Lens' DeleteVirtualServiceResponse VirtualServiceData
deleteVirtualServiceResponse_virtualService = Lens.lens (\DeleteVirtualServiceResponse' {virtualService} -> virtualService) (\s@DeleteVirtualServiceResponse' {} a -> s {virtualService = a} :: DeleteVirtualServiceResponse)

instance Prelude.NFData DeleteVirtualServiceResponse where
  rnf DeleteVirtualServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualService
