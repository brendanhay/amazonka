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
-- Module      : Amazonka.AppMesh.DeleteVirtualGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual gateway. You cannot delete a virtual gateway
-- if any gateway routes are associated to it.
module Amazonka.AppMesh.DeleteVirtualGateway
  ( -- * Creating a Request
    DeleteVirtualGateway (..),
    newDeleteVirtualGateway,

    -- * Request Lenses
    deleteVirtualGateway_meshOwner,
    deleteVirtualGateway_meshName,
    deleteVirtualGateway_virtualGatewayName,

    -- * Destructuring the Response
    DeleteVirtualGatewayResponse (..),
    newDeleteVirtualGatewayResponse,

    -- * Response Lenses
    deleteVirtualGatewayResponse_httpStatus,
    deleteVirtualGatewayResponse_virtualGateway,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVirtualGateway' smart constructor.
data DeleteVirtualGateway = DeleteVirtualGateway'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to delete the virtual gateway from.
    meshName :: Prelude.Text,
    -- | The name of the virtual gateway to delete.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteVirtualGateway_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'deleteVirtualGateway_meshName' - The name of the service mesh to delete the virtual gateway from.
--
-- 'virtualGatewayName', 'deleteVirtualGateway_virtualGatewayName' - The name of the virtual gateway to delete.
newDeleteVirtualGateway ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  DeleteVirtualGateway
newDeleteVirtualGateway
  pMeshName_
  pVirtualGatewayName_ =
    DeleteVirtualGateway'
      { meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteVirtualGateway_meshOwner :: Lens.Lens' DeleteVirtualGateway (Prelude.Maybe Prelude.Text)
deleteVirtualGateway_meshOwner = Lens.lens (\DeleteVirtualGateway' {meshOwner} -> meshOwner) (\s@DeleteVirtualGateway' {} a -> s {meshOwner = a} :: DeleteVirtualGateway)

-- | The name of the service mesh to delete the virtual gateway from.
deleteVirtualGateway_meshName :: Lens.Lens' DeleteVirtualGateway Prelude.Text
deleteVirtualGateway_meshName = Lens.lens (\DeleteVirtualGateway' {meshName} -> meshName) (\s@DeleteVirtualGateway' {} a -> s {meshName = a} :: DeleteVirtualGateway)

-- | The name of the virtual gateway to delete.
deleteVirtualGateway_virtualGatewayName :: Lens.Lens' DeleteVirtualGateway Prelude.Text
deleteVirtualGateway_virtualGatewayName = Lens.lens (\DeleteVirtualGateway' {virtualGatewayName} -> virtualGatewayName) (\s@DeleteVirtualGateway' {} a -> s {virtualGatewayName = a} :: DeleteVirtualGateway)

instance Core.AWSRequest DeleteVirtualGateway where
  type
    AWSResponse DeleteVirtualGateway =
      DeleteVirtualGatewayResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVirtualGateway where
  hashWithSalt _salt DeleteVirtualGateway' {..} =
    _salt `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData DeleteVirtualGateway where
  rnf DeleteVirtualGateway' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualGatewayName

instance Core.ToHeaders DeleteVirtualGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteVirtualGateway where
  toPath DeleteVirtualGateway' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualGateways/",
        Core.toBS virtualGatewayName
      ]

instance Core.ToQuery DeleteVirtualGateway where
  toQuery DeleteVirtualGateway' {..} =
    Prelude.mconcat ["meshOwner" Core.=: meshOwner]

-- | /See:/ 'newDeleteVirtualGatewayResponse' smart constructor.
data DeleteVirtualGatewayResponse = DeleteVirtualGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The virtual gateway that was deleted.
    virtualGateway :: VirtualGatewayData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVirtualGatewayResponse_httpStatus' - The response's http status code.
--
-- 'virtualGateway', 'deleteVirtualGatewayResponse_virtualGateway' - The virtual gateway that was deleted.
newDeleteVirtualGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualGateway'
  VirtualGatewayData ->
  DeleteVirtualGatewayResponse
newDeleteVirtualGatewayResponse
  pHttpStatus_
  pVirtualGateway_ =
    DeleteVirtualGatewayResponse'
      { httpStatus =
          pHttpStatus_,
        virtualGateway = pVirtualGateway_
      }

-- | The response's http status code.
deleteVirtualGatewayResponse_httpStatus :: Lens.Lens' DeleteVirtualGatewayResponse Prelude.Int
deleteVirtualGatewayResponse_httpStatus = Lens.lens (\DeleteVirtualGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualGatewayResponse' {} a -> s {httpStatus = a} :: DeleteVirtualGatewayResponse)

-- | The virtual gateway that was deleted.
deleteVirtualGatewayResponse_virtualGateway :: Lens.Lens' DeleteVirtualGatewayResponse VirtualGatewayData
deleteVirtualGatewayResponse_virtualGateway = Lens.lens (\DeleteVirtualGatewayResponse' {virtualGateway} -> virtualGateway) (\s@DeleteVirtualGatewayResponse' {} a -> s {virtualGateway = a} :: DeleteVirtualGatewayResponse)

instance Prelude.NFData DeleteVirtualGatewayResponse where
  rnf DeleteVirtualGatewayResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualGateway
