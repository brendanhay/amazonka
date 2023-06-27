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
-- Module      : Amazonka.AppMesh.UpdateVirtualNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing virtual node in a specified service mesh.
module Amazonka.AppMesh.UpdateVirtualNode
  ( -- * Creating a Request
    UpdateVirtualNode (..),
    newUpdateVirtualNode,

    -- * Request Lenses
    updateVirtualNode_clientToken,
    updateVirtualNode_meshOwner,
    updateVirtualNode_meshName,
    updateVirtualNode_spec,
    updateVirtualNode_virtualNodeName,

    -- * Destructuring the Response
    UpdateVirtualNodeResponse (..),
    newUpdateVirtualNodeResponse,

    -- * Response Lenses
    updateVirtualNodeResponse_httpStatus,
    updateVirtualNodeResponse_virtualNode,
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
-- /See:/ 'newUpdateVirtualNode' smart constructor.
data UpdateVirtualNode = UpdateVirtualNode'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual node resides in.
    meshName :: Prelude.Text,
    -- | The new virtual node specification to apply. This overwrites the
    -- existing data.
    spec :: VirtualNodeSpec,
    -- | The name of the virtual node to update.
    virtualNodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateVirtualNode_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'updateVirtualNode_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'updateVirtualNode_meshName' - The name of the service mesh that the virtual node resides in.
--
-- 'spec', 'updateVirtualNode_spec' - The new virtual node specification to apply. This overwrites the
-- existing data.
--
-- 'virtualNodeName', 'updateVirtualNode_virtualNodeName' - The name of the virtual node to update.
newUpdateVirtualNode ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualNodeSpec ->
  -- | 'virtualNodeName'
  Prelude.Text ->
  UpdateVirtualNode
newUpdateVirtualNode
  pMeshName_
  pSpec_
  pVirtualNodeName_ =
    UpdateVirtualNode'
      { clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualNodeName = pVirtualNodeName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateVirtualNode_clientToken :: Lens.Lens' UpdateVirtualNode (Prelude.Maybe Prelude.Text)
updateVirtualNode_clientToken = Lens.lens (\UpdateVirtualNode' {clientToken} -> clientToken) (\s@UpdateVirtualNode' {} a -> s {clientToken = a} :: UpdateVirtualNode)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
updateVirtualNode_meshOwner :: Lens.Lens' UpdateVirtualNode (Prelude.Maybe Prelude.Text)
updateVirtualNode_meshOwner = Lens.lens (\UpdateVirtualNode' {meshOwner} -> meshOwner) (\s@UpdateVirtualNode' {} a -> s {meshOwner = a} :: UpdateVirtualNode)

-- | The name of the service mesh that the virtual node resides in.
updateVirtualNode_meshName :: Lens.Lens' UpdateVirtualNode Prelude.Text
updateVirtualNode_meshName = Lens.lens (\UpdateVirtualNode' {meshName} -> meshName) (\s@UpdateVirtualNode' {} a -> s {meshName = a} :: UpdateVirtualNode)

-- | The new virtual node specification to apply. This overwrites the
-- existing data.
updateVirtualNode_spec :: Lens.Lens' UpdateVirtualNode VirtualNodeSpec
updateVirtualNode_spec = Lens.lens (\UpdateVirtualNode' {spec} -> spec) (\s@UpdateVirtualNode' {} a -> s {spec = a} :: UpdateVirtualNode)

-- | The name of the virtual node to update.
updateVirtualNode_virtualNodeName :: Lens.Lens' UpdateVirtualNode Prelude.Text
updateVirtualNode_virtualNodeName = Lens.lens (\UpdateVirtualNode' {virtualNodeName} -> virtualNodeName) (\s@UpdateVirtualNode' {} a -> s {virtualNodeName = a} :: UpdateVirtualNode)

instance Core.AWSRequest UpdateVirtualNode where
  type
    AWSResponse UpdateVirtualNode =
      UpdateVirtualNodeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVirtualNodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateVirtualNode where
  hashWithSalt _salt UpdateVirtualNode' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualNodeName

instance Prelude.NFData UpdateVirtualNode where
  rnf UpdateVirtualNode' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualNodeName

instance Data.ToHeaders UpdateVirtualNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVirtualNode where
  toJSON UpdateVirtualNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath UpdateVirtualNode where
  toPath UpdateVirtualNode' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualNodes/",
        Data.toBS virtualNodeName
      ]

instance Data.ToQuery UpdateVirtualNode where
  toQuery UpdateVirtualNode' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newUpdateVirtualNodeResponse' smart constructor.
data UpdateVirtualNodeResponse = UpdateVirtualNodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the virtual node that was updated.
    virtualNode :: VirtualNodeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVirtualNodeResponse_httpStatus' - The response's http status code.
--
-- 'virtualNode', 'updateVirtualNodeResponse_virtualNode' - A full description of the virtual node that was updated.
newUpdateVirtualNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualNode'
  VirtualNodeData ->
  UpdateVirtualNodeResponse
newUpdateVirtualNodeResponse
  pHttpStatus_
  pVirtualNode_ =
    UpdateVirtualNodeResponse'
      { httpStatus =
          pHttpStatus_,
        virtualNode = pVirtualNode_
      }

-- | The response's http status code.
updateVirtualNodeResponse_httpStatus :: Lens.Lens' UpdateVirtualNodeResponse Prelude.Int
updateVirtualNodeResponse_httpStatus = Lens.lens (\UpdateVirtualNodeResponse' {httpStatus} -> httpStatus) (\s@UpdateVirtualNodeResponse' {} a -> s {httpStatus = a} :: UpdateVirtualNodeResponse)

-- | A full description of the virtual node that was updated.
updateVirtualNodeResponse_virtualNode :: Lens.Lens' UpdateVirtualNodeResponse VirtualNodeData
updateVirtualNodeResponse_virtualNode = Lens.lens (\UpdateVirtualNodeResponse' {virtualNode} -> virtualNode) (\s@UpdateVirtualNodeResponse' {} a -> s {virtualNode = a} :: UpdateVirtualNodeResponse)

instance Prelude.NFData UpdateVirtualNodeResponse where
  rnf UpdateVirtualNodeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualNode
