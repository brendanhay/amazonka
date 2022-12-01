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
-- Module      : Amazonka.AppMesh.CreateVirtualNode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual node within a service mesh.
--
-- A virtual node acts as a logical pointer to a particular task group,
-- such as an Amazon ECS service or a Kubernetes deployment. When you
-- create a virtual node, you can specify the service discovery information
-- for your task group, and whether the proxy running in a task group will
-- communicate with other proxies using Transport Layer Security (TLS).
--
-- You define a @listener@ for any inbound traffic that your virtual node
-- expects. Any virtual service that your virtual node expects to
-- communicate to is specified as a @backend@.
--
-- The response metadata for your new virtual node contains the @arn@ that
-- is associated with the virtual node. Set this value to the full ARN; for
-- example,
-- @arn:aws:appmesh:us-west-2:123456789012:myMesh\/default\/virtualNode\/myApp@)
-- as the @APPMESH_RESOURCE_ARN@ environment variable for your task
-- group\'s Envoy proxy container in your task definition or pod spec. This
-- is then mapped to the @node.id@ and @node.cluster@ Envoy parameters.
--
-- By default, App Mesh uses the name of the resource you specified in
-- @APPMESH_RESOURCE_ARN@ when Envoy is referring to itself in metrics and
-- traces. You can override this behavior by setting the
-- @APPMESH_RESOURCE_CLUSTER@ environment variable with your own name.
--
-- For more information about virtual nodes, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/virtual_nodes.html Virtual nodes>.
-- You must be using @1.15.0@ or later of the Envoy image when setting
-- these variables. For more information aboutApp Mesh Envoy variables, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/envoy.html Envoy image>
-- in the App Mesh User Guide.
module Amazonka.AppMesh.CreateVirtualNode
  ( -- * Creating a Request
    CreateVirtualNode (..),
    newCreateVirtualNode,

    -- * Request Lenses
    createVirtualNode_tags,
    createVirtualNode_clientToken,
    createVirtualNode_meshOwner,
    createVirtualNode_meshName,
    createVirtualNode_spec,
    createVirtualNode_virtualNodeName,

    -- * Destructuring the Response
    CreateVirtualNodeResponse (..),
    newCreateVirtualNodeResponse,

    -- * Response Lenses
    createVirtualNodeResponse_httpStatus,
    createVirtualNodeResponse_virtualNode,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateVirtualNode' smart constructor.
data CreateVirtualNode = CreateVirtualNode'
  { -- | Optional metadata that you can apply to the virtual node to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then the account that you specify must share
    -- the mesh with your account before you can create the resource in the
    -- service mesh. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to create the virtual node in.
    meshName :: Prelude.Text,
    -- | The virtual node specification to apply.
    spec :: VirtualNodeSpec,
    -- | The name to use for the virtual node.
    virtualNodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVirtualNode_tags' - Optional metadata that you can apply to the virtual node to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'clientToken', 'createVirtualNode_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'createVirtualNode_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'createVirtualNode_meshName' - The name of the service mesh to create the virtual node in.
--
-- 'spec', 'createVirtualNode_spec' - The virtual node specification to apply.
--
-- 'virtualNodeName', 'createVirtualNode_virtualNodeName' - The name to use for the virtual node.
newCreateVirtualNode ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualNodeSpec ->
  -- | 'virtualNodeName'
  Prelude.Text ->
  CreateVirtualNode
newCreateVirtualNode
  pMeshName_
  pSpec_
  pVirtualNodeName_ =
    CreateVirtualNode'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualNodeName = pVirtualNodeName_
      }

-- | Optional metadata that you can apply to the virtual node to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createVirtualNode_tags :: Lens.Lens' CreateVirtualNode (Prelude.Maybe [TagRef])
createVirtualNode_tags = Lens.lens (\CreateVirtualNode' {tags} -> tags) (\s@CreateVirtualNode' {} a -> s {tags = a} :: CreateVirtualNode) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createVirtualNode_clientToken :: Lens.Lens' CreateVirtualNode (Prelude.Maybe Prelude.Text)
createVirtualNode_clientToken = Lens.lens (\CreateVirtualNode' {clientToken} -> clientToken) (\s@CreateVirtualNode' {} a -> s {clientToken = a} :: CreateVirtualNode)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
createVirtualNode_meshOwner :: Lens.Lens' CreateVirtualNode (Prelude.Maybe Prelude.Text)
createVirtualNode_meshOwner = Lens.lens (\CreateVirtualNode' {meshOwner} -> meshOwner) (\s@CreateVirtualNode' {} a -> s {meshOwner = a} :: CreateVirtualNode)

-- | The name of the service mesh to create the virtual node in.
createVirtualNode_meshName :: Lens.Lens' CreateVirtualNode Prelude.Text
createVirtualNode_meshName = Lens.lens (\CreateVirtualNode' {meshName} -> meshName) (\s@CreateVirtualNode' {} a -> s {meshName = a} :: CreateVirtualNode)

-- | The virtual node specification to apply.
createVirtualNode_spec :: Lens.Lens' CreateVirtualNode VirtualNodeSpec
createVirtualNode_spec = Lens.lens (\CreateVirtualNode' {spec} -> spec) (\s@CreateVirtualNode' {} a -> s {spec = a} :: CreateVirtualNode)

-- | The name to use for the virtual node.
createVirtualNode_virtualNodeName :: Lens.Lens' CreateVirtualNode Prelude.Text
createVirtualNode_virtualNodeName = Lens.lens (\CreateVirtualNode' {virtualNodeName} -> virtualNodeName) (\s@CreateVirtualNode' {} a -> s {virtualNodeName = a} :: CreateVirtualNode)

instance Core.AWSRequest CreateVirtualNode where
  type
    AWSResponse CreateVirtualNode =
      CreateVirtualNodeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVirtualNodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateVirtualNode where
  hashWithSalt _salt CreateVirtualNode' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualNodeName

instance Prelude.NFData CreateVirtualNode where
  rnf CreateVirtualNode' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualNodeName

instance Core.ToHeaders CreateVirtualNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVirtualNode where
  toJSON CreateVirtualNode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Core..= spec),
            Prelude.Just
              ("virtualNodeName" Core..= virtualNodeName)
          ]
      )

instance Core.ToPath CreateVirtualNode where
  toPath CreateVirtualNode' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualNodes"
      ]

instance Core.ToQuery CreateVirtualNode where
  toQuery CreateVirtualNode' {..} =
    Prelude.mconcat ["meshOwner" Core.=: meshOwner]

-- |
--
-- /See:/ 'newCreateVirtualNodeResponse' smart constructor.
data CreateVirtualNodeResponse = CreateVirtualNodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual node following the create call.
    virtualNode :: VirtualNodeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVirtualNodeResponse_httpStatus' - The response's http status code.
--
-- 'virtualNode', 'createVirtualNodeResponse_virtualNode' - The full description of your virtual node following the create call.
newCreateVirtualNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualNode'
  VirtualNodeData ->
  CreateVirtualNodeResponse
newCreateVirtualNodeResponse
  pHttpStatus_
  pVirtualNode_ =
    CreateVirtualNodeResponse'
      { httpStatus =
          pHttpStatus_,
        virtualNode = pVirtualNode_
      }

-- | The response's http status code.
createVirtualNodeResponse_httpStatus :: Lens.Lens' CreateVirtualNodeResponse Prelude.Int
createVirtualNodeResponse_httpStatus = Lens.lens (\CreateVirtualNodeResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualNodeResponse' {} a -> s {httpStatus = a} :: CreateVirtualNodeResponse)

-- | The full description of your virtual node following the create call.
createVirtualNodeResponse_virtualNode :: Lens.Lens' CreateVirtualNodeResponse VirtualNodeData
createVirtualNodeResponse_virtualNode = Lens.lens (\CreateVirtualNodeResponse' {virtualNode} -> virtualNode) (\s@CreateVirtualNodeResponse' {} a -> s {virtualNode = a} :: CreateVirtualNodeResponse)

instance Prelude.NFData CreateVirtualNodeResponse where
  rnf CreateVirtualNodeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualNode
