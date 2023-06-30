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
-- Module      : Amazonka.AppMesh.CreateVirtualGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual gateway.
--
-- A virtual gateway allows resources outside your mesh to communicate to
-- resources that are inside your mesh. The virtual gateway represents an
-- Envoy proxy running in an Amazon ECS task, in a Kubernetes service, or
-- on an Amazon EC2 instance. Unlike a virtual node, which represents an
-- Envoy running with an application, a virtual gateway represents Envoy
-- deployed by itself.
--
-- For more information about virtual gateways, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/virtual_gateways.html Virtual gateways>.
module Amazonka.AppMesh.CreateVirtualGateway
  ( -- * Creating a Request
    CreateVirtualGateway (..),
    newCreateVirtualGateway,

    -- * Request Lenses
    createVirtualGateway_clientToken,
    createVirtualGateway_meshOwner,
    createVirtualGateway_tags,
    createVirtualGateway_meshName,
    createVirtualGateway_spec,
    createVirtualGateway_virtualGatewayName,

    -- * Destructuring the Response
    CreateVirtualGatewayResponse (..),
    newCreateVirtualGatewayResponse,

    -- * Response Lenses
    createVirtualGatewayResponse_httpStatus,
    createVirtualGatewayResponse_virtualGateway,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVirtualGateway' smart constructor.
data CreateVirtualGateway = CreateVirtualGateway'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then the account that you specify must share
    -- the mesh with your account before you can create the resource in the
    -- service mesh. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you can apply to the virtual gateway to assist
    -- with categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | The name of the service mesh to create the virtual gateway in.
    meshName :: Prelude.Text,
    -- | The virtual gateway specification to apply.
    spec :: VirtualGatewaySpec,
    -- | The name to use for the virtual gateway.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVirtualGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'createVirtualGateway_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'tags', 'createVirtualGateway_tags' - Optional metadata that you can apply to the virtual gateway to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'meshName', 'createVirtualGateway_meshName' - The name of the service mesh to create the virtual gateway in.
--
-- 'spec', 'createVirtualGateway_spec' - The virtual gateway specification to apply.
--
-- 'virtualGatewayName', 'createVirtualGateway_virtualGatewayName' - The name to use for the virtual gateway.
newCreateVirtualGateway ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualGatewaySpec ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  CreateVirtualGateway
newCreateVirtualGateway
  pMeshName_
  pSpec_
  pVirtualGatewayName_ =
    CreateVirtualGateway'
      { clientToken =
          Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        tags = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createVirtualGateway_clientToken :: Lens.Lens' CreateVirtualGateway (Prelude.Maybe Prelude.Text)
createVirtualGateway_clientToken = Lens.lens (\CreateVirtualGateway' {clientToken} -> clientToken) (\s@CreateVirtualGateway' {} a -> s {clientToken = a} :: CreateVirtualGateway)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
createVirtualGateway_meshOwner :: Lens.Lens' CreateVirtualGateway (Prelude.Maybe Prelude.Text)
createVirtualGateway_meshOwner = Lens.lens (\CreateVirtualGateway' {meshOwner} -> meshOwner) (\s@CreateVirtualGateway' {} a -> s {meshOwner = a} :: CreateVirtualGateway)

-- | Optional metadata that you can apply to the virtual gateway to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createVirtualGateway_tags :: Lens.Lens' CreateVirtualGateway (Prelude.Maybe [TagRef])
createVirtualGateway_tags = Lens.lens (\CreateVirtualGateway' {tags} -> tags) (\s@CreateVirtualGateway' {} a -> s {tags = a} :: CreateVirtualGateway) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service mesh to create the virtual gateway in.
createVirtualGateway_meshName :: Lens.Lens' CreateVirtualGateway Prelude.Text
createVirtualGateway_meshName = Lens.lens (\CreateVirtualGateway' {meshName} -> meshName) (\s@CreateVirtualGateway' {} a -> s {meshName = a} :: CreateVirtualGateway)

-- | The virtual gateway specification to apply.
createVirtualGateway_spec :: Lens.Lens' CreateVirtualGateway VirtualGatewaySpec
createVirtualGateway_spec = Lens.lens (\CreateVirtualGateway' {spec} -> spec) (\s@CreateVirtualGateway' {} a -> s {spec = a} :: CreateVirtualGateway)

-- | The name to use for the virtual gateway.
createVirtualGateway_virtualGatewayName :: Lens.Lens' CreateVirtualGateway Prelude.Text
createVirtualGateway_virtualGatewayName = Lens.lens (\CreateVirtualGateway' {virtualGatewayName} -> virtualGatewayName) (\s@CreateVirtualGateway' {} a -> s {virtualGatewayName = a} :: CreateVirtualGateway)

instance Core.AWSRequest CreateVirtualGateway where
  type
    AWSResponse CreateVirtualGateway =
      CreateVirtualGatewayResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVirtualGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable CreateVirtualGateway where
  hashWithSalt _salt CreateVirtualGateway' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData CreateVirtualGateway where
  rnf CreateVirtualGateway' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualGatewayName

instance Data.ToHeaders CreateVirtualGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVirtualGateway where
  toJSON CreateVirtualGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("spec" Data..= spec),
            Prelude.Just
              ("virtualGatewayName" Data..= virtualGatewayName)
          ]
      )

instance Data.ToPath CreateVirtualGateway where
  toPath CreateVirtualGateway' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualGateways"
      ]

instance Data.ToQuery CreateVirtualGateway where
  toQuery CreateVirtualGateway' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- | /See:/ 'newCreateVirtualGatewayResponse' smart constructor.
data CreateVirtualGatewayResponse = CreateVirtualGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual gateway following the create call.
    virtualGateway :: VirtualGatewayData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVirtualGatewayResponse_httpStatus' - The response's http status code.
--
-- 'virtualGateway', 'createVirtualGatewayResponse_virtualGateway' - The full description of your virtual gateway following the create call.
newCreateVirtualGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualGateway'
  VirtualGatewayData ->
  CreateVirtualGatewayResponse
newCreateVirtualGatewayResponse
  pHttpStatus_
  pVirtualGateway_ =
    CreateVirtualGatewayResponse'
      { httpStatus =
          pHttpStatus_,
        virtualGateway = pVirtualGateway_
      }

-- | The response's http status code.
createVirtualGatewayResponse_httpStatus :: Lens.Lens' CreateVirtualGatewayResponse Prelude.Int
createVirtualGatewayResponse_httpStatus = Lens.lens (\CreateVirtualGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualGatewayResponse' {} a -> s {httpStatus = a} :: CreateVirtualGatewayResponse)

-- | The full description of your virtual gateway following the create call.
createVirtualGatewayResponse_virtualGateway :: Lens.Lens' CreateVirtualGatewayResponse VirtualGatewayData
createVirtualGatewayResponse_virtualGateway = Lens.lens (\CreateVirtualGatewayResponse' {virtualGateway} -> virtualGateway) (\s@CreateVirtualGatewayResponse' {} a -> s {virtualGateway = a} :: CreateVirtualGatewayResponse)

instance Prelude.NFData CreateVirtualGatewayResponse where
  rnf CreateVirtualGatewayResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualGateway
