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
-- Module      : Amazonka.AppMesh.DescribeVirtualNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing virtual node.
module Amazonka.AppMesh.DescribeVirtualNode
  ( -- * Creating a Request
    DescribeVirtualNode (..),
    newDescribeVirtualNode,

    -- * Request Lenses
    describeVirtualNode_meshOwner,
    describeVirtualNode_meshName,
    describeVirtualNode_virtualNodeName,

    -- * Destructuring the Response
    DescribeVirtualNodeResponse (..),
    newDescribeVirtualNodeResponse,

    -- * Response Lenses
    describeVirtualNodeResponse_httpStatus,
    describeVirtualNodeResponse_virtualNode,
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
-- /See:/ 'newDescribeVirtualNode' smart constructor.
data DescribeVirtualNode = DescribeVirtualNode'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual node resides in.
    meshName :: Prelude.Text,
    -- | The name of the virtual node to describe.
    virtualNodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeVirtualNode_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'describeVirtualNode_meshName' - The name of the service mesh that the virtual node resides in.
--
-- 'virtualNodeName', 'describeVirtualNode_virtualNodeName' - The name of the virtual node to describe.
newDescribeVirtualNode ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualNodeName'
  Prelude.Text ->
  DescribeVirtualNode
newDescribeVirtualNode pMeshName_ pVirtualNodeName_ =
  DescribeVirtualNode'
    { meshOwner = Prelude.Nothing,
      meshName = pMeshName_,
      virtualNodeName = pVirtualNodeName_
    }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeVirtualNode_meshOwner :: Lens.Lens' DescribeVirtualNode (Prelude.Maybe Prelude.Text)
describeVirtualNode_meshOwner = Lens.lens (\DescribeVirtualNode' {meshOwner} -> meshOwner) (\s@DescribeVirtualNode' {} a -> s {meshOwner = a} :: DescribeVirtualNode)

-- | The name of the service mesh that the virtual node resides in.
describeVirtualNode_meshName :: Lens.Lens' DescribeVirtualNode Prelude.Text
describeVirtualNode_meshName = Lens.lens (\DescribeVirtualNode' {meshName} -> meshName) (\s@DescribeVirtualNode' {} a -> s {meshName = a} :: DescribeVirtualNode)

-- | The name of the virtual node to describe.
describeVirtualNode_virtualNodeName :: Lens.Lens' DescribeVirtualNode Prelude.Text
describeVirtualNode_virtualNodeName = Lens.lens (\DescribeVirtualNode' {virtualNodeName} -> virtualNodeName) (\s@DescribeVirtualNode' {} a -> s {virtualNodeName = a} :: DescribeVirtualNode)

instance Core.AWSRequest DescribeVirtualNode where
  type
    AWSResponse DescribeVirtualNode =
      DescribeVirtualNodeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualNodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeVirtualNode where
  hashWithSalt _salt DescribeVirtualNode' {..} =
    _salt `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualNodeName

instance Prelude.NFData DescribeVirtualNode where
  rnf DescribeVirtualNode' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualNodeName

instance Data.ToHeaders DescribeVirtualNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVirtualNode where
  toPath DescribeVirtualNode' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualNodes/",
        Data.toBS virtualNodeName
      ]

instance Data.ToQuery DescribeVirtualNode where
  toQuery DescribeVirtualNode' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDescribeVirtualNodeResponse' smart constructor.
data DescribeVirtualNodeResponse = DescribeVirtualNodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual node.
    virtualNode :: VirtualNodeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVirtualNodeResponse_httpStatus' - The response's http status code.
--
-- 'virtualNode', 'describeVirtualNodeResponse_virtualNode' - The full description of your virtual node.
newDescribeVirtualNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualNode'
  VirtualNodeData ->
  DescribeVirtualNodeResponse
newDescribeVirtualNodeResponse
  pHttpStatus_
  pVirtualNode_ =
    DescribeVirtualNodeResponse'
      { httpStatus =
          pHttpStatus_,
        virtualNode = pVirtualNode_
      }

-- | The response's http status code.
describeVirtualNodeResponse_httpStatus :: Lens.Lens' DescribeVirtualNodeResponse Prelude.Int
describeVirtualNodeResponse_httpStatus = Lens.lens (\DescribeVirtualNodeResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualNodeResponse' {} a -> s {httpStatus = a} :: DescribeVirtualNodeResponse)

-- | The full description of your virtual node.
describeVirtualNodeResponse_virtualNode :: Lens.Lens' DescribeVirtualNodeResponse VirtualNodeData
describeVirtualNodeResponse_virtualNode = Lens.lens (\DescribeVirtualNodeResponse' {virtualNode} -> virtualNode) (\s@DescribeVirtualNodeResponse' {} a -> s {virtualNode = a} :: DescribeVirtualNodeResponse)

instance Prelude.NFData DescribeVirtualNodeResponse where
  rnf DescribeVirtualNodeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualNode
