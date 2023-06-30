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
-- Module      : Amazonka.AppMesh.DescribeVirtualRouter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing virtual router.
module Amazonka.AppMesh.DescribeVirtualRouter
  ( -- * Creating a Request
    DescribeVirtualRouter (..),
    newDescribeVirtualRouter,

    -- * Request Lenses
    describeVirtualRouter_meshOwner,
    describeVirtualRouter_meshName,
    describeVirtualRouter_virtualRouterName,

    -- * Destructuring the Response
    DescribeVirtualRouterResponse (..),
    newDescribeVirtualRouterResponse,

    -- * Response Lenses
    describeVirtualRouterResponse_httpStatus,
    describeVirtualRouterResponse_virtualRouter,
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
-- /See:/ 'newDescribeVirtualRouter' smart constructor.
data DescribeVirtualRouter = DescribeVirtualRouter'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual router resides in.
    meshName :: Prelude.Text,
    -- | The name of the virtual router to describe.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualRouter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeVirtualRouter_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'describeVirtualRouter_meshName' - The name of the service mesh that the virtual router resides in.
--
-- 'virtualRouterName', 'describeVirtualRouter_virtualRouterName' - The name of the virtual router to describe.
newDescribeVirtualRouter ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  DescribeVirtualRouter
newDescribeVirtualRouter
  pMeshName_
  pVirtualRouterName_ =
    DescribeVirtualRouter'
      { meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeVirtualRouter_meshOwner :: Lens.Lens' DescribeVirtualRouter (Prelude.Maybe Prelude.Text)
describeVirtualRouter_meshOwner = Lens.lens (\DescribeVirtualRouter' {meshOwner} -> meshOwner) (\s@DescribeVirtualRouter' {} a -> s {meshOwner = a} :: DescribeVirtualRouter)

-- | The name of the service mesh that the virtual router resides in.
describeVirtualRouter_meshName :: Lens.Lens' DescribeVirtualRouter Prelude.Text
describeVirtualRouter_meshName = Lens.lens (\DescribeVirtualRouter' {meshName} -> meshName) (\s@DescribeVirtualRouter' {} a -> s {meshName = a} :: DescribeVirtualRouter)

-- | The name of the virtual router to describe.
describeVirtualRouter_virtualRouterName :: Lens.Lens' DescribeVirtualRouter Prelude.Text
describeVirtualRouter_virtualRouterName = Lens.lens (\DescribeVirtualRouter' {virtualRouterName} -> virtualRouterName) (\s@DescribeVirtualRouter' {} a -> s {virtualRouterName = a} :: DescribeVirtualRouter)

instance Core.AWSRequest DescribeVirtualRouter where
  type
    AWSResponse DescribeVirtualRouter =
      DescribeVirtualRouterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualRouterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeVirtualRouter where
  hashWithSalt _salt DescribeVirtualRouter' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData DescribeVirtualRouter where
  rnf DescribeVirtualRouter' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders DescribeVirtualRouter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVirtualRouter where
  toPath DescribeVirtualRouter' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouters/",
        Data.toBS virtualRouterName
      ]

instance Data.ToQuery DescribeVirtualRouter where
  toQuery DescribeVirtualRouter' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDescribeVirtualRouterResponse' smart constructor.
data DescribeVirtualRouterResponse = DescribeVirtualRouterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual router.
    virtualRouter :: VirtualRouterData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualRouterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVirtualRouterResponse_httpStatus' - The response's http status code.
--
-- 'virtualRouter', 'describeVirtualRouterResponse_virtualRouter' - The full description of your virtual router.
newDescribeVirtualRouterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualRouter'
  VirtualRouterData ->
  DescribeVirtualRouterResponse
newDescribeVirtualRouterResponse
  pHttpStatus_
  pVirtualRouter_ =
    DescribeVirtualRouterResponse'
      { httpStatus =
          pHttpStatus_,
        virtualRouter = pVirtualRouter_
      }

-- | The response's http status code.
describeVirtualRouterResponse_httpStatus :: Lens.Lens' DescribeVirtualRouterResponse Prelude.Int
describeVirtualRouterResponse_httpStatus = Lens.lens (\DescribeVirtualRouterResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualRouterResponse' {} a -> s {httpStatus = a} :: DescribeVirtualRouterResponse)

-- | The full description of your virtual router.
describeVirtualRouterResponse_virtualRouter :: Lens.Lens' DescribeVirtualRouterResponse VirtualRouterData
describeVirtualRouterResponse_virtualRouter = Lens.lens (\DescribeVirtualRouterResponse' {virtualRouter} -> virtualRouter) (\s@DescribeVirtualRouterResponse' {} a -> s {virtualRouter = a} :: DescribeVirtualRouterResponse)

instance Prelude.NFData DescribeVirtualRouterResponse where
  rnf DescribeVirtualRouterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualRouter
