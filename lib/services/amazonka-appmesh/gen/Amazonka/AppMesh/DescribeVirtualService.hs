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
-- Module      : Amazonka.AppMesh.DescribeVirtualService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing virtual service.
module Amazonka.AppMesh.DescribeVirtualService
  ( -- * Creating a Request
    DescribeVirtualService (..),
    newDescribeVirtualService,

    -- * Request Lenses
    describeVirtualService_meshOwner,
    describeVirtualService_meshName,
    describeVirtualService_virtualServiceName,

    -- * Destructuring the Response
    DescribeVirtualServiceResponse (..),
    newDescribeVirtualServiceResponse,

    -- * Response Lenses
    describeVirtualServiceResponse_httpStatus,
    describeVirtualServiceResponse_virtualService,
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
-- /See:/ 'newDescribeVirtualService' smart constructor.
data DescribeVirtualService = DescribeVirtualService'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual service resides in.
    meshName :: Prelude.Text,
    -- | The name of the virtual service to describe.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeVirtualService_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'describeVirtualService_meshName' - The name of the service mesh that the virtual service resides in.
--
-- 'virtualServiceName', 'describeVirtualService_virtualServiceName' - The name of the virtual service to describe.
newDescribeVirtualService ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualServiceName'
  Prelude.Text ->
  DescribeVirtualService
newDescribeVirtualService
  pMeshName_
  pVirtualServiceName_ =
    DescribeVirtualService'
      { meshOwner =
          Prelude.Nothing,
        meshName = pMeshName_,
        virtualServiceName = pVirtualServiceName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeVirtualService_meshOwner :: Lens.Lens' DescribeVirtualService (Prelude.Maybe Prelude.Text)
describeVirtualService_meshOwner = Lens.lens (\DescribeVirtualService' {meshOwner} -> meshOwner) (\s@DescribeVirtualService' {} a -> s {meshOwner = a} :: DescribeVirtualService)

-- | The name of the service mesh that the virtual service resides in.
describeVirtualService_meshName :: Lens.Lens' DescribeVirtualService Prelude.Text
describeVirtualService_meshName = Lens.lens (\DescribeVirtualService' {meshName} -> meshName) (\s@DescribeVirtualService' {} a -> s {meshName = a} :: DescribeVirtualService)

-- | The name of the virtual service to describe.
describeVirtualService_virtualServiceName :: Lens.Lens' DescribeVirtualService Prelude.Text
describeVirtualService_virtualServiceName = Lens.lens (\DescribeVirtualService' {virtualServiceName} -> virtualServiceName) (\s@DescribeVirtualService' {} a -> s {virtualServiceName = a} :: DescribeVirtualService)

instance Core.AWSRequest DescribeVirtualService where
  type
    AWSResponse DescribeVirtualService =
      DescribeVirtualServiceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeVirtualService where
  hashWithSalt _salt DescribeVirtualService' {..} =
    _salt `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData DescribeVirtualService where
  rnf DescribeVirtualService' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualServiceName

instance Data.ToHeaders DescribeVirtualService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVirtualService where
  toPath DescribeVirtualService' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualServices/",
        Data.toBS virtualServiceName
      ]

instance Data.ToQuery DescribeVirtualService where
  toQuery DescribeVirtualService' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDescribeVirtualServiceResponse' smart constructor.
data DescribeVirtualServiceResponse = DescribeVirtualServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual service.
    virtualService :: VirtualServiceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVirtualServiceResponse_httpStatus' - The response's http status code.
--
-- 'virtualService', 'describeVirtualServiceResponse_virtualService' - The full description of your virtual service.
newDescribeVirtualServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualService'
  VirtualServiceData ->
  DescribeVirtualServiceResponse
newDescribeVirtualServiceResponse
  pHttpStatus_
  pVirtualService_ =
    DescribeVirtualServiceResponse'
      { httpStatus =
          pHttpStatus_,
        virtualService = pVirtualService_
      }

-- | The response's http status code.
describeVirtualServiceResponse_httpStatus :: Lens.Lens' DescribeVirtualServiceResponse Prelude.Int
describeVirtualServiceResponse_httpStatus = Lens.lens (\DescribeVirtualServiceResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualServiceResponse' {} a -> s {httpStatus = a} :: DescribeVirtualServiceResponse)

-- | The full description of your virtual service.
describeVirtualServiceResponse_virtualService :: Lens.Lens' DescribeVirtualServiceResponse VirtualServiceData
describeVirtualServiceResponse_virtualService = Lens.lens (\DescribeVirtualServiceResponse' {virtualService} -> virtualService) (\s@DescribeVirtualServiceResponse' {} a -> s {virtualService = a} :: DescribeVirtualServiceResponse)

instance
  Prelude.NFData
    DescribeVirtualServiceResponse
  where
  rnf DescribeVirtualServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualService
