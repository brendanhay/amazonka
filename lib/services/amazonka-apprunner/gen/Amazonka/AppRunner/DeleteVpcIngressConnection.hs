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
-- Module      : Amazonka.AppRunner.DeleteVpcIngressConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an App Runner VPC Ingress Connection resource that\'s associated
-- with an App Runner service. The VPC Ingress Connection must be in one of
-- the following states to be deleted:
--
-- -   @AVAILABLE@
--
-- -   @FAILED_CREATION@
--
-- -   @FAILED_UPDATE@
--
-- -   @FAILED_DELETION@
module Amazonka.AppRunner.DeleteVpcIngressConnection
  ( -- * Creating a Request
    DeleteVpcIngressConnection (..),
    newDeleteVpcIngressConnection,

    -- * Request Lenses
    deleteVpcIngressConnection_vpcIngressConnectionArn,

    -- * Destructuring the Response
    DeleteVpcIngressConnectionResponse (..),
    newDeleteVpcIngressConnectionResponse,

    -- * Response Lenses
    deleteVpcIngressConnectionResponse_httpStatus,
    deleteVpcIngressConnectionResponse_vpcIngressConnection,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcIngressConnection' smart constructor.
data DeleteVpcIngressConnection = DeleteVpcIngressConnection'
  { -- | The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
    -- that you want to delete.
    vpcIngressConnectionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcIngressConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcIngressConnectionArn', 'deleteVpcIngressConnection_vpcIngressConnectionArn' - The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
-- that you want to delete.
newDeleteVpcIngressConnection ::
  -- | 'vpcIngressConnectionArn'
  Prelude.Text ->
  DeleteVpcIngressConnection
newDeleteVpcIngressConnection
  pVpcIngressConnectionArn_ =
    DeleteVpcIngressConnection'
      { vpcIngressConnectionArn =
          pVpcIngressConnectionArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
-- that you want to delete.
deleteVpcIngressConnection_vpcIngressConnectionArn :: Lens.Lens' DeleteVpcIngressConnection Prelude.Text
deleteVpcIngressConnection_vpcIngressConnectionArn = Lens.lens (\DeleteVpcIngressConnection' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@DeleteVpcIngressConnection' {} a -> s {vpcIngressConnectionArn = a} :: DeleteVpcIngressConnection)

instance Core.AWSRequest DeleteVpcIngressConnection where
  type
    AWSResponse DeleteVpcIngressConnection =
      DeleteVpcIngressConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVpcIngressConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcIngressConnection")
      )

instance Prelude.Hashable DeleteVpcIngressConnection where
  hashWithSalt _salt DeleteVpcIngressConnection' {..} =
    _salt
      `Prelude.hashWithSalt` vpcIngressConnectionArn

instance Prelude.NFData DeleteVpcIngressConnection where
  rnf DeleteVpcIngressConnection' {..} =
    Prelude.rnf vpcIngressConnectionArn

instance Data.ToHeaders DeleteVpcIngressConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DeleteVpcIngressConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVpcIngressConnection where
  toJSON DeleteVpcIngressConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VpcIngressConnectionArn"
                  Data..= vpcIngressConnectionArn
              )
          ]
      )

instance Data.ToPath DeleteVpcIngressConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpcIngressConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcIngressConnectionResponse' smart constructor.
data DeleteVpcIngressConnectionResponse = DeleteVpcIngressConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC Ingress Connection that this request
    -- just deleted.
    vpcIngressConnection :: VpcIngressConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcIngressConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVpcIngressConnectionResponse_httpStatus' - The response's http status code.
--
-- 'vpcIngressConnection', 'deleteVpcIngressConnectionResponse_vpcIngressConnection' - A description of the App Runner VPC Ingress Connection that this request
-- just deleted.
newDeleteVpcIngressConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcIngressConnection'
  VpcIngressConnection ->
  DeleteVpcIngressConnectionResponse
newDeleteVpcIngressConnectionResponse
  pHttpStatus_
  pVpcIngressConnection_ =
    DeleteVpcIngressConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        vpcIngressConnection =
          pVpcIngressConnection_
      }

-- | The response's http status code.
deleteVpcIngressConnectionResponse_httpStatus :: Lens.Lens' DeleteVpcIngressConnectionResponse Prelude.Int
deleteVpcIngressConnectionResponse_httpStatus = Lens.lens (\DeleteVpcIngressConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcIngressConnectionResponse' {} a -> s {httpStatus = a} :: DeleteVpcIngressConnectionResponse)

-- | A description of the App Runner VPC Ingress Connection that this request
-- just deleted.
deleteVpcIngressConnectionResponse_vpcIngressConnection :: Lens.Lens' DeleteVpcIngressConnectionResponse VpcIngressConnection
deleteVpcIngressConnectionResponse_vpcIngressConnection = Lens.lens (\DeleteVpcIngressConnectionResponse' {vpcIngressConnection} -> vpcIngressConnection) (\s@DeleteVpcIngressConnectionResponse' {} a -> s {vpcIngressConnection = a} :: DeleteVpcIngressConnectionResponse)

instance
  Prelude.NFData
    DeleteVpcIngressConnectionResponse
  where
  rnf DeleteVpcIngressConnectionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf vpcIngressConnection
