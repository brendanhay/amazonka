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
-- Module      : Amazonka.AppRunner.DeleteVpcConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an App Runner VPC connector resource. You can\'t delete a
-- connector that\'s used by one or more App Runner services.
module Amazonka.AppRunner.DeleteVpcConnector
  ( -- * Creating a Request
    DeleteVpcConnector (..),
    newDeleteVpcConnector,

    -- * Request Lenses
    deleteVpcConnector_vpcConnectorArn,

    -- * Destructuring the Response
    DeleteVpcConnectorResponse (..),
    newDeleteVpcConnectorResponse,

    -- * Response Lenses
    deleteVpcConnectorResponse_httpStatus,
    deleteVpcConnectorResponse_vpcConnector,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcConnector' smart constructor.
data DeleteVpcConnector = DeleteVpcConnector'
  { -- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
    -- want to delete.
    --
    -- The ARN must be a full VPC connector ARN.
    vpcConnectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConnectorArn', 'deleteVpcConnector_vpcConnectorArn' - The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to delete.
--
-- The ARN must be a full VPC connector ARN.
newDeleteVpcConnector ::
  -- | 'vpcConnectorArn'
  Prelude.Text ->
  DeleteVpcConnector
newDeleteVpcConnector pVpcConnectorArn_ =
  DeleteVpcConnector'
    { vpcConnectorArn =
        pVpcConnectorArn_
    }

-- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to delete.
--
-- The ARN must be a full VPC connector ARN.
deleteVpcConnector_vpcConnectorArn :: Lens.Lens' DeleteVpcConnector Prelude.Text
deleteVpcConnector_vpcConnectorArn = Lens.lens (\DeleteVpcConnector' {vpcConnectorArn} -> vpcConnectorArn) (\s@DeleteVpcConnector' {} a -> s {vpcConnectorArn = a} :: DeleteVpcConnector)

instance Core.AWSRequest DeleteVpcConnector where
  type
    AWSResponse DeleteVpcConnector =
      DeleteVpcConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVpcConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcConnector")
      )

instance Prelude.Hashable DeleteVpcConnector where
  hashWithSalt _salt DeleteVpcConnector' {..} =
    _salt `Prelude.hashWithSalt` vpcConnectorArn

instance Prelude.NFData DeleteVpcConnector where
  rnf DeleteVpcConnector' {..} =
    Prelude.rnf vpcConnectorArn

instance Data.ToHeaders DeleteVpcConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DeleteVpcConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVpcConnector where
  toJSON DeleteVpcConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VpcConnectorArn" Data..= vpcConnectorArn)
          ]
      )

instance Data.ToPath DeleteVpcConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpcConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcConnectorResponse' smart constructor.
data DeleteVpcConnectorResponse = DeleteVpcConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC connector that this request just
    -- deleted.
    vpcConnector :: VpcConnector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVpcConnectorResponse_httpStatus' - The response's http status code.
--
-- 'vpcConnector', 'deleteVpcConnectorResponse_vpcConnector' - A description of the App Runner VPC connector that this request just
-- deleted.
newDeleteVpcConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcConnector'
  VpcConnector ->
  DeleteVpcConnectorResponse
newDeleteVpcConnectorResponse
  pHttpStatus_
  pVpcConnector_ =
    DeleteVpcConnectorResponse'
      { httpStatus =
          pHttpStatus_,
        vpcConnector = pVpcConnector_
      }

-- | The response's http status code.
deleteVpcConnectorResponse_httpStatus :: Lens.Lens' DeleteVpcConnectorResponse Prelude.Int
deleteVpcConnectorResponse_httpStatus = Lens.lens (\DeleteVpcConnectorResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcConnectorResponse' {} a -> s {httpStatus = a} :: DeleteVpcConnectorResponse)

-- | A description of the App Runner VPC connector that this request just
-- deleted.
deleteVpcConnectorResponse_vpcConnector :: Lens.Lens' DeleteVpcConnectorResponse VpcConnector
deleteVpcConnectorResponse_vpcConnector = Lens.lens (\DeleteVpcConnectorResponse' {vpcConnector} -> vpcConnector) (\s@DeleteVpcConnectorResponse' {} a -> s {vpcConnector = a} :: DeleteVpcConnectorResponse)

instance Prelude.NFData DeleteVpcConnectorResponse where
  rnf DeleteVpcConnectorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcConnector
