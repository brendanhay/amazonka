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
-- Module      : Amazonka.AppRunner.UpdateVpcIngressConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing App Runner VPC Ingress Connection resource. The VPC
-- Ingress Connection must be in one of the following states to be updated:
--
-- -   AVAILABLE
--
-- -   FAILED_CREATION
--
-- -   FAILED_UPDATE
module Amazonka.AppRunner.UpdateVpcIngressConnection
  ( -- * Creating a Request
    UpdateVpcIngressConnection (..),
    newUpdateVpcIngressConnection,

    -- * Request Lenses
    updateVpcIngressConnection_vpcIngressConnectionArn,
    updateVpcIngressConnection_ingressVpcConfiguration,

    -- * Destructuring the Response
    UpdateVpcIngressConnectionResponse (..),
    newUpdateVpcIngressConnectionResponse,

    -- * Response Lenses
    updateVpcIngressConnectionResponse_httpStatus,
    updateVpcIngressConnectionResponse_vpcIngressConnection,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVpcIngressConnection' smart constructor.
data UpdateVpcIngressConnection = UpdateVpcIngressConnection'
  { -- | The Amazon Resource Name (Arn) for the App Runner VPC Ingress Connection
    -- resource that you want to update.
    vpcIngressConnectionArn :: Prelude.Text,
    -- | Specifications for the customer’s Amazon VPC and the related Amazon Web
    -- Services PrivateLink VPC endpoint that are used to update the VPC
    -- Ingress Connection resource.
    ingressVpcConfiguration :: IngressVpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcIngressConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcIngressConnectionArn', 'updateVpcIngressConnection_vpcIngressConnectionArn' - The Amazon Resource Name (Arn) for the App Runner VPC Ingress Connection
-- resource that you want to update.
--
-- 'ingressVpcConfiguration', 'updateVpcIngressConnection_ingressVpcConfiguration' - Specifications for the customer’s Amazon VPC and the related Amazon Web
-- Services PrivateLink VPC endpoint that are used to update the VPC
-- Ingress Connection resource.
newUpdateVpcIngressConnection ::
  -- | 'vpcIngressConnectionArn'
  Prelude.Text ->
  -- | 'ingressVpcConfiguration'
  IngressVpcConfiguration ->
  UpdateVpcIngressConnection
newUpdateVpcIngressConnection
  pVpcIngressConnectionArn_
  pIngressVpcConfiguration_ =
    UpdateVpcIngressConnection'
      { vpcIngressConnectionArn =
          pVpcIngressConnectionArn_,
        ingressVpcConfiguration =
          pIngressVpcConfiguration_
      }

-- | The Amazon Resource Name (Arn) for the App Runner VPC Ingress Connection
-- resource that you want to update.
updateVpcIngressConnection_vpcIngressConnectionArn :: Lens.Lens' UpdateVpcIngressConnection Prelude.Text
updateVpcIngressConnection_vpcIngressConnectionArn = Lens.lens (\UpdateVpcIngressConnection' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@UpdateVpcIngressConnection' {} a -> s {vpcIngressConnectionArn = a} :: UpdateVpcIngressConnection)

-- | Specifications for the customer’s Amazon VPC and the related Amazon Web
-- Services PrivateLink VPC endpoint that are used to update the VPC
-- Ingress Connection resource.
updateVpcIngressConnection_ingressVpcConfiguration :: Lens.Lens' UpdateVpcIngressConnection IngressVpcConfiguration
updateVpcIngressConnection_ingressVpcConfiguration = Lens.lens (\UpdateVpcIngressConnection' {ingressVpcConfiguration} -> ingressVpcConfiguration) (\s@UpdateVpcIngressConnection' {} a -> s {ingressVpcConfiguration = a} :: UpdateVpcIngressConnection)

instance Core.AWSRequest UpdateVpcIngressConnection where
  type
    AWSResponse UpdateVpcIngressConnection =
      UpdateVpcIngressConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVpcIngressConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcIngressConnection")
      )

instance Prelude.Hashable UpdateVpcIngressConnection where
  hashWithSalt _salt UpdateVpcIngressConnection' {..} =
    _salt
      `Prelude.hashWithSalt` vpcIngressConnectionArn
      `Prelude.hashWithSalt` ingressVpcConfiguration

instance Prelude.NFData UpdateVpcIngressConnection where
  rnf UpdateVpcIngressConnection' {..} =
    Prelude.rnf vpcIngressConnectionArn `Prelude.seq`
      Prelude.rnf ingressVpcConfiguration

instance Data.ToHeaders UpdateVpcIngressConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.UpdateVpcIngressConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVpcIngressConnection where
  toJSON UpdateVpcIngressConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VpcIngressConnectionArn"
                  Data..= vpcIngressConnectionArn
              ),
            Prelude.Just
              ( "IngressVpcConfiguration"
                  Data..= ingressVpcConfiguration
              )
          ]
      )

instance Data.ToPath UpdateVpcIngressConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVpcIngressConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVpcIngressConnectionResponse' smart constructor.
data UpdateVpcIngressConnectionResponse = UpdateVpcIngressConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC Ingress Connection resource that\'s
    -- updated by this request.
    vpcIngressConnection :: VpcIngressConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcIngressConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVpcIngressConnectionResponse_httpStatus' - The response's http status code.
--
-- 'vpcIngressConnection', 'updateVpcIngressConnectionResponse_vpcIngressConnection' - A description of the App Runner VPC Ingress Connection resource that\'s
-- updated by this request.
newUpdateVpcIngressConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcIngressConnection'
  VpcIngressConnection ->
  UpdateVpcIngressConnectionResponse
newUpdateVpcIngressConnectionResponse
  pHttpStatus_
  pVpcIngressConnection_ =
    UpdateVpcIngressConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        vpcIngressConnection =
          pVpcIngressConnection_
      }

-- | The response's http status code.
updateVpcIngressConnectionResponse_httpStatus :: Lens.Lens' UpdateVpcIngressConnectionResponse Prelude.Int
updateVpcIngressConnectionResponse_httpStatus = Lens.lens (\UpdateVpcIngressConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateVpcIngressConnectionResponse' {} a -> s {httpStatus = a} :: UpdateVpcIngressConnectionResponse)

-- | A description of the App Runner VPC Ingress Connection resource that\'s
-- updated by this request.
updateVpcIngressConnectionResponse_vpcIngressConnection :: Lens.Lens' UpdateVpcIngressConnectionResponse VpcIngressConnection
updateVpcIngressConnectionResponse_vpcIngressConnection = Lens.lens (\UpdateVpcIngressConnectionResponse' {vpcIngressConnection} -> vpcIngressConnection) (\s@UpdateVpcIngressConnectionResponse' {} a -> s {vpcIngressConnection = a} :: UpdateVpcIngressConnectionResponse)

instance
  Prelude.NFData
    UpdateVpcIngressConnectionResponse
  where
  rnf UpdateVpcIngressConnectionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf vpcIngressConnection
