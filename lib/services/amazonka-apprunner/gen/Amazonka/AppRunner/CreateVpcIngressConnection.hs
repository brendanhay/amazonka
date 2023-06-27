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
-- Module      : Amazonka.AppRunner.CreateVpcIngressConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner VPC Ingress Connection resource. App Runner
-- requires this resource when you want to associate your App Runner
-- service with an Amazon VPC endpoint.
module Amazonka.AppRunner.CreateVpcIngressConnection
  ( -- * Creating a Request
    CreateVpcIngressConnection (..),
    newCreateVpcIngressConnection,

    -- * Request Lenses
    createVpcIngressConnection_tags,
    createVpcIngressConnection_serviceArn,
    createVpcIngressConnection_vpcIngressConnectionName,
    createVpcIngressConnection_ingressVpcConfiguration,

    -- * Destructuring the Response
    CreateVpcIngressConnectionResponse (..),
    newCreateVpcIngressConnectionResponse,

    -- * Response Lenses
    createVpcIngressConnectionResponse_httpStatus,
    createVpcIngressConnectionResponse_vpcIngressConnection,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcIngressConnection' smart constructor.
data CreateVpcIngressConnection = CreateVpcIngressConnection'
  { -- | An optional list of metadata items that you can associate with the VPC
    -- Ingress Connection resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) for this App Runner service that is used
    -- to create the VPC Ingress Connection resource.
    serviceArn :: Prelude.Text,
    -- | A name for the VPC Ingress Connection resource. It must be unique across
    -- all the active VPC Ingress Connections in your Amazon Web Services
    -- account in the Amazon Web Services Region.
    vpcIngressConnectionName :: Prelude.Text,
    -- | Specifications for the customer’s Amazon VPC and the related Amazon Web
    -- Services PrivateLink VPC endpoint that are used to create the VPC
    -- Ingress Connection resource.
    ingressVpcConfiguration :: IngressVpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcIngressConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVpcIngressConnection_tags' - An optional list of metadata items that you can associate with the VPC
-- Ingress Connection resource. A tag is a key-value pair.
--
-- 'serviceArn', 'createVpcIngressConnection_serviceArn' - The Amazon Resource Name (ARN) for this App Runner service that is used
-- to create the VPC Ingress Connection resource.
--
-- 'vpcIngressConnectionName', 'createVpcIngressConnection_vpcIngressConnectionName' - A name for the VPC Ingress Connection resource. It must be unique across
-- all the active VPC Ingress Connections in your Amazon Web Services
-- account in the Amazon Web Services Region.
--
-- 'ingressVpcConfiguration', 'createVpcIngressConnection_ingressVpcConfiguration' - Specifications for the customer’s Amazon VPC and the related Amazon Web
-- Services PrivateLink VPC endpoint that are used to create the VPC
-- Ingress Connection resource.
newCreateVpcIngressConnection ::
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'vpcIngressConnectionName'
  Prelude.Text ->
  -- | 'ingressVpcConfiguration'
  IngressVpcConfiguration ->
  CreateVpcIngressConnection
newCreateVpcIngressConnection
  pServiceArn_
  pVpcIngressConnectionName_
  pIngressVpcConfiguration_ =
    CreateVpcIngressConnection'
      { tags = Prelude.Nothing,
        serviceArn = pServiceArn_,
        vpcIngressConnectionName =
          pVpcIngressConnectionName_,
        ingressVpcConfiguration =
          pIngressVpcConfiguration_
      }

-- | An optional list of metadata items that you can associate with the VPC
-- Ingress Connection resource. A tag is a key-value pair.
createVpcIngressConnection_tags :: Lens.Lens' CreateVpcIngressConnection (Prelude.Maybe [Tag])
createVpcIngressConnection_tags = Lens.lens (\CreateVpcIngressConnection' {tags} -> tags) (\s@CreateVpcIngressConnection' {} a -> s {tags = a} :: CreateVpcIngressConnection) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for this App Runner service that is used
-- to create the VPC Ingress Connection resource.
createVpcIngressConnection_serviceArn :: Lens.Lens' CreateVpcIngressConnection Prelude.Text
createVpcIngressConnection_serviceArn = Lens.lens (\CreateVpcIngressConnection' {serviceArn} -> serviceArn) (\s@CreateVpcIngressConnection' {} a -> s {serviceArn = a} :: CreateVpcIngressConnection)

-- | A name for the VPC Ingress Connection resource. It must be unique across
-- all the active VPC Ingress Connections in your Amazon Web Services
-- account in the Amazon Web Services Region.
createVpcIngressConnection_vpcIngressConnectionName :: Lens.Lens' CreateVpcIngressConnection Prelude.Text
createVpcIngressConnection_vpcIngressConnectionName = Lens.lens (\CreateVpcIngressConnection' {vpcIngressConnectionName} -> vpcIngressConnectionName) (\s@CreateVpcIngressConnection' {} a -> s {vpcIngressConnectionName = a} :: CreateVpcIngressConnection)

-- | Specifications for the customer’s Amazon VPC and the related Amazon Web
-- Services PrivateLink VPC endpoint that are used to create the VPC
-- Ingress Connection resource.
createVpcIngressConnection_ingressVpcConfiguration :: Lens.Lens' CreateVpcIngressConnection IngressVpcConfiguration
createVpcIngressConnection_ingressVpcConfiguration = Lens.lens (\CreateVpcIngressConnection' {ingressVpcConfiguration} -> ingressVpcConfiguration) (\s@CreateVpcIngressConnection' {} a -> s {ingressVpcConfiguration = a} :: CreateVpcIngressConnection)

instance Core.AWSRequest CreateVpcIngressConnection where
  type
    AWSResponse CreateVpcIngressConnection =
      CreateVpcIngressConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcIngressConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcIngressConnection")
      )

instance Prelude.Hashable CreateVpcIngressConnection where
  hashWithSalt _salt CreateVpcIngressConnection' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` vpcIngressConnectionName
      `Prelude.hashWithSalt` ingressVpcConfiguration

instance Prelude.NFData CreateVpcIngressConnection where
  rnf CreateVpcIngressConnection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf vpcIngressConnectionName
      `Prelude.seq` Prelude.rnf ingressVpcConfiguration

instance Data.ToHeaders CreateVpcIngressConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.CreateVpcIngressConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcIngressConnection where
  toJSON CreateVpcIngressConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ServiceArn" Data..= serviceArn),
            Prelude.Just
              ( "VpcIngressConnectionName"
                  Data..= vpcIngressConnectionName
              ),
            Prelude.Just
              ( "IngressVpcConfiguration"
                  Data..= ingressVpcConfiguration
              )
          ]
      )

instance Data.ToPath CreateVpcIngressConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpcIngressConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcIngressConnectionResponse' smart constructor.
data CreateVpcIngressConnectionResponse = CreateVpcIngressConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC Ingress Connection resource that\'s
    -- created by this request.
    vpcIngressConnection :: VpcIngressConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcIngressConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVpcIngressConnectionResponse_httpStatus' - The response's http status code.
--
-- 'vpcIngressConnection', 'createVpcIngressConnectionResponse_vpcIngressConnection' - A description of the App Runner VPC Ingress Connection resource that\'s
-- created by this request.
newCreateVpcIngressConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcIngressConnection'
  VpcIngressConnection ->
  CreateVpcIngressConnectionResponse
newCreateVpcIngressConnectionResponse
  pHttpStatus_
  pVpcIngressConnection_ =
    CreateVpcIngressConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        vpcIngressConnection =
          pVpcIngressConnection_
      }

-- | The response's http status code.
createVpcIngressConnectionResponse_httpStatus :: Lens.Lens' CreateVpcIngressConnectionResponse Prelude.Int
createVpcIngressConnectionResponse_httpStatus = Lens.lens (\CreateVpcIngressConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateVpcIngressConnectionResponse' {} a -> s {httpStatus = a} :: CreateVpcIngressConnectionResponse)

-- | A description of the App Runner VPC Ingress Connection resource that\'s
-- created by this request.
createVpcIngressConnectionResponse_vpcIngressConnection :: Lens.Lens' CreateVpcIngressConnectionResponse VpcIngressConnection
createVpcIngressConnectionResponse_vpcIngressConnection = Lens.lens (\CreateVpcIngressConnectionResponse' {vpcIngressConnection} -> vpcIngressConnection) (\s@CreateVpcIngressConnectionResponse' {} a -> s {vpcIngressConnection = a} :: CreateVpcIngressConnectionResponse)

instance
  Prelude.NFData
    CreateVpcIngressConnectionResponse
  where
  rnf CreateVpcIngressConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcIngressConnection
