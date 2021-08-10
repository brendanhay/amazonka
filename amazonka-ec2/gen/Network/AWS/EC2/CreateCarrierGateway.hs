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
-- Module      : Network.AWS.EC2.CreateCarrierGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a carrier gateway. For more information about carrier gateways,
-- see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#wavelength-carrier-gateway Carrier gateways>
-- in the /AWS Wavelength Developer Guide/.
module Network.AWS.EC2.CreateCarrierGateway
  ( -- * Creating a Request
    CreateCarrierGateway (..),
    newCreateCarrierGateway,

    -- * Request Lenses
    createCarrierGateway_tagSpecifications,
    createCarrierGateway_dryRun,
    createCarrierGateway_clientToken,
    createCarrierGateway_vpcId,

    -- * Destructuring the Response
    CreateCarrierGatewayResponse (..),
    newCreateCarrierGatewayResponse,

    -- * Response Lenses
    createCarrierGatewayResponse_carrierGateway,
    createCarrierGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCarrierGateway' smart constructor.
data CreateCarrierGateway = CreateCarrierGateway'
  { -- | The tags to associate with the carrier gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC to associate with the carrier gateway.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCarrierGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createCarrierGateway_tagSpecifications' - The tags to associate with the carrier gateway.
--
-- 'dryRun', 'createCarrierGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'createCarrierGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'vpcId', 'createCarrierGateway_vpcId' - The ID of the VPC to associate with the carrier gateway.
newCreateCarrierGateway ::
  -- | 'vpcId'
  Prelude.Text ->
  CreateCarrierGateway
newCreateCarrierGateway pVpcId_ =
  CreateCarrierGateway'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The tags to associate with the carrier gateway.
createCarrierGateway_tagSpecifications :: Lens.Lens' CreateCarrierGateway (Prelude.Maybe [TagSpecification])
createCarrierGateway_tagSpecifications = Lens.lens (\CreateCarrierGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateCarrierGateway' {} a -> s {tagSpecifications = a} :: CreateCarrierGateway) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCarrierGateway_dryRun :: Lens.Lens' CreateCarrierGateway (Prelude.Maybe Prelude.Bool)
createCarrierGateway_dryRun = Lens.lens (\CreateCarrierGateway' {dryRun} -> dryRun) (\s@CreateCarrierGateway' {} a -> s {dryRun = a} :: CreateCarrierGateway)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createCarrierGateway_clientToken :: Lens.Lens' CreateCarrierGateway (Prelude.Maybe Prelude.Text)
createCarrierGateway_clientToken = Lens.lens (\CreateCarrierGateway' {clientToken} -> clientToken) (\s@CreateCarrierGateway' {} a -> s {clientToken = a} :: CreateCarrierGateway)

-- | The ID of the VPC to associate with the carrier gateway.
createCarrierGateway_vpcId :: Lens.Lens' CreateCarrierGateway Prelude.Text
createCarrierGateway_vpcId = Lens.lens (\CreateCarrierGateway' {vpcId} -> vpcId) (\s@CreateCarrierGateway' {} a -> s {vpcId = a} :: CreateCarrierGateway)

instance Core.AWSRequest CreateCarrierGateway where
  type
    AWSResponse CreateCarrierGateway =
      CreateCarrierGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCarrierGatewayResponse'
            Prelude.<$> (x Core..@? "carrierGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCarrierGateway

instance Prelude.NFData CreateCarrierGateway

instance Core.ToHeaders CreateCarrierGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCarrierGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCarrierGateway where
  toQuery CreateCarrierGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCarrierGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "ClientToken" Core.=: clientToken,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newCreateCarrierGatewayResponse' smart constructor.
data CreateCarrierGatewayResponse = CreateCarrierGatewayResponse'
  { -- | Information about the carrier gateway.
    carrierGateway :: Prelude.Maybe CarrierGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCarrierGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGateway', 'createCarrierGatewayResponse_carrierGateway' - Information about the carrier gateway.
--
-- 'httpStatus', 'createCarrierGatewayResponse_httpStatus' - The response's http status code.
newCreateCarrierGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCarrierGatewayResponse
newCreateCarrierGatewayResponse pHttpStatus_ =
  CreateCarrierGatewayResponse'
    { carrierGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the carrier gateway.
createCarrierGatewayResponse_carrierGateway :: Lens.Lens' CreateCarrierGatewayResponse (Prelude.Maybe CarrierGateway)
createCarrierGatewayResponse_carrierGateway = Lens.lens (\CreateCarrierGatewayResponse' {carrierGateway} -> carrierGateway) (\s@CreateCarrierGatewayResponse' {} a -> s {carrierGateway = a} :: CreateCarrierGatewayResponse)

-- | The response's http status code.
createCarrierGatewayResponse_httpStatus :: Lens.Lens' CreateCarrierGatewayResponse Prelude.Int
createCarrierGatewayResponse_httpStatus = Lens.lens (\CreateCarrierGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateCarrierGatewayResponse' {} a -> s {httpStatus = a} :: CreateCarrierGatewayResponse)

instance Prelude.NFData CreateCarrierGatewayResponse
