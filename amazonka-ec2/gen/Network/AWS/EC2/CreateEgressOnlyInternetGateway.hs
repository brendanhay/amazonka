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
-- Module      : Network.AWS.EC2.CreateEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [IPv6 only] Creates an egress-only internet gateway for your VPC. An
-- egress-only internet gateway is used to enable outbound communication
-- over IPv6 from instances in your VPC to the internet, and prevents hosts
-- outside of your VPC from initiating an IPv6 connection with your
-- instance.
module Network.AWS.EC2.CreateEgressOnlyInternetGateway
  ( -- * Creating a Request
    CreateEgressOnlyInternetGateway (..),
    newCreateEgressOnlyInternetGateway,

    -- * Request Lenses
    createEgressOnlyInternetGateway_tagSpecifications,
    createEgressOnlyInternetGateway_dryRun,
    createEgressOnlyInternetGateway_clientToken,
    createEgressOnlyInternetGateway_vpcId,

    -- * Destructuring the Response
    CreateEgressOnlyInternetGatewayResponse (..),
    newCreateEgressOnlyInternetGatewayResponse,

    -- * Response Lenses
    createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway,
    createEgressOnlyInternetGatewayResponse_clientToken,
    createEgressOnlyInternetGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEgressOnlyInternetGateway' smart constructor.
data CreateEgressOnlyInternetGateway = CreateEgressOnlyInternetGateway'
  { -- | The tags to assign to the egress-only internet gateway.
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
    -- | The ID of the VPC for which to create the egress-only internet gateway.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEgressOnlyInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createEgressOnlyInternetGateway_tagSpecifications' - The tags to assign to the egress-only internet gateway.
--
-- 'dryRun', 'createEgressOnlyInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'createEgressOnlyInternetGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'vpcId', 'createEgressOnlyInternetGateway_vpcId' - The ID of the VPC for which to create the egress-only internet gateway.
newCreateEgressOnlyInternetGateway ::
  -- | 'vpcId'
  Prelude.Text ->
  CreateEgressOnlyInternetGateway
newCreateEgressOnlyInternetGateway pVpcId_ =
  CreateEgressOnlyInternetGateway'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The tags to assign to the egress-only internet gateway.
createEgressOnlyInternetGateway_tagSpecifications :: Lens.Lens' CreateEgressOnlyInternetGateway (Prelude.Maybe [TagSpecification])
createEgressOnlyInternetGateway_tagSpecifications = Lens.lens (\CreateEgressOnlyInternetGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateEgressOnlyInternetGateway' {} a -> s {tagSpecifications = a} :: CreateEgressOnlyInternetGateway) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createEgressOnlyInternetGateway_dryRun :: Lens.Lens' CreateEgressOnlyInternetGateway (Prelude.Maybe Prelude.Bool)
createEgressOnlyInternetGateway_dryRun = Lens.lens (\CreateEgressOnlyInternetGateway' {dryRun} -> dryRun) (\s@CreateEgressOnlyInternetGateway' {} a -> s {dryRun = a} :: CreateEgressOnlyInternetGateway)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createEgressOnlyInternetGateway_clientToken :: Lens.Lens' CreateEgressOnlyInternetGateway (Prelude.Maybe Prelude.Text)
createEgressOnlyInternetGateway_clientToken = Lens.lens (\CreateEgressOnlyInternetGateway' {clientToken} -> clientToken) (\s@CreateEgressOnlyInternetGateway' {} a -> s {clientToken = a} :: CreateEgressOnlyInternetGateway)

-- | The ID of the VPC for which to create the egress-only internet gateway.
createEgressOnlyInternetGateway_vpcId :: Lens.Lens' CreateEgressOnlyInternetGateway Prelude.Text
createEgressOnlyInternetGateway_vpcId = Lens.lens (\CreateEgressOnlyInternetGateway' {vpcId} -> vpcId) (\s@CreateEgressOnlyInternetGateway' {} a -> s {vpcId = a} :: CreateEgressOnlyInternetGateway)

instance
  Core.AWSRequest
    CreateEgressOnlyInternetGateway
  where
  type
    AWSResponse CreateEgressOnlyInternetGateway =
      CreateEgressOnlyInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateEgressOnlyInternetGatewayResponse'
            Prelude.<$> (x Core..@? "egressOnlyInternetGateway")
            Prelude.<*> (x Core..@? "clientToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateEgressOnlyInternetGateway

instance
  Prelude.NFData
    CreateEgressOnlyInternetGateway

instance
  Core.ToHeaders
    CreateEgressOnlyInternetGateway
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateEgressOnlyInternetGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEgressOnlyInternetGateway where
  toQuery CreateEgressOnlyInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateEgressOnlyInternetGateway" ::
                      Prelude.ByteString
                  ),
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

-- | /See:/ 'newCreateEgressOnlyInternetGatewayResponse' smart constructor.
data CreateEgressOnlyInternetGatewayResponse = CreateEgressOnlyInternetGatewayResponse'
  { -- | Information about the egress-only internet gateway.
    egressOnlyInternetGateway :: Prelude.Maybe EgressOnlyInternetGateway,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEgressOnlyInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressOnlyInternetGateway', 'createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway' - Information about the egress-only internet gateway.
--
-- 'clientToken', 'createEgressOnlyInternetGatewayResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'httpStatus', 'createEgressOnlyInternetGatewayResponse_httpStatus' - The response's http status code.
newCreateEgressOnlyInternetGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEgressOnlyInternetGatewayResponse
newCreateEgressOnlyInternetGatewayResponse
  pHttpStatus_ =
    CreateEgressOnlyInternetGatewayResponse'
      { egressOnlyInternetGateway =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the egress-only internet gateway.
createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Prelude.Maybe EgressOnlyInternetGateway)
createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway = Lens.lens (\CreateEgressOnlyInternetGatewayResponse' {egressOnlyInternetGateway} -> egressOnlyInternetGateway) (\s@CreateEgressOnlyInternetGatewayResponse' {} a -> s {egressOnlyInternetGateway = a} :: CreateEgressOnlyInternetGatewayResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createEgressOnlyInternetGatewayResponse_clientToken :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Prelude.Maybe Prelude.Text)
createEgressOnlyInternetGatewayResponse_clientToken = Lens.lens (\CreateEgressOnlyInternetGatewayResponse' {clientToken} -> clientToken) (\s@CreateEgressOnlyInternetGatewayResponse' {} a -> s {clientToken = a} :: CreateEgressOnlyInternetGatewayResponse)

-- | The response's http status code.
createEgressOnlyInternetGatewayResponse_httpStatus :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse Prelude.Int
createEgressOnlyInternetGatewayResponse_httpStatus = Lens.lens (\CreateEgressOnlyInternetGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateEgressOnlyInternetGatewayResponse' {} a -> s {httpStatus = a} :: CreateEgressOnlyInternetGatewayResponse)

instance
  Prelude.NFData
    CreateEgressOnlyInternetGatewayResponse
