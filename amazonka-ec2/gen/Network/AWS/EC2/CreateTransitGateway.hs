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
-- Module      : Network.AWS.EC2.CreateTransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway.
--
-- You can use a transit gateway to interconnect your virtual private
-- clouds (VPC) and on-premises networks. After the transit gateway enters
-- the @available@ state, you can attach your VPCs and VPN connections to
-- the transit gateway.
--
-- To attach your VPCs, use CreateTransitGatewayVpcAttachment.
--
-- To attach a VPN connection, use CreateCustomerGateway to create a
-- customer gateway and specify the ID of the customer gateway and the ID
-- of the transit gateway in a call to CreateVpnConnection.
--
-- When you create a transit gateway, we create a default transit gateway
-- route table and use it as the default association route table and the
-- default propagation route table. You can use
-- CreateTransitGatewayRouteTable to create additional transit gateway
-- route tables. If you disable automatic route propagation, we do not
-- create a default transit gateway route table. You can use
-- EnableTransitGatewayRouteTablePropagation to propagate routes from a
-- resource attachment to a transit gateway route table. If you disable
-- automatic associations, you can use AssociateTransitGatewayRouteTable to
-- associate a resource attachment with a transit gateway route table.
module Network.AWS.EC2.CreateTransitGateway
  ( -- * Creating a Request
    CreateTransitGateway (..),
    newCreateTransitGateway,

    -- * Request Lenses
    createTransitGateway_tagSpecifications,
    createTransitGateway_dryRun,
    createTransitGateway_options,
    createTransitGateway_description,

    -- * Destructuring the Response
    CreateTransitGatewayResponse (..),
    newCreateTransitGatewayResponse,

    -- * Response Lenses
    createTransitGatewayResponse_transitGateway,
    createTransitGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGateway' smart constructor.
data CreateTransitGateway = CreateTransitGateway'
  { -- | The tags to apply to the transit gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The transit gateway options.
    options :: Prelude.Maybe TransitGatewayRequestOptions,
    -- | A description of the transit gateway.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTransitGateway_tagSpecifications' - The tags to apply to the transit gateway.
--
-- 'dryRun', 'createTransitGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'createTransitGateway_options' - The transit gateway options.
--
-- 'description', 'createTransitGateway_description' - A description of the transit gateway.
newCreateTransitGateway ::
  CreateTransitGateway
newCreateTransitGateway =
  CreateTransitGateway'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      options = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The tags to apply to the transit gateway.
createTransitGateway_tagSpecifications :: Lens.Lens' CreateTransitGateway (Prelude.Maybe [TagSpecification])
createTransitGateway_tagSpecifications = Lens.lens (\CreateTransitGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGateway' {} a -> s {tagSpecifications = a} :: CreateTransitGateway) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGateway_dryRun :: Lens.Lens' CreateTransitGateway (Prelude.Maybe Prelude.Bool)
createTransitGateway_dryRun = Lens.lens (\CreateTransitGateway' {dryRun} -> dryRun) (\s@CreateTransitGateway' {} a -> s {dryRun = a} :: CreateTransitGateway)

-- | The transit gateway options.
createTransitGateway_options :: Lens.Lens' CreateTransitGateway (Prelude.Maybe TransitGatewayRequestOptions)
createTransitGateway_options = Lens.lens (\CreateTransitGateway' {options} -> options) (\s@CreateTransitGateway' {} a -> s {options = a} :: CreateTransitGateway)

-- | A description of the transit gateway.
createTransitGateway_description :: Lens.Lens' CreateTransitGateway (Prelude.Maybe Prelude.Text)
createTransitGateway_description = Lens.lens (\CreateTransitGateway' {description} -> description) (\s@CreateTransitGateway' {} a -> s {description = a} :: CreateTransitGateway)

instance Core.AWSRequest CreateTransitGateway where
  type
    AWSResponse CreateTransitGateway =
      CreateTransitGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayResponse'
            Prelude.<$> (x Core..@? "transitGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTransitGateway

instance Prelude.NFData CreateTransitGateway

instance Core.ToHeaders CreateTransitGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTransitGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTransitGateway where
  toQuery CreateTransitGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateTransitGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Options" Core.=: options,
        "Description" Core.=: description
      ]

-- | /See:/ 'newCreateTransitGatewayResponse' smart constructor.
data CreateTransitGatewayResponse = CreateTransitGatewayResponse'
  { -- | Information about the transit gateway.
    transitGateway :: Prelude.Maybe TransitGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGateway', 'createTransitGatewayResponse_transitGateway' - Information about the transit gateway.
--
-- 'httpStatus', 'createTransitGatewayResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayResponse
newCreateTransitGatewayResponse pHttpStatus_ =
  CreateTransitGatewayResponse'
    { transitGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the transit gateway.
createTransitGatewayResponse_transitGateway :: Lens.Lens' CreateTransitGatewayResponse (Prelude.Maybe TransitGateway)
createTransitGatewayResponse_transitGateway = Lens.lens (\CreateTransitGatewayResponse' {transitGateway} -> transitGateway) (\s@CreateTransitGatewayResponse' {} a -> s {transitGateway = a} :: CreateTransitGatewayResponse)

-- | The response's http status code.
createTransitGatewayResponse_httpStatus :: Lens.Lens' CreateTransitGatewayResponse Prelude.Int
createTransitGatewayResponse_httpStatus = Lens.lens (\CreateTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayResponse)

instance Prelude.NFData CreateTransitGatewayResponse
