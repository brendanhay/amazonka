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
-- Module      : Amazonka.EC2.CreateVpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual private gateway. A virtual private gateway is the
-- endpoint on the VPC side of your VPN connection. You can create a
-- virtual private gateway before creating the VPC itself.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.CreateVpnGateway
  ( -- * Creating a Request
    CreateVpnGateway (..),
    newCreateVpnGateway,

    -- * Request Lenses
    createVpnGateway_amazonSideAsn,
    createVpnGateway_tagSpecifications,
    createVpnGateway_availabilityZone,
    createVpnGateway_dryRun,
    createVpnGateway_type,

    -- * Destructuring the Response
    CreateVpnGatewayResponse (..),
    newCreateVpnGatewayResponse,

    -- * Response Lenses
    createVpnGatewayResponse_vpnGateway,
    createVpnGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateVpnGateway.
--
-- /See:/ 'newCreateVpnGateway' smart constructor.
data CreateVpnGateway = CreateVpnGateway'
  { -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. If you\'re using a 16-bit ASN, it must be in the 64512 to 65534
    -- range. If you\'re using a 32-bit ASN, it must be in the 4200000000 to
    -- 4294967294 range.
    --
    -- Default: 64512
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The tags to apply to the virtual private gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Availability Zone for the virtual private gateway.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The type of VPN connection this virtual private gateway supports.
    type' :: GatewayType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonSideAsn', 'createVpnGateway_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. If you\'re using a 16-bit ASN, it must be in the 64512 to 65534
-- range. If you\'re using a 32-bit ASN, it must be in the 4200000000 to
-- 4294967294 range.
--
-- Default: 64512
--
-- 'tagSpecifications', 'createVpnGateway_tagSpecifications' - The tags to apply to the virtual private gateway.
--
-- 'availabilityZone', 'createVpnGateway_availabilityZone' - The Availability Zone for the virtual private gateway.
--
-- 'dryRun', 'createVpnGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'type'', 'createVpnGateway_type' - The type of VPN connection this virtual private gateway supports.
newCreateVpnGateway ::
  -- | 'type''
  GatewayType ->
  CreateVpnGateway
newCreateVpnGateway pType_ =
  CreateVpnGateway'
    { amazonSideAsn = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      type' = pType_
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. If you\'re using a 16-bit ASN, it must be in the 64512 to 65534
-- range. If you\'re using a 32-bit ASN, it must be in the 4200000000 to
-- 4294967294 range.
--
-- Default: 64512
createVpnGateway_amazonSideAsn :: Lens.Lens' CreateVpnGateway (Prelude.Maybe Prelude.Integer)
createVpnGateway_amazonSideAsn = Lens.lens (\CreateVpnGateway' {amazonSideAsn} -> amazonSideAsn) (\s@CreateVpnGateway' {} a -> s {amazonSideAsn = a} :: CreateVpnGateway)

-- | The tags to apply to the virtual private gateway.
createVpnGateway_tagSpecifications :: Lens.Lens' CreateVpnGateway (Prelude.Maybe [TagSpecification])
createVpnGateway_tagSpecifications = Lens.lens (\CreateVpnGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateVpnGateway' {} a -> s {tagSpecifications = a} :: CreateVpnGateway) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone for the virtual private gateway.
createVpnGateway_availabilityZone :: Lens.Lens' CreateVpnGateway (Prelude.Maybe Prelude.Text)
createVpnGateway_availabilityZone = Lens.lens (\CreateVpnGateway' {availabilityZone} -> availabilityZone) (\s@CreateVpnGateway' {} a -> s {availabilityZone = a} :: CreateVpnGateway)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpnGateway_dryRun :: Lens.Lens' CreateVpnGateway (Prelude.Maybe Prelude.Bool)
createVpnGateway_dryRun = Lens.lens (\CreateVpnGateway' {dryRun} -> dryRun) (\s@CreateVpnGateway' {} a -> s {dryRun = a} :: CreateVpnGateway)

-- | The type of VPN connection this virtual private gateway supports.
createVpnGateway_type :: Lens.Lens' CreateVpnGateway GatewayType
createVpnGateway_type = Lens.lens (\CreateVpnGateway' {type'} -> type') (\s@CreateVpnGateway' {} a -> s {type' = a} :: CreateVpnGateway)

instance Core.AWSRequest CreateVpnGateway where
  type
    AWSResponse CreateVpnGateway =
      CreateVpnGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpnGatewayResponse'
            Prelude.<$> (x Core..@? "vpnGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpnGateway

instance Prelude.NFData CreateVpnGateway

instance Core.ToHeaders CreateVpnGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateVpnGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVpnGateway where
  toQuery CreateVpnGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateVpnGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "AmazonSideAsn" Core.=: amazonSideAsn,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "AvailabilityZone" Core.=: availabilityZone,
        "DryRun" Core.=: dryRun,
        "Type" Core.=: type'
      ]

-- | Contains the output of CreateVpnGateway.
--
-- /See:/ 'newCreateVpnGatewayResponse' smart constructor.
data CreateVpnGatewayResponse = CreateVpnGatewayResponse'
  { -- | Information about the virtual private gateway.
    vpnGateway :: Prelude.Maybe VpnGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnGateway', 'createVpnGatewayResponse_vpnGateway' - Information about the virtual private gateway.
--
-- 'httpStatus', 'createVpnGatewayResponse_httpStatus' - The response's http status code.
newCreateVpnGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpnGatewayResponse
newCreateVpnGatewayResponse pHttpStatus_ =
  CreateVpnGatewayResponse'
    { vpnGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the virtual private gateway.
createVpnGatewayResponse_vpnGateway :: Lens.Lens' CreateVpnGatewayResponse (Prelude.Maybe VpnGateway)
createVpnGatewayResponse_vpnGateway = Lens.lens (\CreateVpnGatewayResponse' {vpnGateway} -> vpnGateway) (\s@CreateVpnGatewayResponse' {} a -> s {vpnGateway = a} :: CreateVpnGatewayResponse)

-- | The response's http status code.
createVpnGatewayResponse_httpStatus :: Lens.Lens' CreateVpnGatewayResponse Prelude.Int
createVpnGatewayResponse_httpStatus = Lens.lens (\CreateVpnGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateVpnGatewayResponse' {} a -> s {httpStatus = a} :: CreateVpnGatewayResponse)

instance Prelude.NFData CreateVpnGatewayResponse
