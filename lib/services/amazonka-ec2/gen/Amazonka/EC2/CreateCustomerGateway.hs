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
-- Module      : Amazonka.EC2.CreateCustomerGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to Amazon Web Services about your VPN customer
-- gateway device. The customer gateway is the appliance at your end of the
-- VPN connection. (The device on the Amazon Web Services side of the VPN
-- connection is the virtual private gateway.) You must provide the
-- internet-routable IP address of the customer gateway\'s external
-- interface. The IP address must be static and can be behind a device
-- performing network address translation (NAT).
--
-- For devices that use Border Gateway Protocol (BGP), you can also provide
-- the device\'s BGP Autonomous System Number (ASN). You can use an
-- existing ASN assigned to your network. If you don\'t have an ASN
-- already, you can use a private ASN (in the 64512 - 65534 range).
--
-- Amazon EC2 supports all 4-byte ASN numbers in the range of 1 -
-- 2147483647, with the exception of the following:
--
-- -   7224 - reserved in the @us-east-1@ Region
--
-- -   9059 - reserved in the @eu-west-1@ Region
--
-- -   17943 - reserved in the @ap-southeast-1@ Region
--
-- -   10124 - reserved in the @ap-northeast-1@ Region
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
--
-- To create more than one customer gateway with the same VPN type, IP
-- address, and BGP ASN, specify a unique device name for each customer
-- gateway. Identical requests return information about the existing
-- customer gateway and do not create new customer gateways.
module Amazonka.EC2.CreateCustomerGateway
  ( -- * Creating a Request
    CreateCustomerGateway (..),
    newCreateCustomerGateway,

    -- * Request Lenses
    createCustomerGateway_certificateArn,
    createCustomerGateway_tagSpecifications,
    createCustomerGateway_deviceName,
    createCustomerGateway_publicIp,
    createCustomerGateway_dryRun,
    createCustomerGateway_bgpAsn,
    createCustomerGateway_type,

    -- * Destructuring the Response
    CreateCustomerGatewayResponse (..),
    newCreateCustomerGatewayResponse,

    -- * Response Lenses
    createCustomerGatewayResponse_customerGateway,
    createCustomerGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateCustomerGateway.
--
-- /See:/ 'newCreateCustomerGateway' smart constructor.
data CreateCustomerGateway = CreateCustomerGateway'
  { -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the customer gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | A name for the customer gateway device.
    --
    -- Length Constraints: Up to 255 characters.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The Internet-routable IP address for the customer gateway\'s outside
    -- interface. The address must be static.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | For devices that support BGP, the customer gateway\'s BGP ASN.
    --
    -- Default: 65000
    bgpAsn :: Prelude.Int,
    -- | The type of VPN connection that this customer gateway supports
    -- (@ipsec.1@).
    type' :: GatewayType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomerGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'createCustomerGateway_certificateArn' - The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- 'tagSpecifications', 'createCustomerGateway_tagSpecifications' - The tags to apply to the customer gateway.
--
-- 'deviceName', 'createCustomerGateway_deviceName' - A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
--
-- 'publicIp', 'createCustomerGateway_publicIp' - The Internet-routable IP address for the customer gateway\'s outside
-- interface. The address must be static.
--
-- 'dryRun', 'createCustomerGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'bgpAsn', 'createCustomerGateway_bgpAsn' - For devices that support BGP, the customer gateway\'s BGP ASN.
--
-- Default: 65000
--
-- 'type'', 'createCustomerGateway_type' - The type of VPN connection that this customer gateway supports
-- (@ipsec.1@).
newCreateCustomerGateway ::
  -- | 'bgpAsn'
  Prelude.Int ->
  -- | 'type''
  GatewayType ->
  CreateCustomerGateway
newCreateCustomerGateway pBgpAsn_ pType_ =
  CreateCustomerGateway'
    { certificateArn =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      bgpAsn = pBgpAsn_,
      type' = pType_
    }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
createCustomerGateway_certificateArn :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_certificateArn = Lens.lens (\CreateCustomerGateway' {certificateArn} -> certificateArn) (\s@CreateCustomerGateway' {} a -> s {certificateArn = a} :: CreateCustomerGateway)

-- | The tags to apply to the customer gateway.
createCustomerGateway_tagSpecifications :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe [TagSpecification])
createCustomerGateway_tagSpecifications = Lens.lens (\CreateCustomerGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateCustomerGateway' {} a -> s {tagSpecifications = a} :: CreateCustomerGateway) Prelude.. Lens.mapping Lens.coerced

-- | A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
createCustomerGateway_deviceName :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_deviceName = Lens.lens (\CreateCustomerGateway' {deviceName} -> deviceName) (\s@CreateCustomerGateway' {} a -> s {deviceName = a} :: CreateCustomerGateway)

-- | The Internet-routable IP address for the customer gateway\'s outside
-- interface. The address must be static.
createCustomerGateway_publicIp :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_publicIp = Lens.lens (\CreateCustomerGateway' {publicIp} -> publicIp) (\s@CreateCustomerGateway' {} a -> s {publicIp = a} :: CreateCustomerGateway)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCustomerGateway_dryRun :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Bool)
createCustomerGateway_dryRun = Lens.lens (\CreateCustomerGateway' {dryRun} -> dryRun) (\s@CreateCustomerGateway' {} a -> s {dryRun = a} :: CreateCustomerGateway)

-- | For devices that support BGP, the customer gateway\'s BGP ASN.
--
-- Default: 65000
createCustomerGateway_bgpAsn :: Lens.Lens' CreateCustomerGateway Prelude.Int
createCustomerGateway_bgpAsn = Lens.lens (\CreateCustomerGateway' {bgpAsn} -> bgpAsn) (\s@CreateCustomerGateway' {} a -> s {bgpAsn = a} :: CreateCustomerGateway)

-- | The type of VPN connection that this customer gateway supports
-- (@ipsec.1@).
createCustomerGateway_type :: Lens.Lens' CreateCustomerGateway GatewayType
createCustomerGateway_type = Lens.lens (\CreateCustomerGateway' {type'} -> type') (\s@CreateCustomerGateway' {} a -> s {type' = a} :: CreateCustomerGateway)

instance Core.AWSRequest CreateCustomerGateway where
  type
    AWSResponse CreateCustomerGateway =
      CreateCustomerGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCustomerGatewayResponse'
            Prelude.<$> (x Core..@? "customerGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomerGateway where
  hashWithSalt _salt CreateCustomerGateway' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` bgpAsn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateCustomerGateway where
  rnf CreateCustomerGateway' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf bgpAsn
      `Prelude.seq` Prelude.rnf type'

instance Core.ToHeaders CreateCustomerGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCustomerGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCustomerGateway where
  toQuery CreateCustomerGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCustomerGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "CertificateArn" Core.=: certificateArn,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DeviceName" Core.=: deviceName,
        "IpAddress" Core.=: publicIp,
        "DryRun" Core.=: dryRun,
        "BgpAsn" Core.=: bgpAsn,
        "Type" Core.=: type'
      ]

-- | Contains the output of CreateCustomerGateway.
--
-- /See:/ 'newCreateCustomerGatewayResponse' smart constructor.
data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse'
  { -- | Information about the customer gateway.
    customerGateway :: Prelude.Maybe CustomerGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomerGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGateway', 'createCustomerGatewayResponse_customerGateway' - Information about the customer gateway.
--
-- 'httpStatus', 'createCustomerGatewayResponse_httpStatus' - The response's http status code.
newCreateCustomerGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomerGatewayResponse
newCreateCustomerGatewayResponse pHttpStatus_ =
  CreateCustomerGatewayResponse'
    { customerGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the customer gateway.
createCustomerGatewayResponse_customerGateway :: Lens.Lens' CreateCustomerGatewayResponse (Prelude.Maybe CustomerGateway)
createCustomerGatewayResponse_customerGateway = Lens.lens (\CreateCustomerGatewayResponse' {customerGateway} -> customerGateway) (\s@CreateCustomerGatewayResponse' {} a -> s {customerGateway = a} :: CreateCustomerGatewayResponse)

-- | The response's http status code.
createCustomerGatewayResponse_httpStatus :: Lens.Lens' CreateCustomerGatewayResponse Prelude.Int
createCustomerGatewayResponse_httpStatus = Lens.lens (\CreateCustomerGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateCustomerGatewayResponse' {} a -> s {httpStatus = a} :: CreateCustomerGatewayResponse)

instance Prelude.NFData CreateCustomerGatewayResponse where
  rnf CreateCustomerGatewayResponse' {..} =
    Prelude.rnf customerGateway
      `Prelude.seq` Prelude.rnf httpStatus
