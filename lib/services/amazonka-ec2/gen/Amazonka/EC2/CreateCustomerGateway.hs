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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to Amazon Web Services about your customer gateway
-- device. The customer gateway device is the appliance at your end of the
-- VPN connection. You must provide the IP address of the customer gateway
-- device’s external interface. The IP address must be static and can be
-- behind a device performing network address translation (NAT).
--
-- For devices that use Border Gateway Protocol (BGP), you can also provide
-- the device\'s BGP Autonomous System Number (ASN). You can use an
-- existing ASN assigned to your network. If you don\'t have an ASN
-- already, you can use a private ASN. For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/cgw-options.html Customer gateway options for your Site-to-Site VPN connection>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
--
-- To create more than one customer gateway with the same VPN type, IP
-- address, and BGP ASN, specify a unique device name for each customer
-- gateway. An identical request returns information about the existing
-- customer gateway; it doesn\'t create a new customer gateway.
module Amazonka.EC2.CreateCustomerGateway
  ( -- * Creating a Request
    CreateCustomerGateway (..),
    newCreateCustomerGateway,

    -- * Request Lenses
    createCustomerGateway_certificateArn,
    createCustomerGateway_deviceName,
    createCustomerGateway_dryRun,
    createCustomerGateway_ipAddress,
    createCustomerGateway_publicIp,
    createCustomerGateway_tagSpecifications,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateCustomerGateway.
--
-- /See:/ 'newCreateCustomerGateway' smart constructor.
data CreateCustomerGateway = CreateCustomerGateway'
  { -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | A name for the customer gateway device.
    --
    -- Length Constraints: Up to 255 characters.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | IPv4 address for the customer gateway device\'s outside interface. The
    -- address must be static.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | /This member has been deprecated./ The Internet-routable IP address for
    -- the customer gateway\'s outside interface. The address must be static.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the customer gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
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
-- 'deviceName', 'createCustomerGateway_deviceName' - A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
--
-- 'dryRun', 'createCustomerGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipAddress', 'createCustomerGateway_ipAddress' - IPv4 address for the customer gateway device\'s outside interface. The
-- address must be static.
--
-- 'publicIp', 'createCustomerGateway_publicIp' - /This member has been deprecated./ The Internet-routable IP address for
-- the customer gateway\'s outside interface. The address must be static.
--
-- 'tagSpecifications', 'createCustomerGateway_tagSpecifications' - The tags to apply to the customer gateway.
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
      deviceName = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      bgpAsn = pBgpAsn_,
      type' = pType_
    }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
createCustomerGateway_certificateArn :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_certificateArn = Lens.lens (\CreateCustomerGateway' {certificateArn} -> certificateArn) (\s@CreateCustomerGateway' {} a -> s {certificateArn = a} :: CreateCustomerGateway)

-- | A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
createCustomerGateway_deviceName :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_deviceName = Lens.lens (\CreateCustomerGateway' {deviceName} -> deviceName) (\s@CreateCustomerGateway' {} a -> s {deviceName = a} :: CreateCustomerGateway)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCustomerGateway_dryRun :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Bool)
createCustomerGateway_dryRun = Lens.lens (\CreateCustomerGateway' {dryRun} -> dryRun) (\s@CreateCustomerGateway' {} a -> s {dryRun = a} :: CreateCustomerGateway)

-- | IPv4 address for the customer gateway device\'s outside interface. The
-- address must be static.
createCustomerGateway_ipAddress :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_ipAddress = Lens.lens (\CreateCustomerGateway' {ipAddress} -> ipAddress) (\s@CreateCustomerGateway' {} a -> s {ipAddress = a} :: CreateCustomerGateway)

-- | /This member has been deprecated./ The Internet-routable IP address for
-- the customer gateway\'s outside interface. The address must be static.
createCustomerGateway_publicIp :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe Prelude.Text)
createCustomerGateway_publicIp = Lens.lens (\CreateCustomerGateway' {publicIp} -> publicIp) (\s@CreateCustomerGateway' {} a -> s {publicIp = a} :: CreateCustomerGateway)

-- | The tags to apply to the customer gateway.
createCustomerGateway_tagSpecifications :: Lens.Lens' CreateCustomerGateway (Prelude.Maybe [TagSpecification])
createCustomerGateway_tagSpecifications = Lens.lens (\CreateCustomerGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateCustomerGateway' {} a -> s {tagSpecifications = a} :: CreateCustomerGateway) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCustomerGatewayResponse'
            Prelude.<$> (x Data..@? "customerGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomerGateway where
  hashWithSalt _salt CreateCustomerGateway' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` bgpAsn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateCustomerGateway where
  rnf CreateCustomerGateway' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf bgpAsn
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateCustomerGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCustomerGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomerGateway where
  toQuery CreateCustomerGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateCustomerGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CertificateArn" Data.=: certificateArn,
        "DeviceName" Data.=: deviceName,
        "DryRun" Data.=: dryRun,
        "IpAddress" Data.=: ipAddress,
        "PublicIp" Data.=: publicIp,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "BgpAsn" Data.=: bgpAsn,
        "Type" Data.=: type'
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
