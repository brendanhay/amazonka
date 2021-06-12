{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CustomerGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CustomerGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a customer gateway.
--
-- /See:/ 'newCustomerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The name of customer gateway device.
    deviceName :: Core.Maybe Core.Text,
    -- | Any tags assigned to the customer gateway.
    tags :: Core.Maybe [Tag],
    -- | The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
    -- Number (ASN).
    bgpAsn :: Core.Text,
    -- | The ID of the customer gateway.
    customerGatewayId :: Core.Text,
    -- | The Internet-routable IP address of the customer gateway\'s outside
    -- interface.
    ipAddress :: Core.Text,
    -- | The current state of the customer gateway
    -- (@pending | available | deleting | deleted@).
    state :: Core.Text,
    -- | The type of VPN connection the customer gateway supports (@ipsec.1@).
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomerGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'customerGateway_certificateArn' - The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- 'deviceName', 'customerGateway_deviceName' - The name of customer gateway device.
--
-- 'tags', 'customerGateway_tags' - Any tags assigned to the customer gateway.
--
-- 'bgpAsn', 'customerGateway_bgpAsn' - The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
--
-- 'customerGatewayId', 'customerGateway_customerGatewayId' - The ID of the customer gateway.
--
-- 'ipAddress', 'customerGateway_ipAddress' - The Internet-routable IP address of the customer gateway\'s outside
-- interface.
--
-- 'state', 'customerGateway_state' - The current state of the customer gateway
-- (@pending | available | deleting | deleted@).
--
-- 'type'', 'customerGateway_type' - The type of VPN connection the customer gateway supports (@ipsec.1@).
newCustomerGateway ::
  -- | 'bgpAsn'
  Core.Text ->
  -- | 'customerGatewayId'
  Core.Text ->
  -- | 'ipAddress'
  Core.Text ->
  -- | 'state'
  Core.Text ->
  -- | 'type''
  Core.Text ->
  CustomerGateway
newCustomerGateway
  pBgpAsn_
  pCustomerGatewayId_
  pIpAddress_
  pState_
  pType_ =
    CustomerGateway'
      { certificateArn = Core.Nothing,
        deviceName = Core.Nothing,
        tags = Core.Nothing,
        bgpAsn = pBgpAsn_,
        customerGatewayId = pCustomerGatewayId_,
        ipAddress = pIpAddress_,
        state = pState_,
        type' = pType_
      }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
customerGateway_certificateArn :: Lens.Lens' CustomerGateway (Core.Maybe Core.Text)
customerGateway_certificateArn = Lens.lens (\CustomerGateway' {certificateArn} -> certificateArn) (\s@CustomerGateway' {} a -> s {certificateArn = a} :: CustomerGateway)

-- | The name of customer gateway device.
customerGateway_deviceName :: Lens.Lens' CustomerGateway (Core.Maybe Core.Text)
customerGateway_deviceName = Lens.lens (\CustomerGateway' {deviceName} -> deviceName) (\s@CustomerGateway' {} a -> s {deviceName = a} :: CustomerGateway)

-- | Any tags assigned to the customer gateway.
customerGateway_tags :: Lens.Lens' CustomerGateway (Core.Maybe [Tag])
customerGateway_tags = Lens.lens (\CustomerGateway' {tags} -> tags) (\s@CustomerGateway' {} a -> s {tags = a} :: CustomerGateway) Core.. Lens.mapping Lens._Coerce

-- | The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
customerGateway_bgpAsn :: Lens.Lens' CustomerGateway Core.Text
customerGateway_bgpAsn = Lens.lens (\CustomerGateway' {bgpAsn} -> bgpAsn) (\s@CustomerGateway' {} a -> s {bgpAsn = a} :: CustomerGateway)

-- | The ID of the customer gateway.
customerGateway_customerGatewayId :: Lens.Lens' CustomerGateway Core.Text
customerGateway_customerGatewayId = Lens.lens (\CustomerGateway' {customerGatewayId} -> customerGatewayId) (\s@CustomerGateway' {} a -> s {customerGatewayId = a} :: CustomerGateway)

-- | The Internet-routable IP address of the customer gateway\'s outside
-- interface.
customerGateway_ipAddress :: Lens.Lens' CustomerGateway Core.Text
customerGateway_ipAddress = Lens.lens (\CustomerGateway' {ipAddress} -> ipAddress) (\s@CustomerGateway' {} a -> s {ipAddress = a} :: CustomerGateway)

-- | The current state of the customer gateway
-- (@pending | available | deleting | deleted@).
customerGateway_state :: Lens.Lens' CustomerGateway Core.Text
customerGateway_state = Lens.lens (\CustomerGateway' {state} -> state) (\s@CustomerGateway' {} a -> s {state = a} :: CustomerGateway)

-- | The type of VPN connection the customer gateway supports (@ipsec.1@).
customerGateway_type :: Lens.Lens' CustomerGateway Core.Text
customerGateway_type = Lens.lens (\CustomerGateway' {type'} -> type') (\s@CustomerGateway' {} a -> s {type' = a} :: CustomerGateway)

instance Core.FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      Core.<$> (x Core..@? "certificateArn")
      Core.<*> (x Core..@? "deviceName")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "bgpAsn")
      Core.<*> (x Core..@ "customerGatewayId")
      Core.<*> (x Core..@ "ipAddress")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "type")

instance Core.Hashable CustomerGateway

instance Core.NFData CustomerGateway
