{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a customer gateway.
--
-- /See:/ 'newCustomerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of customer gateway device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the customer gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
    -- Number (ASN).
    bgpAsn :: Prelude.Text,
    -- | The ID of the customer gateway.
    customerGatewayId :: Prelude.Text,
    -- | The Internet-routable IP address of the customer gateway\'s outside
    -- interface.
    ipAddress :: Prelude.Text,
    -- | The current state of the customer gateway
    -- (@pending | available | deleting | deleted@).
    state :: Prelude.Text,
    -- | The type of VPN connection the customer gateway supports (@ipsec.1@).
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'customerGatewayId'
  Prelude.Text ->
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'state'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  CustomerGateway
newCustomerGateway
  pBgpAsn_
  pCustomerGatewayId_
  pIpAddress_
  pState_
  pType_ =
    CustomerGateway'
      { certificateArn = Prelude.Nothing,
        deviceName = Prelude.Nothing,
        tags = Prelude.Nothing,
        bgpAsn = pBgpAsn_,
        customerGatewayId = pCustomerGatewayId_,
        ipAddress = pIpAddress_,
        state = pState_,
        type' = pType_
      }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
customerGateway_certificateArn :: Lens.Lens' CustomerGateway (Prelude.Maybe Prelude.Text)
customerGateway_certificateArn = Lens.lens (\CustomerGateway' {certificateArn} -> certificateArn) (\s@CustomerGateway' {} a -> s {certificateArn = a} :: CustomerGateway)

-- | The name of customer gateway device.
customerGateway_deviceName :: Lens.Lens' CustomerGateway (Prelude.Maybe Prelude.Text)
customerGateway_deviceName = Lens.lens (\CustomerGateway' {deviceName} -> deviceName) (\s@CustomerGateway' {} a -> s {deviceName = a} :: CustomerGateway)

-- | Any tags assigned to the customer gateway.
customerGateway_tags :: Lens.Lens' CustomerGateway (Prelude.Maybe [Tag])
customerGateway_tags = Lens.lens (\CustomerGateway' {tags} -> tags) (\s@CustomerGateway' {} a -> s {tags = a} :: CustomerGateway) Prelude.. Lens.mapping Prelude._Coerce

-- | The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
customerGateway_bgpAsn :: Lens.Lens' CustomerGateway Prelude.Text
customerGateway_bgpAsn = Lens.lens (\CustomerGateway' {bgpAsn} -> bgpAsn) (\s@CustomerGateway' {} a -> s {bgpAsn = a} :: CustomerGateway)

-- | The ID of the customer gateway.
customerGateway_customerGatewayId :: Lens.Lens' CustomerGateway Prelude.Text
customerGateway_customerGatewayId = Lens.lens (\CustomerGateway' {customerGatewayId} -> customerGatewayId) (\s@CustomerGateway' {} a -> s {customerGatewayId = a} :: CustomerGateway)

-- | The Internet-routable IP address of the customer gateway\'s outside
-- interface.
customerGateway_ipAddress :: Lens.Lens' CustomerGateway Prelude.Text
customerGateway_ipAddress = Lens.lens (\CustomerGateway' {ipAddress} -> ipAddress) (\s@CustomerGateway' {} a -> s {ipAddress = a} :: CustomerGateway)

-- | The current state of the customer gateway
-- (@pending | available | deleting | deleted@).
customerGateway_state :: Lens.Lens' CustomerGateway Prelude.Text
customerGateway_state = Lens.lens (\CustomerGateway' {state} -> state) (\s@CustomerGateway' {} a -> s {state = a} :: CustomerGateway)

-- | The type of VPN connection the customer gateway supports (@ipsec.1@).
customerGateway_type :: Lens.Lens' CustomerGateway Prelude.Text
customerGateway_type = Lens.lens (\CustomerGateway' {type'} -> type') (\s@CustomerGateway' {} a -> s {type' = a} :: CustomerGateway)

instance Prelude.FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      Prelude.<$> (x Prelude..@? "certificateArn")
      Prelude.<*> (x Prelude..@? "deviceName")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "bgpAsn")
      Prelude.<*> (x Prelude..@ "customerGatewayId")
      Prelude.<*> (x Prelude..@ "ipAddress")
      Prelude.<*> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "type")

instance Prelude.Hashable CustomerGateway

instance Prelude.NFData CustomerGateway
