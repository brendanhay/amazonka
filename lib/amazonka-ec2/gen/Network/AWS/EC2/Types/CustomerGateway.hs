{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CustomerGateway
  ( CustomerGateway (..),

    -- * Smart constructor
    mkCustomerGateway,

    -- * Lenses
    cgfState,
    cgfIPAddress,
    cgfCertificateARN,
    cgfBGPASN,
    cgfCustomerGatewayId,
    cgfDeviceName,
    cgfType,
    cgfTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a customer gateway.
--
-- /See:/ 'mkCustomerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { -- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
    state :: Lude.Text,
    -- | The Internet-routable IP address of the customer gateway's outside interface.
    ipAddress :: Lude.Text,
    -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
    bgpASN :: Lude.Text,
    -- | The ID of the customer gateway.
    customerGatewayId :: Lude.Text,
    -- | The name of customer gateway device.
    deviceName :: Lude.Maybe Lude.Text,
    -- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
    type' :: Lude.Text,
    -- | Any tags assigned to the customer gateway.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerGateway' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the customer gateway (@pending | available | deleting | deleted@ ).
-- * 'ipAddress' - The Internet-routable IP address of the customer gateway's outside interface.
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the customer gateway certificate.
-- * 'bgpASN' - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
-- * 'customerGatewayId' - The ID of the customer gateway.
-- * 'deviceName' - The name of customer gateway device.
-- * 'type'' - The type of VPN connection the customer gateway supports (@ipsec.1@ ).
-- * 'tags' - Any tags assigned to the customer gateway.
mkCustomerGateway ::
  -- | 'state'
  Lude.Text ->
  -- | 'ipAddress'
  Lude.Text ->
  -- | 'bgpASN'
  Lude.Text ->
  -- | 'customerGatewayId'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  CustomerGateway
mkCustomerGateway
  pState_
  pIPAddress_
  pBGPASN_
  pCustomerGatewayId_
  pType_ =
    CustomerGateway'
      { state = pState_,
        ipAddress = pIPAddress_,
        certificateARN = Lude.Nothing,
        bgpASN = pBGPASN_,
        customerGatewayId = pCustomerGatewayId_,
        deviceName = Lude.Nothing,
        type' = pType_,
        tags = Lude.Nothing
      }

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfState :: Lens.Lens' CustomerGateway Lude.Text
cgfState = Lens.lens (state :: CustomerGateway -> Lude.Text) (\s a -> s {state = a} :: CustomerGateway)
{-# DEPRECATED cgfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Internet-routable IP address of the customer gateway's outside interface.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfIPAddress :: Lens.Lens' CustomerGateway Lude.Text
cgfIPAddress = Lens.lens (ipAddress :: CustomerGateway -> Lude.Text) (\s a -> s {ipAddress = a} :: CustomerGateway)
{-# DEPRECATED cgfIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfCertificateARN :: Lens.Lens' CustomerGateway (Lude.Maybe Lude.Text)
cgfCertificateARN = Lens.lens (certificateARN :: CustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CustomerGateway)
{-# DEPRECATED cgfCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- /Note:/ Consider using 'bgpASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfBGPASN :: Lens.Lens' CustomerGateway Lude.Text
cgfBGPASN = Lens.lens (bgpASN :: CustomerGateway -> Lude.Text) (\s a -> s {bgpASN = a} :: CustomerGateway)
{-# DEPRECATED cgfBGPASN "Use generic-lens or generic-optics with 'bgpASN' instead." #-}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfCustomerGatewayId :: Lens.Lens' CustomerGateway Lude.Text
cgfCustomerGatewayId = Lens.lens (customerGatewayId :: CustomerGateway -> Lude.Text) (\s a -> s {customerGatewayId = a} :: CustomerGateway)
{-# DEPRECATED cgfCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The name of customer gateway device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfDeviceName :: Lens.Lens' CustomerGateway (Lude.Maybe Lude.Text)
cgfDeviceName = Lens.lens (deviceName :: CustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: CustomerGateway)
{-# DEPRECATED cgfDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfType :: Lens.Lens' CustomerGateway Lude.Text
cgfType = Lens.lens (type' :: CustomerGateway -> Lude.Text) (\s a -> s {type' = a} :: CustomerGateway)
{-# DEPRECATED cgfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Any tags assigned to the customer gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgfTags :: Lens.Lens' CustomerGateway (Lude.Maybe [Tag])
cgfTags = Lens.lens (tags :: CustomerGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CustomerGateway)
{-# DEPRECATED cgfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      Lude.<$> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "ipAddress")
      Lude.<*> (x Lude..@? "certificateArn")
      Lude.<*> (x Lude..@ "bgpAsn")
      Lude.<*> (x Lude..@ "customerGatewayId")
      Lude.<*> (x Lude..@? "deviceName")
      Lude.<*> (x Lude..@ "type")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
