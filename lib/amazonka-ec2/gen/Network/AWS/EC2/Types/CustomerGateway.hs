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
    cusCertificateARN,
    cusDeviceName,
    cusTags,
    cusBGPASN,
    cusCustomerGatewayId,
    cusIPAddress,
    cusState,
    cusType,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a customer gateway.
--
-- /See:/ 'mkCustomerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    deviceName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    bgpASN :: Lude.Text,
    customerGatewayId :: Lude.Text,
    ipAddress :: Lude.Text,
    state :: Lude.Text,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerGateway' with the minimum fields required to make a request.
--
-- * 'bgpASN' - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the customer gateway certificate.
-- * 'customerGatewayId' - The ID of the customer gateway.
-- * 'deviceName' - The name of customer gateway device.
-- * 'ipAddress' - The Internet-routable IP address of the customer gateway's outside interface.
-- * 'state' - The current state of the customer gateway (@pending | available | deleting | deleted@ ).
-- * 'tags' - Any tags assigned to the customer gateway.
-- * 'type'' - The type of VPN connection the customer gateway supports (@ipsec.1@ ).
mkCustomerGateway ::
  -- | 'bgpASN'
  Lude.Text ->
  -- | 'customerGatewayId'
  Lude.Text ->
  -- | 'ipAddress'
  Lude.Text ->
  -- | 'state'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  CustomerGateway
mkCustomerGateway
  pBGPASN_
  pCustomerGatewayId_
  pIPAddress_
  pState_
  pType_ =
    CustomerGateway'
      { certificateARN = Lude.Nothing,
        deviceName = Lude.Nothing,
        tags = Lude.Nothing,
        bgpASN = pBGPASN_,
        customerGatewayId = pCustomerGatewayId_,
        ipAddress = pIPAddress_,
        state = pState_,
        type' = pType_
      }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusCertificateARN :: Lens.Lens' CustomerGateway (Lude.Maybe Lude.Text)
cusCertificateARN = Lens.lens (certificateARN :: CustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CustomerGateway)
{-# DEPRECATED cusCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The name of customer gateway device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusDeviceName :: Lens.Lens' CustomerGateway (Lude.Maybe Lude.Text)
cusDeviceName = Lens.lens (deviceName :: CustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: CustomerGateway)
{-# DEPRECATED cusDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | Any tags assigned to the customer gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusTags :: Lens.Lens' CustomerGateway (Lude.Maybe [Tag])
cusTags = Lens.lens (tags :: CustomerGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CustomerGateway)
{-# DEPRECATED cusTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- /Note:/ Consider using 'bgpASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusBGPASN :: Lens.Lens' CustomerGateway Lude.Text
cusBGPASN = Lens.lens (bgpASN :: CustomerGateway -> Lude.Text) (\s a -> s {bgpASN = a} :: CustomerGateway)
{-# DEPRECATED cusBGPASN "Use generic-lens or generic-optics with 'bgpASN' instead." #-}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusCustomerGatewayId :: Lens.Lens' CustomerGateway Lude.Text
cusCustomerGatewayId = Lens.lens (customerGatewayId :: CustomerGateway -> Lude.Text) (\s a -> s {customerGatewayId = a} :: CustomerGateway)
{-# DEPRECATED cusCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The Internet-routable IP address of the customer gateway's outside interface.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusIPAddress :: Lens.Lens' CustomerGateway Lude.Text
cusIPAddress = Lens.lens (ipAddress :: CustomerGateway -> Lude.Text) (\s a -> s {ipAddress = a} :: CustomerGateway)
{-# DEPRECATED cusIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusState :: Lens.Lens' CustomerGateway Lude.Text
cusState = Lens.lens (state :: CustomerGateway -> Lude.Text) (\s a -> s {state = a} :: CustomerGateway)
{-# DEPRECATED cusState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusType :: Lens.Lens' CustomerGateway Lude.Text
cusType = Lens.lens (type' :: CustomerGateway -> Lude.Text) (\s a -> s {type' = a} :: CustomerGateway)
{-# DEPRECATED cusType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      Lude.<$> (x Lude..@? "certificateArn")
      Lude.<*> (x Lude..@? "deviceName")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "bgpAsn")
      Lude.<*> (x Lude..@ "customerGatewayId")
      Lude.<*> (x Lude..@ "ipAddress")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "type")
