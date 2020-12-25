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
    cBgpAsn,
    cCertificateArn,
    cCustomerGatewayId,
    cDeviceName,
    cIpAddress,
    cState,
    cTags,
    cType,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a customer gateway.
--
-- /See:/ 'mkCustomerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { -- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
    bgpAsn :: Types.String,
    -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateArn :: Core.Maybe Types.String,
    -- | The ID of the customer gateway.
    customerGatewayId :: Types.String,
    -- | The name of customer gateway device.
    deviceName :: Core.Maybe Types.String,
    -- | The Internet-routable IP address of the customer gateway's outside interface.
    ipAddress :: Types.String,
    -- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
    state :: Types.String,
    -- | Any tags assigned to the customer gateway.
    tags :: Core.Maybe [Types.Tag],
    -- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
    type' :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerGateway' value with any optional fields omitted.
mkCustomerGateway ::
  -- | 'bgpAsn'
  Types.String ->
  -- | 'customerGatewayId'
  Types.String ->
  -- | 'ipAddress'
  Types.String ->
  -- | 'state'
  Types.String ->
  -- | 'type\''
  Types.String ->
  CustomerGateway
mkCustomerGateway bgpAsn customerGatewayId ipAddress state type' =
  CustomerGateway'
    { bgpAsn,
      certificateArn = Core.Nothing,
      customerGatewayId,
      deviceName = Core.Nothing,
      ipAddress,
      state,
      tags = Core.Nothing,
      type'
    }

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- /Note:/ Consider using 'bgpAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBgpAsn :: Lens.Lens' CustomerGateway Types.String
cBgpAsn = Lens.field @"bgpAsn"
{-# DEPRECATED cBgpAsn "Use generic-lens or generic-optics with 'bgpAsn' instead." #-}

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' CustomerGateway (Core.Maybe Types.String)
cCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED cCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomerGatewayId :: Lens.Lens' CustomerGateway Types.String
cCustomerGatewayId = Lens.field @"customerGatewayId"
{-# DEPRECATED cCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The name of customer gateway device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeviceName :: Lens.Lens' CustomerGateway (Core.Maybe Types.String)
cDeviceName = Lens.field @"deviceName"
{-# DEPRECATED cDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The Internet-routable IP address of the customer gateway's outside interface.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIpAddress :: Lens.Lens' CustomerGateway Types.String
cIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED cIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' CustomerGateway Types.String
cState = Lens.field @"state"
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Any tags assigned to the customer gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CustomerGateway (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' CustomerGateway Types.String
cType = Lens.field @"type'"
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      Core.<$> (x Core..@ "bgpAsn")
      Core.<*> (x Core..@? "certificateArn")
      Core.<*> (x Core..@ "customerGatewayId")
      Core.<*> (x Core..@? "deviceName")
      Core.<*> (x Core..@ "ipAddress")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "type")
