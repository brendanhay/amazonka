-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGateway
  ( NatGateway (..),

    -- * Smart constructor
    mkNatGateway,

    -- * Lenses
    ngState,
    ngFailureCode,
    ngVPCId,
    ngFailureMessage,
    ngNatGatewayId,
    ngSubnetId,
    ngDeleteTime,
    ngProvisionedBandwidth,
    ngNatGatewayAddresses,
    ngCreateTime,
    ngTags,
  )
where

import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NatGatewayState
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a NAT gateway.
--
-- /See:/ 'mkNatGateway' smart constructor.
data NatGateway = NatGateway'
  { state :: Lude.Maybe NatGatewayState,
    failureCode :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    failureMessage :: Lude.Maybe Lude.Text,
    natGatewayId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    deleteTime :: Lude.Maybe Lude.ISO8601,
    provisionedBandwidth :: Lude.Maybe ProvisionedBandwidth,
    natGatewayAddresses :: Lude.Maybe [NatGatewayAddress],
    createTime :: Lude.Maybe Lude.ISO8601,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NatGateway' with the minimum fields required to make a request.
--
-- * 'createTime' - The date and time the NAT gateway was created.
-- * 'deleteTime' - The date and time the NAT gateway was deleted, if applicable.
-- * 'failureCode' - If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
-- * 'failureMessage' - If the NAT gateway could not be created, specifies the error message for the failure, that corresponds to the error code.
--
--
--     * For InsufficientFreeAddressesInSubnet: "Subnet has insufficient free addresses to create this NAT gateway"
--
--
--     * For Gateway.NotAttached: "Network vpc-xxxxxxxx has no Internet gateway attached"
--
--
--     * For InvalidAllocationID.NotFound: "Elastic IP address eipalloc-xxxxxxxx could not be associated with this NAT gateway"
--
--
--     * For Resource.AlreadyAssociated: "Elastic IP address eipalloc-xxxxxxxx is already associated"
--
--
--     * For InternalError: "Network interface eni-xxxxxxxx, created and used internally by this NAT gateway is in an invalid state. Please try again."
--
--
--     * For InvalidSubnetID.NotFound: "The specified subnet subnet-xxxxxxxx does not exist or could not be found."
--
--
-- * 'natGatewayAddresses' - Information about the IP addresses and network interface associated with the NAT gateway.
-- * 'natGatewayId' - The ID of the NAT gateway.
-- * 'provisionedBandwidth' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
-- * 'state' - The state of the NAT gateway.
--
--
--     * @pending@ : The NAT gateway is being created and is not ready to process traffic.
--
--
--     * @failed@ : The NAT gateway could not be created. Check the @failureCode@ and @failureMessage@ fields for the reason.
--
--
--     * @available@ : The NAT gateway is able to process traffic. This status remains until you delete the NAT gateway, and does not indicate the health of the NAT gateway.
--
--
--     * @deleting@ : The NAT gateway is in the process of being terminated and may still be processing traffic.
--
--
--     * @deleted@ : The NAT gateway has been terminated and is no longer processing traffic.
--
--
-- * 'subnetId' - The ID of the subnet in which the NAT gateway is located.
-- * 'tags' - The tags for the NAT gateway.
-- * 'vpcId' - The ID of the VPC in which the NAT gateway is located.
mkNatGateway ::
  NatGateway
mkNatGateway =
  NatGateway'
    { state = Lude.Nothing,
      failureCode = Lude.Nothing,
      vpcId = Lude.Nothing,
      failureMessage = Lude.Nothing,
      natGatewayId = Lude.Nothing,
      subnetId = Lude.Nothing,
      deleteTime = Lude.Nothing,
      provisionedBandwidth = Lude.Nothing,
      natGatewayAddresses = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the NAT gateway.
--
--
--     * @pending@ : The NAT gateway is being created and is not ready to process traffic.
--
--
--     * @failed@ : The NAT gateway could not be created. Check the @failureCode@ and @failureMessage@ fields for the reason.
--
--
--     * @available@ : The NAT gateway is able to process traffic. This status remains until you delete the NAT gateway, and does not indicate the health of the NAT gateway.
--
--
--     * @deleting@ : The NAT gateway is in the process of being terminated and may still be processing traffic.
--
--
--     * @deleted@ : The NAT gateway has been terminated and is no longer processing traffic.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngState :: Lens.Lens' NatGateway (Lude.Maybe NatGatewayState)
ngState = Lens.lens (state :: NatGateway -> Lude.Maybe NatGatewayState) (\s a -> s {state = a} :: NatGateway)
{-# DEPRECATED ngState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngFailureCode :: Lens.Lens' NatGateway (Lude.Maybe Lude.Text)
ngFailureCode = Lens.lens (failureCode :: NatGateway -> Lude.Maybe Lude.Text) (\s a -> s {failureCode = a} :: NatGateway)
{-# DEPRECATED ngFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The ID of the VPC in which the NAT gateway is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngVPCId :: Lens.Lens' NatGateway (Lude.Maybe Lude.Text)
ngVPCId = Lens.lens (vpcId :: NatGateway -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: NatGateway)
{-# DEPRECATED ngVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | If the NAT gateway could not be created, specifies the error message for the failure, that corresponds to the error code.
--
--
--     * For InsufficientFreeAddressesInSubnet: "Subnet has insufficient free addresses to create this NAT gateway"
--
--
--     * For Gateway.NotAttached: "Network vpc-xxxxxxxx has no Internet gateway attached"
--
--
--     * For InvalidAllocationID.NotFound: "Elastic IP address eipalloc-xxxxxxxx could not be associated with this NAT gateway"
--
--
--     * For Resource.AlreadyAssociated: "Elastic IP address eipalloc-xxxxxxxx is already associated"
--
--
--     * For InternalError: "Network interface eni-xxxxxxxx, created and used internally by this NAT gateway is in an invalid state. Please try again."
--
--
--     * For InvalidSubnetID.NotFound: "The specified subnet subnet-xxxxxxxx does not exist or could not be found."
--
--
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngFailureMessage :: Lens.Lens' NatGateway (Lude.Maybe Lude.Text)
ngFailureMessage = Lens.lens (failureMessage :: NatGateway -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: NatGateway)
{-# DEPRECATED ngFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNatGatewayId :: Lens.Lens' NatGateway (Lude.Maybe Lude.Text)
ngNatGatewayId = Lens.lens (natGatewayId :: NatGateway -> Lude.Maybe Lude.Text) (\s a -> s {natGatewayId = a} :: NatGateway)
{-# DEPRECATED ngNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of the subnet in which the NAT gateway is located.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngSubnetId :: Lens.Lens' NatGateway (Lude.Maybe Lude.Text)
ngSubnetId = Lens.lens (subnetId :: NatGateway -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NatGateway)
{-# DEPRECATED ngSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The date and time the NAT gateway was deleted, if applicable.
--
-- /Note:/ Consider using 'deleteTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngDeleteTime :: Lens.Lens' NatGateway (Lude.Maybe Lude.ISO8601)
ngDeleteTime = Lens.lens (deleteTime :: NatGateway -> Lude.Maybe Lude.ISO8601) (\s a -> s {deleteTime = a} :: NatGateway)
{-# DEPRECATED ngDeleteTime "Use generic-lens or generic-optics with 'deleteTime' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisionedBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngProvisionedBandwidth :: Lens.Lens' NatGateway (Lude.Maybe ProvisionedBandwidth)
ngProvisionedBandwidth = Lens.lens (provisionedBandwidth :: NatGateway -> Lude.Maybe ProvisionedBandwidth) (\s a -> s {provisionedBandwidth = a} :: NatGateway)
{-# DEPRECATED ngProvisionedBandwidth "Use generic-lens or generic-optics with 'provisionedBandwidth' instead." #-}

-- | Information about the IP addresses and network interface associated with the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNatGatewayAddresses :: Lens.Lens' NatGateway (Lude.Maybe [NatGatewayAddress])
ngNatGatewayAddresses = Lens.lens (natGatewayAddresses :: NatGateway -> Lude.Maybe [NatGatewayAddress]) (\s a -> s {natGatewayAddresses = a} :: NatGateway)
{-# DEPRECATED ngNatGatewayAddresses "Use generic-lens or generic-optics with 'natGatewayAddresses' instead." #-}

-- | The date and time the NAT gateway was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngCreateTime :: Lens.Lens' NatGateway (Lude.Maybe Lude.ISO8601)
ngCreateTime = Lens.lens (createTime :: NatGateway -> Lude.Maybe Lude.ISO8601) (\s a -> s {createTime = a} :: NatGateway)
{-# DEPRECATED ngCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The tags for the NAT gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngTags :: Lens.Lens' NatGateway (Lude.Maybe [Tag])
ngTags = Lens.lens (tags :: NatGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: NatGateway)
{-# DEPRECATED ngTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML NatGateway where
  parseXML x =
    NatGateway'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "failureCode")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "failureMessage")
      Lude.<*> (x Lude..@? "natGatewayId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "deleteTime")
      Lude.<*> (x Lude..@? "provisionedBandwidth")
      Lude.<*> ( x Lude..@? "natGatewayAddressSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
