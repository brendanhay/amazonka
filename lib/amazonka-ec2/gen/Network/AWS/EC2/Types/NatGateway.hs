{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NatGateway
  ( NatGateway (..)
  -- * Smart constructor
  , mkNatGateway
  -- * Lenses
  , ngCreateTime
  , ngDeleteTime
  , ngFailureCode
  , ngFailureMessage
  , ngNatGatewayAddresses
  , ngNatGatewayId
  , ngProvisionedBandwidth
  , ngState
  , ngSubnetId
  , ngTags
  , ngVpcId
  ) where

import qualified Network.AWS.EC2.Types.NatGatewayAddress as Types
import qualified Network.AWS.EC2.Types.NatGatewayState as Types
import qualified Network.AWS.EC2.Types.ProvisionedBandwidth as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a NAT gateway.
--
-- /See:/ 'mkNatGateway' smart constructor.
data NatGateway = NatGateway'
  { createTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time the NAT gateway was created.
  , deleteTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time the NAT gateway was deleted, if applicable.
  , failureCode :: Core.Maybe Core.Text
    -- ^ If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
  , failureMessage :: Core.Maybe Core.Text
    -- ^ If the NAT gateway could not be created, specifies the error message for the failure, that corresponds to the error code.
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
  , natGatewayAddresses :: Core.Maybe [Types.NatGatewayAddress]
    -- ^ Information about the IP addresses and network interface associated with the NAT gateway.
  , natGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the NAT gateway.
  , provisionedBandwidth :: Core.Maybe Types.ProvisionedBandwidth
    -- ^ Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
  , state :: Core.Maybe Types.NatGatewayState
    -- ^ The state of the NAT gateway.
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
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet in which the NAT gateway is located.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the NAT gateway.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC in which the NAT gateway is located.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NatGateway' value with any optional fields omitted.
mkNatGateway
    :: NatGateway
mkNatGateway
  = NatGateway'{createTime = Core.Nothing, deleteTime = Core.Nothing,
                failureCode = Core.Nothing, failureMessage = Core.Nothing,
                natGatewayAddresses = Core.Nothing, natGatewayId = Core.Nothing,
                provisionedBandwidth = Core.Nothing, state = Core.Nothing,
                subnetId = Core.Nothing, tags = Core.Nothing, vpcId = Core.Nothing}

-- | The date and time the NAT gateway was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngCreateTime :: Lens.Lens' NatGateway (Core.Maybe Core.UTCTime)
ngCreateTime = Lens.field @"createTime"
{-# INLINEABLE ngCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The date and time the NAT gateway was deleted, if applicable.
--
-- /Note:/ Consider using 'deleteTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngDeleteTime :: Lens.Lens' NatGateway (Core.Maybe Core.UTCTime)
ngDeleteTime = Lens.field @"deleteTime"
{-# INLINEABLE ngDeleteTime #-}
{-# DEPRECATED deleteTime "Use generic-lens or generic-optics with 'deleteTime' instead"  #-}

-- | If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngFailureCode :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
ngFailureCode = Lens.field @"failureCode"
{-# INLINEABLE ngFailureCode #-}
{-# DEPRECATED failureCode "Use generic-lens or generic-optics with 'failureCode' instead"  #-}

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
ngFailureMessage :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
ngFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE ngFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

-- | Information about the IP addresses and network interface associated with the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNatGatewayAddresses :: Lens.Lens' NatGateway (Core.Maybe [Types.NatGatewayAddress])
ngNatGatewayAddresses = Lens.field @"natGatewayAddresses"
{-# INLINEABLE ngNatGatewayAddresses #-}
{-# DEPRECATED natGatewayAddresses "Use generic-lens or generic-optics with 'natGatewayAddresses' instead"  #-}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNatGatewayId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
ngNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE ngNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisionedBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngProvisionedBandwidth :: Lens.Lens' NatGateway (Core.Maybe Types.ProvisionedBandwidth)
ngProvisionedBandwidth = Lens.field @"provisionedBandwidth"
{-# INLINEABLE ngProvisionedBandwidth #-}
{-# DEPRECATED provisionedBandwidth "Use generic-lens or generic-optics with 'provisionedBandwidth' instead"  #-}

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
ngState :: Lens.Lens' NatGateway (Core.Maybe Types.NatGatewayState)
ngState = Lens.field @"state"
{-# INLINEABLE ngState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the subnet in which the NAT gateway is located.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngSubnetId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
ngSubnetId = Lens.field @"subnetId"
{-# INLINEABLE ngSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The tags for the NAT gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngTags :: Lens.Lens' NatGateway (Core.Maybe [Types.Tag])
ngTags = Lens.field @"tags"
{-# INLINEABLE ngTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC in which the NAT gateway is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngVpcId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
ngVpcId = Lens.field @"vpcId"
{-# INLINEABLE ngVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML NatGateway where
        parseXML x
          = NatGateway' Core.<$>
              (x Core..@? "createTime") Core.<*> x Core..@? "deleteTime" Core.<*>
                x Core..@? "failureCode"
                Core.<*> x Core..@? "failureMessage"
                Core.<*>
                x Core..@? "natGatewayAddressSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "natGatewayId"
                Core.<*> x Core..@? "provisionedBandwidth"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcId"
