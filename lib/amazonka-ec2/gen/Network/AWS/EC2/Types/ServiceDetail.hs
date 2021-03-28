{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ServiceDetail
  ( ServiceDetail (..)
  -- * Smart constructor
  , mkServiceDetail
  -- * Lenses
  , sdAcceptanceRequired
  , sdAvailabilityZones
  , sdBaseEndpointDnsNames
  , sdManagesVpcEndpoints
  , sdOwner
  , sdPrivateDnsName
  , sdPrivateDnsNameVerificationState
  , sdPrivateDnsNames
  , sdServiceId
  , sdServiceName
  , sdServiceType
  , sdTags
  , sdVpcEndpointPolicySupported
  ) where

import qualified Network.AWS.EC2.Types.DnsNameState as Types
import qualified Network.AWS.EC2.Types.PrivateDnsDetails as Types
import qualified Network.AWS.EC2.Types.ServiceTypeDetail as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC endpoint service.
--
-- /See:/ 'mkServiceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { acceptanceRequired :: Core.Maybe Core.Bool
    -- ^ Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ The Availability Zones in which the service is available.
  , baseEndpointDnsNames :: Core.Maybe [Core.Text]
    -- ^ The DNS names for the service.
  , managesVpcEndpoints :: Core.Maybe Core.Bool
    -- ^ Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
  , owner :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the service owner.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name for the service.
  , privateDnsNameVerificationState :: Core.Maybe Types.DnsNameState
    -- ^ The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
  , privateDnsNames :: Core.Maybe [Types.PrivateDnsDetails]
    -- ^ The private DNS names assigned to the VPC endpoint service. 
  , serviceId :: Core.Maybe Core.Text
    -- ^ The ID of the endpoint service.
  , serviceName :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the service.
  , serviceType :: Core.Maybe [Types.ServiceTypeDetail]
    -- ^ The type of service.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the service.
  , vpcEndpointPolicySupported :: Core.Maybe Core.Bool
    -- ^ Indicates whether the service supports endpoint policies.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceDetail' value with any optional fields omitted.
mkServiceDetail
    :: ServiceDetail
mkServiceDetail
  = ServiceDetail'{acceptanceRequired = Core.Nothing,
                   availabilityZones = Core.Nothing,
                   baseEndpointDnsNames = Core.Nothing,
                   managesVpcEndpoints = Core.Nothing, owner = Core.Nothing,
                   privateDnsName = Core.Nothing,
                   privateDnsNameVerificationState = Core.Nothing,
                   privateDnsNames = Core.Nothing, serviceId = Core.Nothing,
                   serviceName = Core.Nothing, serviceType = Core.Nothing,
                   tags = Core.Nothing, vpcEndpointPolicySupported = Core.Nothing}

-- | Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAcceptanceRequired :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# INLINEABLE sdAcceptanceRequired #-}
{-# DEPRECATED acceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead"  #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAvailabilityZones :: Lens.Lens' ServiceDetail (Core.Maybe [Core.Text])
sdAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE sdAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBaseEndpointDnsNames :: Lens.Lens' ServiceDetail (Core.Maybe [Core.Text])
sdBaseEndpointDnsNames = Lens.field @"baseEndpointDnsNames"
{-# INLINEABLE sdBaseEndpointDnsNames #-}
{-# DEPRECATED baseEndpointDnsNames "Use generic-lens or generic-optics with 'baseEndpointDnsNames' instead"  #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdManagesVpcEndpoints :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdManagesVpcEndpoints = Lens.field @"managesVpcEndpoints"
{-# INLINEABLE sdManagesVpcEndpoints #-}
{-# DEPRECATED managesVpcEndpoints "Use generic-lens or generic-optics with 'managesVpcEndpoints' instead"  #-}

-- | The AWS account ID of the service owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwner :: Lens.Lens' ServiceDetail (Core.Maybe Core.Text)
sdOwner = Lens.field @"owner"
{-# INLINEABLE sdOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsName :: Lens.Lens' ServiceDetail (Core.Maybe Core.Text)
sdPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE sdPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
--
-- /Note:/ Consider using 'privateDnsNameVerificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsNameVerificationState :: Lens.Lens' ServiceDetail (Core.Maybe Types.DnsNameState)
sdPrivateDnsNameVerificationState = Lens.field @"privateDnsNameVerificationState"
{-# INLINEABLE sdPrivateDnsNameVerificationState #-}
{-# DEPRECATED privateDnsNameVerificationState "Use generic-lens or generic-optics with 'privateDnsNameVerificationState' instead"  #-}

-- | The private DNS names assigned to the VPC endpoint service. 
--
-- /Note:/ Consider using 'privateDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsNames :: Lens.Lens' ServiceDetail (Core.Maybe [Types.PrivateDnsDetails])
sdPrivateDnsNames = Lens.field @"privateDnsNames"
{-# INLINEABLE sdPrivateDnsNames #-}
{-# DEPRECATED privateDnsNames "Use generic-lens or generic-optics with 'privateDnsNames' instead"  #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceId :: Lens.Lens' ServiceDetail (Core.Maybe Core.Text)
sdServiceId = Lens.field @"serviceId"
{-# INLINEABLE sdServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceName :: Lens.Lens' ServiceDetail (Core.Maybe Core.Text)
sdServiceName = Lens.field @"serviceName"
{-# INLINEABLE sdServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceType :: Lens.Lens' ServiceDetail (Core.Maybe [Types.ServiceTypeDetail])
sdServiceType = Lens.field @"serviceType"
{-# INLINEABLE sdServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTags :: Lens.Lens' ServiceDetail (Core.Maybe [Types.Tag])
sdTags = Lens.field @"tags"
{-# INLINEABLE sdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Indicates whether the service supports endpoint policies.
--
-- /Note:/ Consider using 'vpcEndpointPolicySupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdVpcEndpointPolicySupported :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdVpcEndpointPolicySupported = Lens.field @"vpcEndpointPolicySupported"
{-# INLINEABLE sdVpcEndpointPolicySupported #-}
{-# DEPRECATED vpcEndpointPolicySupported "Use generic-lens or generic-optics with 'vpcEndpointPolicySupported' instead"  #-}

instance Core.FromXML ServiceDetail where
        parseXML x
          = ServiceDetail' Core.<$>
              (x Core..@? "acceptanceRequired") Core.<*>
                x Core..@? "availabilityZoneSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "baseEndpointDnsNameSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "managesVpcEndpoints"
                Core.<*> x Core..@? "owner"
                Core.<*> x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateDnsNameVerificationState"
                Core.<*>
                x Core..@? "privateDnsNameSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "serviceId"
                Core.<*> x Core..@? "serviceName"
                Core.<*>
                x Core..@? "serviceType" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcEndpointPolicySupported"
