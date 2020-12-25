{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceDetail
  ( ServiceDetail (..),

    -- * Smart constructor
    mkServiceDetail,

    -- * Lenses
    sdAcceptanceRequired,
    sdAvailabilityZones,
    sdBaseEndpointDnsNames,
    sdManagesVpcEndpoints,
    sdOwner,
    sdPrivateDnsName,
    sdPrivateDnsNameVerificationState,
    sdPrivateDnsNames,
    sdServiceId,
    sdServiceName,
    sdServiceType,
    sdTags,
    sdVpcEndpointPolicySupported,
  )
where

import qualified Network.AWS.EC2.Types.DnsNameState as Types
import qualified Network.AWS.EC2.Types.PrivateDnsDetails as Types
import qualified Network.AWS.EC2.Types.ServiceTypeDetail as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC endpoint service.
--
-- /See:/ 'mkServiceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { -- | Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
    acceptanceRequired :: Core.Maybe Core.Bool,
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Core.Maybe [Types.String],
    -- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Core.Maybe Core.Bool,
    -- | The AWS account ID of the service owner.
    owner :: Core.Maybe Types.String,
    -- | The private DNS name for the service.
    privateDnsName :: Core.Maybe Types.String,
    -- | The verification state of the VPC endpoint service.
    --
    -- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
    privateDnsNameVerificationState :: Core.Maybe Types.DnsNameState,
    -- | The private DNS names assigned to the VPC endpoint service.
    privateDnsNames :: Core.Maybe [Types.PrivateDnsDetails],
    -- | The ID of the endpoint service.
    serviceId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the service.
    serviceName :: Core.Maybe Types.String,
    -- | The type of service.
    serviceType :: Core.Maybe [Types.ServiceTypeDetail],
    -- | Any tags assigned to the service.
    tags :: Core.Maybe [Types.Tag],
    -- | Indicates whether the service supports endpoint policies.
    vpcEndpointPolicySupported :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceDetail' value with any optional fields omitted.
mkServiceDetail ::
  ServiceDetail
mkServiceDetail =
  ServiceDetail'
    { acceptanceRequired = Core.Nothing,
      availabilityZones = Core.Nothing,
      baseEndpointDnsNames = Core.Nothing,
      managesVpcEndpoints = Core.Nothing,
      owner = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateDnsNameVerificationState = Core.Nothing,
      privateDnsNames = Core.Nothing,
      serviceId = Core.Nothing,
      serviceName = Core.Nothing,
      serviceType = Core.Nothing,
      tags = Core.Nothing,
      vpcEndpointPolicySupported = Core.Nothing
    }

-- | Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAcceptanceRequired :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# DEPRECATED sdAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAvailabilityZones :: Lens.Lens' ServiceDetail (Core.Maybe [Types.String])
sdAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED sdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBaseEndpointDnsNames :: Lens.Lens' ServiceDetail (Core.Maybe [Types.String])
sdBaseEndpointDnsNames = Lens.field @"baseEndpointDnsNames"
{-# DEPRECATED sdBaseEndpointDnsNames "Use generic-lens or generic-optics with 'baseEndpointDnsNames' instead." #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdManagesVpcEndpoints :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdManagesVpcEndpoints = Lens.field @"managesVpcEndpoints"
{-# DEPRECATED sdManagesVpcEndpoints "Use generic-lens or generic-optics with 'managesVpcEndpoints' instead." #-}

-- | The AWS account ID of the service owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwner :: Lens.Lens' ServiceDetail (Core.Maybe Types.String)
sdOwner = Lens.field @"owner"
{-# DEPRECATED sdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsName :: Lens.Lens' ServiceDetail (Core.Maybe Types.String)
sdPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED sdPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
--
-- /Note:/ Consider using 'privateDnsNameVerificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsNameVerificationState :: Lens.Lens' ServiceDetail (Core.Maybe Types.DnsNameState)
sdPrivateDnsNameVerificationState = Lens.field @"privateDnsNameVerificationState"
{-# DEPRECATED sdPrivateDnsNameVerificationState "Use generic-lens or generic-optics with 'privateDnsNameVerificationState' instead." #-}

-- | The private DNS names assigned to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDnsNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDnsNames :: Lens.Lens' ServiceDetail (Core.Maybe [Types.PrivateDnsDetails])
sdPrivateDnsNames = Lens.field @"privateDnsNames"
{-# DEPRECATED sdPrivateDnsNames "Use generic-lens or generic-optics with 'privateDnsNames' instead." #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceId :: Lens.Lens' ServiceDetail (Core.Maybe Types.String)
sdServiceId = Lens.field @"serviceId"
{-# DEPRECATED sdServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceName :: Lens.Lens' ServiceDetail (Core.Maybe Types.String)
sdServiceName = Lens.field @"serviceName"
{-# DEPRECATED sdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceType :: Lens.Lens' ServiceDetail (Core.Maybe [Types.ServiceTypeDetail])
sdServiceType = Lens.field @"serviceType"
{-# DEPRECATED sdServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTags :: Lens.Lens' ServiceDetail (Core.Maybe [Types.Tag])
sdTags = Lens.field @"tags"
{-# DEPRECATED sdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the service supports endpoint policies.
--
-- /Note:/ Consider using 'vpcEndpointPolicySupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdVpcEndpointPolicySupported :: Lens.Lens' ServiceDetail (Core.Maybe Core.Bool)
sdVpcEndpointPolicySupported = Lens.field @"vpcEndpointPolicySupported"
{-# DEPRECATED sdVpcEndpointPolicySupported "Use generic-lens or generic-optics with 'vpcEndpointPolicySupported' instead." #-}

instance Core.FromXML ServiceDetail where
  parseXML x =
    ServiceDetail'
      Core.<$> (x Core..@? "acceptanceRequired")
      Core.<*> ( x Core..@? "availabilityZoneSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "baseEndpointDnsNameSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "managesVpcEndpoints")
      Core.<*> (x Core..@? "owner")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateDnsNameVerificationState")
      Core.<*> (x Core..@? "privateDnsNameSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "serviceId")
      Core.<*> (x Core..@? "serviceName")
      Core.<*> (x Core..@? "serviceType" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "vpcEndpointPolicySupported")
