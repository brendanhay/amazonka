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
    sdPrivateDNSNameVerificationState,
    sdVPCEndpointPolicySupported,
    sdBaseEndpointDNSNames,
    sdOwner,
    sdAvailabilityZones,
    sdManagesVPCEndpoints,
    sdServiceName,
    sdServiceType,
    sdAcceptanceRequired,
    sdPrivateDNSNames,
    sdServiceId,
    sdPrivateDNSName,
    sdTags,
  )
where

import Network.AWS.EC2.Types.DNSNameState
import Network.AWS.EC2.Types.PrivateDNSDetails
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC endpoint service.
--
-- /See:/ 'mkServiceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { privateDNSNameVerificationState ::
      Lude.Maybe DNSNameState,
    vpcEndpointPolicySupported :: Lude.Maybe Lude.Bool,
    baseEndpointDNSNames :: Lude.Maybe [Lude.Text],
    owner :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    managesVPCEndpoints :: Lude.Maybe Lude.Bool,
    serviceName :: Lude.Maybe Lude.Text,
    serviceType :: Lude.Maybe [ServiceTypeDetail],
    acceptanceRequired :: Lude.Maybe Lude.Bool,
    privateDNSNames :: Lude.Maybe [PrivateDNSDetails],
    serviceId :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ServiceDetail' with the minimum fields required to make a request.
--
-- * 'acceptanceRequired' - Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
-- * 'availabilityZones' - The Availability Zones in which the service is available.
-- * 'baseEndpointDNSNames' - The DNS names for the service.
-- * 'managesVPCEndpoints' - Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
-- * 'owner' - The AWS account ID of the service owner.
-- * 'privateDNSName' - The private DNS name for the service.
-- * 'privateDNSNameVerificationState' - The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
-- * 'privateDNSNames' - The private DNS names assigned to the VPC endpoint service.
-- * 'serviceId' - The ID of the endpoint service.
-- * 'serviceName' - The Amazon Resource Name (ARN) of the service.
-- * 'serviceType' - The type of service.
-- * 'tags' - Any tags assigned to the service.
-- * 'vpcEndpointPolicySupported' - Indicates whether the service supports endpoint policies.
mkServiceDetail ::
  ServiceDetail
mkServiceDetail =
  ServiceDetail'
    { privateDNSNameVerificationState = Lude.Nothing,
      vpcEndpointPolicySupported = Lude.Nothing,
      baseEndpointDNSNames = Lude.Nothing,
      owner = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      managesVPCEndpoints = Lude.Nothing,
      serviceName = Lude.Nothing,
      serviceType = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      privateDNSNames = Lude.Nothing,
      serviceId = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the state is not @verified@ .
--
-- /Note:/ Consider using 'privateDNSNameVerificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDNSNameVerificationState :: Lens.Lens' ServiceDetail (Lude.Maybe DNSNameState)
sdPrivateDNSNameVerificationState = Lens.lens (privateDNSNameVerificationState :: ServiceDetail -> Lude.Maybe DNSNameState) (\s a -> s {privateDNSNameVerificationState = a} :: ServiceDetail)
{-# DEPRECATED sdPrivateDNSNameVerificationState "Use generic-lens or generic-optics with 'privateDNSNameVerificationState' instead." #-}

-- | Indicates whether the service supports endpoint policies.
--
-- /Note:/ Consider using 'vpcEndpointPolicySupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdVPCEndpointPolicySupported :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Bool)
sdVPCEndpointPolicySupported = Lens.lens (vpcEndpointPolicySupported :: ServiceDetail -> Lude.Maybe Lude.Bool) (\s a -> s {vpcEndpointPolicySupported = a} :: ServiceDetail)
{-# DEPRECATED sdVPCEndpointPolicySupported "Use generic-lens or generic-optics with 'vpcEndpointPolicySupported' instead." #-}

-- | The DNS names for the service.
--
-- /Note:/ Consider using 'baseEndpointDNSNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBaseEndpointDNSNames :: Lens.Lens' ServiceDetail (Lude.Maybe [Lude.Text])
sdBaseEndpointDNSNames = Lens.lens (baseEndpointDNSNames :: ServiceDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {baseEndpointDNSNames = a} :: ServiceDetail)
{-# DEPRECATED sdBaseEndpointDNSNames "Use generic-lens or generic-optics with 'baseEndpointDNSNames' instead." #-}

-- | The AWS account ID of the service owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwner :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Text)
sdOwner = Lens.lens (owner :: ServiceDetail -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: ServiceDetail)
{-# DEPRECATED sdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The Availability Zones in which the service is available.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAvailabilityZones :: Lens.Lens' ServiceDetail (Lude.Maybe [Lude.Text])
sdAvailabilityZones = Lens.lens (availabilityZones :: ServiceDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: ServiceDetail)
{-# DEPRECATED sdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- /Note:/ Consider using 'managesVPCEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdManagesVPCEndpoints :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Bool)
sdManagesVPCEndpoints = Lens.lens (managesVPCEndpoints :: ServiceDetail -> Lude.Maybe Lude.Bool) (\s a -> s {managesVPCEndpoints = a} :: ServiceDetail)
{-# DEPRECATED sdManagesVPCEndpoints "Use generic-lens or generic-optics with 'managesVPCEndpoints' instead." #-}

-- | The Amazon Resource Name (ARN) of the service.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceName :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Text)
sdServiceName = Lens.lens (serviceName :: ServiceDetail -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ServiceDetail)
{-# DEPRECATED sdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceType :: Lens.Lens' ServiceDetail (Lude.Maybe [ServiceTypeDetail])
sdServiceType = Lens.lens (serviceType :: ServiceDetail -> Lude.Maybe [ServiceTypeDetail]) (\s a -> s {serviceType = a} :: ServiceDetail)
{-# DEPRECATED sdServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAcceptanceRequired :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Bool)
sdAcceptanceRequired = Lens.lens (acceptanceRequired :: ServiceDetail -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: ServiceDetail)
{-# DEPRECATED sdAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The private DNS names assigned to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDNSNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDNSNames :: Lens.Lens' ServiceDetail (Lude.Maybe [PrivateDNSDetails])
sdPrivateDNSNames = Lens.lens (privateDNSNames :: ServiceDetail -> Lude.Maybe [PrivateDNSDetails]) (\s a -> s {privateDNSNames = a} :: ServiceDetail)
{-# DEPRECATED sdPrivateDNSNames "Use generic-lens or generic-optics with 'privateDNSNames' instead." #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdServiceId :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Text)
sdServiceId = Lens.lens (serviceId :: ServiceDetail -> Lude.Maybe Lude.Text) (\s a -> s {serviceId = a} :: ServiceDetail)
{-# DEPRECATED sdServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The private DNS name for the service.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrivateDNSName :: Lens.Lens' ServiceDetail (Lude.Maybe Lude.Text)
sdPrivateDNSName = Lens.lens (privateDNSName :: ServiceDetail -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: ServiceDetail)
{-# DEPRECATED sdPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | Any tags assigned to the service.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTags :: Lens.Lens' ServiceDetail (Lude.Maybe [Tag])
sdTags = Lens.lens (tags :: ServiceDetail -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ServiceDetail)
{-# DEPRECATED sdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ServiceDetail where
  parseXML x =
    ServiceDetail'
      Lude.<$> (x Lude..@? "privateDnsNameVerificationState")
      Lude.<*> (x Lude..@? "vpcEndpointPolicySupported")
      Lude.<*> ( x Lude..@? "baseEndpointDnsNameSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "owner")
      Lude.<*> ( x Lude..@? "availabilityZoneSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "managesVpcEndpoints")
      Lude.<*> (x Lude..@? "serviceName")
      Lude.<*> ( x Lude..@? "serviceType" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "acceptanceRequired")
      Lude.<*> ( x Lude..@? "privateDnsNameSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "serviceId")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
