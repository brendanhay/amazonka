{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint service configuration to which service consumers (AWS accounts, IAM users, and IAM roles) can connect.
--
-- To create an endpoint service configuration, you must first create one of the following for your service:
--
--     * A <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/introduction.html Network Load Balancer> . Service consumers connect to your service using an interface endpoint.
--
--
--     * A <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/introduction.html Gateway Load Balancer> . Service consumers connect to your service using a Gateway Load Balancer endpoint.
--
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC Endpoint Services> in the /Amazon Virtual Private Cloud User Guide/ . 
-- If you set the private DNS name, you must prove that you own the private DNS domain name. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
    (
    -- * Creating a request
      CreateVpcEndpointServiceConfiguration (..)
    , mkCreateVpcEndpointServiceConfiguration
    -- ** Request lenses
    , cvescAcceptanceRequired
    , cvescClientToken
    , cvescDryRun
    , cvescGatewayLoadBalancerArns
    , cvescNetworkLoadBalancerArns
    , cvescPrivateDnsName
    , cvescTagSpecifications

    -- * Destructuring the response
    , CreateVpcEndpointServiceConfigurationResponse (..)
    , mkCreateVpcEndpointServiceConfigurationResponse
    -- ** Response lenses
    , cvescrrsClientToken
    , cvescrrsServiceConfiguration
    , cvescrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVpcEndpointServiceConfiguration' smart constructor.
data CreateVpcEndpointServiceConfiguration = CreateVpcEndpointServiceConfiguration'
  { acceptanceRequired :: Core.Maybe Core.Bool
    -- ^ Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , gatewayLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
  , networkLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to associate with the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcEndpointServiceConfiguration' value with any optional fields omitted.
mkCreateVpcEndpointServiceConfiguration
    :: CreateVpcEndpointServiceConfiguration
mkCreateVpcEndpointServiceConfiguration
  = CreateVpcEndpointServiceConfiguration'{acceptanceRequired =
                                             Core.Nothing,
                                           clientToken = Core.Nothing, dryRun = Core.Nothing,
                                           gatewayLoadBalancerArns = Core.Nothing,
                                           networkLoadBalancerArns = Core.Nothing,
                                           privateDnsName = Core.Nothing,
                                           tagSpecifications = Core.Nothing}

-- | Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescAcceptanceRequired :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe Core.Bool)
cvescAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# INLINEABLE cvescAcceptanceRequired #-}
{-# DEPRECATED acceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescClientToken :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe Core.Text)
cvescClientToken = Lens.field @"clientToken"
{-# INLINEABLE cvescClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescDryRun :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe Core.Bool)
cvescDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvescDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
--
-- /Note:/ Consider using 'gatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescGatewayLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
cvescGatewayLoadBalancerArns = Lens.field @"gatewayLoadBalancerArns"
{-# INLINEABLE cvescGatewayLoadBalancerArns #-}
{-# DEPRECATED gatewayLoadBalancerArns "Use generic-lens or generic-optics with 'gatewayLoadBalancerArns' instead"  #-}

-- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
--
-- /Note:/ Consider using 'networkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescNetworkLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
cvescNetworkLoadBalancerArns = Lens.field @"networkLoadBalancerArns"
{-# INLINEABLE cvescNetworkLoadBalancerArns #-}
{-# DEPRECATED networkLoadBalancerArns "Use generic-lens or generic-optics with 'networkLoadBalancerArns' instead"  #-}

-- | (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescPrivateDnsName :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe Core.Text)
cvescPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE cvescPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The tags to associate with the service.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescTagSpecifications :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Core.Maybe [Types.TagSpecification])
cvescTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cvescTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateVpcEndpointServiceConfiguration where
        toQuery CreateVpcEndpointServiceConfiguration{..}
          = Core.toQueryPair "Action"
              ("CreateVpcEndpointServiceConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AcceptanceRequired")
                acceptanceRequired
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GatewayLoadBalancerArn")
                gatewayLoadBalancerArns
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkLoadBalancerArn")
                networkLoadBalancerArns
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateDnsName")
                privateDnsName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateVpcEndpointServiceConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpcEndpointServiceConfiguration
         where
        type Rs CreateVpcEndpointServiceConfiguration =
             CreateVpcEndpointServiceConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateVpcEndpointServiceConfigurationResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "serviceConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpcEndpointServiceConfigurationResponse' smart constructor.
data CreateVpcEndpointServiceConfigurationResponse = CreateVpcEndpointServiceConfigurationResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
  , serviceConfiguration :: Core.Maybe Types.ServiceConfiguration
    -- ^ Information about the service configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcEndpointServiceConfigurationResponse' value with any optional fields omitted.
mkCreateVpcEndpointServiceConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcEndpointServiceConfigurationResponse
mkCreateVpcEndpointServiceConfigurationResponse responseStatus
  = CreateVpcEndpointServiceConfigurationResponse'{clientToken =
                                                     Core.Nothing,
                                                   serviceConfiguration = Core.Nothing,
                                                   responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrrsClientToken :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Core.Maybe Core.Text)
cvescrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cvescrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the service configuration.
--
-- /Note:/ Consider using 'serviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrrsServiceConfiguration :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Core.Maybe Types.ServiceConfiguration)
cvescrrsServiceConfiguration = Lens.field @"serviceConfiguration"
{-# INLINEABLE cvescrrsServiceConfiguration #-}
{-# DEPRECATED serviceConfiguration "Use generic-lens or generic-optics with 'serviceConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrrsResponseStatus :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse Core.Int
cvescrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvescrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
