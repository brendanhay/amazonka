{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of your VPC endpoint service configuration. You can change the Network Load Balancers or Gateway Load Balancers for your service, and you can specify whether acceptance is required for requests to connect to your endpoint service through an interface VPC endpoint.
--
-- If you set or modify the private DNS name, you must prove that you own the private DNS domain name. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
    (
    -- * Creating a request
      ModifyVpcEndpointServiceConfiguration (..)
    , mkModifyVpcEndpointServiceConfiguration
    -- ** Request lenses
    , mvescServiceId
    , mvescAcceptanceRequired
    , mvescAddGatewayLoadBalancerArns
    , mvescAddNetworkLoadBalancerArns
    , mvescDryRun
    , mvescPrivateDnsName
    , mvescRemoveGatewayLoadBalancerArns
    , mvescRemoveNetworkLoadBalancerArns
    , mvescRemovePrivateDnsName

    -- * Destructuring the response
    , ModifyVpcEndpointServiceConfigurationResponse (..)
    , mkModifyVpcEndpointServiceConfigurationResponse
    -- ** Response lenses
    , mvescrrsReturn
    , mvescrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcEndpointServiceConfiguration' smart constructor.
data ModifyVpcEndpointServiceConfiguration = ModifyVpcEndpointServiceConfiguration'
  { serviceId :: Types.VpcEndpointServiceId
    -- ^ The ID of the service.
  , acceptanceRequired :: Core.Maybe Core.Bool
    -- ^ Indicates whether requests to create an endpoint to your service must be accepted.
  , addGatewayLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
  , addNetworkLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
  , removeGatewayLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
  , removeNetworkLoadBalancerArns :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
  , removePrivateDnsName :: Core.Maybe Core.Bool
    -- ^ (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointServiceConfiguration' value with any optional fields omitted.
mkModifyVpcEndpointServiceConfiguration
    :: Types.VpcEndpointServiceId -- ^ 'serviceId'
    -> ModifyVpcEndpointServiceConfiguration
mkModifyVpcEndpointServiceConfiguration serviceId
  = ModifyVpcEndpointServiceConfiguration'{serviceId,
                                           acceptanceRequired = Core.Nothing,
                                           addGatewayLoadBalancerArns = Core.Nothing,
                                           addNetworkLoadBalancerArns = Core.Nothing,
                                           dryRun = Core.Nothing, privateDnsName = Core.Nothing,
                                           removeGatewayLoadBalancerArns = Core.Nothing,
                                           removeNetworkLoadBalancerArns = Core.Nothing,
                                           removePrivateDnsName = Core.Nothing}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescServiceId :: Lens.Lens' ModifyVpcEndpointServiceConfiguration Types.VpcEndpointServiceId
mvescServiceId = Lens.field @"serviceId"
{-# INLINEABLE mvescServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | Indicates whether requests to create an endpoint to your service must be accepted.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAcceptanceRequired :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe Core.Bool)
mvescAcceptanceRequired = Lens.field @"acceptanceRequired"
{-# INLINEABLE mvescAcceptanceRequired #-}
{-# DEPRECATED acceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead"  #-}

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
--
-- /Note:/ Consider using 'addGatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAddGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
mvescAddGatewayLoadBalancerArns = Lens.field @"addGatewayLoadBalancerArns"
{-# INLINEABLE mvescAddGatewayLoadBalancerArns #-}
{-# DEPRECATED addGatewayLoadBalancerArns "Use generic-lens or generic-optics with 'addGatewayLoadBalancerArns' instead"  #-}

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
--
-- /Note:/ Consider using 'addNetworkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAddNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
mvescAddNetworkLoadBalancerArns = Lens.field @"addNetworkLoadBalancerArns"
{-# INLINEABLE mvescAddNetworkLoadBalancerArns #-}
{-# DEPRECATED addNetworkLoadBalancerArns "Use generic-lens or generic-optics with 'addNetworkLoadBalancerArns' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescDryRun :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe Core.Bool)
mvescDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvescDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescPrivateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe Core.Text)
mvescPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE mvescPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
--
-- /Note:/ Consider using 'removeGatewayLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemoveGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
mvescRemoveGatewayLoadBalancerArns = Lens.field @"removeGatewayLoadBalancerArns"
{-# INLINEABLE mvescRemoveGatewayLoadBalancerArns #-}
{-# DEPRECATED removeGatewayLoadBalancerArns "Use generic-lens or generic-optics with 'removeGatewayLoadBalancerArns' instead"  #-}

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
--
-- /Note:/ Consider using 'removeNetworkLoadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemoveNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe [Core.Text])
mvescRemoveNetworkLoadBalancerArns = Lens.field @"removeNetworkLoadBalancerArns"
{-# INLINEABLE mvescRemoveNetworkLoadBalancerArns #-}
{-# DEPRECATED removeNetworkLoadBalancerArns "Use generic-lens or generic-optics with 'removeNetworkLoadBalancerArns' instead"  #-}

-- | (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
--
-- /Note:/ Consider using 'removePrivateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemovePrivateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Core.Maybe Core.Bool)
mvescRemovePrivateDnsName = Lens.field @"removePrivateDnsName"
{-# INLINEABLE mvescRemovePrivateDnsName #-}
{-# DEPRECATED removePrivateDnsName "Use generic-lens or generic-optics with 'removePrivateDnsName' instead"  #-}

instance Core.ToQuery ModifyVpcEndpointServiceConfiguration where
        toQuery ModifyVpcEndpointServiceConfiguration{..}
          = Core.toQueryPair "Action"
              ("ModifyVpcEndpointServiceConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AcceptanceRequired")
                acceptanceRequired
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "AddGatewayLoadBalancerArn")
                addGatewayLoadBalancerArns
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "AddNetworkLoadBalancerArn")
                addNetworkLoadBalancerArns
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateDnsName")
                privateDnsName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "RemoveGatewayLoadBalancerArn")
                removeGatewayLoadBalancerArns
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "RemoveNetworkLoadBalancerArn")
                removeNetworkLoadBalancerArns
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemovePrivateDnsName")
                removePrivateDnsName

instance Core.ToHeaders ModifyVpcEndpointServiceConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcEndpointServiceConfiguration
         where
        type Rs ModifyVpcEndpointServiceConfiguration =
             ModifyVpcEndpointServiceConfigurationResponse
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
                 ModifyVpcEndpointServiceConfigurationResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcEndpointServiceConfigurationResponse' smart constructor.
data ModifyVpcEndpointServiceConfigurationResponse = ModifyVpcEndpointServiceConfigurationResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointServiceConfigurationResponse' value with any optional fields omitted.
mkModifyVpcEndpointServiceConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcEndpointServiceConfigurationResponse
mkModifyVpcEndpointServiceConfigurationResponse responseStatus
  = ModifyVpcEndpointServiceConfigurationResponse'{return =
                                                     Core.Nothing,
                                                   responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescrrsReturn :: Lens.Lens' ModifyVpcEndpointServiceConfigurationResponse (Core.Maybe Core.Bool)
mvescrrsReturn = Lens.field @"return"
{-# INLINEABLE mvescrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescrrsResponseStatus :: Lens.Lens' ModifyVpcEndpointServiceConfigurationResponse Core.Int
mvescrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvescrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
