{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of your VPC endpoint service configuration. You can change the Network Load Balancers or Gateway Load Balancers for your service, and you can specify whether acceptance is required for requests to connect to your endpoint service through an interface VPC endpoint.
--
-- If you set or modify the private DNS name, you must prove that you own the private DNS domain name. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration
  ( -- * Creating a request
    ModifyVPCEndpointServiceConfiguration (..),
    mkModifyVPCEndpointServiceConfiguration,

    -- ** Request lenses
    mvescRemoveGatewayLoadBalancerARNs,
    mvescRemovePrivateDNSName,
    mvescAddGatewayLoadBalancerARNs,
    mvescRemoveNetworkLoadBalancerARNs,
    mvescAcceptanceRequired,
    mvescServiceId,
    mvescAddNetworkLoadBalancerARNs,
    mvescPrivateDNSName,
    mvescDryRun,

    -- * Destructuring the response
    ModifyVPCEndpointServiceConfigurationResponse (..),
    mkModifyVPCEndpointServiceConfigurationResponse,

    -- ** Response lenses
    mvescrsReturn,
    mvescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCEndpointServiceConfiguration' smart constructor.
data ModifyVPCEndpointServiceConfiguration = ModifyVPCEndpointServiceConfiguration'
  { -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
    removeGatewayLoadBalancerARNs :: Lude.Maybe [Lude.Text],
    -- | (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
    removePrivateDNSName :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
    addGatewayLoadBalancerARNs :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
    removeNetworkLoadBalancerARNs :: Lude.Maybe [Lude.Text],
    -- | Indicates whether requests to create an endpoint to your service must be accepted.
    acceptanceRequired :: Lude.Maybe Lude.Bool,
    -- | The ID of the service.
    serviceId :: Lude.Text,
    -- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
    addNetworkLoadBalancerARNs :: Lude.Maybe [Lude.Text],
    -- | (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
    privateDNSName :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointServiceConfiguration' with the minimum fields required to make a request.
--
-- * 'removeGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
-- * 'removePrivateDNSName' - (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
-- * 'addGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
-- * 'removeNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
-- * 'acceptanceRequired' - Indicates whether requests to create an endpoint to your service must be accepted.
-- * 'serviceId' - The ID of the service.
-- * 'addNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
-- * 'privateDNSName' - (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPCEndpointServiceConfiguration ::
  -- | 'serviceId'
  Lude.Text ->
  ModifyVPCEndpointServiceConfiguration
mkModifyVPCEndpointServiceConfiguration pServiceId_ =
  ModifyVPCEndpointServiceConfiguration'
    { removeGatewayLoadBalancerARNs =
        Lude.Nothing,
      removePrivateDNSName = Lude.Nothing,
      addGatewayLoadBalancerARNs = Lude.Nothing,
      removeNetworkLoadBalancerARNs = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      serviceId = pServiceId_,
      addNetworkLoadBalancerARNs = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
--
-- /Note:/ Consider using 'removeGatewayLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemoveGatewayLoadBalancerARNs :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
mvescRemoveGatewayLoadBalancerARNs = Lens.lens (removeGatewayLoadBalancerARNs :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {removeGatewayLoadBalancerARNs = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescRemoveGatewayLoadBalancerARNs "Use generic-lens or generic-optics with 'removeGatewayLoadBalancerARNs' instead." #-}

-- | (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
--
-- /Note:/ Consider using 'removePrivateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemovePrivateDNSName :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe Lude.Bool)
mvescRemovePrivateDNSName = Lens.lens (removePrivateDNSName :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {removePrivateDNSName = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescRemovePrivateDNSName "Use generic-lens or generic-optics with 'removePrivateDNSName' instead." #-}

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
--
-- /Note:/ Consider using 'addGatewayLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAddGatewayLoadBalancerARNs :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
mvescAddGatewayLoadBalancerARNs = Lens.lens (addGatewayLoadBalancerARNs :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {addGatewayLoadBalancerARNs = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescAddGatewayLoadBalancerARNs "Use generic-lens or generic-optics with 'addGatewayLoadBalancerARNs' instead." #-}

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
--
-- /Note:/ Consider using 'removeNetworkLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescRemoveNetworkLoadBalancerARNs :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
mvescRemoveNetworkLoadBalancerARNs = Lens.lens (removeNetworkLoadBalancerARNs :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {removeNetworkLoadBalancerARNs = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescRemoveNetworkLoadBalancerARNs "Use generic-lens or generic-optics with 'removeNetworkLoadBalancerARNs' instead." #-}

-- | Indicates whether requests to create an endpoint to your service must be accepted.
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAcceptanceRequired :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe Lude.Bool)
mvescAcceptanceRequired = Lens.lens (acceptanceRequired :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescServiceId :: Lens.Lens' ModifyVPCEndpointServiceConfiguration Lude.Text
mvescServiceId = Lens.lens (serviceId :: ModifyVPCEndpointServiceConfiguration -> Lude.Text) (\s a -> s {serviceId = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
--
-- /Note:/ Consider using 'addNetworkLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescAddNetworkLoadBalancerARNs :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
mvescAddNetworkLoadBalancerARNs = Lens.lens (addNetworkLoadBalancerARNs :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {addNetworkLoadBalancerARNs = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescAddNetworkLoadBalancerARNs "Use generic-lens or generic-optics with 'addNetworkLoadBalancerARNs' instead." #-}

-- | (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescPrivateDNSName :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe Lude.Text)
mvescPrivateDNSName = Lens.lens (privateDNSName :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescDryRun :: Lens.Lens' ModifyVPCEndpointServiceConfiguration (Lude.Maybe Lude.Bool)
mvescDryRun = Lens.lens (dryRun :: ModifyVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCEndpointServiceConfiguration)
{-# DEPRECATED mvescDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPCEndpointServiceConfiguration where
  type
    Rs ModifyVPCEndpointServiceConfiguration =
      ModifyVPCEndpointServiceConfigurationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCEndpointServiceConfigurationResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCEndpointServiceConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCEndpointServiceConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCEndpointServiceConfiguration where
  toQuery ModifyVPCEndpointServiceConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpcEndpointServiceConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "RemoveGatewayLoadBalancerArn"
              Lude.<$> removeGatewayLoadBalancerARNs
          ),
        "RemovePrivateDnsName" Lude.=: removePrivateDNSName,
        Lude.toQuery
          ( Lude.toQueryList "AddGatewayLoadBalancerArn"
              Lude.<$> addGatewayLoadBalancerARNs
          ),
        Lude.toQuery
          ( Lude.toQueryList "RemoveNetworkLoadBalancerArn"
              Lude.<$> removeNetworkLoadBalancerARNs
          ),
        "AcceptanceRequired" Lude.=: acceptanceRequired,
        "ServiceId" Lude.=: serviceId,
        Lude.toQuery
          ( Lude.toQueryList "AddNetworkLoadBalancerArn"
              Lude.<$> addNetworkLoadBalancerARNs
          ),
        "PrivateDnsName" Lude.=: privateDNSName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPCEndpointServiceConfigurationResponse' smart constructor.
data ModifyVPCEndpointServiceConfigurationResponse = ModifyVPCEndpointServiceConfigurationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointServiceConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkModifyVPCEndpointServiceConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCEndpointServiceConfigurationResponse
mkModifyVPCEndpointServiceConfigurationResponse pResponseStatus_ =
  ModifyVPCEndpointServiceConfigurationResponse'
    { return =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescrsReturn :: Lens.Lens' ModifyVPCEndpointServiceConfigurationResponse (Lude.Maybe Lude.Bool)
mvescrsReturn = Lens.lens (return :: ModifyVPCEndpointServiceConfigurationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyVPCEndpointServiceConfigurationResponse)
{-# DEPRECATED mvescrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvescrsResponseStatus :: Lens.Lens' ModifyVPCEndpointServiceConfigurationResponse Lude.Int
mvescrsResponseStatus = Lens.lens (responseStatus :: ModifyVPCEndpointServiceConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCEndpointServiceConfigurationResponse)
{-# DEPRECATED mvescrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
