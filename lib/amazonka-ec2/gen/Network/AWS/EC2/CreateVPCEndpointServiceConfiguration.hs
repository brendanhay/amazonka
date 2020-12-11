{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
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
module Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
  ( -- * Creating a request
    CreateVPCEndpointServiceConfiguration (..),
    mkCreateVPCEndpointServiceConfiguration,

    -- ** Request lenses
    cvescNetworkLoadBalancerARNs,
    cvescClientToken,
    cvescTagSpecifications,
    cvescGatewayLoadBalancerARNs,
    cvescAcceptanceRequired,
    cvescPrivateDNSName,
    cvescDryRun,

    -- * Destructuring the response
    CreateVPCEndpointServiceConfigurationResponse (..),
    mkCreateVPCEndpointServiceConfigurationResponse,

    -- ** Response lenses
    cvescrsClientToken,
    cvescrsServiceConfiguration,
    cvescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVPCEndpointServiceConfiguration' smart constructor.
data CreateVPCEndpointServiceConfiguration = CreateVPCEndpointServiceConfiguration'
  { networkLoadBalancerARNs ::
      Lude.Maybe
        [Lude.Text],
    clientToken ::
      Lude.Maybe
        Lude.Text,
    tagSpecifications ::
      Lude.Maybe
        [TagSpecification],
    gatewayLoadBalancerARNs ::
      Lude.Maybe
        [Lude.Text],
    acceptanceRequired ::
      Lude.Maybe
        Lude.Bool,
    privateDNSName ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCEndpointServiceConfiguration' with the minimum fields required to make a request.
--
-- * 'acceptanceRequired' - Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'gatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
-- * 'networkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
-- * 'privateDNSName' - (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
-- * 'tagSpecifications' - The tags to associate with the service.
mkCreateVPCEndpointServiceConfiguration ::
  CreateVPCEndpointServiceConfiguration
mkCreateVPCEndpointServiceConfiguration =
  CreateVPCEndpointServiceConfiguration'
    { networkLoadBalancerARNs =
        Lude.Nothing,
      clientToken = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      gatewayLoadBalancerARNs = Lude.Nothing,
      acceptanceRequired = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
--
-- /Note:/ Consider using 'networkLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescNetworkLoadBalancerARNs :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
cvescNetworkLoadBalancerARNs = Lens.lens (networkLoadBalancerARNs :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {networkLoadBalancerARNs = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescNetworkLoadBalancerARNs "Use generic-lens or generic-optics with 'networkLoadBalancerARNs' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescClientToken :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe Lude.Text)
cvescClientToken = Lens.lens (clientToken :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The tags to associate with the service.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescTagSpecifications :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe [TagSpecification])
cvescTagSpecifications = Lens.lens (tagSpecifications :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
--
-- /Note:/ Consider using 'gatewayLoadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescGatewayLoadBalancerARNs :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe [Lude.Text])
cvescGatewayLoadBalancerARNs = Lens.lens (gatewayLoadBalancerARNs :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {gatewayLoadBalancerARNs = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescGatewayLoadBalancerARNs "Use generic-lens or generic-optics with 'gatewayLoadBalancerARNs' instead." #-}

-- | Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
--
-- /Note:/ Consider using 'acceptanceRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescAcceptanceRequired :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe Lude.Bool)
cvescAcceptanceRequired = Lens.lens (acceptanceRequired :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {acceptanceRequired = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescAcceptanceRequired "Use generic-lens or generic-optics with 'acceptanceRequired' instead." #-}

-- | (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescPrivateDNSName :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe Lude.Text)
cvescPrivateDNSName = Lens.lens (privateDNSName :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescDryRun :: Lens.Lens' CreateVPCEndpointServiceConfiguration (Lude.Maybe Lude.Bool)
cvescDryRun = Lens.lens (dryRun :: CreateVPCEndpointServiceConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPCEndpointServiceConfiguration)
{-# DEPRECATED cvescDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateVPCEndpointServiceConfiguration where
  type
    Rs CreateVPCEndpointServiceConfiguration =
      CreateVPCEndpointServiceConfigurationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPCEndpointServiceConfigurationResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "serviceConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCEndpointServiceConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPCEndpointServiceConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCEndpointServiceConfiguration where
  toQuery CreateVPCEndpointServiceConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateVpcEndpointServiceConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "NetworkLoadBalancerArn"
              Lude.<$> networkLoadBalancerARNs
          ),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        Lude.toQuery
          ( Lude.toQueryList "GatewayLoadBalancerArn"
              Lude.<$> gatewayLoadBalancerARNs
          ),
        "AcceptanceRequired" Lude.=: acceptanceRequired,
        "PrivateDnsName" Lude.=: privateDNSName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateVPCEndpointServiceConfigurationResponse' smart constructor.
data CreateVPCEndpointServiceConfigurationResponse = CreateVPCEndpointServiceConfigurationResponse'
  { clientToken ::
      Lude.Maybe
        Lude.Text,
    serviceConfiguration ::
      Lude.Maybe
        ServiceConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateVPCEndpointServiceConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
-- * 'responseStatus' - The response status code.
-- * 'serviceConfiguration' - Information about the service configuration.
mkCreateVPCEndpointServiceConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCEndpointServiceConfigurationResponse
mkCreateVPCEndpointServiceConfigurationResponse pResponseStatus_ =
  CreateVPCEndpointServiceConfigurationResponse'
    { clientToken =
        Lude.Nothing,
      serviceConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrsClientToken :: Lens.Lens' CreateVPCEndpointServiceConfigurationResponse (Lude.Maybe Lude.Text)
cvescrsClientToken = Lens.lens (clientToken :: CreateVPCEndpointServiceConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateVPCEndpointServiceConfigurationResponse)
{-# DEPRECATED cvescrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the service configuration.
--
-- /Note:/ Consider using 'serviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrsServiceConfiguration :: Lens.Lens' CreateVPCEndpointServiceConfigurationResponse (Lude.Maybe ServiceConfiguration)
cvescrsServiceConfiguration = Lens.lens (serviceConfiguration :: CreateVPCEndpointServiceConfigurationResponse -> Lude.Maybe ServiceConfiguration) (\s a -> s {serviceConfiguration = a} :: CreateVPCEndpointServiceConfigurationResponse)
{-# DEPRECATED cvescrsServiceConfiguration "Use generic-lens or generic-optics with 'serviceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvescrsResponseStatus :: Lens.Lens' CreateVPCEndpointServiceConfigurationResponse Lude.Int
cvescrsResponseStatus = Lens.lens (responseStatus :: CreateVPCEndpointServiceConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCEndpointServiceConfigurationResponse)
{-# DEPRECATED cvescrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
