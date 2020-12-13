{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the set of policies associated with the specified port on which the EC2 instance is listening with a new set of policies. At this time, only the back-end server authentication policy type can be applied to the instance ports; this policy type is composed of multiple public key policies.
--
-- Each time you use @SetLoadBalancerPoliciesForBackendServer@ to enable the policies, use the @PolicyNames@ parameter to list the policies that you want to enable.
-- You can use 'DescribeLoadBalancers' or 'DescribeLoadBalancerPolicies' to verify that the policy is associated with the EC2 instance.
-- For more information about enabling back-end instance authentication, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-create-https-ssl-load-balancer.html#configure_backendauth_clt Configure Back-end Instance Authentication> in the /Classic Load Balancers Guide/ . For more information about Proxy Protocol, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-proxy-protocol.html Configure Proxy Protocol Support> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
  ( -- * Creating a request
    SetLoadBalancerPoliciesForBackendServer (..),
    mkSetLoadBalancerPoliciesForBackendServer,

    -- ** Request lenses
    slbpfbsPolicyNames,
    slbpfbsLoadBalancerName,
    slbpfbsInstancePort,

    -- * Destructuring the response
    SetLoadBalancerPoliciesForBackendServerResponse (..),
    mkSetLoadBalancerPoliciesForBackendServerResponse,

    -- ** Response lenses
    slbpfbsrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'mkSetLoadBalancerPoliciesForBackendServer' smart constructor.
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer'
  { -- | The names of the policies. If the list is empty, then all current polices are removed from the EC2 instance.
    policyNames :: [Lude.Text],
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The port number associated with the EC2 instance.
    instancePort :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBalancerPoliciesForBackendServer' with the minimum fields required to make a request.
--
-- * 'policyNames' - The names of the policies. If the list is empty, then all current polices are removed from the EC2 instance.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'instancePort' - The port number associated with the EC2 instance.
mkSetLoadBalancerPoliciesForBackendServer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'instancePort'
  Lude.Int ->
  SetLoadBalancerPoliciesForBackendServer
mkSetLoadBalancerPoliciesForBackendServer
  pLoadBalancerName_
  pInstancePort_ =
    SetLoadBalancerPoliciesForBackendServer'
      { policyNames =
          Lude.mempty,
        loadBalancerName = pLoadBalancerName_,
        instancePort = pInstancePort_
      }

-- | The names of the policies. If the list is empty, then all current polices are removed from the EC2 instance.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsPolicyNames :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer [Lude.Text]
slbpfbsPolicyNames = Lens.lens (policyNames :: SetLoadBalancerPoliciesForBackendServer -> [Lude.Text]) (\s a -> s {policyNames = a} :: SetLoadBalancerPoliciesForBackendServer)
{-# DEPRECATED slbpfbsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsLoadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Lude.Text
slbpfbsLoadBalancerName = Lens.lens (loadBalancerName :: SetLoadBalancerPoliciesForBackendServer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: SetLoadBalancerPoliciesForBackendServer)
{-# DEPRECATED slbpfbsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The port number associated with the EC2 instance.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsInstancePort :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Lude.Int
slbpfbsInstancePort = Lens.lens (instancePort :: SetLoadBalancerPoliciesForBackendServer -> Lude.Int) (\s a -> s {instancePort = a} :: SetLoadBalancerPoliciesForBackendServer)
{-# DEPRECATED slbpfbsInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

instance Lude.AWSRequest SetLoadBalancerPoliciesForBackendServer where
  type
    Rs SetLoadBalancerPoliciesForBackendServer =
      SetLoadBalancerPoliciesForBackendServerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "SetLoadBalancerPoliciesForBackendServerResult"
      ( \s h x ->
          SetLoadBalancerPoliciesForBackendServerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetLoadBalancerPoliciesForBackendServer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetLoadBalancerPoliciesForBackendServer where
  toPath = Lude.const "/"

instance Lude.ToQuery SetLoadBalancerPoliciesForBackendServer where
  toQuery SetLoadBalancerPoliciesForBackendServer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetLoadBalancerPoliciesForBackendServer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyNames" Lude.=: Lude.toQueryList "member" policyNames,
        "LoadBalancerName" Lude.=: loadBalancerName,
        "InstancePort" Lude.=: instancePort
      ]

-- | Contains the output of SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'mkSetLoadBalancerPoliciesForBackendServerResponse' smart constructor.
newtype SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBalancerPoliciesForBackendServerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetLoadBalancerPoliciesForBackendServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetLoadBalancerPoliciesForBackendServerResponse
mkSetLoadBalancerPoliciesForBackendServerResponse pResponseStatus_ =
  SetLoadBalancerPoliciesForBackendServerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsrsResponseStatus :: Lens.Lens' SetLoadBalancerPoliciesForBackendServerResponse Lude.Int
slbpfbsrsResponseStatus = Lens.lens (responseStatus :: SetLoadBalancerPoliciesForBackendServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetLoadBalancerPoliciesForBackendServerResponse)
{-# DEPRECATED slbpfbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
