{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetLoadBalancerPoliciesForBackendServer (..)
    , mkSetLoadBalancerPoliciesForBackendServer
    -- ** Request lenses
    , slbpfbsLoadBalancerName
    , slbpfbsInstancePort
    , slbpfbsPolicyNames

    -- * Destructuring the response
    , SetLoadBalancerPoliciesForBackendServerResponse (..)
    , mkSetLoadBalancerPoliciesForBackendServerResponse
    -- ** Response lenses
    , slbpfbsrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'mkSetLoadBalancerPoliciesForBackendServer' smart constructor.
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , instancePort :: Core.Int
    -- ^ The port number associated with the EC2 instance.
  , policyNames :: [Types.PolicyName]
    -- ^ The names of the policies. If the list is empty, then all current polices are removed from the EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerPoliciesForBackendServer' value with any optional fields omitted.
mkSetLoadBalancerPoliciesForBackendServer
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Core.Int -- ^ 'instancePort'
    -> SetLoadBalancerPoliciesForBackendServer
mkSetLoadBalancerPoliciesForBackendServer loadBalancerName
  instancePort
  = SetLoadBalancerPoliciesForBackendServer'{loadBalancerName,
                                             instancePort, policyNames = Core.mempty}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsLoadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Types.AccessPointName
slbpfbsLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE slbpfbsLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The port number associated with the EC2 instance.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsInstancePort :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Core.Int
slbpfbsInstancePort = Lens.field @"instancePort"
{-# INLINEABLE slbpfbsInstancePort #-}
{-# DEPRECATED instancePort "Use generic-lens or generic-optics with 'instancePort' instead"  #-}

-- | The names of the policies. If the list is empty, then all current polices are removed from the EC2 instance.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsPolicyNames :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer [Types.PolicyName]
slbpfbsPolicyNames = Lens.field @"policyNames"
{-# INLINEABLE slbpfbsPolicyNames #-}
{-# DEPRECATED policyNames "Use generic-lens or generic-optics with 'policyNames' instead"  #-}

instance Core.ToQuery SetLoadBalancerPoliciesForBackendServer where
        toQuery SetLoadBalancerPoliciesForBackendServer{..}
          = Core.toQueryPair "Action"
              ("SetLoadBalancerPoliciesForBackendServer" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<> Core.toQueryPair "InstancePort" instancePort
              Core.<>
              Core.toQueryPair "PolicyNames"
                (Core.toQueryList "member" policyNames)

instance Core.ToHeaders SetLoadBalancerPoliciesForBackendServer
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetLoadBalancerPoliciesForBackendServer
         where
        type Rs SetLoadBalancerPoliciesForBackendServer =
             SetLoadBalancerPoliciesForBackendServerResponse
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
          = Response.receiveXMLWrapper
              "SetLoadBalancerPoliciesForBackendServerResult"
              (\ s h x ->
                 SetLoadBalancerPoliciesForBackendServerResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'mkSetLoadBalancerPoliciesForBackendServerResponse' smart constructor.
newtype SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerPoliciesForBackendServerResponse' value with any optional fields omitted.
mkSetLoadBalancerPoliciesForBackendServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetLoadBalancerPoliciesForBackendServerResponse
mkSetLoadBalancerPoliciesForBackendServerResponse responseStatus
  = SetLoadBalancerPoliciesForBackendServerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpfbsrrsResponseStatus :: Lens.Lens' SetLoadBalancerPoliciesForBackendServerResponse Core.Int
slbpfbsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slbpfbsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
