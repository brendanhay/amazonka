{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetIpAddressType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.
module Network.AWS.ELBv2.SetIpAddressType
    (
    -- * Creating a request
      SetIpAddressType (..)
    , mkSetIpAddressType
    -- ** Request lenses
    , siatLoadBalancerArn
    , siatIpAddressType

    -- * Destructuring the response
    , SetIpAddressTypeResponse (..)
    , mkSetIpAddressTypeResponse
    -- ** Response lenses
    , siatrrsIpAddressType
    , siatrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetIpAddressType' smart constructor.
data SetIpAddressType = SetIpAddressType'
  { loadBalancerArn :: Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , ipAddressType :: Types.IpAddressType
    -- ^ The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ . You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIpAddressType' value with any optional fields omitted.
mkSetIpAddressType
    :: Types.LoadBalancerArn -- ^ 'loadBalancerArn'
    -> Types.IpAddressType -- ^ 'ipAddressType'
    -> SetIpAddressType
mkSetIpAddressType loadBalancerArn ipAddressType
  = SetIpAddressType'{loadBalancerArn, ipAddressType}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatLoadBalancerArn :: Lens.Lens' SetIpAddressType Types.LoadBalancerArn
siatLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE siatLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ . You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatIpAddressType :: Lens.Lens' SetIpAddressType Types.IpAddressType
siatIpAddressType = Lens.field @"ipAddressType"
{-# INLINEABLE siatIpAddressType #-}
{-# DEPRECATED ipAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead"  #-}

instance Core.ToQuery SetIpAddressType where
        toQuery SetIpAddressType{..}
          = Core.toQueryPair "Action" ("SetIpAddressType" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerArn" loadBalancerArn
              Core.<> Core.toQueryPair "IpAddressType" ipAddressType

instance Core.ToHeaders SetIpAddressType where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetIpAddressType where
        type Rs SetIpAddressType = SetIpAddressTypeResponse
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
          = Response.receiveXMLWrapper "SetIpAddressTypeResult"
              (\ s h x ->
                 SetIpAddressTypeResponse' Core.<$>
                   (x Core..@? "IpAddressType") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetIpAddressTypeResponse' smart constructor.
data SetIpAddressTypeResponse = SetIpAddressTypeResponse'
  { ipAddressType :: Core.Maybe Types.IpAddressType
    -- ^ The IP address type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIpAddressTypeResponse' value with any optional fields omitted.
mkSetIpAddressTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetIpAddressTypeResponse
mkSetIpAddressTypeResponse responseStatus
  = SetIpAddressTypeResponse'{ipAddressType = Core.Nothing,
                              responseStatus}

-- | The IP address type.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatrrsIpAddressType :: Lens.Lens' SetIpAddressTypeResponse (Core.Maybe Types.IpAddressType)
siatrrsIpAddressType = Lens.field @"ipAddressType"
{-# INLINEABLE siatrrsIpAddressType #-}
{-# DEPRECATED ipAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatrrsResponseStatus :: Lens.Lens' SetIpAddressTypeResponse Core.Int
siatrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE siatrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
