{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of the specified instances with respect to the specified load balancer. If no instances are specified, the call describes the state of all instances that are currently registered with the load balancer. If instances are specified, their state is returned even if they are no longer registered with the load balancer. The state of terminated instances is not returned.
module Network.AWS.ELB.DescribeInstanceHealth
    (
    -- * Creating a request
      DescribeInstanceHealth (..)
    , mkDescribeInstanceHealth
    -- ** Request lenses
    , dihLoadBalancerName
    , dihInstances

    -- * Destructuring the response
    , DescribeInstanceHealthResponse (..)
    , mkDescribeInstanceHealthResponse
    -- ** Response lenses
    , dihrrsInstanceStates
    , dihrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealth' smart constructor.
data DescribeInstanceHealth = DescribeInstanceHealth'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , instances :: Core.Maybe [Types.Instance]
    -- ^ The IDs of the instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceHealth' value with any optional fields omitted.
mkDescribeInstanceHealth
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> DescribeInstanceHealth
mkDescribeInstanceHealth loadBalancerName
  = DescribeInstanceHealth'{loadBalancerName,
                            instances = Core.Nothing}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihLoadBalancerName :: Lens.Lens' DescribeInstanceHealth Types.AccessPointName
dihLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE dihLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihInstances :: Lens.Lens' DescribeInstanceHealth (Core.Maybe [Types.Instance])
dihInstances = Lens.field @"instances"
{-# INLINEABLE dihInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

instance Core.ToQuery DescribeInstanceHealth where
        toQuery DescribeInstanceHealth{..}
          = Core.toQueryPair "Action" ("DescribeInstanceHealth" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<>
              Core.toQueryPair "Instances"
                (Core.maybe Core.mempty (Core.toQueryList "member") instances)

instance Core.ToHeaders DescribeInstanceHealth where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeInstanceHealth where
        type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse
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
          = Response.receiveXMLWrapper "DescribeInstanceHealthResult"
              (\ s h x ->
                 DescribeInstanceHealthResponse' Core.<$>
                   (x Core..@? "InstanceStates" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealthResponse' smart constructor.
data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'
  { instanceStates :: Core.Maybe [Types.InstanceState]
    -- ^ Information about the health of the instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceHealthResponse' value with any optional fields omitted.
mkDescribeInstanceHealthResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstanceHealthResponse
mkDescribeInstanceHealthResponse responseStatus
  = DescribeInstanceHealthResponse'{instanceStates = Core.Nothing,
                                    responseStatus}

-- | Information about the health of the instances.
--
-- /Note:/ Consider using 'instanceStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsInstanceStates :: Lens.Lens' DescribeInstanceHealthResponse (Core.Maybe [Types.InstanceState])
dihrrsInstanceStates = Lens.field @"instanceStates"
{-# INLINEABLE dihrrsInstanceStates #-}
{-# DEPRECATED instanceStates "Use generic-lens or generic-optics with 'instanceStates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsResponseStatus :: Lens.Lens' DescribeInstanceHealthResponse Core.Int
dihrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dihrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
