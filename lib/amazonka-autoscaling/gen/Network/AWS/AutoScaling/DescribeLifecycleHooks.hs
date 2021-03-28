{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the lifecycle hooks for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Creating a request
      DescribeLifecycleHooks (..)
    , mkDescribeLifecycleHooks
    -- ** Request lenses
    , dlhAutoScalingGroupName
    , dlhLifecycleHookNames

    -- * Destructuring the response
    , DescribeLifecycleHooksResponse (..)
    , mkDescribeLifecycleHooksResponse
    -- ** Response lenses
    , dlhrrsLifecycleHooks
    , dlhrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLifecycleHooks' smart constructor.
data DescribeLifecycleHooks = DescribeLifecycleHooks'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , lifecycleHookNames :: Core.Maybe [Types.AsciiStringMaxLen255]
    -- ^ The names of one or more lifecycle hooks. If you omit this parameter, all lifecycle hooks are described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHooks' value with any optional fields omitted.
mkDescribeLifecycleHooks
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> DescribeLifecycleHooks
mkDescribeLifecycleHooks autoScalingGroupName
  = DescribeLifecycleHooks'{autoScalingGroupName,
                            lifecycleHookNames = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhAutoScalingGroupName :: Lens.Lens' DescribeLifecycleHooks Types.ResourceName
dlhAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dlhAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The names of one or more lifecycle hooks. If you omit this parameter, all lifecycle hooks are described.
--
-- /Note:/ Consider using 'lifecycleHookNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhLifecycleHookNames :: Lens.Lens' DescribeLifecycleHooks (Core.Maybe [Types.AsciiStringMaxLen255])
dlhLifecycleHookNames = Lens.field @"lifecycleHookNames"
{-# INLINEABLE dlhLifecycleHookNames #-}
{-# DEPRECATED lifecycleHookNames "Use generic-lens or generic-optics with 'lifecycleHookNames' instead"  #-}

instance Core.ToQuery DescribeLifecycleHooks where
        toQuery DescribeLifecycleHooks{..}
          = Core.toQueryPair "Action" ("DescribeLifecycleHooks" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "LifecycleHookNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   lifecycleHookNames)

instance Core.ToHeaders DescribeLifecycleHooks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLifecycleHooks where
        type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse
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
          = Response.receiveXMLWrapper "DescribeLifecycleHooksResult"
              (\ s h x ->
                 DescribeLifecycleHooksResponse' Core.<$>
                   (x Core..@? "LifecycleHooks" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLifecycleHooksResponse' smart constructor.
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
  { lifecycleHooks :: Core.Maybe [Types.LifecycleHook]
    -- ^ The lifecycle hooks for the specified group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHooksResponse' value with any optional fields omitted.
mkDescribeLifecycleHooksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLifecycleHooksResponse
mkDescribeLifecycleHooksResponse responseStatus
  = DescribeLifecycleHooksResponse'{lifecycleHooks = Core.Nothing,
                                    responseStatus}

-- | The lifecycle hooks for the specified group.
--
-- /Note:/ Consider using 'lifecycleHooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhrrsLifecycleHooks :: Lens.Lens' DescribeLifecycleHooksResponse (Core.Maybe [Types.LifecycleHook])
dlhrrsLifecycleHooks = Lens.field @"lifecycleHooks"
{-# INLINEABLE dlhrrsLifecycleHooks #-}
{-# DEPRECATED lifecycleHooks "Use generic-lens or generic-optics with 'lifecycleHooks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhrrsResponseStatus :: Lens.Lens' DescribeLifecycleHooksResponse Core.Int
dlhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
