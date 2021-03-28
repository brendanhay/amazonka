{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Creating a request
      DescribeLifecycleHookTypes (..)
    , mkDescribeLifecycleHookTypes

    -- * Destructuring the response
    , DescribeLifecycleHookTypesResponse (..)
    , mkDescribeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtrrsLifecycleHookTypes
    , dlhtrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHookTypes' value with any optional fields omitted.
mkDescribeLifecycleHookTypes
    :: DescribeLifecycleHookTypes
mkDescribeLifecycleHookTypes = DescribeLifecycleHookTypes'

instance Core.ToQuery DescribeLifecycleHookTypes where
        toQuery DescribeLifecycleHookTypes{..}
          = Core.toQueryPair "Action"
              ("DescribeLifecycleHookTypes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)

instance Core.ToHeaders DescribeLifecycleHookTypes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLifecycleHookTypes where
        type Rs DescribeLifecycleHookTypes =
             DescribeLifecycleHookTypesResponse
        toRequest x@_
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
          = Response.receiveXMLWrapper "DescribeLifecycleHookTypesResult"
              (\ s h x ->
                 DescribeLifecycleHookTypesResponse' Core.<$>
                   (x Core..@? "LifecycleHookTypes" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { lifecycleHookTypes :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ The lifecycle hook types.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLifecycleHookTypesResponse' value with any optional fields omitted.
mkDescribeLifecycleHookTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLifecycleHookTypesResponse
mkDescribeLifecycleHookTypesResponse responseStatus
  = DescribeLifecycleHookTypesResponse'{lifecycleHookTypes =
                                          Core.Nothing,
                                        responseStatus}

-- | The lifecycle hook types.
--
-- /Note:/ Consider using 'lifecycleHookTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrrsLifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Core.Maybe [Types.XmlStringMaxLen255])
dlhtrrsLifecycleHookTypes = Lens.field @"lifecycleHookTypes"
{-# INLINEABLE dlhtrrsLifecycleHookTypes #-}
{-# DEPRECATED lifecycleHookTypes "Use generic-lens or generic-optics with 'lifecycleHookTypes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrrsResponseStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Core.Int
dlhtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlhtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
