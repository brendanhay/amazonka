{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeOperatingSystems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the operating systems that are supported by AWS OpsWorks Stacks.
module Network.AWS.OpsWorks.DescribeOperatingSystems
    (
    -- * Creating a request
      DescribeOperatingSystems (..)
    , mkDescribeOperatingSystems

    -- * Destructuring the response
    , DescribeOperatingSystemsResponse (..)
    , mkDescribeOperatingSystemsResponse
    -- ** Response lenses
    , dosrrsOperatingSystems
    , dosrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOperatingSystems' smart constructor.
data DescribeOperatingSystems = DescribeOperatingSystems'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOperatingSystems' value with any optional fields omitted.
mkDescribeOperatingSystems
    :: DescribeOperatingSystems
mkDescribeOperatingSystems = DescribeOperatingSystems'

instance Core.ToQuery DescribeOperatingSystems where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOperatingSystems where
        toHeaders DescribeOperatingSystems{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DescribeOperatingSystems")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOperatingSystems where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeOperatingSystems where
        type Rs DescribeOperatingSystems = DescribeOperatingSystemsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOperatingSystemsResponse' Core.<$>
                   (x Core..:? "OperatingSystems") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a @DescribeOperatingSystems@ request.
--
-- /See:/ 'mkDescribeOperatingSystemsResponse' smart constructor.
data DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse'
  { operatingSystems :: Core.Maybe [Types.OperatingSystem]
    -- ^ Contains information in response to a @DescribeOperatingSystems@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOperatingSystemsResponse' value with any optional fields omitted.
mkDescribeOperatingSystemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOperatingSystemsResponse
mkDescribeOperatingSystemsResponse responseStatus
  = DescribeOperatingSystemsResponse'{operatingSystems =
                                        Core.Nothing,
                                      responseStatus}

-- | Contains information in response to a @DescribeOperatingSystems@ request.
--
-- /Note:/ Consider using 'operatingSystems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dosrrsOperatingSystems :: Lens.Lens' DescribeOperatingSystemsResponse (Core.Maybe [Types.OperatingSystem])
dosrrsOperatingSystems = Lens.field @"operatingSystems"
{-# INLINEABLE dosrrsOperatingSystems #-}
{-# DEPRECATED operatingSystems "Use generic-lens or generic-optics with 'operatingSystems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dosrrsResponseStatus :: Lens.Lens' DescribeOperatingSystemsResponse Core.Int
dosrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dosrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
