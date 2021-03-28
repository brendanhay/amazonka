{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified routing profile.
module Network.AWS.Connect.DescribeRoutingProfile
    (
    -- * Creating a request
      DescribeRoutingProfile (..)
    , mkDescribeRoutingProfile
    -- ** Request lenses
    , drpInstanceId
    , drpRoutingProfileId

    -- * Destructuring the response
    , DescribeRoutingProfileResponse (..)
    , mkDescribeRoutingProfileResponse
    -- ** Response lenses
    , drprrsRoutingProfile
    , drprrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRoutingProfile' smart constructor.
data DescribeRoutingProfile = DescribeRoutingProfile'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRoutingProfile' value with any optional fields omitted.
mkDescribeRoutingProfile
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> DescribeRoutingProfile
mkDescribeRoutingProfile instanceId routingProfileId
  = DescribeRoutingProfile'{instanceId, routingProfileId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpInstanceId :: Lens.Lens' DescribeRoutingProfile Types.InstanceId
drpInstanceId = Lens.field @"instanceId"
{-# INLINEABLE drpInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRoutingProfileId :: Lens.Lens' DescribeRoutingProfile Types.RoutingProfileId
drpRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE drpRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

instance Core.ToQuery DescribeRoutingProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRoutingProfile where
        toHeaders DescribeRoutingProfile{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeRoutingProfile where
        type Rs DescribeRoutingProfile = DescribeRoutingProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRoutingProfileResponse' Core.<$>
                   (x Core..:? "RoutingProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRoutingProfileResponse' smart constructor.
data DescribeRoutingProfileResponse = DescribeRoutingProfileResponse'
  { routingProfile :: Core.Maybe Types.RoutingProfile
    -- ^ The routing profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRoutingProfileResponse' value with any optional fields omitted.
mkDescribeRoutingProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRoutingProfileResponse
mkDescribeRoutingProfileResponse responseStatus
  = DescribeRoutingProfileResponse'{routingProfile = Core.Nothing,
                                    responseStatus}

-- | The routing profile.
--
-- /Note:/ Consider using 'routingProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsRoutingProfile :: Lens.Lens' DescribeRoutingProfileResponse (Core.Maybe Types.RoutingProfile)
drprrsRoutingProfile = Lens.field @"routingProfile"
{-# INLINEABLE drprrsRoutingProfile #-}
{-# DEPRECATED routingProfile "Use generic-lens or generic-optics with 'routingProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DescribeRoutingProfileResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
