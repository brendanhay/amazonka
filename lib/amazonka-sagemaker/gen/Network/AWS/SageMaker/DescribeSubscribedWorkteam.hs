{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeSubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a work team provided by a vendor. It returns details about the subscription with a vendor in the AWS Marketplace.
module Network.AWS.SageMaker.DescribeSubscribedWorkteam
    (
    -- * Creating a request
      DescribeSubscribedWorkteam (..)
    , mkDescribeSubscribedWorkteam
    -- ** Request lenses
    , dswWorkteamArn

    -- * Destructuring the response
    , DescribeSubscribedWorkteamResponse (..)
    , mkDescribeSubscribedWorkteamResponse
    -- ** Response lenses
    , dswrrsSubscribedWorkteam
    , dswrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeSubscribedWorkteam' smart constructor.
newtype DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { workteamArn :: Types.WorkteamArn
    -- ^ The Amazon Resource Name (ARN) of the subscribed work team to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribedWorkteam' value with any optional fields omitted.
mkDescribeSubscribedWorkteam
    :: Types.WorkteamArn -- ^ 'workteamArn'
    -> DescribeSubscribedWorkteam
mkDescribeSubscribedWorkteam workteamArn
  = DescribeSubscribedWorkteam'{workteamArn}

-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswWorkteamArn :: Lens.Lens' DescribeSubscribedWorkteam Types.WorkteamArn
dswWorkteamArn = Lens.field @"workteamArn"
{-# INLINEABLE dswWorkteamArn #-}
{-# DEPRECATED workteamArn "Use generic-lens or generic-optics with 'workteamArn' instead"  #-}

instance Core.ToQuery DescribeSubscribedWorkteam where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSubscribedWorkteam where
        toHeaders DescribeSubscribedWorkteam{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.DescribeSubscribedWorkteam")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSubscribedWorkteam where
        toJSON DescribeSubscribedWorkteam{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WorkteamArn" Core..= workteamArn)])

instance Core.AWSRequest DescribeSubscribedWorkteam where
        type Rs DescribeSubscribedWorkteam =
             DescribeSubscribedWorkteamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSubscribedWorkteamResponse' Core.<$>
                   (x Core..: "SubscribedWorkteam") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { subscribedWorkteam :: Types.SubscribedWorkteam
    -- ^ A @Workteam@ instance that contains information about the work team.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribedWorkteamResponse' value with any optional fields omitted.
mkDescribeSubscribedWorkteamResponse
    :: Types.SubscribedWorkteam -- ^ 'subscribedWorkteam'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeSubscribedWorkteamResponse
mkDescribeSubscribedWorkteamResponse subscribedWorkteam
  responseStatus
  = DescribeSubscribedWorkteamResponse'{subscribedWorkteam,
                                        responseStatus}

-- | A @Workteam@ instance that contains information about the work team.
--
-- /Note:/ Consider using 'subscribedWorkteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrrsSubscribedWorkteam :: Lens.Lens' DescribeSubscribedWorkteamResponse Types.SubscribedWorkteam
dswrrsSubscribedWorkteam = Lens.field @"subscribedWorkteam"
{-# INLINEABLE dswrrsSubscribedWorkteam #-}
{-# DEPRECATED subscribedWorkteam "Use generic-lens or generic-optics with 'subscribedWorkteam' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrrsResponseStatus :: Lens.Lens' DescribeSubscribedWorkteamResponse Core.Int
dswrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dswrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
