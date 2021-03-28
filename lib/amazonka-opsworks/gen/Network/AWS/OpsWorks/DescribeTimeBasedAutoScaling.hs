{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes time-based auto scaling configurations for specified instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
    (
    -- * Creating a request
      DescribeTimeBasedAutoScaling (..)
    , mkDescribeTimeBasedAutoScaling
    -- ** Request lenses
    , dtbasInstanceIds

    -- * Destructuring the response
    , DescribeTimeBasedAutoScalingResponse (..)
    , mkDescribeTimeBasedAutoScalingResponse
    -- ** Response lenses
    , dtbasrrsTimeBasedAutoScalingConfigurations
    , dtbasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTimeBasedAutoScaling' smart constructor.
newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'
  { instanceIds :: [Core.Text]
    -- ^ An array of instance IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTimeBasedAutoScaling' value with any optional fields omitted.
mkDescribeTimeBasedAutoScaling
    :: DescribeTimeBasedAutoScaling
mkDescribeTimeBasedAutoScaling
  = DescribeTimeBasedAutoScaling'{instanceIds = Core.mempty}

-- | An array of instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasInstanceIds :: Lens.Lens' DescribeTimeBasedAutoScaling [Core.Text]
dtbasInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE dtbasInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

instance Core.ToQuery DescribeTimeBasedAutoScaling where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTimeBasedAutoScaling where
        toHeaders DescribeTimeBasedAutoScaling{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DescribeTimeBasedAutoScaling")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTimeBasedAutoScaling where
        toJSON DescribeTimeBasedAutoScaling{..}
          = Core.object
              (Core.catMaybes [Core.Just ("InstanceIds" Core..= instanceIds)])

instance Core.AWSRequest DescribeTimeBasedAutoScaling where
        type Rs DescribeTimeBasedAutoScaling =
             DescribeTimeBasedAutoScalingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTimeBasedAutoScalingResponse' Core.<$>
                   (x Core..:? "TimeBasedAutoScalingConfigurations") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeTimeBasedAutoScaling@ request.
--
-- /See:/ 'mkDescribeTimeBasedAutoScalingResponse' smart constructor.
data DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse'
  { timeBasedAutoScalingConfigurations :: Core.Maybe [Types.TimeBasedAutoScalingConfiguration]
    -- ^ An array of @TimeBasedAutoScalingConfiguration@ objects that describe the configuration for the specified instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTimeBasedAutoScalingResponse' value with any optional fields omitted.
mkDescribeTimeBasedAutoScalingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTimeBasedAutoScalingResponse
mkDescribeTimeBasedAutoScalingResponse responseStatus
  = DescribeTimeBasedAutoScalingResponse'{timeBasedAutoScalingConfigurations
                                            = Core.Nothing,
                                          responseStatus}

-- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe the configuration for the specified instances.
--
-- /Note:/ Consider using 'timeBasedAutoScalingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasrrsTimeBasedAutoScalingConfigurations :: Lens.Lens' DescribeTimeBasedAutoScalingResponse (Core.Maybe [Types.TimeBasedAutoScalingConfiguration])
dtbasrrsTimeBasedAutoScalingConfigurations = Lens.field @"timeBasedAutoScalingConfigurations"
{-# INLINEABLE dtbasrrsTimeBasedAutoScalingConfigurations #-}
{-# DEPRECATED timeBasedAutoScalingConfigurations "Use generic-lens or generic-optics with 'timeBasedAutoScalingConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasrrsResponseStatus :: Lens.Lens' DescribeTimeBasedAutoScalingResponse Core.Int
dtbasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtbasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
