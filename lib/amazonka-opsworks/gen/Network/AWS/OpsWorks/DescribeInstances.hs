{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a set of instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeInstances
    (
    -- * Creating a request
      DescribeInstances (..)
    , mkDescribeInstances
    -- ** Request lenses
    , diInstanceIds
    , diLayerId
    , diStackId

    -- * Destructuring the response
    , DescribeInstancesResponse (..)
    , mkDescribeInstancesResponse
    -- ** Response lenses
    , dirrsInstances
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { instanceIds :: Core.Maybe [Core.Text]
    -- ^ An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
  , layerId :: Core.Maybe Core.Text
    -- ^ A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
  , stackId :: Core.Maybe Core.Text
    -- ^ A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstances' value with any optional fields omitted.
mkDescribeInstances
    :: DescribeInstances
mkDescribeInstances
  = DescribeInstances'{instanceIds = Core.Nothing,
                       layerId = Core.Nothing, stackId = Core.Nothing}

-- | An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceIds :: Lens.Lens' DescribeInstances (Core.Maybe [Core.Text])
diInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE diInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLayerId :: Lens.Lens' DescribeInstances (Core.Maybe Core.Text)
diLayerId = Lens.field @"layerId"
{-# INLINEABLE diLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStackId :: Lens.Lens' DescribeInstances (Core.Maybe Core.Text)
diStackId = Lens.field @"stackId"
{-# INLINEABLE diStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstances where
        toHeaders DescribeInstances{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstances where
        toJSON DescribeInstances{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceIds" Core..=) Core.<$> instanceIds,
                  ("LayerId" Core..=) Core.<$> layerId,
                  ("StackId" Core..=) Core.<$> stackId])

instance Core.AWSRequest DescribeInstances where
        type Rs DescribeInstances = DescribeInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstancesResponse' Core.<$>
                   (x Core..:? "Instances") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeInstances@ request.
--
-- /See:/ 'mkDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { instances :: Core.Maybe [Types.Instance]
    -- ^ An array of @Instance@ objects that describe the instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstancesResponse' value with any optional fields omitted.
mkDescribeInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstancesResponse
mkDescribeInstancesResponse responseStatus
  = DescribeInstancesResponse'{instances = Core.Nothing,
                               responseStatus}

-- | An array of @Instance@ objects that describe the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInstances :: Lens.Lens' DescribeInstancesResponse (Core.Maybe [Types.Instance])
dirrsInstances = Lens.field @"instances"
{-# INLINEABLE dirrsInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeInstancesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
